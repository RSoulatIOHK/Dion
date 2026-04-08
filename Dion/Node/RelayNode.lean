import Dion.Network.Basic
import Dion.Network.Socket
import Dion.Network.Handshake
import Dion.Network.Multiplexer
import Dion.Network.ChainSync
import Dion.Network.Byron
import Dion.Network.Shelley
import Dion.Network.BlockFetch
import Dion.Network.BlockFetchClient
import Dion.Network.ConwayBlock
import Dion.Network.Crypto
import Dion.Network.Bech32
import Dion.Network.TxSubmission2
import Dion.Network.Mempool
import Dion.Network.PeerSharing
import Dion.Network.PeerDb
import Dion.Network.PeerConnection
import Dion.Network.ConnectionManager
import Dion.Network.MuxDispatcher
import Dion.Network.HandshakeServer
import Dion.Config.Topology
import Dion.Config.Genesis
import Dion.Storage.BlockStore
import Dion.Storage.ChainDB
import Dion.Storage.Database
import Dion.Consensus.Praos.LeaderElection
import Dion.Consensus.Praos.ConsensusState
import Dion.Crypto.VRF.ECVRF
import Dion.Crypto.Sign.Ed25519.Signature
import Std.Sync
import Pigment
import Dion.TUI.State
import Dion.TUI.Render
import Dion.Mithril.Types
import Dion.Mithril.Client
import Dion.Mithril.Replay
import Dion.CLI.Args
import Dion.CLI.Query
import Dion.Monitoring.Server
import Dion.Consensus.Praos.SPOKeys
import Dion.Consensus.Praos.ForgeLoop
import Dion.Consensus.Praos.BlockAnnounce
import Dion.Network.N2C.Server
import Dion.Ledger.State
import Dion.Ledger.Certificate
import Dion.Ledger.Snapshot
import Dion.Consensus.Praos.StakeDistribution
import Dion.Node.InboundPeer

open Dion.Network
open Dion.Network.Socket
open Dion.Network.Handshake
open Dion.Network.Multiplexer
open Dion.Network.ChainSync
open Dion.Network.Byron
open Dion.Network.Shelley
open Dion.Network.BlockFetch
open Dion.Network.Crypto
open Dion.Network.Bech32
open Dion.Network.BlockFetchClient
open Dion.Network.ConwayBlock
open Dion.Network.TxSubmission2
open Dion.Network.Mempool
open Dion.Network.PeerDb
open Dion.Network.PeerConnection
open Dion.Network.ConnectionManager
open Dion.Network.MuxDispatcher
open Dion.Network.PeerSharing
open Dion.Network.HandshakeServer
open Dion.Config.Topology
open Dion.Config.Genesis
open Dion.Storage.BlockStore
open Dion.Storage.ChainDB
open Dion.Storage.Database
open Dion.Consensus.Praos.LeaderElection
open Dion.Consensus.Praos.ConsensusState
open Pigment
open Dion.TUI.State
open Dion.TUI.Render
open Dion.Node

namespace Dion.Node

/-- Multi-peer relay node mode -/
def relayNode (proposal : HandshakeMessage) (networkName : String)
    (tuiMode : Bool := false) (listenPort : UInt16 := 3001)
    (metricsPort : Option UInt16 := none)
    (forgeParams : Option Dion.Consensus.Praos.BlockForge.ForgeParams := none)
    (socketPath : Option String := none)
    (epochNonceSeed : Option String := none)
    (chainDb : ChainDB)
    (syncOrigin : Dion.TUI.State.SyncOrigin := .genesis)
    (replayedLedgerState : Option Dion.Ledger.State.LedgerState := none)
    (skipToTip : Bool := false)
    (extraPeers : List (String × UInt16) := [])
    (externalAddr : Option (String × UInt16) := none)
    (customSystemStart : Option UInt64 := none)
    (customEpochLength : Option Nat := none)
    (customMagic : Option Nat := none) : IO Unit := do

  -- Build topology from bootstrap peers
  let bootstrapPeers := match networkName with
    | "Mainnet"   => mainnetBootstrapPeers
    | "Preprod"   => preprodBootstrapPeers
    | "Preview"   => previewBootstrapPeers
    | "SanchoNet" => sanchonetBootstrapPeers
    | _           => mainnetBootstrapPeers

  -- Add curated relay peers (from Cardano peer snapshot — real SPO relays)
  let relayPeers := match networkName with
    | "Mainnet" => mainnetRelayPeers
    | _ => []  -- Only mainnet has curated relays for now
  -- Merge: bootstrap peers + relay peers + extra peers from CLI (--peer flags)
  let peerAddrs := bootstrapPeers ++ relayPeers ++ extraPeers

  -- Shared peer discovery state (plain data, safe across threads)
  let discoveryRef ← IO.mkRef ({
    discovered := []
    known := peerAddrs
  } : DiscoveryState)

  -- Shared dedup: tracks block numbers already displayed, prevents duplicate output
  let seenBlocksRef ← IO.mkRef (some ([] : List Nat))

  -- Shared mempool (all peers read/write via IO.Ref)
  let mempoolRef ← IO.mkRef (Mempool.empty {})

  -- Shared ledger state (for N2C queries from cardano-cli)
  -- Use replayed state from Mithril if available, otherwise start empty
  let initialLedgerState := match replayedLedgerState with
    | some ls => ls
    | none => { Dion.Ledger.State.LedgerState.initial with
        treasury := if networkName == "Preprod" then 1766738361646723 else 0
        reserves := if networkName == "Preprod" then 13181724972929461 else 0
        protocolParams := { Dion.Ledger.State.ProtocolParamsState.mainnetDefaults with
          networkId := match networkName with | "Mainnet" => 1 | _ => 0 } }
  let ledgerStateRef ← Std.Mutex.new initialLedgerState

  -- Shared consensus state (epoch nonces, stake snapshots, chain selection)
  let resolvedEpochLength := customEpochLength.getD (match networkName with
    | "Preview" => 86400
    | _ => 432000)
  let resolvedMagic := match customMagic with
    | some n => n
    | none => match networkName with | "Preprod" => 1 | "Preview" => 2 | "SanchoNet" => 4 | _ => 764824073
  let genesis : Dion.Config.Genesis.ShelleyGenesis := {
    epochLength := resolvedEpochLength
    slotLength := 1
    activeSlotsCoeff := { numerator := 1, denominator := 20 }
    securityParam := 2160
    maxLovelaceSupply := 45000000000000000
    networkMagic := resolvedMagic
    networkId := networkName
    protocolParams := none
  }
  let initialCs := ConsensusState.initial genesis
  -- Try to restore consensus nonce state from previous run
  let cs ← match ← loadConsensusState with
    | some (epoch, nonce, evolving, snapshot) =>
      let snapshotDesc := if snapshot.totalStake > 0
        then s!", stake={snapshot.totalStake} ({snapshot.poolStakes.length} pools)"
        else ""
      IO.println s!"[consensus] Restored: epoch={epoch}, nonce={nonce.size}B{snapshotDesc}"
      pure { initialCs with
        currentEpoch := epoch
        epochFirstSlot := epoch * initialCs.epochLength
        epochNonce := nonce
        evolvingNonce := evolving
        stakeSnapshot := snapshot }
    | none =>
      IO.println "[consensus] No saved state — starting fresh"
      pure initialCs
  let consensusRef ← IO.mkRef cs

  -- Sync ledger epoch + epoch length from consensus state.
  -- The UTxO snapshot is saved/loaded with mainnet defaults (epochLength=432000, epoch=0).
  -- Override with the correct values so epochForSlot doesn't fire spurious transitions.
  if cs.currentEpoch > 0 then
    ledgerStateRef.atomically fun lsRef => do
      let ls ← lsRef.get
      lsRef.set { ls with protocolParams := { ls.protocolParams with
        epoch       := cs.currentEpoch
        epochLength := resolvedEpochLength } }

  -- Apply epoch nonce seed from CLI if provided (--epoch-nonce HEX)
  if let some hexStr := epochNonceSeed then
    let hexClean := hexStr.trim
    if hexClean.length == 64 then
      let nonceBytes := Dion.Network.ChainSync.hexToBytes hexClean
      -- Also set currentEpoch so the first block doesn't fire a spurious epoch transition
      let curEpoch ← do
        let cs ← consensusRef.get
        if cs.currentEpoch > 0 then pure cs.currentEpoch
        else match ← chainDb.loadSyncState with
          | some ss => pure (ss.lastSlot / resolvedEpochLength)
          | none => pure 0
      consensusRef.modify fun c => { c with
        epochNonce := nonceBytes
        currentEpoch := if curEpoch > 0 then curEpoch else c.currentEpoch
        epochFirstSlot := if curEpoch > 0 then curEpoch * c.epochLength else c.epochFirstSlot }
      IO.println s!"[consensus] Epoch nonce seeded from CLI: {hexClean.take 16}..."
    else
      IO.eprintln s!"[consensus] Warning: --epoch-nonce must be 64 hex chars (got {hexClean.length}), ignoring"
  else
    -- Auto-fetch epoch nonce from Koios when nonce is zeros (first startup after Mithril sync)
    let cs ← consensusRef.get
    if cs.epochNonce.data.all (· == 0) then
      let curEpoch ←
        if cs.currentEpoch > 0 then pure cs.currentEpoch
        else match ← chainDb.loadSyncState with
          | some ss => pure (ss.lastSlot / resolvedEpochLength)
          | none => pure 0
      let koiosBase := match networkName with
        | "Mainnet" => "https://api.koios.rest/api/v1"
        | "Preprod" => "https://preprod.koios.rest/api/v1"
        | "Preview" => "https://preview.koios.rest/api/v1"
        | _ => ""
      if curEpoch > 0 && !koiosBase.isEmpty then
        IO.println s!"[consensus] Epoch nonce is zeros — auto-fetching from Koios (epoch {curEpoch})..."
        try
          let url := s!"{koiosBase}/epoch_params?_epoch_no={curEpoch}"
          match ← Dion.Network.Http.httpGetJson url with
          | .error e =>
            IO.eprintln s!"[consensus] Koios fetch failed: {e}"
            IO.eprintln "[consensus] Use --epoch-nonce <64-hex> to seed manually"
          | .ok body =>
            match Lean.Json.parse body with
            | .error _ => IO.eprintln "[consensus] Failed to parse Koios epoch_params response"
            | .ok json =>
              let result : Option ByteArray := do
                let arr ← json.getArr?.toOption
                let item ← arr[0]?
                let hex ← (item.getObjValAs? String "nonce").toOption
                let bytes := Dion.Network.ChainSync.hexToBytes hex
                if bytes.size == 32 then some bytes else none
              match result with
              | some nonceBytes =>
                consensusRef.modify fun c => { c with
                  epochNonce := nonceBytes
                  currentEpoch := curEpoch
                  epochFirstSlot := curEpoch * c.epochLength }
                IO.println s!"[consensus] Epoch nonce auto-seeded from Koios: {(bytesToHex nonceBytes).take 16}..."
              | none =>
                IO.eprintln "[consensus] Could not extract nonce from Koios response"
                IO.eprintln "[consensus] Use --epoch-nonce <64-hex> to seed manually"
        catch e =>
          IO.eprintln s!"[consensus] Koios request error: {e}"
          IO.eprintln "[consensus] Use --epoch-nonce <64-hex> to seed manually"

  -- Seed initial stake snapshot from ledger state (don't wait for epoch boundary)
  let initLs ← ledgerStateRef.atomically (fun ref => ref.get)
  let initSnap := Dion.Consensus.Praos.StakeDistribution.buildSnapshotFromLedger initLs
  if initSnap.totalStake > 0 then do
    consensusRef.modify fun c => { c with stakeSnapshot := initSnap }
    IO.println s!"[consensus] Initial stake snapshot: {initSnap.poolStakes.length} pools, {initSnap.totalStake} lovelace"
  else
    IO.println "[consensus] No stake data yet — forge loop will wait for epoch boundary"

  -- TUI state (always created — also used by metrics server)
  let now ← Dion.TUI.Render.nowMs
  let tuiStateRefInner ← IO.mkRef ({ TUIState.empty networkName now with syncOrigin := syncOrigin })
  let tuiStateRef : Option (IO.Ref TUIState) := some tuiStateRefInner
  -- In non-TUI mode, peer loops get `none` so displayBlock runs and connection events
  -- go to stdout instead of being silently swallowed by the TUI state.
  let peerTuiRef : Option (IO.Ref TUIState) := if tuiMode then some tuiStateRefInner else none

  -- Start metrics server if configured
  if let some mp := metricsPort then
    let _ ← Dion.Monitoring.Server.startMetricsServer mp tuiStateRefInner

  -- Start periodic status file writer (for `dion query` commands)
  let _ ← IO.asTask (Dion.CLI.Query.statusFileWriterLoop tuiStateRefInner)
  if tuiMode then
    let _ ← startTUI tuiStateRefInner
  else do
    IO.println "Dion: a Cardano LEAN 4 Node"
    IO.println "===================================="
    IO.println s!"Network: {networkName}"
    IO.println ""
    IO.println "  Chain database opened (data/chain.db)"
    IO.println s!"  Bootstrap peers: {bootstrapPeers.length}"
    if relayPeers.length > 0 then
      IO.println s!"  Relay peers (from peer snapshot): {relayPeers.length}"
    if extraPeers.length > 0 then
      IO.println s!"  Extra peers (--peer flags): {extraPeers.length}"
    IO.println s!"  Total initial peers: {peerAddrs.length}"
    IO.println ""
    IO.println "=== Relay Node Active (Ctrl+C to stop) ==="

  -- Launch inbound connection listener FIRST (before peer tasks exhaust the thread pool)
  -- NOTE: socket_listen MUST be called inside the task, not outside.
  -- Lean external objects (Socket) crash when captured across task boundaries.
  let network : NetworkMagic := match networkName with
    | "Preprod"   => .Preprod
    | "Preview"   => .Preview
    | "SanchoNet" => .SanchoNet
    | _           => .Mainnet
  -- Shared peer registry for ChainSync/BlockFetch server + announcement loop
  let registryRef ← IO.mkRef Dion.Consensus.Praos.BlockAnnounce.PeerRegistry.empty

  let mut tasks : List (Task (Except IO.Error Unit)) := []
  IO.eprintln s!"[Listen] Creating listener task for port {listenPort}..."
  let listenTask ← IO.asTask (do
    IO.eprintln s!"[Listen] Task started, attempting to bind port {listenPort}..."
    match ← socket_listen listenPort with
    | .error e =>
        IO.eprintln s!"[Listen] FAILED: {e}"
        tuiLog tuiStateRef s!"Failed to listen on port {listenPort}: {e}"
    | .ok listenSock => do
        IO.eprintln s!"[Listen] OK — listening on port {listenPort}"
        tuiLog tuiStateRef s!"Listening for inbound peers on port {listenPort}"
        try acceptLoop listenSock mempoolRef tuiStateRef registryRef network ledgerStateRef
        catch e =>
          IO.eprintln s!"[Listen] acceptLoop crashed: {e}"
          tuiLog tuiStateRef s!"Listener: {e}")
  tasks := tasks ++ [listenTask]

  -- Launch N2C server BEFORE peer tasks (peer tasks can exhaust the thread pool)
  if let some sockPath := socketPath then
    let n2cTask ← IO.asTask (do
      try
        Dion.Network.N2C.Server.n2cServerLoop sockPath ledgerStateRef mempoolRef network tuiMode
      catch e =>
        IO.eprintln s!"[n2c] Server crashed: {e}")
    tasks := tasks ++ [n2cTask]
    if !tuiMode then
      IO.println s!"  [n2c] Unix socket server started on {sockPath}"

  -- Shared chain tip state — initialize from ChainDB for forge loop + sync updates
  let (initPrevHash, initBlockNo) ← do
    match ← chainDb.loadSyncState with
    | some ss =>
      IO.println s!"[chain] Tip from DB: block #{ss.lastBlock}, slot {ss.lastSlot}"
      pure (ss.lastHash, ss.lastBlock)
    | none =>
      pure (ByteArray.mk (Array.replicate 32 0), 0)
  let prevHashRef ← IO.mkRef initPrevHash
  let blockNoRef ← IO.mkRef initBlockNo
  let checkpointRef ← IO.mkRef Dion.Ledger.State.CheckpointRing.empty
  -- Build our self-address for PeerSharing advertisement (so peers can connect back to us)
  let selfAddr : Option Dion.Network.PeerSharing.PeerAddress :=
    match externalAddr with
    | some (host, port) => some { host := host, port := port }
    | none => none
  if let some sa := selfAddr then
    IO.println s!"  [peersharing] Advertising self as {sa.host}:{sa.port}"
  -- Launch per-peer reconnect loops — each task owns its connection lifecycle
  for (host, port) in peerAddrs do
    let task ← IO.asTask (do
      try
        peerReconnectLoop host port proposal chainDb (some discoveryRef) (some seenBlocksRef) peerTuiRef (some mempoolRef) (some consensusRef) (some ledgerStateRef) (some prevHashRef) (some blockNoRef) (some checkpointRef) skipToTip selfAddr
      catch e =>
        IO.println s!"Peer {host}:{port}: {e}")
    tasks := tasks ++ [task]

  -- Launch peer discovery spawner
  let spawnerTask ← IO.asTask (do
    try
      peerSpawnerLoop discoveryRef proposal chainDb (some seenBlocksRef) peerTuiRef (some mempoolRef) (some consensusRef) (some ledgerStateRef) (some prevHashRef) (some blockNoRef) (some checkpointRef) skipToTip selfAddr
    catch e =>
      IO.println s!"Peer spawner: {e}")
  tasks := tasks ++ [spawnerTask]

  -- Start block production forge loop if SPO keys are configured
  if let some fp := forgeParams then
    let baseClock := match networkName with
      | "Preprod" => Dion.Consensus.Praos.ForgeLoop.SlotClock.preprod
      | "Preview" => Dion.Consensus.Praos.ForgeLoop.SlotClock.preview
      | _ => Dion.Consensus.Praos.ForgeLoop.SlotClock.mainnet
    -- Override with custom params when provided (e.g. local private testnet)
    let clock : Dion.Consensus.Praos.ForgeLoop.SlotClock := {
      systemStart    := customSystemStart.getD baseClock.systemStart
      slotLength     := baseClock.slotLength
      epochLength    := customEpochLength.getD baseClock.epochLength
      slotsPerKESPeriod := baseClock.slotsPerKESPeriod
    }
    let (forgeTask, _forgeStateRef, forgedBlocksRef) ←
      Dion.Consensus.Praos.ForgeLoop.startForgeLoop fp clock consensusRef mempoolRef ledgerStateRef prevHashRef blockNoRef peerTuiRef
    tasks := tasks ++ [forgeTask]
    -- Start block announcement loop (inbound subscribers + outbound push)
    let announceTask ← (Dion.Consensus.Praos.BlockAnnounce.startAnnouncementLoop
        registryRef forgedBlocksRef peerAddrs (some proposal))
    tasks := tasks ++ [announceTask]
    IO.println "  [spo] Block production + announcement loops started"

  if !tuiMode then
    IO.println s!"  Syncing from {peerAddrs.length} bootstrap peers (discovering more)...\n"

  -- Wait for all tasks (they reconnect indefinitely until Ctrl+C)
  try
    for task in tasks do
      let _ ← IO.wait task
  finally
    -- Restore terminal if in TUI mode
    if tuiMode then stopTUI

  chainDb.close

end Dion.Node
