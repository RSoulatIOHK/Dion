import Cleanode.Network.Basic
import Cleanode.Network.Socket
import Cleanode.Network.Handshake
import Cleanode.Network.Multiplexer
import Cleanode.Network.ChainSync
import Cleanode.Network.Byron
import Cleanode.Network.Shelley
import Cleanode.Network.BlockFetch
import Cleanode.Network.BlockFetchClient
import Cleanode.Network.ConwayBlock
import Cleanode.Network.Crypto
import Cleanode.Network.Bech32
import Cleanode.Network.TxSubmission2
import Cleanode.Network.Mempool
import Cleanode.Network.PeerSharing
import Cleanode.Network.PeerDb
import Cleanode.Network.PeerConnection
import Cleanode.Network.ConnectionManager
import Cleanode.Network.MuxDispatcher
import Cleanode.Network.HandshakeServer
import Cleanode.Config.Topology
import Cleanode.Config.Genesis
import Cleanode.Storage.BlockStore
import Cleanode.Storage.ChainDB
import Cleanode.Storage.Database
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Consensus.Praos.ConsensusState
import Cleanode.Crypto.VRF.ECVRF
import Cleanode.Crypto.Sign.Ed25519.Signature
import Std.Sync
import Pigment
import Cleanode.TUI.State
import Cleanode.TUI.Render
import Cleanode.Mithril.Types
import Cleanode.Mithril.Client
import Cleanode.Mithril.Replay
import Cleanode.CLI.Args
import Cleanode.CLI.Query
import Cleanode.Monitoring.Server
import Cleanode.Consensus.Praos.SPOKeys
import Cleanode.Consensus.Praos.ForgeLoop
import Cleanode.Consensus.Praos.BlockAnnounce
import Cleanode.Network.N2C.Server
import Cleanode.Ledger.State
import Cleanode.Ledger.Certificate
import Cleanode.Ledger.Snapshot
import Cleanode.Consensus.Praos.StakeDistribution
import Cleanode.Node.InboundPeer

open Cleanode.Network
open Cleanode.Network.Socket
open Cleanode.Network.Handshake
open Cleanode.Network.Multiplexer
open Cleanode.Network.ChainSync
open Cleanode.Network.Byron
open Cleanode.Network.Shelley
open Cleanode.Network.BlockFetch
open Cleanode.Network.Crypto
open Cleanode.Network.Bech32
open Cleanode.Network.BlockFetchClient
open Cleanode.Network.ConwayBlock
open Cleanode.Network.TxSubmission2
open Cleanode.Network.Mempool
open Cleanode.Network.PeerDb
open Cleanode.Network.PeerConnection
open Cleanode.Network.ConnectionManager
open Cleanode.Network.MuxDispatcher
open Cleanode.Network.PeerSharing
open Cleanode.Network.HandshakeServer
open Cleanode.Config.Topology
open Cleanode.Config.Genesis
open Cleanode.Storage.BlockStore
open Cleanode.Storage.ChainDB
open Cleanode.Storage.Database
open Cleanode.Consensus.Praos.LeaderElection
open Cleanode.Consensus.Praos.ConsensusState
open Pigment
open Cleanode.TUI.State
open Cleanode.TUI.Render
open Cleanode.Node

namespace Cleanode.Node

/-- Multi-peer relay node mode -/
def relayNode (proposal : HandshakeMessage) (networkName : String)
    (tuiMode : Bool := false) (listenPort : UInt16 := 3001)
    (metricsPort : Option UInt16 := none)
    (forgeParams : Option Cleanode.Consensus.Praos.BlockForge.ForgeParams := none)
    (socketPath : Option String := none)
    (epochNonceSeed : Option String := none)
    (chainDb : ChainDB)
    (syncOrigin : Cleanode.TUI.State.SyncOrigin := .genesis)
    (replayedLedgerState : Option Cleanode.Ledger.State.LedgerState := none) : IO Unit := do

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
  -- Merge: bootstrap peers + relay peers
  let peerAddrs := bootstrapPeers ++ relayPeers

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
    | none => { Cleanode.Ledger.State.LedgerState.initial with
        treasury := if networkName == "Preprod" then 1766738361646723 else 0
        reserves := if networkName == "Preprod" then 13181724972929461 else 0
        protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
          networkId := if networkName == "Preprod" then 0 else 1 } }
  let ledgerStateRef ← Std.Mutex.new initialLedgerState

  -- Shared consensus state (epoch nonces, stake snapshots, chain selection)
  let genesis : Cleanode.Config.Genesis.ShelleyGenesis := {
    epochLength := 432000
    slotLength := 1
    activeSlotsCoeff := { numerator := 1, denominator := 20 }
    securityParam := 2160
    maxLovelaceSupply := 45000000000000000
    networkMagic := match networkName with | "Preprod" => 1 | "Preview" => 2 | "SanchoNet" => 4 | _ => 764824073
    networkId := networkName
    protocolParams := none
  }
  let initialCs := ConsensusState.initial genesis
  -- Try to restore consensus nonce state from previous run
  let cs ← match ← loadConsensusState with
    | some (epoch, nonce, evolving) =>
      IO.println s!"[consensus] Restored: epoch={epoch}, nonce={nonce.size}B"
      pure { initialCs with
        currentEpoch := epoch
        epochFirstSlot := epoch * initialCs.epochLength
        epochNonce := nonce
        evolvingNonce := evolving }
    | none =>
      IO.println "[consensus] No saved state — starting fresh"
      pure initialCs
  let consensusRef ← IO.mkRef cs

  -- Apply epoch nonce seed from CLI if provided (--epoch-nonce HEX)
  if let some hexStr := epochNonceSeed then
    let hexClean := hexStr.trim
    if hexClean.length == 64 then
      let nonceBytes := Cleanode.Network.ChainSync.hexToBytes hexClean
      consensusRef.modify fun c => { c with epochNonce := nonceBytes }
      IO.println s!"[consensus] Epoch nonce seeded from CLI: {hexClean.take 16}..."
    else
      IO.eprintln s!"[consensus] Warning: --epoch-nonce must be 64 hex chars (got {hexClean.length}), ignoring"

  -- Seed initial stake snapshot from ledger state (don't wait for epoch boundary)
  let initLs ← ledgerStateRef.atomically (fun ref => ref.get)
  let initSnap := Cleanode.Consensus.Praos.StakeDistribution.buildSnapshotFromLedger initLs
  if initSnap.totalStake > 0 then do
    consensusRef.modify fun c => { c with stakeSnapshot := initSnap }
    IO.println s!"[consensus] Initial stake snapshot: {initSnap.poolStakes.length} pools, {initSnap.totalStake} lovelace"
  else
    IO.println "[consensus] No stake data yet — forge loop will wait for epoch boundary"

  -- TUI state (always created — also used by metrics server)
  let now ← Cleanode.TUI.Render.nowMs
  let tuiStateRefInner ← IO.mkRef ({ TUIState.empty networkName now with syncOrigin := syncOrigin })
  let tuiStateRef : Option (IO.Ref TUIState) := some tuiStateRefInner

  -- Start metrics server if configured
  if let some mp := metricsPort then
    let _ ← Cleanode.Monitoring.Server.startMetricsServer mp tuiStateRefInner

  -- Start periodic status file writer (for `cleanode query` commands)
  let _ ← IO.asTask (Cleanode.CLI.Query.statusFileWriterLoop tuiStateRefInner)

  -- In TUI mode: launch the render loop; otherwise: print banner
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
  let registryRef ← IO.mkRef Cleanode.Consensus.Praos.BlockAnnounce.PeerRegistry.empty

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
        Cleanode.Network.N2C.Server.n2cServerLoop sockPath ledgerStateRef mempoolRef network tuiMode
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
  -- Launch per-peer reconnect loops — each task owns its connection lifecycle
  for (host, port) in peerAddrs do
    let task ← IO.asTask (do
      try
        peerReconnectLoop host port proposal chainDb (some discoveryRef) (some seenBlocksRef) tuiStateRef (some mempoolRef) (some consensusRef) (some ledgerStateRef) (some prevHashRef) (some blockNoRef)
      catch e =>
        tuiLog tuiStateRef s!"Peer {host}:{port}: {e}")
    tasks := tasks ++ [task]

  -- Launch peer discovery spawner
  let spawnerTask ← IO.asTask (do
    try
      peerSpawnerLoop discoveryRef proposal chainDb (some seenBlocksRef) tuiStateRef (some mempoolRef) (some consensusRef) (some ledgerStateRef) (some prevHashRef) (some blockNoRef)
    catch e =>
      tuiLog tuiStateRef s!"Peer spawner: {e}")
  tasks := tasks ++ [spawnerTask]

  -- Start block production forge loop if SPO keys are configured
  if let some fp := forgeParams then
    let clock := match networkName with
      | "Preprod" => Cleanode.Consensus.Praos.ForgeLoop.SlotClock.preprod
      | "Preview" => Cleanode.Consensus.Praos.ForgeLoop.SlotClock.preview
      | _ => Cleanode.Consensus.Praos.ForgeLoop.SlotClock.mainnet
    let (forgeTask, _forgeStateRef, forgedBlocksRef) ←
      Cleanode.Consensus.Praos.ForgeLoop.startForgeLoop fp clock consensusRef mempoolRef ledgerStateRef prevHashRef blockNoRef
    tasks := tasks ++ [forgeTask]
    -- Start block announcement loop (broadcasts forged blocks to shared registry)
    let announceTask ← Cleanode.Consensus.Praos.BlockAnnounce.startAnnouncementLoop registryRef forgedBlocksRef
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

end Cleanode.Node
