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
import Dion.Mithril.ImmutableDB
import Dion.Mithril.Replay
import Dion.CLI.Args
import Dion.CLI.Query
import Dion.CLI.SPO
import Dion.Monitoring.Server
import Dion.Consensus.Praos.SPOKeys
import Dion.Consensus.Praos.ForgeLoop
import Dion.Consensus.Praos.BlockAnnounce
import Dion.Network.N2C.Server
import Dion.Ledger.State
import Dion.Ledger.Certificate
import Dion.Ledger.Snapshot
import Dion.Consensus.Praos.StakeDistribution
import Dion.Node.RelayNode

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
open Dion.CLI.Args
open Dion.Node

/-- Run the relay node with the given config -/
def runNode (config : NodeConfig) : IO Unit := do
  let networkName := toString config.network

  let proposal := match config.customMagic with
    | some magic => createProposal (.Custom magic)
    | none => match config.network with
      | .preprod   => createPreprodProposal
      | .preview   => createPreviewProposal
      | .sanchonet => createSanchoNetProposal
      | .mainnet   => createMainnetProposal

  -- Open ChainDB early so Mithril can save sync state into it
  let chainDb ← ChainDB.open {}

  -- Mithril fast-sync if requested
  let mut syncOrigin : Dion.TUI.State.SyncOrigin := .genesis
  let mut replayedLedger : Option Dion.Ledger.State.LedgerState := none
  -- Epoch nonce derived from ImmutableDB replay (populated when replay runs)
  let mut replayedEpochNonce : Option String := none
  if config.mithrilSync then
    let mithrilNetwork := match config.network with
      | .preprod => Dion.Mithril.Types.MithrilNetwork.preprod
      | .preview => Dion.Mithril.Types.MithrilNetwork.preview
      | _ => Dion.Mithril.Types.MithrilNetwork.mainnet
    let mithrilConfig := Dion.Mithril.Types.AggregatorConfig.default mithrilNetwork
    let dataDir := s!"./data/{networkName.toLower}"
    IO.println s!"[mithril] Starting fast-sync for {networkName}..."
    let result ← Dion.Mithril.Client.mithrilSync mithrilConfig dataDir
    match result with
    | .error e =>
      IO.eprintln s!"[mithril] Fast-sync failed: {e}"
      IO.eprintln "[mithril] Falling back to normal peer sync..."
    | .ok syncResult =>
      IO.println s!"[mithril] Restored to epoch={syncResult.snapshot.beacon.epoch}, immutable={syncResult.snapshot.beacon.immutableFileNumber}"
      IO.println s!"[mithril] Chain tip: slot={syncResult.tipSlot}, hash={syncResult.tipPoint.hash.size}B"
      syncOrigin := .mithril syncResult.snapshot.beacon.epoch syncResult.snapshot.beacon.immutableFileNumber syncResult.snapshot.createdAt syncResult.snapshot.digest

      -- Read the actual ImmutableDB tip — this is where replay ends, and where peer
      -- sync must resume from so the volatile gap gets filled before we hit live blocks.
      -- (syncResult.tipSlot is the snapshot network tip, which is *ahead* of the
      -- immutable DB, so using it would skip the volatile gap and leave missing UTxOs.)
      let immutableTip ← Dion.Mithril.ImmutableDB.readImmutableTip syncResult.dbPath
      let (immTipSlot, immTipHash) ← match immutableTip with
        | some t => do
          IO.println s!"[mithril] ImmutableDB tip: slot={t.slot} — peer sync will resume from here"
          pure (t.slot, t.headerHash)
        | none => do
          IO.eprintln "[mithril] Could not read ImmutableDB tip, falling back to snapshot tip"
          pure (syncResult.tipSlot, syncResult.tipPoint.hash)

      -- Try loading UTxO snapshot first (fast path)
      -- Snapshot stores lastSlot = ImmutableDB tip slot (set during replay)
      let snapshotFile := s!"./data/{networkName.toLower}/utxo-snapshot.dat"
      let loaded ← Dion.Ledger.Snapshot.loadSnapshot snapshotFile
      let initialState := { Dion.Ledger.State.LedgerState.initial with
        treasury := if networkName == "Preprod" then 1766738361646723 else 0
        reserves := if networkName == "Preprod" then 13181724972929461 else 0
        protocolParams := { Dion.Ledger.State.ProtocolParamsState.mainnetDefaults with
          networkId := config.network.networkId } }
      match loaded with
      | some cachedState =>
        if cachedState.lastSlot == immTipSlot then
          IO.println s!"[snapshot] Using cached UTxO set (slot={cachedState.lastSlot} matches ImmutableDB tip) — skipping replay"
          replayedLedger := some cachedState
        else
          IO.println s!"[snapshot] Stale snapshot (slot={cachedState.lastSlot}, ImmutableDB tip={immTipSlot}) — replaying ImmutableDB..."
          let (finalState, replayNonce, replayEpoch) ← Dion.Mithril.Replay.replayImmutableDB
            syncResult.dbPath initialState Dion.Mithril.Replay.defaultReplayReporter 0 networkName
          IO.println s!"[replay] UTxO set reconstructed: {finalState.utxo.size} entries, epoch nonce derived (epoch {replayEpoch})"
          Dion.Ledger.Snapshot.createSnapshot finalState ⟨s!"./data/{networkName.toLower}"⟩
          replayedLedger := some finalState
          replayedEpochNonce := some (Dion.Network.Crypto.bytesToHex replayNonce)
      | none =>
        -- No snapshot — replay ImmutableDB to build UTxO set
        IO.println "[replay] No UTxO snapshot found, replaying ImmutableDB..."
        let (finalState, replayNonce, replayEpoch) ← Dion.Mithril.Replay.replayImmutableDB
          syncResult.dbPath initialState Dion.Mithril.Replay.defaultReplayReporter 0 networkName
        IO.println s!"[replay] UTxO set reconstructed: {finalState.utxo.size} entries, epoch nonce derived (epoch {replayEpoch})"
        -- Save snapshot for next startup
        Dion.Ledger.Snapshot.createSnapshot finalState ⟨s!"./data/{networkName.toLower}"⟩
        replayedLedger := some finalState
        replayedEpochNonce := some (Dion.Network.Crypto.bytesToHex replayNonce)

      -- Save ImmutableDB tip as sync state so peer sync covers the volatile gap
      chainDb.saveSyncState immTipSlot 0 immTipHash
      IO.println s!"[mithril] Sync state saved — peer sync will resume from ImmutableDB tip (slot={immTipSlot})"

  -- Load SPO keys if block production is configured
  let mut spoForgeParams : Option Dion.Consensus.Praos.BlockForge.ForgeParams := none
  if let some keyDir := config.spoKeyDir then
    let paths := Dion.Consensus.Praos.SPOKeys.SPOKeyPaths.default keyDir
    Dion.Consensus.Praos.SPOKeys.printKeyInfo paths
    let protocolMajor := config.customProtocolMajor.getD 10
    let keysResult ← Dion.Consensus.Praos.SPOKeys.loadSPOKeys paths protocolMajor
    match keysResult with
    | .error e =>
      IO.eprintln s!"[spo] Failed to load SPO keys: {e}"
      IO.eprintln "[spo] Running as relay-only node."
    | .ok fp =>
      IO.println s!"[spo] SPO keys loaded successfully."
      IO.println s!"[spo] Pool ID (hex): {Dion.Crypto.TextEnvelope.encodeHex fp.poolId}"
      IO.println s!"[spo] VRF public key: {fp.vrfPublicKey.length} bytes"
      IO.println s!"[spo] OpCert sequence: {fp.operationalCert.sequenceNumber}, KES period: {fp.operationalCert.kesPeriod}"
      spoForgeParams := some fp

  -- Ensure networkId in loaded ledger state matches the actual network
  -- (snapshots saved with a bug may have networkId=1 for testnets)
  replayedLedger := replayedLedger.map fun ls =>
    { ls with protocolParams := { ls.protocolParams with networkId := config.network.networkId } }

  -- Use replay-derived epoch nonce if the user didn't provide one via --epoch-nonce.
  -- The replay computes the correct nonce from chain data — no external API needed.
  let epochNonce := match config.epochNonce with
    | some n => some n        -- CLI flag takes precedence
    | none   => replayedEpochNonce  -- from ImmutableDB replay

  -- Run as multi-peer relay node (with optional block production)
  relayNode proposal networkName config.tui config.port config.metricsPort spoForgeParams config.socketPath epochNonce chainDb syncOrigin replayedLedger config.skipToTip config.extraPeers config.externalAddr config.customSystemStart config.customEpochLength config.customMagic

/-- Handle query subcommands (reads status file from a running node) -/
def runQuery (target : QueryTarget) : IO Unit := do
  let statusFile := Dion.CLI.Query.statusFilePath
  let fileExists ← System.FilePath.pathExists statusFile
  if !fileExists then
    IO.eprintln "Error: node status file not found. Is the node running?"
    IO.eprintln s!"Expected: {statusFile}"
    IO.Process.exit 1
  match target with
  | .tip => Dion.CLI.Query.queryTip
  | .peers => Dion.CLI.Query.queryPeers
  | .mempool => Dion.CLI.Query.queryMempool

/-- Replay saved block CBOR files and validate every tx (no UTxO existence check).
    Usage: dion replay failed_blocks/block_*.cbor -/
def runReplay (paths : List String) : IO Unit := do
  if paths.isEmpty then
    IO.println "Usage: dion replay <block.cbor> [block2.cbor ...]"
    return
  -- Expand directories: use ls to find .cbor files
  let mut allFiles : List String := []
  for p in paths do
    if p.endsWith "/" || p.endsWith "failed_blocks" then
      let output ← IO.Process.output { cmd := "ls", args := #[p] }
      for line in output.stdout.splitOn "\n" do
        if line.endsWith ".cbor" then
          let path := if p.endsWith "/" then s!"{p}{line}" else s!"{p}/{line}"
          allFiles := path :: allFiles
    else
      allFiles := p :: allFiles
  allFiles := allFiles.reverse
  IO.println s!"Replaying {allFiles.length} block(s)...\n"

  -- Minimal ledger state with preprod params
  let state : Dion.Ledger.State.LedgerState := { Dion.Ledger.State.LedgerState.initial with
    protocolParams := { Dion.Ledger.State.ProtocolParamsState.mainnetDefaults with
      maxValueSize := 5000, networkId := 0 } }

  for file in allFiles do
    let blockBytes ← IO.FS.readBinFile file
    IO.println s!"── {file} ({blockBytes.size} bytes) ──"

    let parsed ← Dion.Network.ConwayBlock.parseConwayBlockBodyIO blockBytes
    match parsed with
    | none => IO.println "  ✗ Failed to parse block body CBOR"
    | some body => do
      IO.println s!"  txs={body.transactions.length} invalidTxs={body.invalidTxs}"
      let mut txIdx := 0
      for tx in body.transactions do
        let isInvalid := body.invalidTxs.contains txIdx
        let tag := if isInvalid then "[PHASE2-INVALID] " else ""
        IO.println s!"\n  {tag}tx[{txIdx}]: fee={tx.body.fee} inputs={tx.body.inputs.length} outputs={tx.body.outputs.length} serializedSize={tx.serializedSize}"
        IO.println s!"    mint={tx.body.mint.length} withdrawals={tx.body.withdrawals.length} certs={tx.body.certificates.length}"
        IO.println s!"    vkeys={tx.witnesses.vkeyWitnesses.length} bootstrap={tx.witnesses.bootstrapWitnesses.length} redeemers={tx.witnesses.redeemers.length}"
        IO.println s!"    native={tx.witnesses.nativeScripts.length} plutusV1={tx.witnesses.plutusV1Scripts.length} plutusV2={tx.witnesses.plutusV2Scripts.length} plutusV3={tx.witnesses.plutusV3Scripts.length}"
        if let some ttl := tx.body.ttl then IO.println s!"    ttl={ttl}"
        if let some vf := tx.body.validityIntervalStart then IO.println s!"    validFrom={vf}"
        -- Print raw bytes info for script debugging
        for (i, s) in (List.range tx.witnesses.plutusV1Scripts.length).zip tx.witnesses.plutusV1Scripts do
          IO.println s!"    plutusV1[{i}]: {s.size} bytes, first4={bytesToHex (s.extract 0 (min 4 s.size))}"
        for (i, s) in (List.range tx.witnesses.plutusV2Scripts.length).zip tx.witnesses.plutusV2Scripts do
          IO.println s!"    plutusV2[{i}]: {s.size} bytes, first4={bytesToHex (s.extract 0 (min 4 s.size))}"
        for (i, s) in (List.range tx.witnesses.plutusV3Scripts.length).zip tx.witnesses.plutusV3Scripts do
          IO.println s!"    plutusV3[{i}]: {s.size} bytes, first4={bytesToHex (s.extract 0 (min 4 s.size))}"

        if !isInvalid then
          -- Validate (skip UTxO existence — use empty UTxO set but don't fail on missing inputs)
          -- Extract slot from filename if possible
          let slot := match file.splitOn "_slot_" with
            | [_, rest] => (rest.splitOn ".").head!.toNat?.getD 0
            | _ => 0

          -- Fee check
          let feeParams := state.protocolParams.feeParams
          let effectiveSize := if tx.serializedSize > 0 then tx.serializedSize else tx.body.rawBytes.size
          let minFee := Dion.Ledger.Fee.totalMinFee feeParams effectiveSize tx.witnesses.redeemers
          if tx.body.fee < minFee then
            IO.println s!"    ✗ FEE: paid={tx.body.fee} min={minFee} (size={effectiveSize})"
          else
            IO.println s!"    ✓ FEE: paid={tx.body.fee} >= min={minFee}"

          -- Signature check
          let sigResult ← Dion.Ledger.Validation.validateSignatures state.utxo tx.body tx.witnesses
          match sigResult with
          | .ok () => IO.println s!"    ✓ SIGNATURES"
          | .error e => IO.println s!"    ✗ SIGNATURES: {repr e}"

          -- Native script check
          let mut signerKeyHashes : List ByteArray := []
          for w in tx.witnesses.vkeyWitnesses do
            let keyHash ← blake2b_224 w.vkey
            signerKeyHashes := keyHash :: signerKeyHashes
          match Dion.Ledger.Validation.validateNativeScripts tx.witnesses signerKeyHashes slot with
          | .ok () => IO.println s!"    ✓ NATIVE SCRIPTS"
          | .error e => IO.println s!"    ✗ NATIVE SCRIPTS: {repr e}"

          -- Output value size check (maxValueSize)
          let mut outputSizeOk := true
          for (oi, output) in (List.range tx.body.outputs.length).zip tx.body.outputs do
            let sz := output.rawValueBytes.size
            if sz > 0 && sz > state.protocolParams.maxValueSize then
              IO.println s!"    ✗ OUTPUT SIZE: idx={oi} size={sz} > max={state.protocolParams.maxValueSize}"
              outputSizeOk := false
          if outputSizeOk then
            IO.println s!"    ✓ OUTPUT SIZE (max rawValue={tx.body.outputs.foldl (fun acc o => max acc o.rawValueBytes.size) 0})"

          -- Extraneous script witness check (withdrawal + cert sources)
          let mut neededHashes : List ByteArray := []
          for asset in tx.body.mint do
            neededHashes := asset.policyId :: neededHashes
          for (rewardAddr, _) in tx.body.withdrawals do
            if rewardAddr.size >= 29 && rewardAddr[0]!.toNat &&& 0x10 != 0 then
              neededHashes := (rewardAddr.extract 1 29) :: neededHashes
          let certRedeemerIdx := tx.witnesses.redeemers.filterMap fun r =>
            if r.tag == .Cert then some r.index else none
          for (ci, cert) in (List.range tx.body.certificates.length).zip tx.body.certificates do
            if certRedeemerIdx.contains ci then
              let h := match cert with
                | .stakeKeyDeregistration _ h | .stakeDelegation _ h _
                | .conwayRegistration _ h _ | .conwayDeregistration _ h _
                | .stakeRegDelegation _ h _ _ | .voteRegDelegation _ h _ _
                | .stakeVoteRegDelegation _ h _ _ _ | .stakeKeyRegistration _ h => some h
                | _ => none
              match h with | some h => neededHashes := h :: neededHashes | none => pure ()
          let mut scriptOk := true
          for scriptBytes in tx.witnesses.plutusV1Scripts ++ tx.witnesses.plutusV2Scripts ++ tx.witnesses.plutusV3Scripts do
            let scriptHash ← blake2b_224 scriptBytes
            -- Check non-UTxO sources (minting + withdrawal + cert); UTxO spending needs full state
            if !neededHashes.any (· == scriptHash) then
              IO.println s!"    ⚠ SCRIPT WITNESS: hash={bytesToHex scriptHash} not in non-UTxO needed set (may match spending input)"
              scriptOk := false
          if scriptOk && !(tx.witnesses.plutusV1Scripts ++ tx.witnesses.plutusV2Scripts ++ tx.witnesses.plutusV3Scripts).isEmpty then
            IO.println s!"    ✓ SCRIPT WITNESSES (all matched non-UTxO sources)"

          -- Datum presence check
          let mut datumInfo : List String := []
          for (oi, output) in (List.range tx.body.outputs.length).zip tx.body.outputs do
            if output.datum.isSome then datumInfo := s!"out[{oi}]:hash" :: datumInfo
            if output.inlineDatum.isSome then datumInfo := s!"out[{oi}]:inline" :: datumInfo
          if !datumInfo.isEmpty then
            IO.println s!"    ℹ DATUMS: {datumInfo} witness_datums={tx.witnesses.datums.length}"

          -- Plutus script check
          let txHash ← blake2b_256 tx.body.rawBytes
          match ← Dion.Plutus.Evaluate.evaluateTransactionScriptsIO tx.body tx.witnesses state.utxo txHash with
          | .ok () => IO.println s!"    ✓ PLUTUS SCRIPTS"
          | .error e => IO.println s!"    ✗ PLUTUS SCRIPTS: {e}"

        txIdx := txIdx + 1
      IO.println ""

def main (args : List String) : IO Unit := do
  match parseArgs args with
  | .help => Dion.CLI.Args.printUsage
  | .version => IO.println "Dion v0.1.0"
  | .query target => runQuery target
  | .replay paths => runReplay paths
  | .run config => runNode config
  | .spoKeygen cfg =>
      Dion.CLI.SPO.spoKeygen cfg.keyDir cfg.kesPeriod
  | .spoMetadata cfg =>
      Dion.CLI.SPO.spoMetadata cfg.name cfg.ticker cfg.description cfg.homepage cfg.outFile
  | .spoRegister cfg =>
      Dion.CLI.SPO.spoRegister cfg.keyDir cfg.relayHost cfg.relayPort
        cfg.pledgeLovelace cfg.costLovelace (socketPath := cfg.socketPath)
        (metadataUrl := cfg.metadataUrl) (metadataFile := cfg.metadataFile)
        (update := cfg.update)
  | .spoRotateKES cfg =>
      Dion.CLI.SPO.rotateKES cfg.keyDir cfg.kesPeriod
