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
import Cleanode.Node.RelayNode

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
open Cleanode.CLI.Args
open Cleanode.Node

/-- Run the relay node with the given config -/
def runNode (config : NodeConfig) : IO Unit := do
  let networkName := toString config.network

  let proposal := match config.network with
    | .preprod   => createPreprodProposal
    | .preview   => createPreviewProposal
    | .sanchonet => createSanchoNetProposal
    | .mainnet   => createMainnetProposal

  -- Open ChainDB early so Mithril can save sync state into it
  let chainDb ← ChainDB.open {}

  -- Mithril fast-sync if requested
  let mut syncOrigin : Cleanode.TUI.State.SyncOrigin := .genesis
  let mut replayedLedger : Option Cleanode.Ledger.State.LedgerState := none
  if config.mithrilSync then
    let mithrilNetwork := match config.network with
      | .preprod => Cleanode.Mithril.Types.MithrilNetwork.preprod
      | .preview => Cleanode.Mithril.Types.MithrilNetwork.preview
      | _ => Cleanode.Mithril.Types.MithrilNetwork.mainnet
    let mithrilConfig := Cleanode.Mithril.Types.AggregatorConfig.default mithrilNetwork
    let dataDir := s!"./data/{networkName.toLower}"
    IO.println s!"[mithril] Starting fast-sync for {networkName}..."
    let result ← Cleanode.Mithril.Client.mithrilSync mithrilConfig dataDir
    match result with
    | .error e =>
      IO.eprintln s!"[mithril] Fast-sync failed: {e}"
      IO.eprintln "[mithril] Falling back to normal peer sync..."
    | .ok syncResult =>
      IO.println s!"[mithril] Restored to epoch={syncResult.snapshot.beacon.epoch}, immutable={syncResult.snapshot.beacon.immutableFileNumber}"
      IO.println s!"[mithril] Chain tip: slot={syncResult.tipSlot}, hash={syncResult.tipPoint.hash.size}B"
      -- Save the Mithril tip as sync state so peer sync resumes from here
      chainDb.saveSyncState syncResult.tipSlot 0 syncResult.tipPoint.hash
      IO.println "[mithril] Sync state saved — peer sync will resume from snapshot tip"
      syncOrigin := .mithril syncResult.snapshot.beacon.epoch syncResult.snapshot.beacon.immutableFileNumber syncResult.snapshot.createdAt syncResult.snapshot.digest

      -- Try loading UTxO snapshot first (fast path)
      -- Verify snapshot matches current Mithril tip to avoid stale UTxO state
      let snapshotFile := s!"./data/{networkName.toLower}/utxo-snapshot.dat"
      let loaded ← Cleanode.Ledger.Snapshot.loadSnapshot snapshotFile
      let mithrilTipSlot := syncResult.tipSlot
      match loaded with
      | some cachedState =>
        if cachedState.lastSlot == mithrilTipSlot then
          IO.println s!"[snapshot] Using cached UTxO set (slot={cachedState.lastSlot} matches Mithril tip) — skipping replay"
          replayedLedger := some cachedState
        else
          IO.println s!"[snapshot] Stale snapshot (slot={cachedState.lastSlot}, Mithril tip={mithrilTipSlot}) — replaying ImmutableDB..."
          let initialState := { Cleanode.Ledger.State.LedgerState.initial with
            treasury := if networkName == "Preprod" then 1766738361646723 else 0
            reserves := if networkName == "Preprod" then 13181724972929461 else 0
            protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
              networkId := if networkName == "Preprod" then 0 else 1 } }
          let finalState ← Cleanode.Mithril.Replay.replayImmutableDB syncResult.dbPath initialState
            Cleanode.Mithril.Replay.defaultReplayReporter 0
          IO.println s!"[replay] UTxO set reconstructed: {finalState.utxo.size} entries"
          Cleanode.Ledger.Snapshot.createSnapshot finalState ⟨s!"./data/{networkName.toLower}"⟩
          replayedLedger := some finalState
      | none =>
        -- No snapshot — replay ImmutableDB to build UTxO set
        IO.println "[replay] No UTxO snapshot found, replaying ImmutableDB..."
        let initialState := { Cleanode.Ledger.State.LedgerState.initial with
          treasury := if networkName == "Preprod" then 1766738361646723 else 0
          reserves := if networkName == "Preprod" then 13181724972929461 else 0
          protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
            networkId := if networkName == "Preprod" then 0 else 1 } }
        let finalState ← Cleanode.Mithril.Replay.replayImmutableDB syncResult.dbPath initialState
          Cleanode.Mithril.Replay.defaultReplayReporter 0
        IO.println s!"[replay] UTxO set reconstructed: {finalState.utxo.size} entries"
        -- Save snapshot for next startup
        Cleanode.Ledger.Snapshot.createSnapshot finalState ⟨s!"./data/{networkName.toLower}"⟩
        replayedLedger := some finalState

  -- Load SPO keys if block production is configured
  let mut spoForgeParams : Option Cleanode.Consensus.Praos.BlockForge.ForgeParams := none
  if let some keyDir := config.spoKeyDir then
    let paths := Cleanode.Consensus.Praos.SPOKeys.SPOKeyPaths.default keyDir
    Cleanode.Consensus.Praos.SPOKeys.printKeyInfo paths
    let keysResult ← Cleanode.Consensus.Praos.SPOKeys.loadSPOKeys paths
    match keysResult with
    | .error e =>
      IO.eprintln s!"[spo] Failed to load SPO keys: {e}"
      IO.eprintln "[spo] Running as relay-only node."
    | .ok fp =>
      IO.println s!"[spo] SPO keys loaded successfully."
      IO.println s!"[spo] Pool ID: {fp.poolId.size} bytes"
      IO.println s!"[spo] VRF public key: {fp.vrfPublicKey.length} bytes"
      IO.println s!"[spo] OpCert sequence: {fp.operationalCert.sequenceNumber}, KES period: {fp.operationalCert.kesPeriod}"
      spoForgeParams := some fp

  -- Run as multi-peer relay node (with optional block production)
  relayNode proposal networkName config.tui config.port config.metricsPort spoForgeParams config.socketPath config.epochNonce chainDb syncOrigin replayedLedger

/-- Handle query subcommands (reads status file from a running node) -/
def runQuery (target : QueryTarget) : IO Unit := do
  let statusFile := Cleanode.CLI.Query.statusFilePath
  let fileExists ← System.FilePath.pathExists statusFile
  if !fileExists then
    IO.eprintln "Error: node status file not found. Is the node running?"
    IO.eprintln s!"Expected: {statusFile}"
    IO.Process.exit 1
  match target with
  | .tip => Cleanode.CLI.Query.queryTip
  | .peers => Cleanode.CLI.Query.queryPeers
  | .mempool => Cleanode.CLI.Query.queryMempool

/-- Replay saved block CBOR files and validate every tx (no UTxO existence check).
    Usage: cleanode replay failed_blocks/block_*.cbor -/
def runReplay (paths : List String) : IO Unit := do
  if paths.isEmpty then
    IO.println "Usage: cleanode replay <block.cbor> [block2.cbor ...]"
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
  let state : Cleanode.Ledger.State.LedgerState := { Cleanode.Ledger.State.LedgerState.initial with
    protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
      maxValueSize := 5000, networkId := 0 } }

  for file in allFiles do
    let blockBytes ← IO.FS.readBinFile file
    IO.println s!"── {file} ({blockBytes.size} bytes) ──"

    let parsed ← Cleanode.Network.ConwayBlock.parseConwayBlockBodyIO blockBytes
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
          let minFee := Cleanode.Ledger.Fee.totalMinFee feeParams effectiveSize tx.witnesses.redeemers
          if tx.body.fee < minFee then
            IO.println s!"    ✗ FEE: paid={tx.body.fee} min={minFee} (size={effectiveSize})"
          else
            IO.println s!"    ✓ FEE: paid={tx.body.fee} >= min={minFee}"

          -- Signature check
          let sigResult ← Cleanode.Ledger.Validation.validateSignatures state.utxo tx.body tx.witnesses
          match sigResult with
          | .ok () => IO.println s!"    ✓ SIGNATURES"
          | .error e => IO.println s!"    ✗ SIGNATURES: {repr e}"

          -- Native script check
          let mut signerKeyHashes : List ByteArray := []
          for w in tx.witnesses.vkeyWitnesses do
            let keyHash ← blake2b_224 w.vkey
            signerKeyHashes := keyHash :: signerKeyHashes
          match Cleanode.Ledger.Validation.validateNativeScripts tx.witnesses signerKeyHashes slot with
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
          match ← Cleanode.Plutus.Evaluate.evaluateTransactionScriptsIO tx.body tx.witnesses state.utxo txHash with
          | .ok () => IO.println s!"    ✓ PLUTUS SCRIPTS"
          | .error e => IO.println s!"    ✗ PLUTUS SCRIPTS: {e}"

        txIdx := txIdx + 1
      IO.println ""

def main (args : List String) : IO Unit := do
  match parseArgs args with
  | .help => Cleanode.CLI.Args.printUsage
  | .version => IO.println "Cleanode v0.1.0"
  | .query target => runQuery target
  | .replay paths => runReplay paths
  | .run config => runNode config
