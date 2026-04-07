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
import Dion.Node.HeaderValidation
import Dion.Node.SyncState

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

/-- Convert a RawCertificate (from block parser) to a ledger Certificate.
    Pool registrations use a simplified FullPoolParams. -/
def rawCertToLedger : RawCertificate → Option Dion.Ledger.Certificate.Certificate
  | .stakeKeyRegistration _ kh => some (.stakeKeyRegistration kh)
  | .stakeKeyDeregistration _ kh => some (.stakeKeyDeregistration kh)
  | .stakeDelegation _ kh pid => some (.stakeDelegation kh pid)
  | .poolRegistration pid vrfKH pledge cost _margin rewardAcct owners =>
      some (.poolRegistration {
        poolId := pid
        vrfKeyHash := vrfKH
        pledge := pledge
        cost := cost
        margin := 0
        rewardAccount := rewardAcct
        owners := owners
        relays := []
        metadata := none
      })
  | .poolRetirement pid epoch => some (.poolRetirement pid epoch)
  | .conwayRegistration _ kh deposit => some (.conwayRegistration kh deposit)
  | .conwayDeregistration _ kh refund => some (.conwayDeregistration kh refund)
  | .voteDelegation _ kh drepCred => some (.voteDelegation kh (.keyHash drepCred))
  | .stakeVoteDelegation _ kh pid drepCred => some (.stakeVoteDelegation kh pid (.keyHash drepCred))
  | .stakeRegDelegation _ kh pid deposit => some (.stakeRegDelegation kh pid deposit)
  | .voteRegDelegation _ kh drepCred deposit => some (.voteRegDelegation kh (.keyHash drepCred) deposit)
  | .stakeVoteRegDelegation _ kh pid drepCred deposit => some (.stakeVoteRegDelegation kh pid (.keyHash drepCred) deposit)
  | .authCommitteeHot _ cold hot => some (.authCommitteeHot cold hot)
  | .resignCommitteeCold _ cold => some (.resignCommitteeCold cold)
  | .unknown _ => none

/-- Display a full block with colored transaction details -/
def displayBlock (header : Header) (tip : Tip) (blockPoint : Point) (blockBytes : ByteArray) : IO Unit := run do
  match ← parseConwayBlockBodyIO blockBytes with
  | none =>
      println (("⚠ Failed to parse block".style |> yellow))
  | some blockBody => do
      -- Calculate statistics
      let totalInputs := blockBody.transactions.foldl (fun acc tx => acc + tx.body.inputs.length) 0
      let totalOutputs := blockBody.transactions.foldl (fun acc tx => acc + tx.body.outputs.length) 0
      let totalFees := blockBody.transactions.foldl (fun acc tx => acc + tx.body.fee) 0
      let totalFeesAda := totalFees / 1000000
      let totalFeesLovelace := totalFees % 1000000

      -- Compute block hash
      let blockHash ← computeBlockHash header.headerBytes

      -- Display block header with granular colors
      println ("".style)
      println ((("╔" ++ String.join (List.replicate 100 "═") ++ "╗").style |> blue |> bold))

      -- Block number and slot line
      concat [
        ("║ ".style |> blue |> bold),
        ("Block ".style |> white),
        (s!"#{tip.blockNo}".style |> yellow |> bold),
        ("   Slot: ".style |> white),
        (s!"{blockPoint.slot}".style |> cyan |> bold)
      ]

      -- Hash line (full)
      concat [
        ("║ ".style |> blue |> bold),
        ("Hash: ".style |> white),
        (s!"{blockHash}".style |> cyan |> dim)
      ]

      println ((("╟" ++ String.join (List.replicate 100 "─") ++ "╢").style |> blue |> bold))

      -- Size and era line
      concat [
        ("║ ".style |> blue |> bold),
        ("Size: ".style |> white),
        (s!"{blockBytes.size}".style |> magenta),
        (" bytes   Era: ".style |> white),
        (s!"{header.era}".style |> magenta)
      ]

      -- Transactions, inputs, outputs line
      concat [
        ("║ ".style |> blue |> bold),
        ("Transactions: ".style |> white),
        (s!"{blockBody.transactions.length}".style |> yellow),
        ("   Inputs: ".style |> white),
        (s!"{totalInputs}".style |> cyan),
        ("   Outputs: ".style |> white),
        (s!"{totalOutputs}".style |> green)
      ]

      -- Total fees line
      let paddedTotalFees :=
        let s := toString totalFeesLovelace
        String.join (List.replicate (6 - s.length) "0") ++ s
      concat [
        ("║ ".style |> blue |> bold),
        ("Total Fees: ".style |> white),
        (s!"{totalFeesAda}.{paddedTotalFees}".style |> green |> bold),
        (" ₳".style |> green |> bold)
      ]

      println ((("╚" ++ String.join (List.replicate 100 "═") ++ "╝").style |> blue |> bold))
      println ("".style)

      -- Display transactions
      if blockBody.transactions.length > 0 then do
        let mut txNum := 1
        for tx in blockBody.transactions do
          let feeAda := tx.body.fee / 1000000
          let feeLovelace := tx.body.fee % 1000000

          -- Compute transaction ID
          let txId ← computeTxId tx.body.rawBytes

          -- Calculate total input and output amounts
          let _totalOut := tx.body.outputs.foldl (fun acc out => acc + out.amount) 0
          let paddedFee :=
            let s := toString feeLovelace
            String.join (List.replicate (6 - s.length) "0") ++ s

          -- Transaction box header with granular colors
          println ("".style)
          println ((("┌" ++ String.join (List.replicate 100 "─") ++ "┐").style |> blue))

          -- TX header line (full hash)
          concat [
            ("│ ".style |> blue),
            ("TX ".style |> white),
            (s!"#{txNum}".style |> yellow |> bold),
            (": ".style |> white),
            (s!"{txId}".style |> magenta),
            ("  Fee: ".style |> white),
            (s!"{feeAda}.{paddedFee}".style |> green |> bold),
            (" ₳".style |> green |> bold)
          ]

          println ((("├" ++ String.join (List.replicate 100 "─") ++ "┤").style |> blue))

          -- UTXO Graph visualization
          let maxRows := max tx.body.inputs.length tx.body.outputs.length
          let minRows := min tx.body.inputs.length tx.body.outputs.length
          let midRow := minRows / 2

          for i in [:maxRows] do
            -- Input part (left side) - fixed width 25 chars
            let inputPart := if h : i < tx.body.inputs.length then
              let input := tx.body.inputs.get ⟨i, h⟩
              let txHash := bytesToHex input.txId
              let inputStr := s!"{txHash.take 16}...#{input.outputIndex}"
              -- Pad to 25 chars
              inputStr ++ String.join (List.replicate (25 - inputStr.length) " ")
            else
              String.join (List.replicate 25 " ")

            -- Middle connector - fixed width 22 chars
            let connector := if i == midRow then
              s!"│───[{txId.take 8}...]───>│"
            else
              "│                    │"

            -- Output part (right side) - build styled text list
            let outputStyled : List StyledText := if h : i < tx.body.outputs.length then
              let output := tx.body.outputs.get ⟨i, h⟩
              let addr := encodeAddress output.address false
              let amt := output.amount / 1000000
              let amtLov := output.amount % 1000000
              let padLov := let s := toString amtLov
                            String.join (List.replicate (6 - s.length) "0") ++ s

              -- Build colored output: cyan address + green amount
              let addrPart := (s!"{addr.take 20}...".style |> cyan)
              let amtPart := (s!" {amt}.{padLov}₳".style |> green |> bold)
              let assetPart := if output.nativeAssets.length > 0 then
                [(s!" +{output.nativeAssets.length}a".style |> yellow)]
              else
                []
              [addrPart, amtPart] ++ assetPart
            else
              []

            -- Print with individual colors using Pigment's list-based approach
            let parts : List StyledText :=
              [("│ ".style |> blue),
               (inputPart.style |> dim),
               (connector.style |> magenta),
               (" ".style)] ++ outputStyled
            concat parts

            -- Display native assets on separate lines
            if h : i < tx.body.outputs.length then
              let output := tx.body.outputs.get ⟨i, h⟩
              for asset in output.nativeAssets do
                let policyHex := bytesToHex asset.policyId

                -- Decode asset name if printable
                let assetNameDisplay := if asset.assetName.size > 0 then
                  match String.fromUTF8? asset.assetName with
                  | some name =>
                      let isPrintable := name.all fun c => c.toNat >= 32 && c.toNat <= 126
                      if isPrintable && name.length > 0 then
                        s!":{name}"
                      else
                        let nameHex := bytesToHex asset.assetName
                        s!":{nameHex.take 8}..."
                  | none =>
                      let nameHex := bytesToHex asset.assetName
                      s!":{nameHex.take 8}..."
                else
                  ""

                let assetDisplay := s!"  +{asset.amount} {policyHex.take 8}...{assetNameDisplay}"

                -- Empty input and connector for asset lines
                let emptyInput := String.join (List.replicate 25 " ")
                let emptyConnector := "│                    │"
                let assetParts : List StyledText :=
                  [("│ ".style |> blue),
                   (emptyInput.style |> dim),
                   (emptyConnector.style |> magenta),
                   (" ".style),
                   (assetDisplay.style |> yellow)]
                concat assetParts

          -- Display redeemers if present
          let redeemers := tx.witnesses.redeemers
          if redeemers.length > 0 then
            redeemers.forM fun redeemer => do
              let tagStr := match redeemer.tag with
                | .Spend => "Spend"
                | .Mint => "Mint"
                | .Cert => "Cert"
                | .Reward => "Reward"
                | .Vote => "Vote"
                | .Propose => "Propose"
              let redeemerDisplay := s!"  Redeemer: {tagStr} #{redeemer.index} (mem={redeemer.exUnits.mem}, steps={redeemer.exUnits.steps})"

              -- Empty input and connector for redeemer lines
              let emptyInput := String.join (List.replicate 17 " ")
              let emptyConnector := "│                  │"
              let redeemerParts : List StyledText :=
                [("│ ".style |> blue),
                 (emptyInput.style |> dim),
                 (emptyConnector.style |> magenta),
                 (" ".style),
                 (redeemerDisplay.style |> cyan)]
              concat redeemerParts

          println ((("└" ++ String.join (List.replicate 100 "─") ++ "┘").style |> blue))
          txNum := txNum + 1

/-- Fetch and display a block from a ChainSync RollForward header -/
def fetchAndDisplayBlock (sock : Socket) (header : Header) (tip : Tip)
    (chainDb : Option ChainDB := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (peerAddr : String := "")
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (checkpointRef : Option (IO.Ref Dion.Ledger.State.CheckpointRing) := none) : IO Bool := do
  -- Extract the actual block point from the header (not the tip)
  -- The headerBytes is tag24-wrapped: d8 18 <bytestring>
  -- Block hash in Cardano = blake2b_256(content inside tag24 bytestring)
  let rawHeaderBytes :=
    if header.headerBytes.size >= 2 &&
       header.headerBytes[0]! == 0xd8 && header.headerBytes[1]! == 0x18 then
      match Dion.Network.Cbor.decodeBytes (header.headerBytes.extract 2 header.headerBytes.size) with
      | some result => result.value
      | none => header.headerBytes  -- fallback
    else
      header.headerBytes
  let blockHash ← blake2b_256 rawHeaderBytes
  let blockSlot : UInt64 :=
    if header.era == 0 then
      match extractByronInfo header.headerBytes with
      | some info => UInt64.ofNat info.slot
      | none => tip.point.slot  -- fallback
    else
      match extractShelleyInfo header.headerBytes with
      | some info => UInt64.ofNat info.slot
      | none => tip.point.slot  -- fallback
  let blockPoint : Point := { slot := blockSlot, hash := blockHash }
  -- Extract actual block number from header (not the tip)
  let blockNo :=
    if header.era == 0 then tip.blockNo.toNat  -- Byron: use tip
    else match extractShelleyInfo header.headerBytes with
      | some info => info.blockNo
      | none => tip.blockNo.toNat  -- fallback to tip
  -- Deduplicate: atomically check-and-mark block as seen
  let alreadySeen ← do
    if let some ref := seenBlocks then
      atomicCheckAndMark ref blockNo
    else
      pure false
  if alreadySeen then
    -- Block already claimed by another peer — still fetch to satisfy BlockFetch protocol
    match ← fetchBlock sock blockPoint mempoolRef with
    | .error _ => return false
    | _ => return true
  else
    match ← fetchBlock sock blockPoint mempoolRef with
    | .error e => do
        IO.println s!"✗ Failed to fetch block: {e}"
        return false
    | .ok none => do
        IO.println "✗ No block received"
        return false
    | .ok (some blockBytes) => do
        -- In TUI mode, push a summary to TUI state; otherwise display inline
        match tuiRef with
        | some ref => do
            let blockHashHex ← computeBlockHash header.headerBytes
            let parsed ← Dion.Network.ConwayBlock.parseConwayBlockBodyIO blockBytes
            let txCount := match parsed with
              | some body => body.transactions.length
              | none => 0
            let totalFees := match parsed with
              | some body => body.transactions.foldl (fun acc tx => acc + tx.body.fee) 0
              | none => 0
            let summary : BlockSummary := {
              blockNo := blockNo
              slot := blockPoint.slot.toNat
              hash := blockHashHex.take 16
              txCount := txCount
              size := blockBytes.size
              totalFees := totalFees
              era := header.era
              peerAddr := peerAddr
            }
            ref.modify (·.addBlock summary)
            ref.modify (·.peerSyncedBlock peerAddr)
            -- Validate consensus fields for Shelley+ headers
            if header.era >= 1 then
              match extractShelleyInfo header.headerBytes with
              | some shelleyInfo => do
                let hdrResult ← validateBlockHeader shelleyInfo ref consensusRef
                -- Store per-block header validation results in the block summary
                ref.modify fun tui =>
                  let updateHdr := fun b : BlockSummary =>
                    if b.blockNo == blockNo then
                      { b with headerValidated := true
                               vrfOk := hdrResult.vrfOk
                               kesOk := hdrResult.kesOk
                               opCertOk := hdrResult.opCertOk }
                    else b
                  { tui with recentBlocks := tui.recentBlocks.map updateHdr
                             pendingBlocks := tui.pendingBlocks.map updateHdr }
              | none => pure ()
            -- Remove confirmed txs from mempool and evict stale txs with spent inputs
            if let some mpRef := mempoolRef then
              if let some body := parsed then
                let mut confirmedHashes : List ByteArray := []
                let mut spentInputs : List (ByteArray × Nat) := []
                for tx in body.transactions do
                  let txHash ← blake2b_256 tx.body.rawBytes
                  confirmedHashes := confirmedHashes ++ [txHash]
                  for inp in tx.body.inputs do
                    spentInputs := spentInputs ++ [(inp.txId, inp.outputIndex)]
                if confirmedHashes.length > 0 then
                  mpRef.modify (·.removeConfirmed confirmedHashes)
                if spentInputs.length > 0 then
                  mpRef.modify (·.removeStaleInputs spentInputs)
              let pool ← mpRef.get
              ref.modify (·.updateMempool pool.entries.length pool.totalBytes)
        | none => displayBlock header tip blockPoint blockBytes
        -- Update ledger state: validate each tx, apply certificates and UTxO changes
        if let some lsMutex := ledgerStateRef then
          let parsedBody ← Dion.Network.ConwayBlock.parseConwayBlockBodyIO blockBytes
          if let some body := parsedBody then
            lsMutex.atomically fun lsRef => do
            let mut s ← lsRef.get
            let mut validationOk := 0
            let mut validationFail := 0
            let mut validationSkipped := 0
            let mut validationErrors : List String := []
            -- Collect failure details for file logging
            let mut failureDetails : List String := []
            -- Track which inputs were actually missing at skip time (for unknown block log)
            let mut skippedTxDetails : List (Nat × ByteArray × List String) := []
            let mut txIdx := 0
            for tx in body.transactions do
              -- Phase-2 invalid txs (Plutus failures — collateral taken, block still valid)
              if body.invalidTxs.contains txIdx then
                validationOk := validationOk + 1
                txIdx := txIdx + 1
                continue
              -- Check if all inputs exist in our UTxO set (skip tx if any missing)
              -- Includes regular inputs, reference inputs, and collateral inputs
              let allInputsKnown := tx.body.inputs.all (fun inp =>
                s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex }) &&
                tx.body.referenceInputs.all (fun inp =>
                s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex }) &&
                tx.body.collateralInputs.all (fun inp =>
                s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex })
              if allInputsKnown && !tx.body.inputs.isEmpty then
                let errs ← Dion.Ledger.Validation.validateTransaction
                  s tx.body tx.witnesses .Conway blockPoint.slot.toNat tx.serializedSize
                if errs.isEmpty then
                  validationOk := validationOk + 1
                else
                  validationFail := validationFail + 1
                  for e in errs do
                    let errStr := s!"{repr e}"
                    validationErrors := validationErrors ++ [errStr]
                  let errSummary := String.intercalate ", " (errs.map fun e => s!"{repr e}")
                  let debugWit := s!"[validate] Block #{blockNo} tx FAILED: {errSummary} | vkeys={tx.witnesses.vkeyWitnesses.length} bootstrap={tx.witnesses.bootstrapWitnesses.length} redeemers={tx.witnesses.redeemers.length}"
                  match tuiRef with
                  | some ref => ref.modify (·.addLog debugWit)
                  | none => pure ()
                  -- Collect details for file log
                  let txHashBytes ← blake2b_256 tx.body.rawBytes
                  let txHashHex := bytesToHex txHashBytes
                  let mut lines : List String := []
                  lines := lines ++ [s!"  tx[{txIdx}] hash={(txHashHex.take 32)}..."]
                  for e in errs do
                    lines := lines ++ [s!"    error: {repr e}"]
                  lines := lines ++ [s!"    fee={tx.body.fee} serializedSize={tx.serializedSize} bodySize={tx.body.rawBytes.size}"]
                  lines := lines ++ [s!"    inputs={tx.body.inputs.length} refInputs={tx.body.referenceInputs.length} outputs={tx.body.outputs.length} mint={tx.body.mint.length} withdrawals={tx.body.withdrawals.length} certs={tx.body.certificates.length}"]
                  lines := lines ++ [s!"    vkeys={tx.witnesses.vkeyWitnesses.length} bootstrap={tx.witnesses.bootstrapWitnesses.length} redeemers={tx.witnesses.redeemers.length} native={tx.witnesses.nativeScripts.length}"]
                  lines := lines ++ [s!"    plutusV1={tx.witnesses.plutusV1Scripts.length} plutusV2={tx.witnesses.plutusV2Scripts.length} plutusV3={tx.witnesses.plutusV3Scripts.length}"]
                  if let some ttl := tx.body.ttl then lines := lines ++ [s!"    ttl={ttl}"]
                  if let some validFrom := tx.body.validityIntervalStart then lines := lines ++ [s!"    validFrom={validFrom}"]
                  -- Diagnostic: print mint policy IDs
                  for asset in tx.body.mint do
                    lines := lines ++ [s!"    mintPolicy={bytesToHex asset.policyId} name={bytesToHex asset.assetName} amt={asset.signedAmount}"]
                  -- Diagnostic: print spend input addresses (to find spurious script-locked)
                  for inp in tx.body.inputs do
                    let inpId : Dion.Ledger.UTxO.UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
                    match s.utxo.lookup inpId with
                    | none => pure ()
                    | some out =>
                      let addrHdr := if out.address.size > 0 then bytesToHex (out.address.extract 0 1) else "?"
                      let addrCred := if out.address.size >= 29 then bytesToHex (out.address.extract 1 29) else "short"
                      lines := lines ++ [s!"    spendInput={bytesToHex inp.txId}#{inp.outputIndex} addrHdr={addrHdr} payCredHash={addrCred}"]
                  -- Diagnostic: print withdrawal addresses
                  for (rewardAddr, amt) in tx.body.withdrawals do
                    let hdr := if rewardAddr.size > 0 then bytesToHex (rewardAddr.extract 0 1) else "?"
                    let cred := if rewardAddr.size >= 29 then bytesToHex (rewardAddr.extract 1 29) else "short"
                    lines := lines ++ [s!"    withdrawal=hdr:{hdr} cred:{cred} amt:{amt}"]
                  -- Diagnostic: print certificates
                  for cert in tx.body.certificates do
                    lines := lines ++ [s!"    cert={repr cert}"]
                  -- Diagnostic: print reference input script ref hashes
                  for refInp in tx.body.referenceInputs do
                    let refId : Dion.Ledger.UTxO.UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
                    match s.utxo.lookup refId with
                    | none => lines := lines ++ [s!"    refInput={bytesToHex refInp.txId}#{refInp.outputIndex} UTxO=MISSING"]
                    | some out =>
                      match out.scriptRef with
                      | none => lines := lines ++ [s!"    refInput={bytesToHex refInp.txId}#{refInp.outputIndex} scriptRef=none"]
                      | some refBytes =>
                        let hdrStr := if refBytes.size >= 2 then bytesToHex (refBytes.extract 0 2) else "?"
                        lines := lines ++ [s!"    refInput={bytesToHex refInp.txId}#{refInp.outputIndex} scriptRefSize={refBytes.size} header={hdrStr}"]
                  failureDetails := failureDetails ++ lines
              else do
                validationSkipped := validationSkipped + 1
                -- Capture missing inputs NOW (against intermediate state, not final)
                let txHashBytes ← blake2b_256 tx.body.rawBytes
                let mut missing : List String := []
                for inp in tx.body.inputs do
                  if !s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex } then
                    missing := s!"    missing input: {bytesToHex inp.txId}#{inp.outputIndex}" :: missing
                for inp in tx.body.referenceInputs do
                  if !s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex } then
                    missing := s!"    missing ref-input: {bytesToHex inp.txId}#{inp.outputIndex}" :: missing
                for inp in tx.body.collateralInputs do
                  if !s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex } then
                    missing := s!"    missing collateral: {bytesToHex inp.txId}#{inp.outputIndex}" :: missing
                skippedTxDetails := (txIdx, txHashBytes, missing.reverse) :: skippedTxDetails
              -- Apply regardless (the chain is authoritative)
              let certs := tx.body.certificates.filterMap rawCertToLedger
              s := Dion.Ledger.Certificate.applyCertificates s certs
              let txHash ← blake2b_256 tx.body.rawBytes
              s := { s with utxo := s.utxo.applyTx txHash tx.body,
                            epochFees := s.epochFees + tx.body.fee }
              txIdx := txIdx + 1
            -- Update TUI with validation results
            match tuiRef with
            | some ref =>
              ref.modify fun tui =>
                let updateVal := fun b : BlockSummary =>
                  if b.blockNo == blockNo then { b with validTxs := validationOk, failedTxs := validationFail, skippedTxs := validationSkipped, validationErrors := validationErrors } else b
                let blocks := tui.recentBlocks.map updateVal
                let pending := tui.pendingBlocks.map updateVal
                let tui := { tui with recentBlocks := blocks, pendingBlocks := pending, totalTxsValidated := tui.totalTxsValidated + validationOk, totalTxsFailed := tui.totalTxsFailed + validationFail }
                -- Check header validation from the block summary we just updated
                let allBlocks := blocks ++ pending
                let headerOk := match allBlocks.find? (fun b => b.blockNo == blockNo) with
                  | some b => b.headerValidated && b.vrfOk && b.kesOk && b.opCertOk
                  | none => false
                -- Invalid = any tx failure OR header failure
                -- Valid = header OK AND no tx failures AND no skipped txs
                -- Unknown (computed) = skipped txs, unparsed blocks, etc.
                if validationFail > 0 || !headerOk then
                  { tui with blocksWithFailures := tui.blocksWithFailures + 1 }
                else if validationSkipped == 0 then
                  { tui with blocksFullyValid := tui.blocksFullyValid + 1 }
                else tui  -- has skipped txs → "unknown"
            | none => pure ()
            -- Log unknown blocks (skipped txs with no failures) for investigation
            if validationSkipped > 0 && validationFail == 0 then
              let h ← IO.FS.Handle.mk "validation_unknown.log" .append
              h.putStrLn s!"═══ Block #{blockNo} | slot {blockPoint.slot.toNat} | txs {body.transactions.length} (ok={validationOk} skip={validationSkipped}) ═══"
              for (idx, txHash, missingLines) in skippedTxDetails.reverse do
                let txHashHex := bytesToHex txHash
                h.putStrLn s!"  tx[{idx}] hash={(txHashHex.take 32)}..."
                for line in missingLines do
                  h.putStrLn line
              h.putStrLn ""
            -- Log invalid blocks and save raw bytes for replay
            if validationFail > 0 then
              let h ← IO.FS.Handle.mk "validation_failures.log" .append
              h.putStrLn s!"═══ Block #{blockNo} | slot {blockPoint.slot.toNat} | txs {body.transactions.length} (ok={validationOk} fail={validationFail} skip={validationSkipped}) ═══"
              for line in failureDetails do
                h.putStrLn line
              h.putStrLn ""
              -- Save raw block bytes for offline replay
              IO.FS.createDirAll "failed_blocks"
              IO.FS.writeBinFile s!"failed_blocks/block_{blockNo}_slot_{blockPoint.slot.toNat}.cbor" blockBytes
            -- Track block producer for epoch reward calculation (Shelley+)
            if header.era >= 1 then
              match extractShelleyInfo header.headerBytes with
              | some info =>
                if info.issuerVKey.size == 32 then
                  let poolId ← blake2b_224 info.issuerVKey
                  let cur := s.epochBlocksByPool[poolId]?.getD 0
                  s := { s with epochBlocksByPool := s.epochBlocksByPool.insert poolId (cur + 1) }
              | none => pure ()
            -- Epoch boundary: distribute rewards, retire pools, take snapshot
            let slot := blockPoint.slot.toNat
            let newEpoch := Dion.Ledger.State.epochForSlot s slot
            s := if newEpoch > s.protocolParams.epoch then
              Dion.Ledger.State.processEpochBoundary s newEpoch
            else s
            lsRef.set { s with lastSlot := slot, lastBlockNo := blockNo, lastBlockHash := blockHash }
        -- Store block in SQLite if ChainDB is available (batched every 100 blocks for speed)
        if let some cdb := chainDb then
          let slot := blockPoint.slot.toNat
          if blockNo % 100 == 0 then
            IO.println s!"[sync] Block #{blockNo} slot={slot} (batch commit at #{blockNo - 1})"
            cdb.beginBatch
          cdb.addBlock blockNo slot header.era blockHash blockPoint.hash header.headerBytes
          cdb.addBlockBody blockNo blockBytes
          cdb.saveSyncState slot blockNo blockHash
          if blockNo % 100 == 99 then cdb.commitBatch
        -- Update shared chain tip for forge loop
        if let some phRef := prevHashRef then
          phRef.set blockHash
        if let some bnRef := blockNoRef then
          bnRef.set blockNo
        -- Push ledger checkpoint for rollback support (capped ring buffer)
        if let some cpRef := checkpointRef then
          if let some lsMutex := ledgerStateRef then
            let snap ← lsMutex.atomically (fun ref => ref.get)
            let cp : Dion.Ledger.State.LedgerCheckpoint :=
              { slot := blockPoint.slot.toNat, blockNo, hash := blockHash, ledger := snap }
            cpRef.modify (·.push cp)
          match tuiRef with
          | some ref => ref.modify (·.addLog s!"Block #{blockNo} stored in chain.db")
          | none => IO.println s!"  💾 Block #{blockNo} stored in chain.db"
        -- Update consensus state: epoch boundary detection and nonce evolution
        if let some csRef := consensusRef then
          let slot := blockPoint.slot.toNat
          -- At epoch boundary, build real stake snapshot from ledger state
          let freshSnapshot ← match ledgerStateRef with
            | some lsMutex => do
                let ls ← lsMutex.atomically (fun ref => ref.get)
                pure (Dion.Consensus.Praos.StakeDistribution.buildSnapshotFromLedger ls)
            | none => do
                let cs ← csRef.get
                pure cs.stakeSnapshot
          let cs ← csRef.get
          let newEpoch := slotToEpoch cs slot
          let cs ← if needsEpochTransition cs slot then
            processEpochTransitionIO cs newEpoch freshSnapshot
          else pure cs
          -- Update evolving nonce with VRF output (Blake2b-256, stability window aware)
          let cs ← if header.era >= 1 then
            match extractShelleyInfo header.headerBytes with
            | some info =>
              match info.vrfResult with
              | some vrf => updateEvolvingNonceFromBlock cs slot vrf.output
              | none => pure cs
            | none => pure cs
          else pure cs
          csRef.set cs
          -- Persist consensus state periodically (every 1000 blocks) for restart recovery
          if blockNo % 1000 == 0 then
            let csNow ← csRef.get
            saveConsensusState csNow
          -- Push nonce info to TUI
          if let some ref := tuiRef then
            let cs ← csRef.get
            let epochHex := bytesToHex cs.epochNonce |>.take 16
            let evolvingHex := bytesToHex cs.evolvingNonce |>.take 16
            ref.modify (·.updateConsensus fun c => { c with
              epochNonceHex := epochHex
              evolvingNonceHex := evolvingHex
            })
        return true

end Dion.Node
