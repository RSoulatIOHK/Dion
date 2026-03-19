import Cleanode.Consensus.Praos.BlockForge
import Cleanode.Consensus.Praos.ConsensusState
import Cleanode.Consensus.Praos.StakeDistribution
import Cleanode.Ledger.State
import Cleanode.Ledger.Validation
import Cleanode.Network.CborCursor
import Cleanode.Network.ConwayBlock
import Cleanode.Network.Crypto
import Cleanode.Network.Mempool
import Std.Sync

/-!
# Real-Time Block Forge Loop

Runs a background task that checks leader election each slot and
forges blocks when the pool is elected.

## Slot Clock

Maps wall-clock time to slot numbers using:
- `systemStart`: Unix timestamp of slot 0 (from Shelley genesis)
- `slotLength`: Duration of each slot in seconds (typically 1)

Current slot = (now - systemStart) / slotLength

## Forge Pipeline (per slot)

1. Compute current slot from wall clock
2. Check if this slot is in a new epoch → rotate stake snapshots
3. Run VRF leader election with the active (2-epoch-old) stake snapshot
4. If elected: select transactions, build block header, KES-sign, emit

## References
- Ouroboros Praos spec: Section 4.2 (slot leadership)
- Cardano Node: ForgeLoop module
-/

namespace Cleanode.Consensus.Praos.ForgeLoop

open Cleanode.Consensus.Praos.BlockForge
open Cleanode.Consensus.Praos.ConsensusState
open Cleanode.Consensus.Praos.StakeDistribution

-- ====================
-- = Time FFI         =
-- ====================

/-- Get current Unix timestamp in seconds -/
@[extern "cleanode_get_unix_time"]
opaque getUnixTime : IO UInt64

/-- Get current Unix timestamp in milliseconds -/
@[extern "cleanode_get_unix_time_ms"]
opaque getUnixTimeMs : IO UInt64

-- ====================
-- = Slot Clock       =
-- ====================

/-- Slot clock configuration derived from genesis -/
structure SlotClock where
  /-- Unix timestamp (seconds) of slot 0 -/
  systemStart : UInt64
  /-- Slot duration in seconds (typically 1 for Shelley+) -/
  slotLength : UInt64
  /-- Slots per epoch (typically 432000 = 5 days) -/
  epochLength : Nat
  /-- Slots per KES period (typically 129600 = 36 hours) -/
  slotsPerKESPeriod : Nat

instance : Repr SlotClock where
  reprPrec c _ := s!"SlotClock(start={c.systemStart}, slotLen={c.slotLength}, epochLen={c.epochLength})"

/-- Known system start times (Unix epoch seconds) -/
def mainnetSystemStart : UInt64 := 1596059091   -- 2020-07-29T21:44:51Z (Shelley)
def preprodSystemStart : UInt64 := 1654041600   -- 2022-06-01T00:00:00Z
def previewSystemStart : UInt64 := 1666656000   -- 2022-10-25T00:00:00Z

/-- Create a slot clock for mainnet -/
def SlotClock.mainnet : SlotClock :=
  { systemStart := mainnetSystemStart, slotLength := 1,
    epochLength := 432000, slotsPerKESPeriod := 129600 }

/-- Create a slot clock for preprod -/
def SlotClock.preprod : SlotClock :=
  { systemStart := preprodSystemStart, slotLength := 1,
    epochLength := 432000, slotsPerKESPeriod := 129600 }

/-- Create a slot clock for preview -/
def SlotClock.preview : SlotClock :=
  { systemStart := previewSystemStart, slotLength := 1,
    epochLength := 432000, slotsPerKESPeriod := 129600 }

/-- Get the current slot number from wall-clock time -/
def SlotClock.currentSlot (clock : SlotClock) : IO Nat := do
  let now ← getUnixTime
  if now < clock.systemStart then return 0
  let elapsed := now - clock.systemStart
  return (elapsed / clock.slotLength).toNat

/-- Get the epoch for a given slot -/
def SlotClock.slotEpoch (clock : SlotClock) (slot : Nat) : Nat :=
  slot / clock.epochLength

/-- Get the KES period for a given slot -/
def SlotClock.slotKESPeriod (clock : SlotClock) (slot : Nat) : Nat :=
  slot / clock.slotsPerKESPeriod

/-- Get the wall-clock time (Unix seconds) when a slot starts -/
def SlotClock.slotStartTime (clock : SlotClock) (slot : Nat) : UInt64 :=
  clock.systemStart + UInt64.ofNat slot * clock.slotLength

/-- Milliseconds until the next slot starts -/
def SlotClock.msUntilNextSlot (clock : SlotClock) : IO Nat := do
  let nowMs ← getUnixTimeMs
  let startMs := clock.systemStart.toNat * 1000
  let slotMs := clock.slotLength.toNat * 1000
  if nowMs.toNat < startMs then return slotMs
  let elapsed := nowMs.toNat - startMs
  let intoSlot := elapsed % slotMs
  return slotMs - intoSlot

-- ====================
-- = Forge State      =
-- ====================

/-- Mutable state for the forge loop -/
structure ForgeState where
  /-- Forge parameters (keys, pool config) -/
  forgeParams : ForgeParams
  /-- Slot clock for time → slot mapping -/
  clock : SlotClock
  /-- Consensus state (epoch nonce, stake snapshot) -/
  consensusState : ConsensusState
  /-- Stake snapshot ring (manages 2-epoch lag) -/
  snapshotRing : StakeSnapshotRing
  /-- Last slot we checked for leadership -/
  lastCheckedSlot : Nat
  /-- Total blocks forged by this node -/
  blocksForged : Nat
  /-- Total leader checks performed -/
  leaderChecks : Nat
  /-- Total times elected leader -/
  timesElected : Nat

/-- Create initial forge state -/
def ForgeState.initial (params : ForgeParams) (clock : SlotClock)
    (cs : ConsensusState) : ForgeState :=
  { forgeParams := params,
    clock := clock,
    consensusState := cs,
    snapshotRing := StakeSnapshotRing.empty,
    lastCheckedSlot := 0,
    blocksForged := 0,
    leaderChecks := 0,
    timesElected := 0 }

-- ====================
-- = Forge Step       =
-- ====================

/-- Result of a single forge step -/
inductive ForgeStepResult where
  | notYet (currentSlot : Nat)           -- Same slot as last check
  | notLeader (slot : Nat)               -- Not elected for this slot
  | forged (block : ForgedBlock)          -- Block successfully forged
  | forgeError (slot : Nat) (err : String) -- Forge failed
  | epochTransition (epoch : Nat)        -- Crossed epoch boundary

/-- Run one iteration of the forge loop.
    Returns the updated state and the step result.
    Reads live consensus state from `consensusRef` (updated by sync loop). -/
def forgeStep (stateRef : IO.Ref ForgeState)
    (mempoolRef : IO.Ref Cleanode.Network.Mempool.Mempool)
    (consensusRef : IO.Ref ConsensusState)
    (ledgerStateRef : Std.Mutex Cleanode.Ledger.State.LedgerState)
    (prevHash : ByteArray) (blockNumber : Nat)
    : IO ForgeStepResult := do
  let state ← stateRef.get
  let currentSlot ← state.clock.currentSlot

  -- Skip if we already checked this slot
  if currentSlot <= state.lastCheckedSlot then
    return .notYet currentSlot

  stateRef.modify fun s => { s with lastCheckedSlot := currentSlot }

  -- Pull live consensus state from the sync loop
  let cs ← consensusRef.get

  -- Check for epoch transition
  let currentEpoch := state.clock.slotEpoch currentSlot
  if currentEpoch > cs.currentEpoch then
    IO.println s!"[forge] Epoch transition detected: {cs.currentEpoch} → {currentEpoch}"
    return .epochTransition currentEpoch

  -- Compute KES period and warn if approaching expiry
  let kesPeriod := state.clock.slotKESPeriod currentSlot
  let certStartPeriod := state.forgeParams.operationalCert.kesPeriod
  let maxKESEvolutions := 62  -- standard Cardano max KES evolutions
  let periodsRemaining := if kesPeriod >= certStartPeriod then
    let used := kesPeriod - certStartPeriod
    if used >= maxKESEvolutions then 0 else maxKESEvolutions - used
  else maxKESEvolutions
  if periodsRemaining == 0 then
    IO.eprintln s!"[forge] CRITICAL: KES key EXPIRED (cert period {certStartPeriod}, current {kesPeriod}, max evolutions {maxKESEvolutions}) — rotate opcert immediately!"
  else if periodsRemaining <= 5 then
    IO.eprintln s!"[forge] WARNING: KES key expiring soon — {periodsRemaining} period(s) remaining (~{periodsRemaining * 36}h). Rotate opcert!"

  -- Prune time-expired mempool entries and select transactions
  let nowMs ← getUnixTimeMs
  mempoolRef.modify (·.prune nowMs.toNat)
  let mempool ← mempoolRef.get
  let maxBlockBodySize := (← ledgerStateRef.atomically (fun ref => ref.get)).protocolParams.maxBlockSize
  let blockBody := Cleanode.Consensus.Praos.TxSelection.selectTransactions mempool maxBlockBodySize currentSlot

  stateRef.modify fun s => { s with leaderChecks := s.leaderChecks + 1 }

  -- Attempt to forge (IO version with real crypto) using live consensus state
  let result ← tryForgeBlockIO state.forgeParams cs
    blockNumber currentSlot prevHash blockBody kesPeriod

  match result with
  | .error e => return .forgeError currentSlot e
  | .ok none =>
    return .notLeader currentSlot
  | .ok (some block) =>
    stateRef.modify fun s => { s with
      blocksForged := s.blocksForged + 1,
      timesElected := s.timesElected + 1
    }
    return .forged block

-- ============================
-- = Forged Block Validation  =
-- ============================

/-- Self-validate a forged block before announcing it to peers.
    Parses the block CBOR back and runs all ledger validation rules on each tx.
    Returns a list of validation errors (empty = all good). -/
def validateForgedBlock (block : ForgedBlock) (ledgerState : Cleanode.Ledger.State.LedgerState)
    : IO (List String) := do
  let mut errors : List String := []

  -- 1. Encode the full block and parse it back
  let fullBlockCbor := block.encodeFullBlock
  let parsed ← Cleanode.Network.ConwayBlock.parseConwayBlockBodyIO fullBlockCbor
  match parsed with
  | none =>
    return ["CBOR round-trip FAILED: could not parse forged block back"]
  | some body => do

  -- 2. Check tx count matches
  if body.transactions.length != block.selectedTxs.transactions.length then
    errors := s!"Tx count mismatch: forged {block.selectedTxs.transactions.length}, parsed {body.transactions.length}" :: errors

  -- 3. Validate each parsed transaction against ledger rules
  for (idx, tx) in (List.range body.transactions.length).zip body.transactions do
    let valErrs ← Cleanode.Ledger.Validation.validateTransaction
      ledgerState tx.body tx.witnesses .Conway block.slot
    for e in valErrs do
      errors := s!"Tx {idx} validation FAILED: {repr e}" :: errors

  -- 4. Check header structure: body hash matches
  let bodyHash ← Cleanode.Network.Crypto.blake2b_256 block.bodyComponents.serialize
  let headerHash ← Cleanode.Network.Crypto.blake2b_256 block.headerBytes
  if bodyHash.size != 32 then
    errors := "Body hash is not 32 bytes" :: errors
  if headerHash.size != 32 then
    errors := "Header hash is not 32 bytes" :: errors

  -- 5. Check body size is within protocol limits (90112 bytes max block body)
  let bodySize := block.bodyComponents.serialize.size
  if bodySize > 90112 then
    errors := s!"Block body too large: {bodySize} > 90112" :: errors

  return errors

-- ====================
-- = Forge Loop       =
-- ====================

/-- Run the forge loop as a background task.
    Checks each slot for leadership and forges blocks when elected.

    The loop sleeps until the next slot boundary, then runs forgeStep.
    Forged blocks are pushed to `forgedBlocksRef` for the announcement layer. -/
partial def runForgeLoop (stateRef : IO.Ref ForgeState)
    (mempoolRef : IO.Ref Cleanode.Network.Mempool.Mempool)
    (consensusRef : IO.Ref ConsensusState)
    (ledgerStateRef : Std.Mutex Cleanode.Ledger.State.LedgerState)
    (prevHashRef : IO.Ref ByteArray)
    (blockNoRef : IO.Ref Nat)
    (forgedBlocksRef : IO.Ref (Array ForgedBlock))
    : IO Unit := do
  IO.println "[forge] Block production loop started"
  let state ← stateRef.get
  IO.println s!"[forge] Pool ID: {state.forgeParams.poolId.size} bytes"
  IO.println s!"[forge] Slot clock: system start={state.clock.systemStart}"
  IO.println "[forge] Waiting for chain sync to provide stake snapshot..."

  let mut snapshotReady := false
  while true do
    -- Sleep until next slot
    let state ← stateRef.get
    let msToWait ← state.clock.msUntilNextSlot
    if msToWait > 0 then
      IO.sleep (UInt32.ofNat msToWait)

    -- Check if consensus state has real stake data
    let cs ← consensusRef.get
    if cs.stakeSnapshot.totalStake == 0 then
      IO.sleep 5000  -- no stake yet, wait for sync to catch up
      continue
    if !snapshotReady then
      IO.println s!"[forge] Stake snapshot ready: {cs.stakeSnapshot.poolStakes.length} pools, total stake {cs.stakeSnapshot.totalStake}"
      IO.println s!"[forge] Epoch nonce: {cs.epochNonce.size} bytes, epoch {cs.currentEpoch}"
      snapshotReady := true

    let prevHash ← prevHashRef.get
    let blockNo ← blockNoRef.get

    let result ← forgeStep stateRef mempoolRef consensusRef ledgerStateRef prevHash blockNo
    match result with
    | .notYet _ => pure ()
    | .notLeader _slot =>
      -- Silent — too noisy to log every non-leader slot
      pure ()
    | .forged block => do
      IO.println s!"[forge] BLOCK FORGED at slot {block.slot}, block #{block.blockNumber}"
      IO.println s!"[forge]   header: {block.headerBytes.size} bytes, body: {block.bodyComponents.serialize.size} bytes"
      IO.println s!"[forge]   txs: {block.selectedTxs.transactions.length}"
      -- Self-validate before announcing
      let ls ← ledgerStateRef.atomically (fun ref => ref.get)
      let validationErrors ← validateForgedBlock block ls
      if validationErrors.isEmpty then
        IO.println s!"[forge]   ✓ self-validation PASSED"
      else
        IO.eprintln s!"[forge]   ✗ self-validation FAILED ({validationErrors.length} errors):"
        for err in validationErrors do
          IO.eprintln s!"[forge]     - {err}"
      -- Apply forged block to ledger state (UTxO + fees)
      let blockHash ← Cleanode.Network.Crypto.blake2b_256 block.headerBytes
      ledgerStateRef.atomically fun lsRef => do
        let mut s ← lsRef.get
        for tx in block.selectedTxs.transactions do
          let cur := Cleanode.Network.CborCursor.Cursor.mk' tx.bodyRawBytes
          match Cleanode.Network.ConwayBlock.parseTransactionBodyMapC cur with
          | some bodyResult =>
            let body := bodyResult.value
            s := { s with utxo := s.utxo.applyTx tx.txHash body,
                          epochFees := s.epochFees + body.fee }
          | none => pure ()
        lsRef.set { s with lastSlot := block.slot,
                           lastBlockNo := block.blockNumber,
                           lastBlockHash := blockHash }
      -- Accumulate VRF nonce output into consensus state
      let vrfBytes := ByteArray.mk block.vrfOutput.toArray
      let cs ← consensusRef.get
      let cs ← updateEvolvingNonceFromBlock cs block.slot vrfBytes
      consensusRef.set cs
      -- Remove forged txs from mempool
      let txHashes := block.selectedTxs.transactions.map (·.txHash)
      mempoolRef.modify (·.removeConfirmed txHashes)
      -- Push to announcement queue and advance chain tip
      forgedBlocksRef.modify fun arr => arr.push block
      blockNoRef.set (blockNo + 1)
      prevHashRef.set blockHash
    | .forgeError slot err =>
      IO.eprintln s!"[forge] Error at slot {slot}: {err}"
    | .epochTransition epoch =>
      IO.println s!"[forge] Now in epoch {epoch}"

/-- Start the forge loop as a background IO task. -/
def startForgeLoop (forgeParams : ForgeParams) (clock : SlotClock)
    (consensusRef : IO.Ref ConsensusState)
    (mempoolRef : IO.Ref Cleanode.Network.Mempool.Mempool)
    (ledgerStateRef : Std.Mutex Cleanode.Ledger.State.LedgerState)
    (prevHashRef : IO.Ref ByteArray) (blockNoRef : IO.Ref Nat)
    : IO (Task (Except IO.Error Unit) × IO.Ref ForgeState × IO.Ref (Array ForgedBlock)) := do
  let cs ← consensusRef.get
  let forgeState := ForgeState.initial forgeParams clock cs
  let stateRef ← IO.mkRef forgeState
  let forgedBlocksRef ← IO.mkRef (#[] : Array ForgedBlock)

  let task ← IO.asTask (prio := .dedicated) do
    runForgeLoop stateRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef forgedBlocksRef

  return (task, stateRef, forgedBlocksRef)

-- ====================
-- = Diagnostics      =
-- ====================

/-- Print forge loop statistics -/
def printForgeStats (stateRef : IO.Ref ForgeState) : IO Unit := do
  let state ← stateRef.get
  IO.println s!"[forge] Statistics:"
  IO.println s!"  Leader checks: {state.leaderChecks}"
  IO.println s!"  Times elected: {state.timesElected}"
  IO.println s!"  Blocks forged: {state.blocksForged}"
  IO.println s!"  Last checked slot: {state.lastCheckedSlot}"
  IO.println s!"  Current epoch: {state.consensusState.currentEpoch}"

end Cleanode.Consensus.Praos.ForgeLoop
