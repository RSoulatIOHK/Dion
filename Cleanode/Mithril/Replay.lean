import Cleanode.Mithril.ImmutableDB
import Cleanode.Network.ConwayBlock
import Cleanode.Network.CborCursor
import Cleanode.Network.Crypto
import Cleanode.Ledger.State
import Cleanode.Ledger.UTxO
import Cleanode.Ledger.Certificate

/-!
# ImmutableDB Replay — UTxO Set Reconstruction

After Mithril fast-sync downloads the ImmutableDB, we need to replay all blocks
to build the UTxO set. This module reads every block from every chunk file,
extracts transactions, and applies UTxO changes (consume inputs, produce outputs).

Validation is skipped — we trust Mithril-verified data.

## ImmutableDB Block Format
Blocks in the chunk file are stored as `[era_id, block]` where:
- era_id: 0=Byron, 1=Shelley, 2=Allegra, 3=Mary, 4=Alonzo, 5=Babbage, 6=Babbage', 7=Conway
- block: the 5-element array `[header, tx_bodies, witness_sets, aux_data, invalid_txs]`

The secondary index has NO block size — sizes are computed from consecutive offsets.
-/

namespace Cleanode.Mithril.Replay

open Cleanode.Mithril.ImmutableDB
open Cleanode.Network.ConwayBlock
open Cleanode.Network.CborCursor
open Cleanode.Network.Crypto
open Cleanode.Ledger.State
open Cleanode.Ledger.UTxO
open Cleanode.Ledger.Certificate

/-- Progress callback for replay -/
structure ReplayProgress where
  chunkNum    : Nat   -- Current chunk being processed
  totalChunks : Nat   -- Total chunk files to replay
  blocksInChunk : Nat -- Blocks processed in this chunk
  totalBlocks : Nat   -- Total blocks replayed so far
  utxoSize    : Nat   -- Current UTxO set size
  lastSlot    : Nat   -- Slot of last processed block
  skippedByron : Nat  -- Blocks skipped (Byron era)

/-- Find all chunk numbers in the immutable directory, sorted ascending -/
def findAllChunkNumbers (immutableDir : String) : IO (List Nat) := do
  let entries ← System.FilePath.readDir immutableDir
  let mut chunks : List Nat := []
  for entry in entries do
    let name := entry.fileName
    if name.endsWith ".chunk" then
      let numStr := name.dropRight 6
      match numStr.toNat? with
      | some n => chunks := n :: chunks
      | none => pure ()
  return chunks.mergeSort (· ≤ ·)

/-- Read all secondary index entries from a .secondary file.
    Computes block sizes from consecutive offsets (more efficiently). -/
def readAllSecondaryEntries (secondaryPath : String) (chunkFileSize : Nat) : IO (Array SecondaryEntry) := do
  let contents ← IO.FS.readBinFile secondaryPath
  let entryCount := contents.size / secondaryEntrySize
  -- First pass: parse all entries into an array
  let mut rawEntries : Array SecondaryEntry := Array.mkEmpty entryCount
  for i in List.range entryCount do
    let offset := i * secondaryEntrySize
    match parseSecondaryEntry contents offset with
    | some entry => rawEntries := rawEntries.push entry
    | none => pure ()
  -- Second pass: compute block sizes from consecutive offsets
  let mut result : Array SecondaryEntry := Array.mkEmpty rawEntries.size
  for i in List.range rawEntries.size do
    let entry : SecondaryEntry := rawEntries[i]!
    let nextOffset : Nat :=
      if i + 1 < rawEntries.size then (rawEntries[i + 1]! : SecondaryEntry).blockOffset.toNat
      else chunkFileSize
    let size := nextOffset - entry.blockOffset.toNat
    result := result.push { entry with blockSize := size }
  return result

/-- Read a block's raw bytes from a chunk file -/
def readBlockFromChunk (chunkData : ByteArray) (entry : SecondaryEntry) : Option ByteArray :=
  let offset := entry.blockOffset.toNat
  let size := entry.blockSize
  if offset + size ≤ chunkData.size && size > 0 then
    some (chunkData.extract offset (offset + size))
  else
    none

/-- Unwrap the ImmutableDB era wrapper: [era_id, block] → block bytes.
    Byron (era 0) blocks are skipped (returns none). -/
def unwrapEraBlock (blockBytes : ByteArray) : Option ByteArray :=
  if blockBytes.size < 3 then none
  else
    -- Expect CBOR array of 2: 0x82
    let b0 := blockBytes[0]!.toNat
    if b0 != 0x82 then none
    else
      let era := blockBytes[1]!.toNat
      if era == 0 then none  -- Byron era — skip
      else
        -- Skip the outer array header (0x82) and era byte
        some (blockBytes.extract 2 blockBytes.size)

/-- Convert a raw certificate to a ledger certificate -/
def rawCertToLedger : RawCertificate → Option Certificate
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

/-- Process a single block: parse transactions and apply UTxO changes.
    Returns updated ledger state. -/
def replayBlock (s : LedgerState) (blockBytes : ByteArray) : IO LedgerState := do
  -- Unwrap the [era, block] envelope from ImmutableDB format
  match unwrapEraBlock blockBytes with
  | none => return s  -- Byron era or malformed — skip
  | some innerBytes =>
  let parsed ← parseConwayBlockBodyIO innerBytes
  match parsed with
  | none => return s  -- Unparseable block — skip
  | some body =>
    let mut st := s
    for tx in body.transactions do
      -- Apply certificates (pool registrations, delegations, etc.)
      let certs := tx.body.certificates.filterMap rawCertToLedger
      st := applyCertificates st certs
      -- Apply UTxO changes: remove consumed inputs, add new outputs
      let txHash ← blake2b_256 tx.body.rawBytes
      st := { st with utxo := st.utxo.applyTx txHash tx.body }
    return st

/-- Replay ImmutableDB blocks to reconstruct the UTxO set.
    `maxChunks`: if > 0, only replay the last N chunks (faster startup, covers recent UTxOs).
    Default 0 = replay all chunks. -/
partial def replayImmutableDB (dbDir : String) (initialState : LedgerState)
    (report : ReplayProgress → IO Unit := fun _ => pure ())
    (maxChunks : Nat := 0)
    : IO LedgerState := do
  let immutableDir := s!"{dbDir}/immutable"

  -- Find all chunk files
  let allChunks ← findAllChunkNumbers immutableDir
  if allChunks.isEmpty then
    IO.eprintln "[replay] No chunk files found — UTxO set will be empty"
    return initialState

  -- If maxChunks > 0, only replay the last N chunks
  let chunks := if maxChunks > 0 && allChunks.length > maxChunks then
    allChunks.drop (allChunks.length - maxChunks)
  else allChunks

  let skippedCount := allChunks.length - chunks.length
  IO.println s!"[replay] Found {allChunks.length} chunk files, replaying last {chunks.length} (skipping {skippedCount} oldest)"

  let mut state := initialState
  let mut totalBlocks : Nat := 0
  let mut byronSkipped : Nat := 0

  for chunkIdx in List.range chunks.length do
    let chunkNum := chunks[chunkIdx]!
    let chunkPath := s!"{immutableDir}/{formatChunkNumber chunkNum}.chunk"
    let secondaryPath := s!"{immutableDir}/{formatChunkNumber chunkNum}.secondary"

    -- Check secondary index exists
    let secExists ← (⟨secondaryPath⟩ : System.FilePath).pathExists
    if !secExists then continue

    -- Read the chunk file first (need size for computing block sizes)
    let chunkData ← IO.FS.readBinFile chunkPath

    -- Quick check: if first block is Byron (era 0), skip entire chunk
    if chunkData.size >= 2 && chunkData[0]!.toNat == 0x82 && chunkData[1]!.toNat == 0 then
      -- Count entries for accurate block count
      let secData ← IO.FS.readBinFile secondaryPath
      let entryCount := secData.size / secondaryEntrySize
      byronSkipped := byronSkipped + entryCount
      -- Only report every 100 chunks during Byron skip
      if chunkIdx % 100 == 0 then
        report {
          chunkNum := chunkIdx + 1, totalChunks := chunks.length
          blocksInChunk := 0, totalBlocks, utxoSize := state.utxo.size
          lastSlot := state.lastSlot, skippedByron := byronSkipped
        }
      continue

    -- Read secondary index entries (with computed block sizes)
    let entries ← readAllSecondaryEntries secondaryPath chunkData.size

    let mut blocksInChunk : Nat := 0
    for entry in entries.toList do
      -- Skip EBBs (Epoch Boundary Blocks)
      if entry.isEBB then continue

      match readBlockFromChunk chunkData entry with
      | none => pure ()
      | some blockBytes =>
        state ← replayBlock state blockBytes
        blocksInChunk := blocksInChunk + 1
        totalBlocks := totalBlocks + 1

        -- Report progress every 10000 blocks
        if totalBlocks % 10000 == 0 then
          let slot := entry.slot.toNat
          state := { state with lastSlot := slot }
          report {
            chunkNum := chunkIdx + 1, totalChunks := chunks.length
            blocksInChunk, totalBlocks, utxoSize := state.utxo.size
            lastSlot := slot, skippedByron := byronSkipped
          }

    -- Report at end of each chunk (every 50 chunks to reduce output)
    if chunkIdx % 50 == 0 || chunkIdx == chunks.length - 1 then
      let lastSlot := match entries.back? with
        | some e => e.slot.toNat
        | none => state.lastSlot
      state := { state with lastSlot := lastSlot }
      report {
        chunkNum := chunkIdx + 1, totalChunks := chunks.length
        blocksInChunk, totalBlocks, utxoSize := state.utxo.size
        lastSlot := lastSlot, skippedByron := byronSkipped
      }

  IO.println s!"[replay] Done! Replayed {totalBlocks} blocks ({byronSkipped} Byron skipped), UTxO set size: {state.utxo.size}"
  return state

/-- Default progress reporter for replay (prints to stdout) -/
def defaultReplayReporter (p : ReplayProgress) : IO Unit :=
  let pct := if p.totalChunks > 0 then p.chunkNum * 100 / p.totalChunks else 0
  IO.println s!"[replay] {pct}% — chunk {p.chunkNum}/{p.totalChunks}, {p.totalBlocks} blocks, UTxO: {p.utxoSize}, slot: {p.lastSlot}"

end Cleanode.Mithril.Replay
