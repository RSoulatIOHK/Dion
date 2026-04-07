import Dion.Network.ChainSync
import Dion.Network.Byron

/-!
# Block Storage

Simple persistent storage for synced blockchain data.
Stores block headers and tracks our current position on the chain.

## Storage Format

- `blocks/` directory contains individual block files
- `state.json` tracks our current sync position (last slot, block number, hash)

## Future Improvements

- Use a proper database (SQLite, RocksDB)
- Add indices for efficient lookups
- Implement block validation
- Add pruning/compaction
-/

namespace Dion.Storage.BlockStore

open Dion.Network.ChainSync
open Dion.Network.Byron
open System (FilePath)

/-- Current sync state -/
structure SyncState where
  lastSlot : UInt64
  lastBlockNo : UInt64
  lastHash : ByteArray

instance : Repr SyncState where
  reprPrec s _ := s!"SyncState(slot={s.lastSlot}, blockNo={s.lastBlockNo}, hash={s.lastHash.size}B)"

/-- Saved block data -/
structure SavedBlock where
  slot : Nat
  blockNo : Nat
  era : Nat
  headerBytes : ByteArray
  prevHash : ByteArray

instance : Repr SavedBlock where
  reprPrec b _ := s!"SavedBlock(slot={b.slot}, blockNo={b.blockNo}, era={b.era}, header={b.headerBytes.size}B, prevHash={b.prevHash.size}B)"

/-- Initialize block storage directory -/
def initStorage : IO Unit := do
  -- Create blocks directory if it doesn't exist
  let blocksDir : FilePath := "data/blocks"
  IO.FS.createDirAll blocksDir
  IO.println s!"✓ Storage initialized at {blocksDir}"

/-- Save a block header to disk -/
def saveBlock (slot : Nat) (blockNo : Nat) (era : Nat) (headerBytes : ByteArray) (prevHash : ByteArray) : IO Unit := do
  let filename : FilePath := s!"data/blocks/block_{blockNo}_{slot}.dat"

  -- Create a simple format: slot(8) | blockNo(8) | era(4) | hashLen(4) | hash | headerLen(4) | header
  let mut data := ByteArray.empty

  -- Slot (8 bytes, big-endian)
  for i in [0:8] do
    data := data.push (UInt8.ofNat ((slot / (256 ^ (7 - i))) % 256))

  -- Block number (8 bytes, big-endian)
  for i in [0:8] do
    data := data.push (UInt8.ofNat ((blockNo / (256 ^ (7 - i))) % 256))

  -- Era (4 bytes)
  for i in [0:4] do
    data := data.push (UInt8.ofNat ((era / (256 ^ (3 - i))) % 256))

  -- Previous hash length (4 bytes)
  let hashLen := prevHash.size
  for i in [0:4] do
    data := data.push (UInt8.ofNat ((hashLen / (256 ^ (3 - i))) % 256))

  -- Previous hash
  data := data ++ prevHash

  -- Header length (4 bytes)
  let headerLen := headerBytes.size
  for i in [0:4] do
    data := data.push (UInt8.ofNat ((headerLen / (256 ^ (3 - i))) % 256))

  -- Header bytes
  data := data ++ headerBytes

  -- Write to file
  IO.FS.writeBinFile filename data

/-- Save current sync state -/
def saveSyncState (state : SyncState) : IO Unit := do
  let filename : FilePath := "data/sync_state.dat"

  let mut data := ByteArray.empty

  -- Last slot (8 bytes)
  for i in [0:8] do
    data := data.push (UInt8.ofNat ((state.lastSlot.toNat / (256 ^ (7 - i))) % 256))

  -- Last block number (8 bytes)
  for i in [0:8] do
    data := data.push (UInt8.ofNat ((state.lastBlockNo.toNat / (256 ^ (7 - i))) % 256))

  -- Hash length (4 bytes)
  let hashLen := state.lastHash.size
  for i in [0:4] do
    data := data.push (UInt8.ofNat ((hashLen / (256 ^ (3 - i))) % 256))

  -- Hash
  data := data ++ state.lastHash

  IO.FS.writeBinFile filename data

/-- Load sync state from disk -/
def loadSyncState : IO (Option SyncState) := do
  let filename : FilePath := "data/sync_state.dat"

  -- Try to read file, return none if it doesn't exist
  try
    let data ← IO.FS.readBinFile filename

    if data.size < 20 then
      return none

    -- Read last slot (8 bytes)
    let mut lastSlot := 0
    for i in [0:8] do
      lastSlot := lastSlot * 256 + data[i]!.toNat

    -- Read last block number (8 bytes)
    let mut lastBlockNo := 0
    for i in [8:16] do
      lastBlockNo := lastBlockNo * 256 + data[i]!.toNat

    -- Read hash length (4 bytes)
    let mut hashLen := 0
    for i in [16:20] do
      hashLen := hashLen * 256 + data[i]!.toNat

    -- Read hash
    if data.size < 20 + hashLen then
      return none

    let lastHash := data.extract 20 (20 + hashLen)

    return some {
      lastSlot := UInt64.ofNat lastSlot,
      lastBlockNo := UInt64.ofNat lastBlockNo,
      lastHash := lastHash
    }
  catch _ =>
    return none

/-- Get the last synced point for intersection finding -/
def getLastSyncedPoint : IO (Option Point) := do
  match ← loadSyncState with
  | none => return none
  | some state =>
      return some {
        slot := state.lastSlot,
        hash := state.lastHash
      }

/-- Count synced blocks -/
def countSyncedBlocks : IO Nat := do
  -- Return block count from sync state (block number)
  match ← loadSyncState with
  | none => return 0
  | some state => return state.lastBlockNo.toNat

end Dion.Storage.BlockStore
