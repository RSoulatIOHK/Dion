import Cleanode.Storage.BlockStore
import Cleanode.Network.ChainSync

/-!
# Immutable Block Database

Stores blocks that are considered immutable (beyond the security parameter k).
Blocks are stored sequentially by block number for efficient access.

## Design
- Immutable blocks are stored in chunk files (each containing N blocks)
- An index maps block numbers to file offsets
- Once written, immutable blocks are never modified

## References
- Ouroboros Consensus: ImmutableDB design
-/

namespace Cleanode.Storage.ImmutableDB

open Cleanode.Storage.BlockStore
open Cleanode.Network.ChainSync
open System (FilePath)

-- ====================
-- = Types            =
-- ====================

/-- Configuration for the immutable DB -/
structure ImmutableDBConfig where
  basePath : FilePath := "data/immutable"
  chunkSize : Nat := 1000         -- Blocks per chunk file
  deriving Repr

/-- Index entry for a block in the immutable DB -/
structure ImmutableIndex where
  blockNo : Nat
  slot : Nat
  chunkId : Nat                    -- Which chunk file
  offset : Nat                     -- Byte offset within chunk
  size : Nat                       -- Block data size
  deriving Repr

/-- Immutable DB state -/
structure ImmutableDB where
  config : ImmutableDBConfig
  tip : Nat                        -- Highest immutable block number
  index : List ImmutableIndex      -- In-memory index
  deriving Repr

-- ====================
-- = Operations       =
-- ====================

/-- Initialize immutable DB -/
def ImmutableDB.init (config : ImmutableDBConfig := {}) : IO ImmutableDB := do
  IO.FS.createDirAll config.basePath
  return { config := config, tip := 0, index := [] }

/-- Get chunk file path for a block number -/
def ImmutableDB.chunkPath (db : ImmutableDB) (blockNo : Nat) : FilePath :=
  let chunkId := blockNo / db.config.chunkSize
  db.config.basePath / s!"chunk_{chunkId}.dat"

/-- Append a block to the immutable DB -/
def ImmutableDB.appendBlock (db : ImmutableDB) (block : SavedBlock) : IO ImmutableDB := do
  let chunkId := block.blockNo / db.config.chunkSize
  let chunkPath := db.chunkPath block.blockNo

  -- Serialize block data
  let mut data := ByteArray.empty
  -- Block number (8 bytes)
  for i in [0:8] do
    data := data.push (UInt8.ofNat ((block.blockNo / (256 ^ (7 - i))) % 256))
  -- Slot (8 bytes)
  for i in [0:8] do
    data := data.push (UInt8.ofNat ((block.slot / (256 ^ (7 - i))) % 256))
  -- Era (4 bytes)
  for i in [0:4] do
    data := data.push (UInt8.ofNat ((block.era / (256 ^ (3 - i))) % 256))
  -- Header size (4 bytes)
  let hSize := block.headerBytes.size
  for i in [0:4] do
    data := data.push (UInt8.ofNat ((hSize / (256 ^ (3 - i))) % 256))
  -- Header bytes
  data := data ++ block.headerBytes
  -- PrevHash size (4 bytes)
  let phSize := block.prevHash.size
  for i in [0:4] do
    data := data.push (UInt8.ofNat ((phSize / (256 ^ (3 - i))) % 256))
  -- PrevHash
  data := data ++ block.prevHash

  -- Get current file size for offset
  let offset ← try
    let existing ← IO.FS.readBinFile chunkPath
    pure existing.size
  catch _ =>
    pure 0

  -- Append to chunk file
  let handle ← IO.FS.Handle.mk chunkPath .append
  handle.write data

  let indexEntry : ImmutableIndex := {
    blockNo := block.blockNo,
    slot := block.slot,
    chunkId := chunkId,
    offset := offset,
    size := data.size
  }

  return { db with
    tip := block.blockNo,
    index := db.index ++ [indexEntry] }

/-- Read a block by number from the immutable DB -/
def ImmutableDB.readBlock (db : ImmutableDB) (blockNo : Nat) : IO (Option ByteArray) := do
  match db.index.find? (fun e => e.blockNo == blockNo) with
  | none => return none
  | some entry =>
    try
      let chunkPath := db.chunkPath blockNo
      let data ← IO.FS.readBinFile chunkPath
      if entry.offset + entry.size <= data.size then
        return some (data.extract entry.offset (entry.offset + entry.size))
      else
        return none
    catch _ =>
      return none

/-- Get the tip (highest immutable block) -/
def ImmutableDB.getTip (db : ImmutableDB) : Nat :=
  db.tip

/-- Count blocks in the immutable DB -/
def ImmutableDB.blockCount (db : ImmutableDB) : Nat :=
  db.index.length

end Cleanode.Storage.ImmutableDB
