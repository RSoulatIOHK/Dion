import Cleanode.Storage.BlockStore
import Cleanode.Network.ChainSync

/-!
# Volatile Block Database

Stores recent blocks that may still be subject to rollback.
Blocks are kept in memory and persisted for crash recovery.

## Design
- Recent blocks within the security parameter k are stored here
- When blocks become old enough (beyond k), they move to ImmutableDB
- Supports rollback by removing blocks

## References
- Ouroboros Consensus: VolatileDB design
-/

namespace Cleanode.Storage.VolatileDB

open Cleanode.Storage.BlockStore
open Cleanode.Network.ChainSync
open System (FilePath)

-- ====================
-- = Types            =
-- ====================

/-- Volatile DB configuration -/
structure VolatileDBConfig where
  basePath : FilePath := "data/volatile"
  securityParam : Nat := 2160    -- k: security parameter (blocks that can be rolled back)
  deriving Repr

/-- Block entry in volatile storage -/
structure VolatileBlock where
  blockNo : Nat
  slot : Nat
  era : Nat
  hash : ByteArray                -- Block hash for chain linking
  prevHash : ByteArray            -- Previous block hash
  headerBytes : ByteArray
  bodyBytes : Option ByteArray    -- Full block body (if fetched)

instance : Repr VolatileBlock where
  reprPrec b _ := s!"VolatileBlock(blockNo={b.blockNo}, slot={b.slot}, era={b.era})"

/-- Volatile DB state -/
structure VolatileDB where
  config : VolatileDBConfig
  blocks : List VolatileBlock     -- Recent blocks (newest first)

instance : Repr VolatileDB where
  reprPrec db _ := s!"VolatileDB(blocks={db.blocks.length})"

-- ====================
-- = Operations       =
-- ====================

/-- Initialize volatile DB -/
def VolatileDB.init (config : VolatileDBConfig := {}) : IO VolatileDB := do
  IO.FS.createDirAll config.basePath
  return { config := config, blocks := [] }

/-- Add a block to volatile storage -/
def VolatileDB.addBlock (db : VolatileDB) (block : VolatileBlock) : VolatileDB :=
  { db with blocks := block :: db.blocks }

/-- Get a block by number -/
def VolatileDB.getBlock (db : VolatileDB) (blockNo : Nat) : Option VolatileBlock :=
  db.blocks.find? (fun b => b.blockNo == blockNo)

/-- Get a block by hash -/
def VolatileDB.getBlockByHash (db : VolatileDB) (hash : ByteArray) : Option VolatileBlock :=
  db.blocks.find? (fun b => b.hash == hash)

/-- Get the tip (newest block) -/
def VolatileDB.tip (db : VolatileDB) : Option VolatileBlock :=
  db.blocks.head?

/-- Rollback to a specific block number (remove all blocks after it) -/
def VolatileDB.rollbackTo (db : VolatileDB) (blockNo : Nat) : VolatileDB :=
  { db with blocks := db.blocks.filter (fun b => b.blockNo <= blockNo) }

/-- Get blocks that can be moved to immutable storage -/
def VolatileDB.immutableBlocks (db : VolatileDB) : List VolatileBlock :=
  match db.blocks.head? with
  | none => []
  | some newest =>
      let cutoff := if newest.blockNo > db.config.securityParam
                    then newest.blockNo - db.config.securityParam
                    else 0
      db.blocks.filter (fun b => b.blockNo <= cutoff)

/-- Remove blocks that have been moved to immutable storage -/
def VolatileDB.pruneImmutable (db : VolatileDB) (upToBlockNo : Nat) : VolatileDB :=
  { db with blocks := db.blocks.filter (fun b => b.blockNo > upToBlockNo) }

/-- Count blocks in volatile DB -/
def VolatileDB.blockCount (db : VolatileDB) : Nat :=
  db.blocks.length

/-- Persist volatile DB state to disk for crash recovery -/
def VolatileDB.persist (db : VolatileDB) : IO Unit := do
  -- Write each volatile block to a file
  for block in db.blocks do
    let filename : FilePath := db.config.basePath / s!"vol_{block.blockNo}_{block.slot}.dat"
    let mut data := ByteArray.empty
    -- Block number (8 bytes)
    for i in [0:8] do
      data := data.push (UInt8.ofNat ((block.blockNo / (256 ^ (7 - i))) % 256))
    -- Slot (8 bytes)
    for i in [0:8] do
      data := data.push (UInt8.ofNat ((block.slot / (256 ^ (7 - i))) % 256))
    -- Header
    let hSize := block.headerBytes.size
    for i in [0:4] do
      data := data.push (UInt8.ofNat ((hSize / (256 ^ (3 - i))) % 256))
    data := data ++ block.headerBytes
    IO.FS.writeBinFile filename data

end Cleanode.Storage.VolatileDB
