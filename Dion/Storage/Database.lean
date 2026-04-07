/-!
# SQLite Database Backend

FFI bindings to SQLite3 for persistent block storage.
Provides type-safe Lean wrappers around the C FFI layer.

## Schema
- `blocks`: block_no, slot, era, hash, prev_hash, header, body, immutable
- `sync_state`: last synced position (slot, block, hash)
- `metadata`: key-value store for configuration

## Usage
```
let db ← Database.open "data/chain.db"
db.putBlock { blockNo := 1, slot := 100, ... }
let block ← db.getBlock 1
db.close
```
-/

namespace Dion.Storage.Database

-- ====================
-- = Opaque DB Handle =
-- ====================

/-- Opaque handle to an open SQLite3 database -/
opaque DatabasePointer : NonemptyType
def Database : Type := DatabasePointer.type
instance : Nonempty Database := DatabasePointer.property

-- ====================
-- = Block Row Type   =
-- ====================

/-- A row from the blocks table -/
structure BlockRow where
  blockNo   : Nat
  slot      : Nat
  era       : Nat
  hash      : ByteArray
  prevHash  : ByteArray
  header    : ByteArray
  body      : Option ByteArray
  immutable : Bool

instance : Repr BlockRow where
  reprPrec b _ :=
    s!"BlockRow(blockNo={b.blockNo}, slot={b.slot}, era={b.era}, hash={b.hash.size}B, immutable={b.immutable})"

-- ====================
-- = FFI Declarations =
-- ====================

/-- Open or create a database at the given path -/
@[extern "dion_db_open"]
opaque dbOpen (path : @& String) : IO Database

/-- Close the database -/
@[extern "dion_db_close"]
opaque dbClose (db : @& Database) : IO Unit

/-- Begin a transaction (batches subsequent writes into one fsync) -/
@[extern "dion_db_begin"]
opaque dbBegin (db : @& Database) : IO Unit

/-- Commit a transaction -/
@[extern "dion_db_commit"]
opaque dbCommit (db : @& Database) : IO Unit

/-- Store a block -/
@[extern "dion_db_put_block"]
opaque dbPutBlock (db : @& Database) (blockNo : @& Nat) (slot : @& Nat) (era : @& Nat)
    (hash : @& ByteArray) (prevHash : @& ByteArray) (header : @& ByteArray)
    (immutable : Bool) : IO Unit

/-- Store block body data -/
@[extern "dion_db_put_block_body"]
opaque dbPutBlockBody (db : @& Database) (blockNo : @& Nat) (body : @& ByteArray) : IO Unit

/-- Retrieve a block by block number -/
@[extern "dion_db_get_block"]
opaque dbGetBlockRaw (db : @& Database) (blockNo : @& Nat) : IO (Option BlockRow)

/-- Retrieve a block by hash -/
@[extern "dion_db_get_block_by_hash"]
opaque dbGetBlockByHashRaw (db : @& Database) (hash : @& ByteArray) : IO (Option BlockRow)

/-- Get the tip (highest block number) -/
@[extern "dion_db_get_tip"]
opaque dbGetTip (db : @& Database) : IO (Option Nat)

/-- Mark blocks up to a given number as immutable -/
@[extern "dion_db_mark_immutable"]
opaque dbMarkImmutable (db : @& Database) (upTo : @& Nat) : IO Unit

/-- Rollback: delete volatile blocks above a given block number. Returns count deleted. -/
@[extern "dion_db_rollback"]
opaque dbRollback (db : @& Database) (keepUpTo : @& Nat) : IO Nat

/-- Count total blocks -/
@[extern "dion_db_count_blocks"]
opaque dbCountBlocks (db : @& Database) : IO Nat

/-- Save sync state -/
@[extern "dion_db_save_sync_state"]
opaque dbSaveSyncState (db : @& Database) (lastSlot : @& Nat) (lastBlock : @& Nat)
    (lastHash : @& ByteArray) : IO Unit

/-- Load sync state -/
@[extern "dion_db_load_sync_state"]
opaque dbLoadSyncStateRaw (db : @& Database) : IO (Option (Nat × Nat × ByteArray))

/-- Get block numbers in a slot range -/
@[extern "dion_db_get_blocks_in_range"]
opaque dbGetBlocksInRange (db : @& Database) (fromSlot : @& Nat) (toSlot : @& Nat) : IO (Array Nat)

-- ====================
-- = High-Level API   =
-- ====================

/-- Sync state: last known position on chain -/
structure SyncState where
  lastSlot  : Nat
  lastBlock : Nat
  lastHash  : ByteArray

instance : Repr SyncState where
  reprPrec s _ := s!"SyncState(slot={s.lastSlot}, block={s.lastBlock}, hash={s.lastHash.size}B)"

namespace Database

/-- Open a database at the given path -/
def «open» (path : String := "data/chain.db") : IO Database := do
  dbOpen path

/-- Close the database -/
def «close» (db : Database) : IO Unit :=
  dbClose db

/-- Begin a write transaction -/
def begin (db : Database) : IO Unit := dbBegin db

/-- Commit a write transaction -/
def commit (db : Database) : IO Unit := dbCommit db

/-- Store a block -/
def putBlock (db : Database) (row : BlockRow) : IO Unit :=
  dbPutBlock db row.blockNo row.slot row.era row.hash row.prevHash row.header row.immutable

/-- Store block body data (fetched separately via BlockFetch) -/
def putBlockBody (db : Database) (blockNo : Nat) (body : ByteArray) : IO Unit :=
  dbPutBlockBody db blockNo body

/-- Retrieve a block by number -/
def getBlock (db : Database) (blockNo : Nat) : IO (Option BlockRow) :=
  dbGetBlockRaw db blockNo

/-- Retrieve a block by hash -/
def getBlockByHash (db : Database) (hash : ByteArray) : IO (Option BlockRow) :=
  dbGetBlockByHashRaw db hash

/-- Get the tip block number -/
def getTip (db : Database) : IO (Option Nat) :=
  dbGetTip db

/-- Mark all blocks up to blockNo as immutable (beyond security parameter k) -/
def markImmutable (db : Database) (upTo : Nat) : IO Unit :=
  dbMarkImmutable db upTo

/-- Rollback volatile blocks above a given block number -/
def rollback (db : Database) (keepUpTo : Nat) : IO Nat :=
  dbRollback db keepUpTo

/-- Total number of stored blocks -/
def blockCount (db : Database) : IO Nat :=
  dbCountBlocks db

/-- Save current sync position -/
def saveSyncState (db : Database) (state : SyncState) : IO Unit :=
  dbSaveSyncState db state.lastSlot state.lastBlock state.lastHash

/-- Load last sync position -/
def loadSyncState (db : Database) : IO (Option SyncState) := do
  match ← dbLoadSyncStateRaw db with
  | none => return none
  | some (slot, block, hash) =>
    return some { lastSlot := slot, lastBlock := block, lastHash := hash }

/-- Get block numbers in a slot range (useful for chain selection) -/
def getBlocksInSlotRange (db : Database) (fromSlot toSlot : Nat) : IO (Array Nat) :=
  dbGetBlocksInRange db fromSlot toSlot

end Database

end Dion.Storage.Database
