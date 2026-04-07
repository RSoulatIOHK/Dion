import Dion.Storage.Database
import Dion.Network.ChainSync

/-!
# Unified Chain Database

High-level block storage that replaces the separate ImmutableDB and VolatileDB
with a single SQLite-backed store. Provides the same Ouroboros-style semantics:

- **Volatile blocks**: Recent blocks within the security parameter k, subject to rollback
- **Immutable blocks**: Blocks beyond k, permanently stored
- **Automatic promotion**: Volatile blocks are promoted to immutable as the chain grows

## Architecture
```
┌─────────────────────────────────┐
│           ChainDB               │
│  ┌───────────┬───────────────┐  │
│  │ Immutable  │   Volatile    │  │
│  │ (frozen)   │ (rollback ok) │  │
│  └───────────┴───────────────┘  │
│         SQLite3 (WAL mode)      │
└─────────────────────────────────┘
```

## References
- Ouroboros Consensus: ChainDB design
- Cardano Node: ImmutableDB + VolatileDB architecture
-/

namespace Dion.Storage.ChainDB

open Dion.Storage.Database
open Dion.Network.ChainSync

-- ====================
-- = Configuration    =
-- ====================

/-- ChainDB configuration -/
structure ChainDBConfig where
  dbPath : String := "data/chain.db"
  securityParam : Nat := 2160          -- k: maximum rollback depth
  deriving Repr

-- ====================
-- = ChainDB State    =
-- ====================

/-- The unified chain database -/
structure ChainDB where
  db : Database
  config : ChainDBConfig

/-- Statistics about the chain database -/
structure ChainDBStats where
  totalBlocks : Nat
  tipBlockNo : Option Nat
  deriving Repr

-- ====================
-- = Lifecycle        =
-- ====================

/-- Open or create the chain database -/
def ChainDB.open (config : ChainDBConfig := {}) : IO ChainDB := do
  let db ← Database.open config.dbPath
  return { db := db, config := config }

/-- Close the chain database -/
def ChainDB.close (cdb : ChainDB) : IO Unit :=
  cdb.db.close

-- ====================
-- = Block Operations =
-- ====================

/-- Begin a write transaction (call before a batch of addBlock/addBlockBody) -/
def ChainDB.beginBatch (cdb : ChainDB) : IO Unit := cdb.db.begin

/-- Commit a write transaction (call after a batch of addBlock/addBlockBody) -/
def ChainDB.commitBatch (cdb : ChainDB) : IO Unit := cdb.db.commit

/-- Add a new block (as volatile) -/
def ChainDB.addBlock (cdb : ChainDB) (blockNo slot era : Nat)
    (hash prevHash header : ByteArray) : IO Unit := do
  let row : BlockRow := {
    blockNo := blockNo, slot := slot, era := era,
    hash := hash, prevHash := prevHash, header := header,
    body := none, immutable := false
  }
  cdb.db.putBlock row

  -- Auto-promote: if chain has grown past k, mark old blocks as immutable
  if blockNo > cdb.config.securityParam then
    let immutableTip := blockNo - cdb.config.securityParam
    cdb.db.markImmutable immutableTip

/-- Store the full block body (fetched via BlockFetch protocol) -/
def ChainDB.addBlockBody (cdb : ChainDB) (blockNo : Nat) (body : ByteArray) : IO Unit :=
  cdb.db.putBlockBody blockNo body

/-- Retrieve a block by number -/
def ChainDB.getBlock (cdb : ChainDB) (blockNo : Nat) : IO (Option BlockRow) :=
  cdb.db.getBlock blockNo

/-- Retrieve a block by hash -/
def ChainDB.getBlockByHash (cdb : ChainDB) (hash : ByteArray) : IO (Option BlockRow) :=
  cdb.db.getBlockByHash hash

/-- Get the chain tip (highest block number) -/
def ChainDB.getTip (cdb : ChainDB) : IO (Option Nat) :=
  cdb.db.getTip

-- ====================
-- = Chain Management =
-- ====================

/-- Rollback the chain to a given block number.
    Only volatile (non-immutable) blocks can be rolled back.
    Returns the number of blocks removed. -/
def ChainDB.rollback (cdb : ChainDB) (toBlockNo : Nat) : IO Nat :=
  cdb.db.rollback toBlockNo

/-- Get block numbers in a slot range -/
def ChainDB.getBlocksInSlotRange (cdb : ChainDB) (fromSlot toSlot : Nat) : IO (Array Nat) :=
  cdb.db.getBlocksInSlotRange fromSlot toSlot

/-- Get database statistics -/
def ChainDB.stats (cdb : ChainDB) : IO ChainDBStats := do
  let total ← cdb.db.blockCount
  let tip ← cdb.db.getTip
  return { totalBlocks := total, tipBlockNo := tip }

-- ====================
-- = Sync State       =
-- ====================

/-- Save current sync position -/
def ChainDB.saveSyncState (cdb : ChainDB) (slot block : Nat) (hash : ByteArray) : IO Unit :=
  cdb.db.saveSyncState { lastSlot := slot, lastBlock := block, lastHash := hash }

/-- Load last sync position -/
def ChainDB.loadSyncState (cdb : ChainDB) : IO (Option Database.SyncState) :=
  cdb.db.loadSyncState

/-- Get the last synced point for ChainSync intersection -/
def ChainDB.getLastSyncedPoint (cdb : ChainDB) : IO (Option Point) := do
  match ← cdb.db.loadSyncState with
  | none => return none
  | some state =>
    return some { slot := UInt64.ofNat state.lastSlot, hash := state.lastHash }

end Dion.Storage.ChainDB
