import Dion.Network.ConwayBlock
import Dion.Network.TxSubmission2
import Dion.Network.Crypto
import Dion.Ledger.Validation
import Dion.Ledger.State
import Dion.Network.EraTx

/-!
# Transaction Mempool

In-memory pool of validated, unconfirmed transactions. The mempool serves as the
source for TxSubmission2 announcements and is pruned when transactions are
confirmed in blocks or expire.

## Design
- Transactions are validated against current ledger state before admission
- Entries track metadata: hash, size, timestamp for expiry
- Size-bounded: both by entry count and total byte size
- Thread-safe access via IO.Ref in the ConnectionManager

## References
- Cardano node mempool: capped at 2x max block size by default
-/

namespace Dion.Network.Mempool

open Dion.Network.ConwayBlock
open Dion.Network.TxSubmission2
open Dion.Network.Crypto
open Dion.Ledger.Validation
open Dion.Ledger.State

-- ====================
-- = Mempool Entry    =
-- ====================

/-- A transaction in the mempool with metadata -/
structure MempoolEntry where
  txHash      : ByteArray       -- Blake2b-256 of raw tx bytes
  transaction : Transaction
  rawBytes    : ByteArray       -- Serialized CBOR for network relay
  addedAt     : Nat             -- Timestamp in milliseconds
  size        : Nat             -- Byte size of rawBytes

instance : Repr MempoolEntry where
  reprPrec e _ := s!"MempoolEntry(hash={e.txHash.size}B, size={e.size}, age={e.addedAt})"

-- ====================
-- = Configuration    =
-- ====================

/-- Mempool configuration -/
structure MempoolConfig where
  maxSize   : Nat := 20000              -- Max number of transactions
  maxBytes  : Nat := 64 * 1024 * 1024  -- 64 MB capacity
  expiryMs  : Nat := 3600000           -- 1 hour TTL
  deriving Repr

-- ====================
-- = Mempool State    =
-- ====================

/-- The mempool: a bounded set of validated pending transactions -/
structure Mempool where
  entries    : List MempoolEntry
  totalBytes : Nat
  config     : MempoolConfig
  deriving Repr

-- ====================
-- = Construction     =
-- ====================

/-- Create an empty mempool with default or custom config -/
def Mempool.empty (config : MempoolConfig := {}) : Mempool :=
  { entries := [], totalBytes := 0, config := config }

-- ====================
-- = Queries          =
-- ====================

/-- Number of transactions in the mempool -/
def Mempool.size (pool : Mempool) : Nat :=
  pool.entries.length

/-- Total bytes of all transactions -/
def Mempool.byteSize (pool : Mempool) : Nat :=
  pool.totalBytes

/-- Check if the mempool contains a transaction by hash -/
def Mempool.contains (pool : Mempool) (txHash : ByteArray) : Bool :=
  pool.entries.any (fun e => e.txHash == txHash)

/-- Look up a transaction by hash -/
def Mempool.lookup (pool : Mempool) (txHash : ByteArray) : Option MempoolEntry :=
  pool.entries.find? (fun e => e.txHash == txHash)

/-- Get transaction IDs for TxSubmission2 announcements -/
def Mempool.getTxIds (pool : Mempool) (count : Nat) : List TxId :=
  (pool.entries.take count).map fun e =>
    { hash := e.txHash, size := UInt32.ofNat e.size }

/-- Get full transaction bytes by hash (for MsgReplyTxs) -/
def Mempool.getTxsByHash (pool : Mempool) (hashes : List ByteArray) : List ByteArray :=
  hashes.filterMap fun h =>
    (pool.entries.find? (fun e => e.txHash == h)).map (·.rawBytes)

-- ====================
-- = Mutations        =
-- ====================

/-- Add a transaction after validation against current ledger state.
    Returns updated mempool or validation error. -/
def Mempool.addTx (pool : Mempool) (state : LedgerState)
    (rawBytes : ByteArray) (tx : Transaction) : IO (Except ValidationError Mempool) := do
  -- Compute transaction hash
  let txHash ← blake2b_256 rawBytes
  -- Check for duplicates
  if pool.contains txHash then
    return .ok pool  -- Already have it, no-op
  -- Validate against ledger state
  let era := Dion.Network.EraTx.CardanoEra.Conway  -- Default to current era
  let errs ← validateTransaction state tx.body tx.witnesses era
  match errs with
  | e :: _ => return .error e
  | [] => do
      -- Check capacity
      if pool.entries.length >= pool.config.maxSize then
        return .error (.TxTooLarge 0 0)  -- Mempool full
      if pool.totalBytes + rawBytes.size > pool.config.maxBytes then
        return .error (.TxTooLarge 0 0)  -- Mempool byte limit
      let entry : MempoolEntry := {
        txHash := txHash
        transaction := tx
        rawBytes := rawBytes
        addedAt := (← IO.monoNanosNow) / 1000000  -- milliseconds since boot
        size := rawBytes.size
      }
      return .ok {
        entries := pool.entries ++ [entry]
        totalBytes := pool.totalBytes + rawBytes.size
        config := pool.config
      }

/-- Remove transactions confirmed in a block -/
def Mempool.removeConfirmed (pool : Mempool) (confirmedHashes : List ByteArray) : Mempool :=
  let remaining := pool.entries.filter fun e =>
    !confirmedHashes.any (· == e.txHash)
  let newBytes := remaining.foldl (fun acc e => acc + e.size) 0
  { pool with entries := remaining, totalBytes := newBytes }

/-- Remove mempool txs whose inputs were spent by confirmed transactions.
    This evicts stale txs that can never land because their UTxOs are gone. -/
def Mempool.removeStaleInputs (pool : Mempool) (spentInputs : List (ByteArray × Nat)) : Mempool :=
  let remaining := pool.entries.filter fun e =>
    -- Keep entry only if none of its inputs are in the spent set
    !e.transaction.body.inputs.any fun inp =>
      spentInputs.any fun (txId, idx) => inp.txId == txId && inp.outputIndex == idx
  let newBytes := remaining.foldl (fun acc e => acc + e.size) 0
  { pool with entries := remaining, totalBytes := newBytes }

/-- Remove a single transaction by hash -/
def Mempool.remove (pool : Mempool) (txHash : ByteArray) : Mempool :=
  let remaining := pool.entries.filter (fun e => e.txHash != txHash)
  let newBytes := remaining.foldl (fun acc e => acc + e.size) 0
  { pool with entries := remaining, totalBytes := newBytes }

/-- Prune mempool: remove expired entries and enforce size limits -/
def Mempool.prune (pool : Mempool) (currentTimeMs : Nat) : Mempool :=
  -- Remove expired entries
  let notExpired := pool.entries.filter fun e =>
    currentTimeMs - e.addedAt < pool.config.expiryMs
  -- Enforce max size (keep newest = last added)
  let trimmed := if notExpired.length > pool.config.maxSize
    then notExpired.drop (notExpired.length - pool.config.maxSize)
    else notExpired
  let newBytes := trimmed.foldl (fun acc e => acc + e.size) 0
  { pool with entries := trimmed, totalBytes := newBytes }

-- ====================
-- = Stats            =
-- ====================

/-- Mempool statistics for monitoring -/
structure MempoolStats where
  txCount   : Nat
  totalBytes : Nat
  oldestAge : Nat  -- Age of oldest entry in ms
  deriving Repr

/-- Get mempool statistics -/
def Mempool.stats (pool : Mempool) (currentTimeMs : Nat) : MempoolStats :=
  let oldestAge := match pool.entries.head? with
    | some e => currentTimeMs - e.addedAt
    | none => 0
  { txCount := pool.entries.length
    totalBytes := pool.totalBytes
    oldestAge := oldestAge }

/-- Add a raw transaction to mempool from peer relay (skip ledger validation).
    Peers already validated — we just dedup and check capacity. -/
def Mempool.addTxRaw (pool : Mempool) (rawBytes : ByteArray) (nowMs : Nat)
    : IO (Except String Mempool) := do
  let txHash ← blake2b_256 rawBytes
  if pool.contains txHash then return .ok pool  -- Already have it
  if pool.entries.length >= pool.config.maxSize then return .error "mempool full"
  if pool.totalBytes + rawBytes.size > pool.config.maxBytes then return .error "mempool byte limit"
  let entry : MempoolEntry := {
    txHash := txHash
    transaction := { body := { inputs := [], outputs := [], fee := 0, certificates := [], rawBytes := rawBytes }, witnesses := { redeemers := [] } }
    rawBytes := rawBytes
    addedAt := nowMs
    size := rawBytes.size
  }
  return .ok {
    entries := pool.entries ++ [entry]
    totalBytes := pool.totalBytes + rawBytes.size
    config := pool.config
  }

/-- Add a transaction to the mempool after full ledger validation.
    Parses the raw CBOR, runs validateTransaction, rejects if invalid. -/
def Mempool.addTxValidated (pool : Mempool) (rawBytes : ByteArray) (nowMs : Nat)
    (ledger : Dion.Ledger.State.LedgerState) (slot : Nat)
    : IO (Except String Mempool) := do
  -- Capacity checks first (cheap)
  if pool.totalBytes + rawBytes.size > pool.config.maxBytes then
    return .error "mempool byte limit"
  if pool.entries.length >= pool.config.maxSize then
    return .error "mempool full"
  -- Parse the standalone transaction
  match Dion.Network.ConwayBlock.parseStandaloneTx rawBytes with
  | none => return .error "tx parse failed"
  | some (body, witnesses, _rawBody) =>
    -- Dedup check
    let txHash ← Dion.Network.Crypto.blake2b_256 rawBytes
    if pool.contains txHash then return .ok pool
    -- Full ledger validation (same logic as block validation)
    let allInputsKnown := body.inputs.all fun inp =>
      let id : Dion.Ledger.UTxO.UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
      ledger.utxo.contains id
    if !allInputsKnown then
      return .error "unknown inputs (UTxO not synced yet)"
    let valErrs ← Dion.Ledger.Validation.validateTransaction
        ledger body witnesses .Conway slot rawBytes.size
    if !valErrs.isEmpty then
      return .error s!"validation failed: {String.intercalate ", " (valErrs.map fun e => s!"{repr e}")}"
    do
      let entry : MempoolEntry := {
        txHash, rawBytes
        transaction := { body, witnesses := { witnesses with } }
        addedAt := nowMs
        size := rawBytes.size
      }
      return .ok {
        entries := pool.entries ++ [entry]
        totalBytes := pool.totalBytes + rawBytes.size
        config := pool.config
      }

/-- Per-peer TxSubmission2 sliding window state.
    Tracks which txs have been announced to this peer. -/
structure TxSubmPeerState where
  announcedTxIds : List ByteArray
  /-- When a blocking MsgRequestTxIds arrives and the mempool is empty,
      we store the requested count here. The main receive loop checks this
      before each socket read and sends the reply inline when txs appear. -/
  pendingBlockingReq : Option Nat := none

def TxSubmPeerState.empty : TxSubmPeerState :=
  { announcedTxIds := [], pendingBlockingReq := none }

/-- Get tx IDs not yet announced to a specific peer, up to `count` -/
def Mempool.getNewTxIds (pool : Mempool) (announced : List ByteArray) (count : Nat) : List TxId :=
  let fresh := pool.entries.filter fun e =>
    !announced.any (· == e.txHash)
  (fresh.take count).map fun e =>
    { hash := e.txHash, size := UInt32.ofNat e.size }

end Dion.Network.Mempool
