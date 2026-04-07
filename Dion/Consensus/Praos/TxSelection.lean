import Dion.Network.Mempool
import Dion.Network.ConwayBlock
import Dion.Ledger.State

/-!
# Transaction Selection for Block Forging

Selects transactions from the mempool to include in a new block.
Uses a greedy algorithm sorted by fee density (fee / size).

## Constraints
- Total size must fit within maxBlockBodySize
- Total execution units must fit within maxBlockExUnits
- Each transaction must still be valid against current ledger state

## References
- Cardano Node: Mempool snapshotting and tx selection
-/

namespace Dion.Consensus.Praos.TxSelection

open Dion.Network.Mempool
open Dion.Ledger.State

-- ====================
-- = Selection Types  =
-- ====================

/-- A selected transaction with its metadata -/
structure SelectedTx where
  txHash : ByteArray
  rawBytes : ByteArray           -- Full raw tx CBOR (for relay/hashing)
  bodyRawBytes : ByteArray       -- Raw CBOR of tx body only (for block body encoding)
  witnessRawBytes : ByteArray    -- Raw CBOR of witness set only (for block body encoding)
  auxDataRawBytes : Option ByteArray  -- Raw CBOR of auxiliary data (if present)
  fee : Nat
  size : Nat

/-- Block body after transaction selection -/
structure BlockBody where
  transactions : List SelectedTx
  totalSize : Nat
  totalFees : Nat

-- ====================
-- = Selection Logic  =
-- ====================

/-- Compute fee density (fee per byte, scaled by 1000 for integer arithmetic) -/
private def feeDensity (fee size : Nat) : Nat :=
  if size == 0 then 0
  else fee * 1000 / size

/-- Select transactions from the mempool for inclusion in a new block.
    maxBodySize: maximum block body size in bytes
    currentSlot: current slot number (used to filter expired transactions)
    Returns the selected transactions and their total size/fees. -/
def selectTransactions (mempool : Mempool) (maxBodySize : Nat)
    (currentSlot : Nat := 0) : BlockBody :=
  -- Get available entries from mempool, filtering out expired txs
  let available := mempool.entries.filter fun e =>
    match e.transaction.body.ttl with
    | some ttl => ttl >= currentSlot
    | none => true  -- No TTL = never expires
  -- Sort by fee density (descending) - higher fee density first
  let sorted := available.toArray
    |>.qsort (fun a b =>
      feeDensity a.transaction.body.fee a.size > feeDensity b.transaction.body.fee b.size)
    |>.toList
  -- Greedily select transactions that fit
  let (selected, totalSize, totalFees) := sorted.foldl (fun (acc, curSize, curFees) entry =>
    if curSize + entry.size > maxBodySize then
      (acc, curSize, curFees)  -- Skip: doesn't fit
    else
      let fee := entry.transaction.body.fee
      -- Split raw tx CBOR into body/witness/auxdata components
      let (bodyBytes, witnessBytes, auxBytes) :=
        match Dion.Network.ConwayBlock.splitTxCbor entry.rawBytes with
        | some components => (components.bodyRawBytes, components.witnessRawBytes, components.auxDataRawBytes)
        | none => (entry.rawBytes, Dion.Network.Cbor.encodeMapHeader 0, none)  -- fallback
      let tx : SelectedTx := {
        txHash := entry.txHash
        rawBytes := entry.rawBytes
        bodyRawBytes := bodyBytes
        witnessRawBytes := witnessBytes
        auxDataRawBytes := auxBytes
        fee := fee
        size := entry.size
      }
      (tx :: acc, curSize + entry.size, curFees + fee)
  ) ([], 0, 0)
  { transactions := selected.reverse
    totalSize := totalSize
    totalFees := totalFees }

end Dion.Consensus.Praos.TxSelection
