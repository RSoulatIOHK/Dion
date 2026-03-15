import Cleanode.Network.Mempool
import Cleanode.Ledger.State

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

namespace Cleanode.Consensus.Praos.TxSelection

open Cleanode.Network.Mempool
open Cleanode.Ledger.State

-- ====================
-- = Selection Types  =
-- ====================

/-- A selected transaction with its metadata -/
structure SelectedTx where
  txHash : ByteArray
  rawBytes : ByteArray
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
    Returns the selected transactions and their total size/fees. -/
def selectTransactions (mempool : Mempool) (maxBodySize : Nat)
    : BlockBody :=
  -- Get available entries from mempool
  let available := mempool.entries
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
      let tx : SelectedTx := {
        txHash := entry.txHash
        rawBytes := entry.rawBytes
        fee := fee
        size := entry.size
      }
      (tx :: acc, curSize + entry.size, curFees + fee)
  ) ([], 0, 0)
  { transactions := selected.reverse
    totalSize := totalSize
    totalFees := totalFees }

end Cleanode.Consensus.Praos.TxSelection
