import Dion.Consensus.Praos.ConsensusState
import Dion.Consensus.Praos.LeaderElection
import Dion.Ledger.State

/-!
# Stake Distribution for Leader Election

Bridges the ledger's epoch boundary snapshots to the consensus layer's
`StakeSnapshot` used for leader election.

## Praos Stake Lag

Ouroboros Praos uses the stake distribution from **2 epochs ago** for
leader election in the current epoch. This ensures that:
1. Stake changes in epoch N don't affect leadership until epoch N+2
2. All honest nodes agree on the same stake distribution
3. Adversaries cannot manipulate the current epoch's leadership

## Snapshot Pipeline

```
Epoch N-2    Epoch N-1    Epoch N (current)
  ├─ snapshot ─────────────► used for leader election
  │            ├─ snapshot ─────────────► (future use)
  │            │            ├─ accumulating
```

## References
- Ouroboros Praos spec, Section 4.1: Stake distribution
- Cardano Ledger Spec: SNAP rule
-/

namespace Dion.Consensus.Praos.StakeDistribution

open Dion.Consensus.Praos.ConsensusState
open Dion.Consensus.Praos.LeaderElection
open Dion.Ledger.State

-- ====================
-- = Snapshot Ring    =
-- ====================

/-- Manages the 2-epoch lag for stake snapshots.
    Keeps the last 3 snapshots: mark (current accumulating),
    set (from 1 epoch ago), and go (from 2 epochs ago, used for elections). -/
structure StakeSnapshotRing where
  /-- "Go" snapshot: from 2 epochs ago, actively used for leader election -/
  goSnapshot : StakeSnapshot
  /-- Epoch when goSnapshot was taken -/
  goEpoch : Nat
  /-- "Set" snapshot: from 1 epoch ago, becomes "go" next epoch -/
  setSnapshot : StakeSnapshot
  /-- Epoch when setSnapshot was taken -/
  setEpoch : Nat
  /-- "Mark" snapshot: current epoch accumulation, becomes "set" next epoch -/
  markSnapshot : StakeSnapshot
  /-- Epoch when markSnapshot was taken -/
  markEpoch : Nat

instance : Repr StakeSnapshotRing where
  reprPrec r _ :=
    s!"StakeSnapshotRing(go=epoch{r.goEpoch}/{r.goSnapshot.poolStakes.length}pools, " ++
    s!"set=epoch{r.setEpoch}/{r.setSnapshot.poolStakes.length}pools, " ++
    s!"mark=epoch{r.markEpoch}/{r.markSnapshot.poolStakes.length}pools)"

/-- Empty snapshot ring for initial state -/
def StakeSnapshotRing.empty : StakeSnapshotRing :=
  let empty : StakeSnapshot := { poolStakes := [], totalStake := 0 }
  { goSnapshot := empty, goEpoch := 0,
    setSnapshot := empty, setEpoch := 0,
    markSnapshot := empty, markEpoch := 0 }

/-- Rotate snapshots at an epoch boundary:
    mark → set → go, then take a new mark from the ledger. -/
def StakeSnapshotRing.rotate (ring : StakeSnapshotRing) (newMark : StakeSnapshot)
    (newEpoch : Nat) : StakeSnapshotRing :=
  { goSnapshot := ring.setSnapshot, goEpoch := ring.setEpoch,
    setSnapshot := ring.markSnapshot, setEpoch := ring.markEpoch,
    markSnapshot := newMark, markEpoch := newEpoch }

/-- Get the active stake snapshot for leader election (the "go" snapshot). -/
def StakeSnapshotRing.activeSnapshot (ring : StakeSnapshotRing) : StakeSnapshot :=
  ring.goSnapshot

-- ====================
-- = Bridge Functions =
-- ====================

/-- Convert the ledger's EpochBoundaryState to a consensus StakeSnapshot. -/
def epochBoundaryToSnapshot (ebs : EpochBoundaryState) : StakeSnapshot :=
  { poolStakes := ebs.stakeDistribution,
    totalStake := ebs.totalStake }

/-- Build a StakeSnapshot directly from the ledger state.
    Uses an efficient two-pass algorithm:
      1. Build stakeKeyHash → poolId index from delegations (O(delegations))
      2. Scan UTxO once, accumulating stake per pool (O(UTxO))
    This avoids the O(pools × UTxO × delegators_per_pool) naive approach. -/
def buildSnapshotFromLedger (state : LedgerState) : StakeSnapshot :=
  -- Pass 1: Build stakeKeyHash → poolId lookup table
  let delegIndex : Std.HashMap ByteArray ByteArray :=
    state.delegation.delegations.foldl (fun m d => m.insert d.stakeKeyHash d.poolId)
      (Std.HashMap.emptyWithCapacity state.delegation.delegations.length)
  -- Pass 2: Scan UTxO once, accumulate per-pool stake
  let poolStakeMap : Std.HashMap ByteArray Nat :=
    state.utxo.map.fold (fun acc _ output =>
      -- Extract stake credential from base addresses (header type 0x0_ or 0x1_)
      if output.address.size < 57 then acc
      else
        let headerType := (output.address[0]!.toNat) >>> 4
        if headerType != 0 && headerType != 1 then acc
        else
          let stakeHash := output.address.extract 29 57
          match delegIndex[stakeHash]? with
          | none => acc
          | some poolId =>
            acc.insert poolId ((acc[poolId]?.getD 0) + output.amount)
    ) Std.HashMap.emptyWithCapacity
  let poolStakeList := poolStakeMap.toList
  let totalStake := poolStakeList.foldl (fun acc (_, s) => acc + s) 0
  { poolStakes := poolStakeList, totalStake := totalStake }

-- ====================
-- = Consensus Update =
-- ====================

/-- Update the ConsensusState's stake snapshot from the snapshot ring.
    Called at each epoch transition to install the correct (2-epoch-old) snapshot. -/
def updateConsensusStake (cs : ConsensusState) (ring : StakeSnapshotRing)
    : ConsensusState :=
  { cs with stakeSnapshot := ring.activeSnapshot }

/-- Full epoch transition: rotate the snapshot ring, compute new mark from ledger,
    then update consensus state with the new "go" snapshot. -/
def processStakeEpochTransition (cs : ConsensusState) (ring : StakeSnapshotRing)
    (ledger : LedgerState) (newEpoch : Nat)
    : ConsensusState × StakeSnapshotRing :=
  -- Build fresh snapshot from current ledger state
  let newMark := buildSnapshotFromLedger ledger
  -- Rotate: old mark → set → go
  let newRing := ring.rotate newMark newEpoch
  -- Update consensus with the new active (go) snapshot
  let newCs := updateConsensusStake cs newRing
  (newCs, newRing)

-- ====================
-- = Pool Lookup      =
-- ====================

/-- Look up a specific pool's stake in a snapshot. -/
def lookupPoolStake (snapshot : StakeSnapshot) (poolId : ByteArray) : Nat :=
  match snapshot.poolStakes.find? (fun (pid, _) => pid == poolId) with
  | some (_, stake) => stake
  | none => 0

/-- Get the relative stake of a pool (scaled by 10^18 for precision). -/
def relativeStake (snapshot : StakeSnapshot) (poolId : ByteArray) : Nat :=
  let poolStake := lookupPoolStake snapshot poolId
  if snapshot.totalStake == 0 then 0
  else poolStake * 1000000000000000000 / snapshot.totalStake

/-- List all pools with non-zero stake, sorted by stake descending. -/
def rankedPools (snapshot : StakeSnapshot) : List (ByteArray × Nat) :=
  snapshot.poolStakes
    |>.filter (fun (_, s) => s > 0)
    |>.mergeSort (fun (_, a) (_, b) => a > b)

-- ====================
-- = Diagnostics      =
-- ====================

/-- Print a summary of the current stake distribution. -/
def printStakeDistribution (ring : StakeSnapshotRing) : IO Unit := do
  IO.println s!"[stake] Snapshot ring:"
  IO.println s!"  go  (epoch {ring.goEpoch}): {ring.goSnapshot.poolStakes.length} pools, total={ring.goSnapshot.totalStake} lovelace"
  IO.println s!"  set (epoch {ring.setEpoch}): {ring.setSnapshot.poolStakes.length} pools, total={ring.setSnapshot.totalStake} lovelace"
  IO.println s!"  mark(epoch {ring.markEpoch}): {ring.markSnapshot.poolStakes.length} pools, total={ring.markSnapshot.totalStake} lovelace"
  let ranked := rankedPools ring.goSnapshot
  let top5 := ranked.take 5
  if top5.length > 0 then
    IO.println s!"  Top {top5.length} pools (by stake):"
    for pair in top5 do
      let poolId := pair.1
      let stake := pair.2
      let idHex := poolId.toList.take 4 |>.map (fun b => s!"{b.toNat}")
      IO.println s!"    pool[{String.intercalate "." idHex}...]: {stake} lovelace ({stake / 1000000} ADA)"

end Dion.Consensus.Praos.StakeDistribution
