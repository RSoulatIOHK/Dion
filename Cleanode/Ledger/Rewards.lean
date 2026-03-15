import Cleanode.Ledger.State
import Cleanode.Config.Genesis

/-!
# Rewards Calculation

Implements the Cardano rewards formula per the Shelley specification.

## Formula
For each pool:
  poolReward = (R / (1 + a0)) * (σ' + s' * a0 * ((σ' - s' * z0) / z0))

Where:
- R = total rewards pot for the epoch
- a0 = pledge influence factor (protocol parameter)
- σ' = pool's relative stake (pool_stake / total_stake)
- s' = pool's relative pledge (pledge / total_stake)
- z0 = 1 / nOpt (saturation point)

Pool operator receives: cost + margin * (poolReward - cost)
Delegators share the remainder proportionally.

All arithmetic uses scaled integers to avoid floating-point non-determinism.

## References
- Shelley Formal Specification, Section 11.7
- Design Specification for Delegation and Incentives in Cardano
-/

namespace Cleanode.Ledger.Rewards

open Cleanode.Ledger.State
open Cleanode.Config.Genesis

-- ====================
-- = Constants        =
-- ====================

/-- Precision scale factor for fixed-point arithmetic -/
def scale : Nat := 1000000000000  -- 10^12

-- ====================
-- = Reward Types     =
-- ====================

/-- Reward parameters from protocol -/
structure RewardParams where
  totalRewardsPot : Nat       -- R: total rewards for this epoch (lovelace)
  totalStake : Nat            -- Total active stake
  a0Numerator : Nat           -- Pledge influence factor numerator
  a0Denominator : Nat         -- Pledge influence factor denominator (default 1)
  nOpt : Nat                  -- Desired number of pools (k parameter)
  deriving Repr

/-- Per-pool reward breakdown -/
structure PoolReward where
  poolId : ByteArray
  totalReward : Nat           -- Total reward for the pool (lovelace)
  operatorReward : Nat        -- Reward going to the pool operator
  delegatorRewards : Nat      -- Reward shared among delegators

instance : Repr PoolReward where
  reprPrec r _ := s!"PoolReward(total={r.totalReward}, operator={r.operatorReward}, delegators={r.delegatorRewards})"

-- ====================
-- = Reward Formulas  =
-- ====================

/-- Compute the apparent pool performance factor.
    For simplicity, assume all pools produced their expected blocks (factor = 1).
    In production, this should be blocks_produced / blocks_expected. -/
private def poolPerformance (_ : ByteArray) : Nat := scale  -- 1.0 scaled

/-- Compute reward for a single pool.
    Uses the Shelley rewards formula with integer arithmetic. -/
def computePoolReward (params : RewardParams) (pool : PoolParams)
    (poolStake : Nat) : Nat :=
  if params.totalStake == 0 || params.nOpt == 0 then 0
  else
    -- z0 = 1 / nOpt (scaled)
    let z0 := scale / params.nOpt
    -- σ' = min(poolStake / totalStake, z0) (scaled)
    let sigma := poolStake * scale / params.totalStake
    let sigma' := min sigma z0
    -- s' = min(pledge / totalStake, z0) (scaled)
    let pledgeRatio := pool.pledge * scale / params.totalStake
    let s' := min pledgeRatio z0
    -- Base reward: R / (1 + a0) * σ'
    -- Simplified (a0 = 0 case): poolReward = R * σ' / scale
    if params.a0Numerator == 0 then
      params.totalRewardsPot * sigma' / scale
    else
      -- Full formula with pledge influence:
      -- reward = (R / (1 + a0)) * (σ' + s' * a0 * max(σ' - s'*z0/z0, 0) / z0)
      let a0Scaled := params.a0Numerator * scale / params.a0Denominator
      let oneScaled := scale
      -- R / (1 + a0)
      let rAdjusted := params.totalRewardsPot * scale / (oneScaled + a0Scaled)
      -- Pledge bonus: s' * a0 * (σ' - s' * z0 / z0)
      -- Simplified: if σ' > s', add pledge bonus
      let pledgeBonus := if sigma' > s' then
        s' * a0Scaled * (sigma' - s') / (z0 * scale)
      else 0
      rAdjusted * (sigma' + pledgeBonus) / scale

/-- Split pool reward between operator and delegators -/
def splitPoolReward (pool : PoolParams) (totalReward : Nat) : PoolReward :=
  if totalReward <= pool.cost then
    -- Reward doesn't cover fixed cost: operator gets everything
    { poolId := pool.poolId
      totalReward := totalReward
      operatorReward := totalReward
      delegatorRewards := 0 }
  else
    let afterCost := totalReward - pool.cost
    -- Margin: pool.margin / 1000000 of the remainder
    let marginReward := afterCost * pool.margin / 1000000
    let operatorReward := pool.cost + marginReward
    let delegatorRewards := afterCost - marginReward
    { poolId := pool.poolId
      totalReward := totalReward
      operatorReward := operatorReward
      delegatorRewards := delegatorRewards }

-- ====================
-- = Epoch Rewards    =
-- ====================

/-- Compute rewards for all pools in an epoch -/
def computeEpochRewards (params : RewardParams) (pools : PoolState)
    (delegation : DelegationState) (utxo : Cleanode.Ledger.UTxO.UTxOSet)
    : List PoolReward :=
  pools.registeredPools.map fun pool =>
    let poolStake := delegation.poolStake pool.poolId utxo
    let reward := computePoolReward params pool poolStake
    splitPoolReward pool reward

/-- Compute total distributed rewards (for sum preservation check) -/
def totalDistributed (rewards : List PoolReward) : Nat :=
  rewards.foldl (fun acc r => acc + r.totalReward) 0

end Cleanode.Ledger.Rewards
