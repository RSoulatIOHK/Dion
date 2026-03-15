import Cleanode.Crypto.VRF.ECVRF
import Cleanode.Config.Genesis

/-!
# Ouroboros Praos Leader Election

In Ouroboros Praos, a stake pool is elected to produce a block in a slot
if its VRF output is below a threshold determined by its relative stake.

## Leader Check
For slot s with epoch nonce η:
1. Compute VRF proof: π = VRF_prove(sk_vrf, η || s)
2. Compute VRF output: y = VRF_proof_to_hash(π)
3. Pool is leader if: y < threshold(σ, f)

Where:
- σ is the pool's relative stake (pool_stake / total_stake)
- f is the active slots coefficient (typically 0.05)
- threshold = 2^512 * (1 - (1-f)^σ)

## Integer Arithmetic
To avoid floating-point non-determinism, the threshold comparison is done
using integer arithmetic with sufficient precision.

## References
- Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake blockchain
- Cardano Ledger Spec: Leader Election
-/

namespace Cleanode.Consensus.Praos.LeaderElection

open Cleanode.Crypto.VRF.ECVRF
open Cleanode.Config.Genesis

-- ====================
-- = Types            =
-- ====================

/-- Stake distribution snapshot for leader election -/
structure StakeSnapshot where
  poolStakes : List (ByteArray × Nat)   -- (poolId, stake)
  totalStake : Nat

/-- Leader check result -/
inductive LeaderCheckResult where
  | isLeader (vrfProof : VRFProof) (vrfOutput : List UInt8)
  | notLeader
  | invalidPool

-- ====================
-- = VRF Input        =
-- ====================

/-- Construct the VRF input for a given slot and epoch nonce.
    vrfInput = epochNonce ++ slotNumber (as 8 bytes big-endian) -/
def vrfInput (epochNonce : ByteArray) (slot : Nat) : List UInt8 :=
  let slotBytes := List.range 8 |>.reverse |>.map fun i =>
    ((slot >>> (i * 8)) % 256).toUInt8
  epochNonce.toList ++ slotBytes

-- ====================
-- = Threshold        =
-- ====================

/-- Compute (1-f)^σ approximated using integer arithmetic.
    We use the formula: (1-f)^σ ≈ (denom-num)^poolStake / denom^poolStake
    where f = num/denom (the activeSlotsCoeff as a Rational).

    To make this tractable, we use the natural log approximation:
    ln(1 - f) * σ, and then compare against the VRF output.

    For Cardano's actual implementation, the threshold comparison is:
    certNatMax * (1 - (1-f)^σ_rel) where certNatMax = 2^256

    We approximate using: if vrfOutput (as Nat) < threshold, pool is leader.

    Since exact exponentiation with rational exponents is expensive,
    we use the linear approximation for small f:
    1 - (1-f)^σ ≈ f*σ (valid when f is small, which it is at 0.05)
-/
def computeThreshold (activeSlotsCoeff : Rational) (poolStake totalStake : Nat)
    : Nat :=
  if totalStake == 0 then 0
  else
    -- certNatMax = 2^256 (size of VRF output space we compare against)
    let certNatMax := 2 ^ 256
    -- Linear approximation: threshold ≈ certNatMax * f * σ_rel
    -- where σ_rel = poolStake / totalStake
    -- threshold = certNatMax * f_num * poolStake / (f_denom * totalStake)
    certNatMax * activeSlotsCoeff.numerator * poolStake /
      (activeSlotsCoeff.denominator * totalStake)

/-- Convert a VRF output (64 bytes) to a natural number for threshold comparison.
    We use the first 32 bytes (256 bits) for the comparison. -/
def vrfOutputToNat (output : List UInt8) : Nat :=
  let bytes := output.take 32
  bytes.foldl (fun acc b => acc * 256 + b.toNat) 0

-- ====================
-- = Leader Check     =
-- ====================

/-- Check if a pool is the slot leader for a given slot.
    Returns the VRF proof and output if the pool is elected. -/
def checkLeader (vrfSecretKey : List UInt8) (epochNonce : ByteArray)
    (slot : Nat) (activeSlotsCoeff : Rational)
    (poolStake totalStake : Nat) : LeaderCheckResult :=
  if poolStake == 0 then .invalidPool
  else
    -- Compute VRF input
    let input := vrfInput epochNonce slot
    -- Generate VRF proof
    let proof := Cleanode.Crypto.VRF.ECVRF.prove vrfSecretKey input
    -- Get VRF output
    let output := Cleanode.Crypto.VRF.ECVRF.proofToHash proof
    -- Convert to number and compare against threshold
    let y := vrfOutputToNat output
    let threshold := computeThreshold activeSlotsCoeff poolStake totalStake
    if y < threshold then
      .isLeader proof output
    else
      .notLeader

/-- Verify that a block header's VRF proof is valid for the claimed slot.
    Used when validating received blocks. -/
def verifyLeaderProof (vrfPublicKey : List UInt8) (epochNonce : ByteArray)
    (slot : Nat) (activeSlotsCoeff : Rational)
    (poolStake totalStake : Nat) (proof : VRFProof) : Bool :=
  -- Verify the VRF proof
  let input := vrfInput epochNonce slot
  if !Cleanode.Crypto.VRF.ECVRF.verify vrfPublicKey input proof then false
  else
    -- Check the output is below threshold
    let output := Cleanode.Crypto.VRF.ECVRF.proofToHash proof
    let y := vrfOutputToNat output
    let threshold := computeThreshold activeSlotsCoeff poolStake totalStake
    y < threshold

-- ====================
-- = Chain Density    =
-- ====================

/-- Track chain density (ratio of blocks to slots) for chain selection.
    A denser chain is preferred in Praos. -/
structure ChainDensity where
  blockCount : Nat
  slotRange : Nat           -- Number of slots in the range

/-- Compute chain density as blocks per slot (scaled by 1000 for integer arithmetic) -/
def ChainDensity.densityPermille (d : ChainDensity) : Nat :=
  if d.slotRange == 0 then 0
  else d.blockCount * 1000 / d.slotRange

/-- Compare two chain densities. Returns true if the first is denser. -/
def ChainDensity.isDenser (a b : ChainDensity) : Bool :=
  a.densityPermille > b.densityPermille

-- =========================
-- = Chain Selection Rule  =
-- =========================

/-- A candidate chain fragment for fork comparison.
    Represents a peer's chain tip and the fork point relative to our current chain. -/
structure ChainCandidate where
  tipBlockNo : Nat            -- Block number at the tip
  tipSlot : Nat               -- Slot number at the tip
  forkBlockNo : Nat           -- Block number where this chain diverges from ours
  density : ChainDensity      -- Chain density in the fork region

instance : Repr ChainCandidate where
  reprPrec c _ := s!"ChainCandidate(tip={c.tipBlockNo}, slot={c.tipSlot}, fork={c.forkBlockNo})"

/-- Ouroboros Praos chain selection: prefer the candidate chain over our current chain.

    Rules (in order):
    1. Never adopt a chain that forks deeper than k blocks (security parameter)
    2. Prefer the chain with the highest block number (longest chain)
    3. Within the same length, prefer the denser chain (more blocks per slot)
       in the 3k/f window after the fork point

    Returns true if the candidate should be adopted over our chain. -/
def preferChain (securityParam : Nat) (ourTipBlockNo : Nat)
    (candidate : ChainCandidate) : Bool :=
  -- Rule 1: Reject deep forks beyond k
  if ourTipBlockNo > candidate.forkBlockNo + securityParam then false
  -- Rule 2: Longer chain wins
  else if candidate.tipBlockNo > ourTipBlockNo then true
  else if candidate.tipBlockNo < ourTipBlockNo then false
  -- Rule 3: Same length — use chain density as tiebreaker
  else candidate.density.densityPermille > 500  -- prefer denser-than-average fork

/-- Compare two candidate chains. Returns true if `a` is preferred over `b`. -/
def preferCandidate (a b : ChainCandidate) : Bool :=
  if a.tipBlockNo > b.tipBlockNo then true
  else if a.tipBlockNo < b.tipBlockNo then false
  else a.density.densityPermille > b.density.densityPermille

/-- Select the best chain from a list of candidates.
    Returns none if no candidate is preferred over our current chain. -/
def selectBestChain (securityParam : Nat) (ourTipBlockNo : Nat)
    (candidates : List ChainCandidate) : Option ChainCandidate :=
  let viable := candidates.filter (preferChain securityParam ourTipBlockNo)
  match viable with
  | [] => none
  | first :: rest =>
    some (rest.foldl (fun best c => if preferCandidate c best then c else best) first)

-- =========================
-- = Improved Threshold    =
-- =========================

/-- Compute (1 - (1-f)^σ) using Taylor series for spec-accurate VRF threshold.

    The exact formula is: threshold = certNatMax * (1 - (1-f)^σ_rel)
    where σ_rel = poolStake / totalStake.

    We approximate (1-f)^σ via: exp(σ * ln(1-f))
    Using Taylor series for ln(1-f) ≈ -f - f²/2 - f³/3 - ...
    and exp(x) ≈ 1 + x + x²/2 + x³/6 + ...

    All arithmetic is scaled by 10^18 to maintain precision. -/
private def precisionScale : Nat := 1000000000000000000  -- 10^18

/-- Compute ln(1-f) * σ_rel using Taylor expansion, scaled by precisionScale.
    f = fNum/fDen, σ_rel = poolStake/totalStake.
    ln(1-f) ≈ -(f + f²/2 + f³/3 + f⁴/4)
    Result is negative, returned as (isNeg, |value|). -/
private def lnOneMinusF_times_sigma (fNum fDen poolStake totalStake : Nat)
    : Nat :=
  if fDen == 0 || totalStake == 0 then 0
  else
    let s := precisionScale
    -- f scaled
    let fScaled := fNum * s / fDen
    -- Terms of -ln(1-f) = f + f²/2 + f³/3 + f⁴/4
    let f2 := fScaled * fScaled / s
    let f3 := f2 * fScaled / s
    let f4 := f3 * fScaled / s
    let negLn := fScaled + f2 / 2 + f3 / 3 + f4 / 4
    -- Multiply by σ_rel = poolStake / totalStake
    negLn * poolStake / totalStake

/-- Compute exp(-x) ≈ 1 - x + x²/2 - x³/6 + x⁴/24
    where x is scaled by precisionScale. Returns result scaled by precisionScale.
    x must be positive (we compute exp of negative value). -/
private def expNeg (x : Nat) : Nat :=
  let s := precisionScale
  if x >= s then 0  -- exp(-x) ≈ 0 for x >= 1 (in scaled terms, x >= scale)
  else
    let x2 := x * x / s
    let x3 := x2 * x / s
    let x4 := x3 * x / s
    -- 1 - x + x²/2 - x³/6 + x⁴/24
    let pos := s + x2 / 2 + x4 / 24
    let neg := x + x3 / 6
    if pos > neg then pos - neg else 0

/-- Spec-accurate VRF threshold using Taylor series approximation.
    threshold = certNatMax * (1 - (1-f)^σ_rel)
    where (1-f)^σ_rel = exp(σ_rel * ln(1-f)) -/
def computeThresholdAccurate (activeSlotsCoeff : Rational) (poolStake totalStake : Nat)
    : Nat :=
  if totalStake == 0 then 0
  else
    let certNatMax := 2 ^ 256
    let s := precisionScale
    -- Compute |σ * ln(1-f)| (always positive since ln(1-f) < 0)
    let negExponent := lnOneMinusF_times_sigma
      activeSlotsCoeff.numerator activeSlotsCoeff.denominator poolStake totalStake
    -- (1-f)^σ = exp(-|exponent|) = exp(σ * ln(1-f))
    let oneMinusFsigma := expNeg negExponent
    -- threshold = certNatMax * (1 - (1-f)^σ)
    let phi := if s > oneMinusFsigma then s - oneMinusFsigma else 0
    certNatMax * phi / s

end Cleanode.Consensus.Praos.LeaderElection
