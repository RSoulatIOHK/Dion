import Cleanode.Network.ConwayBlock
import Cleanode.Network.EraTx

/-!
# Fee Calculation

Cardano fee calculation per era. The basic fee formula is:
  fee = a * tx_size + b
where `a` (fee per byte) and `b` (constant fee) are protocol parameters.

For Alonzo+ eras with Plutus scripts, execution costs are added:
  total_fee = base_fee + script_fee
  script_fee = price_mem * mem_units + price_steps * step_units

## References
- Cardano Ledger Spec: Fee Calculation
- CIP-0040: Babbage fee changes
-/

namespace Cleanode.Ledger.Fee

open Cleanode.Network.ConwayBlock
open Cleanode.Network.EraTx

-- ====================
-- = Protocol Params  =
-- ====================

/-- Fee-related protocol parameters -/
structure FeeParams where
  minFeeA : Nat              -- Fee per byte (lovelace)
  minFeeB : Nat              -- Constant fee (lovelace)
  priceMem : Nat             -- Price per memory unit (numerator, denom=10000)
  priceSteps : Nat           -- Price per CPU step (numerator, denom=10000000)
  maxTxSize : Nat            -- Maximum transaction size in bytes
  deriving Repr, BEq

/-- Default mainnet fee parameters (as of Conway era) -/
def defaultFeeParams : FeeParams :=
  { minFeeA := 44,           -- 44 lovelace per byte
    minFeeB := 155381,       -- 155381 lovelace constant
    priceMem := 577,         -- 0.0577 ADA per memory unit (577/10000)
    priceSteps := 721,       -- 0.0000721 ADA per step (721/10000000)
    maxTxSize := 16384 }     -- 16KB max tx size

-- ====================
-- = Fee Formulas     =
-- ====================

/-- Calculate minimum fee for a transaction (base fee only) -/
def minFee (params : FeeParams) (txSizeBytes : Nat) : Nat :=
  params.minFeeA * txSizeBytes + params.minFeeB

/-- Calculate script execution fee from execution units -/
def scriptFee (params : FeeParams) (exUnits : ExUnits) : Nat :=
  -- priceMem is scaled by 10000, priceSteps by 10000000
  let memCost := params.priceMem * exUnits.mem / 10000
  let stepCost := params.priceSteps * exUnits.steps / 10000000
  memCost + stepCost

/-- Calculate total script fees for all redeemers in a transaction -/
def totalScriptFee (params : FeeParams) (redeemers : List Redeemer) : Nat :=
  redeemers.foldl (fun acc r => acc + scriptFee params r.exUnits) 0

/-- Calculate total minimum fee for a transaction -/
def totalMinFee (params : FeeParams) (txSizeBytes : Nat) (redeemers : List Redeemer) : Nat :=
  minFee params txSizeBytes + totalScriptFee params redeemers

/-- Validate that a transaction's fee meets the minimum -/
def validateFee (params : FeeParams) (body : TransactionBody) (witnesses : WitnessSet) : Bool :=
  let txSize := body.rawBytes.size
  let required := totalMinFee params txSize witnesses.redeemers
  body.fee >= required

/-- Calculate the minimum ADA required for a UTxO (min-ada-per-utxo).
    Babbage/Conway formula: coinsPerUTxOByte * (serializedOutputSize + 160)
    The +160 accounts for the UTxO entry overhead (tx hash + output index in the UTxO map).
    `serializedOutputSize` is the CBOR-serialized size of the entire output. -/
def minAdaPerUTxO (serializedOutputSize : Nat) (coinsPerUTxOByte : Nat := 4310) : Nat :=
  coinsPerUTxOByte * (serializedOutputSize + 160)

-- ====================
-- = Era-Specific     =
-- ====================

/-- Get fee parameters for an era (simplified - uses defaults) -/
def feeParamsForEra : CardanoEra → FeeParams
  | .Byron    => { minFeeA := 0, minFeeB := 0, priceMem := 0, priceSteps := 0, maxTxSize := 8192 }
  | .Shelley  => { minFeeA := 44, minFeeB := 155381, priceMem := 0, priceSteps := 0, maxTxSize := 16384 }
  | .Allegra  => { minFeeA := 44, minFeeB := 155381, priceMem := 0, priceSteps := 0, maxTxSize := 16384 }
  | .Mary     => { minFeeA := 44, minFeeB := 155381, priceMem := 0, priceSteps := 0, maxTxSize := 16384 }
  | .Alonzo   => defaultFeeParams
  | .Babbage  => defaultFeeParams
  | .Conway   => defaultFeeParams

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- Fee is monotonically increasing with transaction size -/
theorem fee_monotone_size :
    ∀ (params : FeeParams) (s1 s2 : Nat),
      s1 ≤ s2 → minFee params s1 ≤ minFee params s2 := by
  intros params s1 s2 h
  unfold minFee
  exact Nat.add_le_add_right (Nat.mul_le_mul_left _ h) _

/-- Fee is non-negative -/
theorem fee_nonneg :
    ∀ (params : FeeParams) (s : Nat),
      0 ≤ minFee params s := by
  intros; omega

/-- Total fee includes both base and script components -/
theorem total_fee_components :
    ∀ (params : FeeParams) (s : Nat) (rs : List Redeemer),
      totalMinFee params s rs = minFee params s + totalScriptFee params rs := by
  intros; rfl

end Cleanode.Ledger.Fee
