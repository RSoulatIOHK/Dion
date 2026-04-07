import Dion.Network.ConwayBlock
import Std.Data.HashMap

/-!
# Multi-Asset Value

Cardano transactions can carry ADA (lovelace) and native tokens (multi-assets).
A `Value` bundles lovelace with a nested map: policyId → assetName → amount.

The balance equation for Conway is:
  inputs + mint + withdrawals = outputs + fee

All arithmetic is component-wise across all (policyId, assetName) pairs.
-/

namespace Dion.Ledger.Value

open Dion.Network.ConwayBlock

-- ====================
-- = Asset ID         =
-- ====================

/-- Unique identifier for a native asset: (policyId, assetName) -/
structure AssetId where
  policyId : ByteArray   -- 28 bytes
  assetName : ByteArray  -- 0..32 bytes

instance : BEq AssetId where
  beq a b := a.policyId == b.policyId && a.assetName == b.assetName

instance : Hashable AssetId where
  hash a :=
    let h1 := if a.policyId.size >= 8 then
      a.policyId[0]!.toUInt64 <<< 56 ||| a.policyId[1]!.toUInt64 <<< 48 |||
      a.policyId[2]!.toUInt64 <<< 40 ||| a.policyId[3]!.toUInt64 <<< 32 |||
      a.policyId[4]!.toUInt64 <<< 24 ||| a.policyId[5]!.toUInt64 <<< 16 |||
      a.policyId[6]!.toUInt64 <<< 8  ||| a.policyId[7]!.toUInt64
    else 0
    let h2 := if a.assetName.size >= 4 then
      a.assetName[0]!.toUInt64 <<< 24 ||| a.assetName[1]!.toUInt64 <<< 16 |||
      a.assetName[2]!.toUInt64 <<< 8  ||| a.assetName[3]!.toUInt64
    else a.assetName.size.toUInt64
    h1 ^^^ h2

instance : Repr AssetId where
  reprPrec a _ := s!"AssetId(policy={a.policyId.size}B, name={a.assetName.size}B)"

-- ====================
-- = Value Type       =
-- ====================

/-- Multi-asset value: lovelace + native tokens.
    Assets stored as flat HashMap from AssetId → amount for O(1) lookup. -/
structure Value where
  lovelace : Nat
  assets : Std.HashMap AssetId Nat := Std.HashMap.emptyWithCapacity

instance : Repr Value where
  reprPrec v _ :=
    let assetCount := v.assets.size
    s!"Value(lovelace={v.lovelace}, assets={assetCount})"

-- ====================
-- = Constructors     =
-- ====================

/-- Value with only lovelace -/
def Value.lovelaceOnly (amount : Nat) : Value :=
  { lovelace := amount }

/-- Zero value -/
def Value.zero : Value :=
  { lovelace := 0 }

/-- Build a Value from a TxOutput's amount and nativeAssets -/
def Value.fromTxOutput (output : TxOutput) : Value :=
  let assets := output.nativeAssets.foldl (fun acc (na : NativeAsset) =>
    let aid : AssetId := { policyId := na.policyId, assetName := na.assetName }
    let cur := acc[aid]?.getD 0
    acc.insert aid (cur + na.amount)
  ) (Std.HashMap.emptyWithCapacity : Std.HashMap AssetId Nat)
  { lovelace := output.amount, assets }

/-- Build a Value from a list of NativeAsset (for mint field) -/
def Value.fromNativeAssets (nas : List NativeAsset) : Value :=
  let assets := nas.foldl (fun acc (na : NativeAsset) =>
    let aid : AssetId := { policyId := na.policyId, assetName := na.assetName }
    let cur := acc[aid]?.getD 0
    acc.insert aid (cur + na.amount)
  ) (Std.HashMap.emptyWithCapacity : Std.HashMap AssetId Nat)
  { lovelace := 0, assets }

/-- Build a Value from mint entries — positive amounts only (mints, not burns) -/
def Value.fromMintPositive (nas : List NativeAsset) : Value :=
  let assets := nas.foldl (fun acc (na : NativeAsset) =>
    if na.signedAmount > 0 then
      let aid : AssetId := { policyId := na.policyId, assetName := na.assetName }
      let cur := acc[aid]?.getD 0
      acc.insert aid (cur + na.signedAmount.toNat)
    else acc
  ) (Std.HashMap.emptyWithCapacity : Std.HashMap AssetId Nat)
  { lovelace := 0, assets }

/-- Build a Value from mint entries — negative amounts only (burns, as absolute values) -/
def Value.fromMintNegative (nas : List NativeAsset) : Value :=
  let assets := nas.foldl (fun acc (na : NativeAsset) =>
    if na.signedAmount < 0 then
      let aid : AssetId := { policyId := na.policyId, assetName := na.assetName }
      let cur := acc[aid]?.getD 0
      acc.insert aid (cur + (-na.signedAmount).toNat)
    else acc
  ) (Std.HashMap.emptyWithCapacity : Std.HashMap AssetId Nat)
  { lovelace := 0, assets }

/-- Build a Value from withdrawals (lovelace only, summed) -/
def Value.fromWithdrawals (withdrawals : List (ByteArray × Nat)) : Value :=
  let total := withdrawals.foldl (fun acc (_, amount) => acc + amount) 0
  { lovelace := total }

-- ====================
-- = Arithmetic       =
-- ====================

/-- Component-wise addition of two Values -/
def Value.add (a b : Value) : Value :=
  let merged := b.assets.fold (fun acc aid amt =>
    let cur := acc[aid]?.getD 0
    acc.insert aid (cur + amt)
  ) a.assets
  { lovelace := a.lovelace + b.lovelace, assets := merged }

instance : HAdd Value Value Value where
  hAdd := Value.add

/-- Check if `a` is component-wise ≥ `b` (i.e. a - b is non-negative everywhere).
    Used for the balance check: inputValue >= outputValue + fee. -/
def Value.geq (a b : Value) : Bool :=
  if a.lovelace < b.lovelace then false
  else
    -- Check every asset in b exists in a with sufficient amount
    b.assets.fold (fun ok aid bAmt =>
      if !ok then false
      else
        let aAmt := a.assets[aid]?.getD 0
        aAmt >= bAmt
    ) true

/-- Check if all asset amounts are zero (only lovelace may be nonzero) -/
def Value.hasOnlyLovelace (v : Value) : Bool :=
  v.assets.size == 0

/-- Total number of distinct assets (not counting lovelace) -/
def Value.assetCount (v : Value) : Nat :=
  v.assets.size

/-- Convert Value back to a flat list of NativeAssets (for encoding) -/
def Value.toNativeAssets (v : Value) : List NativeAsset :=
  v.assets.fold (fun acc aid amt =>
    { policyId := aid.policyId, assetName := aid.assetName, amount := amt } :: acc
  ) []

end Dion.Ledger.Value
