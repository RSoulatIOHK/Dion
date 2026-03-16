import Cleanode.Network.ConwayBlock
import Cleanode.Ledger.UTxO
import Cleanode.Ledger.Value

/-!
# Plutus Script Context

Constructs the `ScriptContext` that is passed as the final argument to Plutus scripts.
The context is encoded as Plutus Data (the `PlutusData` type below).

## Conway ScriptContext Structure (PlutusV3)

```
ScriptContext = Constr 0 [txInfo, redeemer, scriptInfo]
TxInfo = Constr 0 [inputs, refInputs, outputs, fee, mint, certs, wdrl,
                    validRange, signatories, redeemers, datums, id]
ScriptPurpose:
  Minting   = Constr 0 [policyId]
  Spending  = Constr 1 [txOutRef, datum]
  Rewarding = Constr 2 [credential]
  Certifying = Constr 3 [index, cert]
  Voting    = Constr 4 [voter]
  Proposing = Constr 5 [index, procedure]
```

## References
- Plutus Core Spec: ScriptContext encoding
- CIP-0069: PlutusV3 script context
-/

namespace Cleanode.Plutus.ScriptContext

open Cleanode.Network.ConwayBlock
open Cleanode.Ledger.UTxO
open Cleanode.Ledger.Value

-- ====================
-- = Plutus Data      =
-- ====================

/-- Plutus Data type — the universal data encoding for Plutus scripts.
    This mirrors the on-chain `Data` type exactly. -/
inductive PlutusData where
  | Constr (tag : Nat) (fields : List PlutusData)
  | Map (entries : List (PlutusData × PlutusData))
  | List (items : List PlutusData)
  | Integer (value : Int)
  | ByteString (bytes : ByteArray)
  deriving BEq

partial def PlutusData.repr : PlutusData → String
  | .Constr tag fields =>
    let fs := fields.map PlutusData.repr |>.intersperse ", " |> String.join
    s!"Constr({tag}, [{fs}])"
  | .Map entries =>
    let es := entries.map (fun (k, v) => s!"({PlutusData.repr k}, {PlutusData.repr v})")
      |>.intersperse ", " |> String.join
    s!"Map([{es}])"
  | .List items =>
    let is_ := items.map PlutusData.repr |>.intersperse ", " |> String.join
    s!"List([{is_}])"
  | .Integer v => s!"I({v})"
  | .ByteString bs => s!"B({bs.size}B)"

instance : Repr PlutusData where
  reprPrec d _ := PlutusData.repr d

-- ====================
-- = Script Purpose   =
-- ====================

/-- What a Plutus script is validating -/
inductive ScriptPurpose where
  | Spending (txOutRef : UTxOId) (datum : Option PlutusData)
  | Minting (policyId : ByteArray)
  | Rewarding (rewardAddr : ByteArray)
  | Certifying (index : Nat) (cert : PlutusData)
  | Voting (voter : PlutusData)
  | Proposing (index : Nat) (procedure : PlutusData)

-- ====================
-- = Data Encoding    =
-- ====================

/-- Encode a ByteArray as PlutusData -/
def encodeByteArray (bs : ByteArray) : PlutusData :=
  .ByteString bs

/-- Encode a Nat as PlutusData -/
def encodeNat (n : Nat) : PlutusData :=
  .Integer (Int.ofNat n)

/-- Encode a TxInput as PlutusData TxOutRef: Constr 0 [Constr 0 [txId], index] -/
def encodeTxOutRef (inp : TxInput) : PlutusData :=
  .Constr 0 [.Constr 0 [.ByteString inp.txId], .Integer (Int.ofNat inp.outputIndex)]

/-- Encode UTxOId as PlutusData -/
def encodeUTxOId (id : UTxOId) : PlutusData :=
  .Constr 0 [.Constr 0 [.ByteString id.txHash], .Integer (Int.ofNat id.outputIndex)]

/-- Encode a Value as PlutusData: Map of (policyId → Map of (assetName → amount))
    ADA is encoded as policyId="" assetName="" -/
def encodeValue (v : Value) : PlutusData :=
  let adaEntry : PlutusData × PlutusData :=
    (.ByteString (ByteArray.mk #[]),
     .Map [(.ByteString (ByteArray.mk #[]), .Integer (Int.ofNat v.lovelace))])
  let assets := v.toNativeAssets
  let policyIds := assets.map (·.policyId) |>.eraseDups
  let assetEntries := policyIds.map fun pid =>
    let policyAssets := assets.filter (·.policyId == pid)
    let inner := policyAssets.map fun na =>
      (.ByteString na.assetName, .Integer (Int.ofNat na.amount))
    (PlutusData.ByteString pid, PlutusData.Map inner)
  .Map (adaEntry :: assetEntries)

/-- Encode a TxOutput as PlutusData:
    Constr 0 [address, value, datum, referenceScript] -/
def encodeTxOutput (output : TxOutput) : PlutusData :=
  let value := encodeValue (Value.fromTxOutput output)
  let datum := match output.datum with
    | some d => .Constr 0 [.ByteString d]  -- DatumHash
    | none => .Constr 1 []                  -- NoDatum
  .Constr 0 [.ByteString output.address, value, datum, .Constr 1 []]

/-- Encode an input with its resolved UTxO as PlutusData:
    Constr 0 [txOutRef, txOut] -/
def encodeTxInInfo (inp : TxInput) (output : TxOutput) : PlutusData :=
  .Constr 0 [encodeTxOutRef inp, encodeTxOutput output]

/-- Encode a validity interval as PlutusData:
    Constr 0 [lowerBound, upperBound]
    where bound = Constr 0/1 [Constr 0/1 [value]] -/
def encodeValidityRange (validFrom : Option Nat) (ttl : Option Nat) : PlutusData :=
  let lower := match validFrom with
    | some s => .Constr 0 [.Constr 1 [.Integer (Int.ofNat s)], .Constr 1 []]  -- Finite, inclusive
    | none => .Constr 0 [.Constr 0 [], .Constr 1 []]                          -- NegInf
  let upper := match ttl with
    | some s => .Constr 0 [.Constr 1 [.Integer (Int.ofNat s)], .Constr 1 []]  -- Finite, inclusive
    | none => .Constr 0 [.Constr 2 [], .Constr 1 []]                          -- PosInf
  .Constr 0 [lower, upper]

-- ====================
-- = TxInfo Builder   =
-- ====================

/-- Build the TxInfo PlutusData from a transaction and its resolved inputs.
    `resolvedInputs`: list of (TxInput, TxOutput) for regular inputs
    `resolvedRefInputs`: list of (TxInput, TxOutput) for reference inputs
    `txHash`: the transaction ID (blake2b_256 of body) -/
def buildTxInfo (body : TransactionBody) (witnesses : WitnessSet)
    (resolvedInputs : List (TxInput × TxOutput))
    (resolvedRefInputs : List (TxInput × TxOutput))
    (txHash : ByteArray) : PlutusData :=
  let inputs := .List (resolvedInputs.map fun (inp, out) => encodeTxInInfo inp out)
  let refInputs := .List (resolvedRefInputs.map fun (inp, out) => encodeTxInInfo inp out)
  let outputs := .List (body.outputs.map encodeTxOutput)
  let fee := encodeValue (Value.lovelaceOnly body.fee)
  let mint := encodeValue (Value.fromNativeAssets body.mint)
  let certs := .List []  -- TODO: encode certificates
  let withdrawals := .Map (body.withdrawals.map fun (addr, amt) =>
    (.ByteString addr, .Integer (Int.ofNat amt)))
  let validRange := encodeValidityRange body.validityIntervalStart body.ttl
  let signatories := .List (body.requiredSigners.map fun h => .ByteString h)
  let redeemers := PlutusData.Map (witnesses.redeemers.map fun r =>
    let tagIdx := match r.tag with
      | .Spend => 1 | .Mint => 0 | .Cert => 3 | .Reward => 2
    let key := PlutusData.Constr tagIdx [.Integer (Int.ofNat r.index)]
    let val := PlutusData.Constr 0 [.ByteString r.data,
      PlutusData.Constr 0 [.Integer (Int.ofNat r.exUnits.mem),
                            .Integer (Int.ofNat r.exUnits.steps)]]
    (key, val))
  let datums := .Map []  -- TODO: collect datums from witnesses + inline datums
  let txId := .Constr 0 [.ByteString txHash]
  .Constr 0 [inputs, refInputs, outputs, fee, mint, certs, withdrawals,
             validRange, signatories, redeemers, datums, txId]

/-- Build a full ScriptContext for a given script purpose.
    PlutusV3: Constr 0 [txInfo, redeemer, scriptInfo] -/
def buildScriptContext (body : TransactionBody) (witnesses : WitnessSet)
    (resolvedInputs : List (TxInput × TxOutput))
    (resolvedRefInputs : List (TxInput × TxOutput))
    (txHash : ByteArray) (redeemer : PlutusData)
    (purpose : ScriptPurpose) : PlutusData :=
  let txInfo := buildTxInfo body witnesses resolvedInputs resolvedRefInputs txHash
  let scriptInfo := match purpose with
    | .Minting policyId => .Constr 0 [.ByteString policyId]
    | .Spending ref datum =>
      let datumArg := match datum with
        | some d => .Constr 0 [d]
        | none => .Constr 1 []
      .Constr 1 [encodeUTxOId ref, datumArg]
    | .Rewarding addr => .Constr 2 [.ByteString addr]
    | .Certifying idx cert => .Constr 3 [.Integer (Int.ofNat idx), cert]
    | .Voting voter => .Constr 4 [voter]
    | .Proposing idx proc => .Constr 5 [.Integer (Int.ofNat idx), proc]
  .Constr 0 [txInfo, redeemer, scriptInfo]

/-- Resolve inputs from UTxO set for script context construction -/
def resolveInputs (utxo : UTxOSet) (inputs : List TxInput) : List (TxInput × TxOutput) :=
  inputs.filterMap fun inp =>
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | some output => some (inp, output)
    | none => none

end Cleanode.Plutus.ScriptContext
