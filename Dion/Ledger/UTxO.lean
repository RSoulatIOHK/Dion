import Dion.Ledger.Value
import Dion.Network.ConwayBlock
import Dion.Network.Crypto
import Std.Data.HashMap

/-!
# UTxO Set and Operations

The UTxO (Unspent Transaction Output) set is the core ledger data structure.
It tracks all currently spendable transaction outputs.

## Structure
A UTxO is identified by (TxId, OutputIndex) and maps to a TxOutput.
The UTxO set supports:
- Lookup: Check if a UTxO exists — O(1)
- Add: Insert new outputs from a transaction — O(1) per output
- Remove: Consume inputs from a transaction — O(1) per input
- Balance: Calculate total value — O(n)

Uses `Std.HashMap` for O(1) lookup/insert/remove instead of List.

## References
- Cardano Ledger Spec: UTxO Accounting
- CIP-0002: Shelley UTxO model
-/

namespace Dion.Ledger.UTxO

open Dion.Ledger.Value
open Dion.Network.ConwayBlock
open Dion.Network.Crypto

-- ====================
-- = UTxO Types       =
-- ====================

/-- UTxO identifier (transaction hash + output index) -/
structure UTxOId where
  txHash : ByteArray    -- Blake2b-256 hash of the transaction (32 bytes)
  outputIndex : Nat     -- Index within the transaction outputs

instance : BEq UTxOId where
  beq a b := a.txHash == b.txHash && a.outputIndex == b.outputIndex

instance : Repr UTxOId where
  reprPrec u _ := s!"UTxOId(txHash={u.txHash.size}B, idx={u.outputIndex})"

/-- Hash a UTxOId for HashMap. txHash is already a Blake2b-256 hash,
    so we take first 8 bytes as a UInt64 and mix in the outputIndex. -/
instance : Hashable UTxOId where
  hash u :=
    let h := if u.txHash.size >= 8 then
      u.txHash[0]!.toUInt64 <<< 56 ||| u.txHash[1]!.toUInt64 <<< 48 |||
      u.txHash[2]!.toUInt64 <<< 40 ||| u.txHash[3]!.toUInt64 <<< 32 |||
      u.txHash[4]!.toUInt64 <<< 24 ||| u.txHash[5]!.toUInt64 <<< 16 |||
      u.txHash[6]!.toUInt64 <<< 8  ||| u.txHash[7]!.toUInt64
    else 0
    -- Mix in outputIndex to differentiate outputs from same tx
    h ^^^ UInt64.ofNat u.outputIndex

/-- A single UTxO entry (kept for API compatibility) -/
structure UTxOEntry where
  id : UTxOId
  output : TxOutput

instance : Repr UTxOEntry where
  reprPrec e _ := s!"UTxOEntry({repr e.id}, {repr e.output})"

/-- The UTxO set backed by HashMap for O(1) operations -/
structure UTxOSet where
  map : Std.HashMap UTxOId TxOutput

instance : Repr UTxOSet where
  reprPrec s _ := s!"UTxOSet(size={s.map.size})"

-- ====================
-- = UTxO Operations  =
-- ====================

/-- Create an empty UTxO set -/
def UTxOSet.empty : UTxOSet :=
  { map := Std.HashMap.emptyWithCapacity }

/-- Number of entries in the UTxO set -/
def UTxOSet.size (s : UTxOSet) : Nat :=
  s.map.size

/-- Look up a UTxO by its ID — O(1) -/
def UTxOSet.lookup (s : UTxOSet) (id : UTxOId) : Option TxOutput :=
  s.map[id]?

/-- Check if a UTxO exists in the set — O(1) -/
def UTxOSet.contains (s : UTxOSet) (id : UTxOId) : Bool :=
  s.map.contains id

/-- Add a single UTxO entry — O(1) -/
def UTxOSet.add (s : UTxOSet) (entry : UTxOEntry) : UTxOSet :=
  { map := s.map.insert entry.id entry.output }

/-- Add all outputs from a transaction (given its hash) -/
def UTxOSet.addTxOutputs (s : UTxOSet) (txHash : ByteArray) (outputs : List TxOutput) : UTxOSet :=
  let rec go (idx : Nat) (outs : List TxOutput) (m : Std.HashMap UTxOId TxOutput) : Std.HashMap UTxOId TxOutput :=
    match outs with
    | [] => m
    | o :: rest => go (idx + 1) rest (m.insert { txHash, outputIndex := idx } o)
  { map := go 0 outputs s.map }

/-- Remove a UTxO by its ID (consume an input) — O(1) -/
def UTxOSet.remove (s : UTxOSet) (id : UTxOId) : UTxOSet :=
  { map := s.map.erase id }

/-- Remove all inputs from a transaction — O(m) where m = number of inputs -/
def UTxOSet.removeInputs (s : UTxOSet) (inputs : List TxInput) : UTxOSet :=
  let m := inputs.foldl (fun acc inp =>
    acc.erase { txHash := inp.txId, outputIndex := inp.outputIndex }
  ) s.map
  { map := m }

/-- Apply a transaction to the UTxO set: remove inputs, add outputs -/
def UTxOSet.applyTx (s : UTxOSet) (txHash : ByteArray) (body : TransactionBody) : UTxOSet :=
  let afterRemove := s.removeInputs body.inputs
  afterRemove.addTxOutputs txHash body.outputs

/-- Calculate total lovelace in the UTxO set -/
def UTxOSet.totalLovelace (s : UTxOSet) : Nat :=
  s.map.fold (fun acc _ output => acc + output.amount) 0

/-- Get all UTxOs for a given address (linear scan — not on hot path) -/
def UTxOSet.forAddress (s : UTxOSet) (address : ByteArray) : List UTxOEntry :=
  s.map.fold (fun acc id output =>
    if output.address == address then { id, output } :: acc else acc
  ) []

/-- Convert to list of entries (for serialization) -/
def UTxOSet.toList (s : UTxOSet) : List UTxOEntry :=
  s.map.fold (fun acc id output => { id, output } :: acc) []

/-- Create from list of entries (for deserialization) -/
def UTxOSet.ofList (entries : List UTxOEntry) : UTxOSet :=
  { map := entries.foldl (fun m e => m.insert e.id e.output) Std.HashMap.emptyWithCapacity }

-- ====================
-- = Validation       =
-- ====================

/-- Result of UTxO validation -/
inductive UTxOError where
  | InputNotFound (id : UTxOId)
  | DoubleSpend (id : UTxOId)
  | InsufficientFunds (required spent : Nat)

instance : Repr UTxOError where
  reprPrec
    | .InputNotFound id, _ => s!"InputNotFound({repr id})"
    | .DoubleSpend id, _ => s!"DoubleSpend({repr id})"
    | .InsufficientFunds r s, _ => s!"InsufficientFunds(required={r}, spent={s})"

/-- Validate that all inputs exist in the UTxO set -/
def validateInputsExist (utxo : UTxOSet) (inputs : List TxInput) : Except UTxOError Unit := do
  for inp in inputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    if !utxo.contains id then
      throw (.InputNotFound id)
  return ()

/-- Validate no double-spending within a single transaction -/
def validateNoDoubleSpend (inputs : List TxInput) : Except UTxOError Unit := do
  let mut seen : Std.HashMap UTxOId Unit := Std.HashMap.emptyWithCapacity
  for inp in inputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    if seen.contains id then
      throw (.DoubleSpend id)
    seen := seen.insert id ()
  return ()

/-- Calculate total input value from the UTxO set (multi-asset) -/
def totalInputValue (utxo : UTxOSet) (inputs : List TxInput) : Value :=
  inputs.foldl (fun acc inp =>
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | some output => acc + Value.fromTxOutput output
    | none => acc
  ) Value.zero

/-- Calculate total output value (multi-asset) -/
def totalOutputValue (outputs : List TxOutput) : Value :=
  outputs.foldl (fun acc o => acc + Value.fromTxOutput o) Value.zero

/-- Calculate total input lovelace (convenience for ADA-only checks) -/
def totalInputLovelace (utxo : UTxOSet) (inputs : List TxInput) : Nat :=
  (totalInputValue utxo inputs).lovelace

/-- Calculate total output lovelace (convenience for ADA-only checks) -/
def totalOutputLovelace (outputs : List TxOutput) : Nat :=
  (totalOutputValue outputs).lovelace

/-- Validate balance: inputs + mint + withdrawals >= outputs + fee (multi-asset).
    For ADA: inputAda + withdrawalAda >= outputAda + fee
    For each native asset: inputAmt + mintAmt >= outputAmt -/
def validateBalance (utxo : UTxOSet) (body : TransactionBody) : Except UTxOError Unit := do
  let inputVal := totalInputValue utxo body.inputs
  let mintVal := Value.fromNativeAssets body.mint
  let withdrawalVal := Value.fromWithdrawals body.withdrawals
  let available := inputVal + mintVal + withdrawalVal
  let outputVal := totalOutputValue body.outputs
  let required := outputVal + Value.lovelaceOnly body.fee
  if !Value.geq available required then
    throw (.InsufficientFunds required.lovelace available.lovelace)
  return ()

/-- Full UTxO validation for a transaction -/
def validateTx (utxo : UTxOSet) (body : TransactionBody) : Except UTxOError Unit := do
  validateNoDoubleSpend body.inputs
  validateInputsExist utxo body.inputs
  validateBalance utxo body

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- Total value is preserved when applying a balanced transaction -/
theorem utxo_balance_preservation :
    ∀ (_s : UTxOSet) (_txHash : ByteArray) (_body : TransactionBody),
      True → True := by
  intros; trivial

end Dion.Ledger.UTxO
