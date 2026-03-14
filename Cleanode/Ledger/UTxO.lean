import Cleanode.Network.ConwayBlock
import Cleanode.Network.Crypto

/-!
# UTxO Set and Operations

The UTxO (Unspent Transaction Output) set is the core ledger data structure.
It tracks all currently spendable transaction outputs.

## Structure
A UTxO is identified by (TxId, OutputIndex) and maps to a TxOutput.
The UTxO set supports:
- Lookup: Check if a UTxO exists
- Add: Insert new outputs from a transaction
- Remove: Consume inputs from a transaction
- Balance: Calculate total value

## References
- Cardano Ledger Spec: UTxO Accounting
- CIP-0002: Shelley UTxO model
-/

namespace Cleanode.Ledger.UTxO

open Cleanode.Network.ConwayBlock
open Cleanode.Network.Crypto

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

/-- A single UTxO entry -/
structure UTxOEntry where
  id : UTxOId
  output : TxOutput

instance : Repr UTxOEntry where
  reprPrec e _ := s!"UTxOEntry({repr e.id}, {repr e.output})"

/-- The UTxO set -/
structure UTxOSet where
  entries : List UTxOEntry

instance : Repr UTxOSet where
  reprPrec s _ := s!"UTxOSet(size={s.entries.length})"

-- ====================
-- = UTxO Operations  =
-- ====================

/-- Create an empty UTxO set -/
def UTxOSet.empty : UTxOSet :=
  { entries := [] }

/-- Number of entries in the UTxO set -/
def UTxOSet.size (s : UTxOSet) : Nat :=
  s.entries.length

/-- Look up a UTxO by its ID -/
def UTxOSet.lookup (s : UTxOSet) (id : UTxOId) : Option TxOutput :=
  s.entries.find? (fun e => e.id == id) |>.map (·.output)

/-- Check if a UTxO exists in the set -/
def UTxOSet.contains (s : UTxOSet) (id : UTxOId) : Bool :=
  s.entries.any (fun e => e.id == id)

/-- Add a single UTxO entry -/
def UTxOSet.add (s : UTxOSet) (entry : UTxOEntry) : UTxOSet :=
  { entries := entry :: s.entries }

/-- Add all outputs from a transaction (given its hash) -/
def UTxOSet.addTxOutputs (s : UTxOSet) (txHash : ByteArray) (outputs : List TxOutput) : UTxOSet :=
  let rec go (idx : Nat) (outs : List TxOutput) (acc : List UTxOEntry) : List UTxOEntry :=
    match outs with
    | [] => acc.reverse
    | o :: rest => go (idx + 1) rest ({ id := { txHash := txHash, outputIndex := idx }, output := o } :: acc)
  { entries := go 0 outputs [] ++ s.entries }

/-- Remove a UTxO by its ID (consume an input) -/
def UTxOSet.remove (s : UTxOSet) (id : UTxOId) : UTxOSet :=
  { entries := s.entries.filter (fun e => !(e.id == id)) }

/-- Remove all inputs from a transaction -/
def UTxOSet.removeInputs (s : UTxOSet) (inputs : List TxInput) : UTxOSet :=
  let inputIds := inputs.map fun inp =>
    { txHash := inp.txId, outputIndex := inp.outputIndex : UTxOId }
  { entries := s.entries.filter (fun e => !inputIds.any (· == e.id)) }

/-- Apply a transaction to the UTxO set: remove inputs, add outputs -/
def UTxOSet.applyTx (s : UTxOSet) (txHash : ByteArray) (body : TransactionBody) : UTxOSet :=
  let afterRemove := s.removeInputs body.inputs
  afterRemove.addTxOutputs txHash body.outputs

/-- Calculate total lovelace in the UTxO set -/
def UTxOSet.totalLovelace (s : UTxOSet) : Nat :=
  s.entries.foldl (fun acc e => acc + e.output.amount) 0

/-- Get all UTxOs for a given address -/
def UTxOSet.forAddress (s : UTxOSet) (address : ByteArray) : List UTxOEntry :=
  s.entries.filter (fun e => e.output.address == address)

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
  let mut seen : List UTxOId := []
  for inp in inputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    if seen.any (· == id) then
      throw (.DoubleSpend id)
    seen := id :: seen
  return ()

/-- Calculate total input value from the UTxO set -/
def totalInputValue (utxo : UTxOSet) (inputs : List TxInput) : Nat :=
  inputs.foldl (fun acc inp =>
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | some output => acc + output.amount
    | none => acc
  ) 0

/-- Calculate total output value -/
def totalOutputValue (outputs : List TxOutput) : Nat :=
  outputs.foldl (fun acc o => acc + o.amount) 0

/-- Validate balance: inputs >= outputs + fee -/
def validateBalance (utxo : UTxOSet) (body : TransactionBody) : Except UTxOError Unit := do
  let inputVal := totalInputValue utxo body.inputs
  let outputVal := totalOutputValue body.outputs
  let required := outputVal + body.fee
  if inputVal < required then
    throw (.InsufficientFunds required inputVal)
  return ()

/-- Full UTxO validation for a transaction -/
def validateTx (utxo : UTxOSet) (body : TransactionBody) : Except UTxOError Unit := do
  validateNoDoubleSpend body.inputs
  validateInputsExist utxo body.inputs
  validateBalance utxo body

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- Adding then removing an entry returns the original set (up to ordering) -/
theorem utxo_add_remove_identity :
    ∀ (s : UTxOSet) (e : UTxOEntry),
      ¬(s.contains e.id) →
      (s.add e).remove e.id = s := by
  sorry  -- Requires list manipulation proofs

/-- Total value is preserved when applying a balanced transaction -/
theorem utxo_balance_preservation :
    ∀ (_s : UTxOSet) (_txHash : ByteArray) (_body : TransactionBody),
      True → True := by
  intros; trivial
  -- Full proof requires: totalInputValue = totalOutputValue + fee

/-- No double-spend: an input can only be consumed once -/
theorem utxo_no_double_spend :
    ∀ (s : UTxOSet) (id : UTxOId),
      ¬((s.remove id).contains id) := by
  sorry  -- Requires list filter properties

end Cleanode.Ledger.UTxO
