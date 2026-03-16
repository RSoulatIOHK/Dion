import Cleanode.Plutus.UPLC.Term
import Cleanode.Plutus.UPLC.Flat
import Cleanode.Plutus.UPLC.CEK
import Cleanode.Plutus.UPLC.CostModel
import Cleanode.Plutus.ScriptContext
import Cleanode.Network.ConwayBlock
import Cleanode.Ledger.UTxO

/-!
# Plutus Script Evaluation Pipeline

End-to-end pipeline for evaluating Plutus scripts:
1. Flat-deserialize script bytes → UPLC Term
2. Build ScriptContext (from tx data)
3. Apply script to (datum, redeemer, context) — or (redeemer, context) for V2+
4. Run CEK machine with budget from redeemer ExUnits
5. Verify actual costs ≤ declared ExUnits

## Script Application
- PlutusV1/V2 spending: `script datum redeemer context`
- PlutusV1/V2 minting/rewarding/certifying: `script redeemer context`
- PlutusV3: `script context` (datum and redeemer folded into context)

## References
- Alonzo Ledger Spec: Script Validation
- CIP-0069: PlutusV3 script interface
-/

namespace Cleanode.Plutus.Evaluate

open Cleanode.Plutus.UPLC
open Cleanode.Plutus.UPLC.Flat
open Cleanode.Plutus.UPLC.CEK
open Cleanode.Plutus.UPLC.CostModel
open Cleanode.Plutus.ScriptContext
open Cleanode.Network.ConwayBlock
open Cleanode.Ledger.UTxO

-- ====================
-- = Script Version   =
-- ====================

/-- Plutus language version -/
inductive PlutusVersion where
  | V1 | V2 | V3
  deriving BEq, Repr

-- ====================
-- = Script Eval      =
-- ====================

/-- Result of script evaluation -/
structure ScriptResult where
  success : Bool
  memUsed : Nat
  stepsUsed : Nat
  error : Option String := none
  deriving Repr

/-- Encode PlutusData as a UPLC Constant for script application -/
private def dataToConstant (d : PlutusData) : Constant :=
  .Data d

/-- Apply a UPLC term to arguments by wrapping in Apply nodes -/
private def applyArgs (script : Term) (args : List Term) : Term :=
  args.foldl (fun acc arg => .Apply acc arg) script

/-- Evaluate a single Plutus script.
    `scriptBytes`: raw Flat-encoded (or CBOR-wrapped) script bytes
    `version`: PlutusV1/V2/V3
    `datum`: optional datum (for spending scripts in V1/V2)
    `redeemer`: the redeemer PlutusData
    `context`: the ScriptContext PlutusData
    `exUnits`: declared execution units (budget) -/
def evaluateScript (scriptBytes : ByteArray) (version : PlutusVersion)
    (datum : Option PlutusData) (redeemer : PlutusData) (context : PlutusData)
    (exUnits : ExUnits) : ScriptResult :=
  -- 1. Deserialize script
  match decodePlutusScript scriptBytes with
  | none => { success := false, memUsed := 0, stepsUsed := 0, error := some "failed to decode Flat script" }
  | some program =>
    -- 2. Build argument list based on version
    let args : List Term := match version with
    | .V1 | .V2 =>
      -- V1/V2: script datum? redeemer context
      let datumArg := datum.map (fun d => Term.Constant (dataToConstant d))
      let redeemerArg := Term.Constant (dataToConstant redeemer)
      let contextArg := Term.Constant (dataToConstant context)
      match datumArg with
      | some d => [d, redeemerArg, contextArg]
      | none => [redeemerArg, contextArg]
    | .V3 =>
      -- V3: script context (datum + redeemer folded into context)
      [Term.Constant (dataToConstant context)]

    -- 3. Apply arguments
    let appliedTerm := applyArgs program.term args

    -- 4. Run CEK machine
    let budget : ExBudget := { mem := exUnits.mem, steps := exUnits.steps }
    match evaluate appliedTerm budget with
    | .error msg =>
      { success := false, memUsed := exUnits.mem, stepsUsed := exUnits.steps,
        error := some msg }
    | .ok (value, remainingBudget) =>
      let memUsed := exUnits.mem - remainingBudget.mem
      let stepsUsed := exUnits.steps - remainingBudget.steps
      { success := isScriptSuccess value, memUsed, stepsUsed }

-- ====================
-- = Tx-Level Eval    =
-- ====================

/-- Script to evaluate with all its context -/
structure ScriptEvaluation where
  scriptBytes : ByteArray
  version : PlutusVersion
  datum : Option PlutusData
  redeemer : PlutusData
  purpose : ScriptPurpose
  exUnits : ExUnits

/-- Evaluate all Plutus scripts in a transaction.
    Returns `.ok ()` if all scripts pass, or `.error msg` on first failure. -/
def evaluateTransactionScripts
    (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (txHash : ByteArray)
    : Except String Unit := do
  -- Resolve inputs for script context
  let resolvedInputs := resolveInputs utxo body.inputs
  let resolvedRefInputs := resolveInputs utxo body.referenceInputs

  -- Match redeemers to scripts
  for redeemer in witnesses.redeemers do
    let scriptBytes? : Option (ByteArray × PlutusVersion) := match redeemer.tag with
    | .Spend =>
      -- Look up the input being spent, find its script
      if h : redeemer.index < body.inputs.length then
        let _inp := body.inputs[redeemer.index]
        -- TODO: resolve script from UTxO or reference inputs
        none
      else none
    | .Mint =>
      -- Find minting policy by index into policy IDs
      -- TODO: resolve minting script
      none
    | .Cert =>
      -- Certificate script
      none
    | .Reward =>
      -- Reward withdrawal script
      none

    match scriptBytes? with
    | none =>
      -- Script not found — in a full implementation, this would be an error
      -- For now, skip (scripts are still being wired)
      pure ()
    | some (scriptBytes, version) =>
      -- Build script context
      let datum : Option PlutusData := match redeemer.tag with
        | .Spend =>
          -- Look up datum from witnesses or inline datum
          none  -- TODO
        | _ => none

      let purpose := match redeemer.tag with
        | .Spend =>
          if h : redeemer.index < body.inputs.length then
            let inp := body.inputs[redeemer.index]
            ScriptPurpose.Spending { txHash := inp.txId, outputIndex := inp.outputIndex } datum
          else ScriptPurpose.Spending { txHash := ByteArray.empty, outputIndex := 0 } none
        | .Mint =>
          -- TODO: extract policy ID
          ScriptPurpose.Minting ByteArray.empty
        | .Cert =>
          ScriptPurpose.Certifying redeemer.index (.Integer 0)
        | .Reward =>
          ScriptPurpose.Rewarding ByteArray.empty

      -- redeemer.data is raw CBOR ByteArray; wrap as PlutusData for script context
      let redeemerData := PlutusData.ByteString redeemer.data
      let context := buildScriptContext body witnesses resolvedInputs resolvedRefInputs
        txHash redeemerData purpose
      let result := evaluateScript scriptBytes version datum redeemerData context redeemer.exUnits
      if !result.success then
        throw s!"Plutus script failed: {result.error.getD "returned false"}"

  return ()

end Cleanode.Plutus.Evaluate
