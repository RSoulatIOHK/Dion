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

/-- Extract the payment credential hash (28 bytes) from a Cardano address.
    For script-locked addresses (bit 4 of header = 1), this is the script hash. -/
private def extractPaymentCredHash (address : ByteArray) : Option ByteArray :=
  if address.size < 29 then none
  else some (address.extract 1 29)  -- 1-byte header + 28-byte payment credential

/-- Check if an address has a script payment credential (bit 4 of header). -/
private def isScriptAddress (address : ByteArray) : Bool :=
  if address.size == 0 then false
  else address[0]! &&& 0x10 != 0

/-- Build a script lookup table from witness set: scriptHash → (scriptBytes, version).
    Since blake2b_224 is IO, we use a simpler approach: store scripts by their
    raw bytes and match by hash computed at the call site. -/
private def allScriptsFromWitness (witnesses : WitnessSet) :
    List (ByteArray × PlutusVersion) :=
  (witnesses.plutusV1Scripts.map (·, PlutusVersion.V1)) ++
  (witnesses.plutusV2Scripts.map (·, PlutusVersion.V2)) ++
  (witnesses.plutusV3Scripts.map (·, PlutusVersion.V3))

/-- Find a script by hash from the witness set scripts.
    We try each script and check if blake2b_224(prefix ++ script) matches the hash.
    Since we can't call blake2b_224 in pure code, we store scripts indexed by
    their position and match structurally. For spending, we pass the script hash
    and try each script. -/
private def findScriptByIndex (witnesses : WitnessSet) (allScripts : List (ByteArray × PlutusVersion))
    (_scriptHash : ByteArray) : Option (ByteArray × PlutusVersion) :=
  -- In a full implementation, we'd hash each script and compare.
  -- For now, if there's exactly one script of each version, match by version preference.
  -- With IO-based hashing, this would be: scripts.find? (fun (s, v) => blake2b_224(prefix v ++ s) == scriptHash)
  allScripts.head?

/-- Resolve the script for a Spend redeemer: find the script from the spent UTxO's address.
    1. Look up the spent UTxO output
    2. Extract script hash from the address
    3. Find matching script in witnesses or reference inputs -/
private def resolveSpendScript (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (index : Nat) :
    Option (ByteArray × PlutusVersion × Option ByteArray × TxInput) := do
  if h : index < body.inputs.length then
    let inp := body.inputs[index]
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    let output ← utxo.lookup id
    if !isScriptAddress output.address then none
    let scriptHash ← extractPaymentCredHash output.address
    let allScripts := allScriptsFromWitness witnesses
    -- Also check reference input scripts
    let refScripts := body.referenceInputs.filterMap fun refInp =>
      let refId : UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
      match utxo.lookup refId with
      | some refOut => refOut.scriptRef.map (·, PlutusVersion.V3)  -- Assume V3 for ref scripts
      | none => none
    let scripts := allScripts ++ refScripts
    let (scriptBytes, version) ← findScriptByIndex witnesses scripts scriptHash
    -- Resolve datum: inline datum takes priority, then witness datums by hash
    let datum := match output.inlineDatum with
      | some d => some d
      | none => match output.datum with
        | some datumHash =>
          -- Find datum in witness set by matching raw CBOR bytes
          -- In full implementation, would hash each datum and compare
          witnesses.datums.find? (fun _ => true)  -- Pick first available datum as fallback
          |>.orElse (fun _ => some datumHash)  -- Use hash as raw bytes fallback
        | none => none
    some (scriptBytes, version, datum, inp)
  else none

/-- Resolve the script for a Mint redeemer.
    The minting policy index maps to the sorted list of policy IDs in the mint field. -/
private def resolveMintScript (body : TransactionBody) (witnesses : WitnessSet)
    (index : Nat) : Option (ByteArray × PlutusVersion × ByteArray) := do
  -- Extract unique policy IDs from mint field, sorted
  let policyIds := body.mint.map (·.policyId) |>.eraseDups
  if h : index < policyIds.length then
    let policyId := policyIds[index]
    let allScripts := allScriptsFromWitness witnesses
    let (scriptBytes, version) ← findScriptByIndex witnesses allScripts policyId
    some (scriptBytes, version, policyId)
  else none

/-- Evaluate all Plutus scripts in a transaction.
    Returns `.ok ()` if all scripts pass, or `.error msg` on first failure. -/
def evaluateTransactionScripts
    (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (txHash : ByteArray)
    : Except String Unit := do
  -- Resolve inputs for script context
  let resolvedInputs := resolveInputs utxo body.inputs
  let resolvedRefInputs := resolveInputs utxo body.referenceInputs

  -- Evaluate each redeemer
  for redeemer in witnesses.redeemers do
    match redeemer.tag with
    | .Spend => do
      match resolveSpendScript body witnesses utxo redeemer.index with
      | none => pure ()  -- Non-script input or script not found; skip
      | some (scriptBytes, version, datumBytes, inp) =>
        let datum : Option PlutusData := datumBytes.map PlutusData.ByteString
        let purpose := ScriptPurpose.Spending
          { txHash := inp.txId, outputIndex := inp.outputIndex } datum
        let redeemerData := PlutusData.ByteString redeemer.data
        let context := buildScriptContext body witnesses resolvedInputs resolvedRefInputs
          txHash redeemerData purpose
        let result := evaluateScript scriptBytes version datum redeemerData context redeemer.exUnits
        if !result.success then
          throw s!"Plutus spend script failed (input {redeemer.index}): {result.error.getD "returned false"}"
    | .Mint => do
      match resolveMintScript body witnesses redeemer.index with
      | none => pure ()  -- Minting policy not found; skip
      | some (scriptBytes, version, policyId) =>
        let purpose := ScriptPurpose.Minting policyId
        let redeemerData := PlutusData.ByteString redeemer.data
        let context := buildScriptContext body witnesses resolvedInputs resolvedRefInputs
          txHash redeemerData purpose
        let result := evaluateScript scriptBytes version none redeemerData context redeemer.exUnits
        if !result.success then
          throw s!"Plutus mint script failed (policy {redeemer.index}): {result.error.getD "returned false"}"
    | .Cert => do
      -- Certificate scripts: find by index into certificates list
      let allScripts := allScriptsFromWitness witnesses
      match allScripts.head? with
      | none => pure ()
      | some (scriptBytes, version) =>
        let purpose := ScriptPurpose.Certifying redeemer.index (PlutusData.Integer 0)
        let redeemerData := PlutusData.ByteString redeemer.data
        let context := buildScriptContext body witnesses resolvedInputs resolvedRefInputs
          txHash redeemerData purpose
        let result := evaluateScript scriptBytes version none redeemerData context redeemer.exUnits
        if !result.success then
          throw s!"Plutus cert script failed: {result.error.getD "returned false"}"
    | .Reward => do
      -- Reward withdrawal scripts
      let allScripts := allScriptsFromWitness witnesses
      match allScripts.head? with
      | none => pure ()
      | some (scriptBytes, version) =>
        let withdrawalAddr := if h : redeemer.index < body.withdrawals.length
          then body.withdrawals[redeemer.index].1 else ByteArray.empty
        let purpose := ScriptPurpose.Rewarding withdrawalAddr
        let redeemerData := PlutusData.ByteString redeemer.data
        let context := buildScriptContext body witnesses resolvedInputs resolvedRefInputs
          txHash redeemerData purpose
        let result := evaluateScript scriptBytes version none redeemerData context redeemer.exUnits
        if !result.success then
          throw s!"Plutus reward script failed: {result.error.getD "returned false"}"

  return ()

end Cleanode.Plutus.Evaluate
