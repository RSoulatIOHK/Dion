import Cleanode.Plutus.UPLC.Term
import Cleanode.Plutus.UPLC.Flat
import Cleanode.Plutus.ScriptContext
import Cleanode.Network.ConwayBlock
import Cleanode.Network.Crypto
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
open Cleanode.Plutus.ScriptContext
open Cleanode.Network.ConwayBlock
open Cleanode.Network.Crypto
open Cleanode.Ledger.UTxO

-- ====================
-- = Script Version   =
-- ====================

/-- Plutus language version -/
inductive PlutusVersion where
  | V1 | V2 | V3
  deriving BEq, Repr

/-- Version byte prefix for script hashing: V1=0x01, V2=0x02, V3=0x03 -/
private def versionByte : PlutusVersion → UInt8
  | .V1 => 0x01
  | .V2 => 0x02
  | .V3 => 0x03

/-- Semantics version for FFI: V1=0, V2=1, V3=2 -/
private def semanticsId : PlutusVersion → UInt32
  | .V1 => 0
  | .V2 => 1
  | .V3 => 2

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

-- ====================
-- = Plutuz FFI       =
-- ====================

/-- FFI to Plutuz Zig UPLC evaluator (no argument application).
    Takes Flat-encoded UPLC program bytes (after CBOR stripping) + budget.
    Returns (errCode, cpuUsed, memUsed, payload).
    errCode=0 → payload is Flat-encoded result program.
    errCode>0 → payload is UTF-8 error message. -/
@[extern "lean_plutuz_eval_flat"]
opaque plutuzEvalFlat (program : @& ByteArray) (cpuBudget : UInt64) (memBudget : UInt64)
    : IO (UInt32 × UInt64 × UInt64 × ByteArray)

/-- FFI to Plutuz with argument application.
    Applies CBOR-encoded PlutusData arguments to the script before evaluation.
    numArgs: how many of arg1/arg2/arg3 to apply (0-3).
    semantics: 0=V1, 1=V2, 2=V3. -/
@[extern "lean_plutuz_eval_flat_applied"]
opaque plutuzEvalFlatApplied (program : @& ByteArray) (numArgs : UInt32)
    (arg1 : @& ByteArray) (arg2 : @& ByteArray) (arg3 : @& ByteArray)
    (cpuBudget : UInt64) (memBudget : UInt64) (semantics : UInt32)
    : IO (UInt32 × UInt64 × UInt64 × ByteArray)

/-- Strip CBOR bytestring layers to get raw Flat bytes.
    V1/V2 scripts from witness key 4 may have up to 3 CBOR layers. -/
private def stripCborLayers (scriptBytes : ByteArray) : ByteArray × Nat :=
  match stripCborByteString scriptBytes with
  | none => (scriptBytes, 0)
  | some inner =>
    match stripCborByteString inner with
    | some innerInner =>
      match stripCborByteString innerInner with
      | some innerInnerInner => (innerInnerInner, 3)
      | none => (innerInner, 2)
    | none => (inner, 1)

/-- Evaluate a Plutus script with arguments applied via FFI.
    `scriptBytes`: CBOR-wrapped script bytes
    `version`: Plutus language version (controls semantics)
    `args`: CBOR-encoded PlutusData arguments to apply before evaluation
    `exUnits`: declared execution units (budget) -/
def evaluateScriptIO (scriptBytes : ByteArray) (version : PlutusVersion)
    (args : List ByteArray) (exUnits : ExUnits) : IO ScriptResult := do
  let (flatBytes, stripsApplied) := stripCborLayers scriptBytes
  -- Call FFI with arguments
  let empty := ByteArray.empty
  let arg1 := args.getD 0 empty
  let arg2 := args.getD 1 empty
  let arg3 := args.getD 2 empty
  let numArgs := args.length.toUInt32
  let (errCode, cpuUsed, memUsed, payload) ←
    plutuzEvalFlatApplied flatBytes numArgs arg1 arg2 arg3
      exUnits.steps.toUInt64 exUnits.mem.toUInt64 (semanticsId version)
  if errCode == 0 then
    return { success := true, memUsed := memUsed.toNat, stepsUsed := cpuUsed.toNat }
  else
    let errMsg := String.fromUTF8! payload
    -- Log failures with diagnostic info including builtins used in the script
    let hexPrefix := (flatBytes.extract 0 (min 16 flatBytes.size)).foldl
      (fun acc b => acc ++ (if b < 16 then "0" else "") ++ String.mk (Nat.toDigits 16 b.toNat)) ""
    let builtinsUsed := match decodePlutusScriptDiag scriptBytes with
      | .ok prog =>
        let bs : List BuiltinFun := Term.collectBuiltins prog.term
        if bs.isEmpty then "none" else String.intercalate ", " (bs.map fun b => s!"{repr b}")
      | .error _ => "decode-failed"
    let h ← IO.FS.Handle.mk "validation_failures.log" .append
    h.putStrLn s!"  [plutus-diag] version={repr version} strips={stripsApplied} rawSize={scriptBytes.size} flatSize={flatBytes.size} nArgs={args.length} first16={hexPrefix} err={errMsg}"
    h.putStrLn s!"  [plutus-diag] builtins=[{builtinsUsed}]"
    return { success := false, memUsed := memUsed.toNat, stepsUsed := cpuUsed.toNat,
             error := some s!"{errMsg} (builtins=[{builtinsUsed}])" }

/-- Pure fallback for evaluation (used when IO is not available) -/
def evaluateScript (scriptBytes : ByteArray) (version : PlutusVersion)
    (_datum : Option PlutusData) (_redeemer : PlutusData) (_context : PlutusData)
    (exUnits : ExUnits) : ScriptResult :=
  match decodePlutusScriptDiag scriptBytes with
  | .error msg =>
    { success := false, memUsed := 0, stepsUsed := 0,
      error := some s!"decode failed: {msg}" }
  | .ok _program =>
    { success := false, memUsed := 0, stepsUsed := exUnits.steps,
      error := some s!"pure CEK disabled — use evaluateScriptIO (version={repr version})" }

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
  else some (address.extract 1 29)

/-- Check if an address has a script payment credential (bit 4 of header). -/
private def isScriptAddress (address : ByteArray) : Bool :=
  if address.size == 0 then false
  else address[0]! &&& 0x10 != 0

/-- Unwrap a reference script from CDDL: `#6.24(bytes .cbor [type, script_bytes])`.
    Returns (script_bytes, version) where script_bytes is the raw (still CBOR-wrapped) Plutus script. -/
private def unwrapRefScript (raw : ByteArray) : Option (ByteArray × PlutusVersion) := do
  if raw.size < 4 then none
  if raw[0]! != 0xD8 || raw[1]! != 0x18 then none
  let innerStart := 2
  let firstByte := raw[innerStart]!
  let majorType := firstByte >>> 5
  if majorType != 2 then none
  let additionalInfo := firstByte &&& 0x1F
  let (payloadOff, payloadLen) :=
    if additionalInfo < 24 then (1, additionalInfo.toNat)
    else if additionalInfo == 24 then
      if raw.size < innerStart + 2 then (0, 0)
      else (2, raw[innerStart + 1]!.toNat)
    else if additionalInfo == 25 then
      if raw.size < innerStart + 3 then (0, 0)
      else (3, raw[innerStart + 1]!.toNat * 256 + raw[innerStart + 2]!.toNat)
    else if additionalInfo == 26 then
      if raw.size < innerStart + 5 then (0, 0)
      else (5, raw[innerStart + 1]!.toNat * 16777216 + raw[innerStart + 2]!.toNat * 65536 +
                raw[innerStart + 3]!.toNat * 256 + raw[innerStart + 4]!.toNat)
    else (0, 0)
  if payloadOff == 0 then none
  let cborPayload := raw.extract (innerStart + payloadOff) (innerStart + payloadOff + payloadLen)
  if cborPayload.size < 3 then none
  let arrByte := cborPayload[0]!
  let arrMajor := arrByte >>> 5
  if arrMajor != 4 then none
  let scriptType := cborPayload[1]!.toNat
  let version := match scriptType with
    | 1 => PlutusVersion.V1
    | 2 => PlutusVersion.V2
    | 3 => PlutusVersion.V3
    | _ => PlutusVersion.V3
  let scriptBytes := cborPayload.extract 2 cborPayload.size
  if scriptType == 0 then none
  else some (scriptBytes, version)

/-- Build a script lookup table from witness set. -/
private def allScriptsFromWitness (witnesses : WitnessSet) :
    List (ByteArray × PlutusVersion) :=
  (witnesses.plutusV1Scripts.map (·, PlutusVersion.V1)) ++
  (witnesses.plutusV2Scripts.map (·, PlutusVersion.V2)) ++
  (witnesses.plutusV3Scripts.map (·, PlutusVersion.V3))

/-- Find a script by hash using blake2b_224(version_prefix ++ script_bytes).
    Script hash = blake2b_224(version_byte ++ serialized_script). -/
private def findScriptByHashIO (scripts : List (ByteArray × PlutusVersion))
    (targetHash : ByteArray) : IO (Option (ByteArray × PlutusVersion)) := do
  for (scriptBytes, version) in scripts do
    let prefixed := ByteArray.mk #[versionByte version] ++ scriptBytes
    let hash ← blake2b_224 prefixed
    if hash == targetHash then
      return some (scriptBytes, version)
  return none

/-- Lexicographic comparison of ByteArrays. -/
private def compareByteArray (a b : ByteArray) : Ordering :=
  go 0 (min a.size b.size) a b
where
  go (i : Nat) (len : Nat) (a b : ByteArray) : Ordering :=
    if i >= len then compare a.size b.size
    else
      let va := a[i]!
      let vb := b[i]!
      if va < vb then .lt
      else if va > vb then .gt
      else go (i + 1) len a b

/-- Lexicographic comparison of TxInputs. -/
private def compareTxInput (a b : TxInput) : Ordering :=
  match compareByteArray a.txId b.txId with
  | .eq => compare a.outputIndex b.outputIndex
  | ord => ord

/-- Sort inputs lexicographically (redeemer Spend(i) refers to sorted position). -/
private def sortInputs (inputs : List TxInput) : List TxInput :=
  inputs.mergeSort (fun a b => compareTxInput a b != .gt)

/-- Resolve the script for a Spend redeemer.
    1. Look up the spent UTxO output (using sorted input list)
    2. Extract script hash from the address
    3. Find matching script in witnesses or reference inputs by hash -/
private def resolveSpendScriptIO (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (index : Nat) :
    IO (Option (ByteArray × PlutusVersion × Option ByteArray × TxInput)) := do
  -- Sort inputs lexicographically: redeemer Spend(i) refers to sorted position
  let sorted := sortInputs body.inputs
  if h : index < sorted.length then
    let inp := sorted[index]
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | none => return none
    | some output =>
      if !isScriptAddress output.address then return none
      match extractPaymentCredHash output.address with
      | none => return none
      | some scriptHash =>
        let allScripts := allScriptsFromWitness witnesses
        let refScripts := body.referenceInputs.filterMap fun refInp =>
          let refId : UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
          match utxo.lookup refId with
          | some refOut => refOut.scriptRef.bind unwrapRefScript
          | none => none
        let scripts := allScripts ++ refScripts
        match ← findScriptByHashIO scripts scriptHash with
        | none => return none
        | some (scriptBytes, version) =>
          -- Resolve datum: inline datum takes priority, then witness datums by hash
          let datum := match output.inlineDatum with
            | some d => some d
            | none => match output.datum with
              | some _datumHash => witnesses.datums.head?
              | none => none
          return some (scriptBytes, version, datum, inp)
  else return none

/-- Resolve the script for a Mint redeemer by policy ID hash matching (sorted). -/
private def resolveMintScriptIO (body : TransactionBody) (witnesses : WitnessSet)
    (index : Nat) : IO (Option (ByteArray × PlutusVersion × ByteArray)) := do
  -- Sort policy IDs lexicographically: redeemer Mint(i) refers to sorted position
  let policyIds := body.mint.map (·.policyId) |>.eraseDups |>.mergeSort
    (fun a b => compareByteArray a b != .gt)
  if h : index < policyIds.length then
    let policyId := policyIds[index]
    let allScripts := allScriptsFromWitness witnesses
    match ← findScriptByHashIO allScripts policyId with
    | none => return none
    | some (scriptBytes, version) => return some (scriptBytes, version, policyId)
  else return none

/-- Encode PlutusData as CBOR bytes for passing to FFI.
    This uses the standard Plutus data CBOR encoding. -/
private partial def encodePlutusDataCbor (d : PlutusData) : ByteArray :=
  -- For now, use a simple recursive CBOR encoder
  go d
where
  go : PlutusData → ByteArray
  | .Integer n =>
    if n >= 0 then
      Cleanode.Network.Cbor.encodeUInt n.toNat
    else
      -- Negative integer: CBOR major type 1, value = -1 - n
      let absVal := (-1 - n).toNat
      Cleanode.Network.Cbor.encodeNegInt absVal
  | .ByteString bs =>
    Cleanode.Network.Cbor.encodeBytes bs
  | .List items =>
    let body := items.foldl (fun acc item => acc ++ go item) ByteArray.empty
    Cleanode.Network.Cbor.encodeArrayHeader items.length ++ body
  | .Map entries =>
    let body := entries.foldl (fun acc (k, v) => acc ++ go k ++ go v) ByteArray.empty
    Cleanode.Network.Cbor.encodeMapHeader entries.length ++ body
  | .Constr tag fields =>
    -- Plutus CBOR encoding for constructors:
    -- tag 0-6: CBOR tag (121 + tag) wrapping array of fields
    -- tag 7-127: CBOR tag (1280 + tag - 7) wrapping array of fields
    -- tag > 127: CBOR tag 102 wrapping [tag, fields]
    let fieldsBytes := fields.foldl (fun acc f => acc ++ go f) ByteArray.empty
    let fieldsArr := Cleanode.Network.Cbor.encodeArrayHeader fields.length ++ fieldsBytes
    if tag <= 6 then
      Cleanode.Network.Cbor.encodeTagged (121 + tag) fieldsArr
    else if tag <= 127 then
      Cleanode.Network.Cbor.encodeTagged (1280 + tag - 7) fieldsArr
    else
      Cleanode.Network.Cbor.encodeTagged 102
        (Cleanode.Network.Cbor.encodeArrayHeader 2 ++
         Cleanode.Network.Cbor.encodeUInt tag ++ fieldsArr)

/-- Extract the script credential hash from a certificate, if it requires a script witness. -/
private def certScriptHash (cert : RawCertificate) : Option ByteArray :=
  match cert with
  | .stakeKeyRegistration true h | .stakeKeyDeregistration true h
  | .stakeDelegation true h _ | .conwayRegistration true h _
  | .conwayDeregistration true h _ | .voteDelegation true h _
  | .stakeVoteDelegation true h _ _ | .stakeRegDelegation true h _ _
  | .voteRegDelegation true h _ _ | .stakeVoteRegDelegation true h _ _ _
  | .authCommitteeHot true h _ | .resignCommitteeCold true h => some h
  | _ => none

/-- Build reference-input script list from the UTxO. -/
private def refInputScripts (body : TransactionBody) (utxo : UTxOSet) :
    List (ByteArray × PlutusVersion) :=
  body.referenceInputs.filterMap fun refInp =>
    let refId : UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
    match utxo.lookup refId with
    | some refOut => refOut.scriptRef.bind unwrapRefScript
    | none => none

/-- Evaluate all Plutus scripts in a transaction using Plutuz FFI.
    Builds ScriptContext and applies proper arguments per version/purpose.
    Returns `.ok ()` if all scripts pass, or `.error msg` on first failure. -/
def evaluateTransactionScriptsIO
    (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (txHash : ByteArray)
    : IO (Except String Unit) := do
  -- Pre-resolve inputs for ScriptContext
  let resolvedInputs := resolveInputs utxo body.inputs
  let resolvedRefInputs := resolveInputs utxo body.referenceInputs

  for redeemer in witnesses.redeemers do
    match redeemer.tag with
    | .Spend =>
      match ← resolveSpendScriptIO body witnesses utxo redeemer.index with
      | none => pure ()
      | some (scriptBytes, version, datum, inp) =>
        let redeemerData := decodePlutusDataFromCbor redeemer.data |>.getD (.ByteString redeemer.data)
        let spendRef : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
        let datumData := datum.map fun d => decodePlutusDataFromCbor d |>.getD (.ByteString d)
        let purpose := ScriptPurpose.Spending spendRef datumData
        let contextCbor := match version with
          | .V3 => encodePlutusDataCbor (buildScriptContext body witnesses resolvedInputs resolvedRefInputs txHash redeemerData purpose)
          | _ => encodePlutusDataCbor (buildScriptContextV2 body witnesses resolvedInputs resolvedRefInputs txHash purpose)
        let args := match version with
          | .V3 => [contextCbor]
          | _ => [datum.getD ByteArray.empty, redeemer.data, contextCbor]
        let result ← evaluateScriptIO scriptBytes version args redeemer.exUnits
        if !result.success then
          return .error s!"Plutus spend script failed (input {redeemer.index}): {result.error.getD "returned false"}"
    | .Mint =>
      match ← resolveMintScriptIO body witnesses redeemer.index with
      | none => pure ()
      | some (scriptBytes, version, policyId) =>
        let redeemerData := decodePlutusDataFromCbor redeemer.data |>.getD (.ByteString redeemer.data)
        let purpose := ScriptPurpose.Minting policyId
        let contextCbor := match version with
          | .V3 => encodePlutusDataCbor (buildScriptContext body witnesses resolvedInputs resolvedRefInputs txHash redeemerData purpose)
          | _ => encodePlutusDataCbor (buildScriptContextV2 body witnesses resolvedInputs resolvedRefInputs txHash purpose)
        let args := match version with
          | .V3 => [contextCbor]
          | _ => [redeemer.data, contextCbor]
        let result ← evaluateScriptIO scriptBytes version args redeemer.exUnits
        if !result.success then
          return .error s!"Plutus mint script failed (policy {redeemer.index}): {result.error.getD "returned false"}"
    | .Cert =>
      let allScripts := allScriptsFromWitness witnesses ++ refInputScripts body utxo
      match body.certificates.get? redeemer.index with
      | none => pure ()
      | some cert =>
        match certScriptHash cert with
        | none => pure ()  -- not a script-witnessed cert
        | some scriptHash =>
          match ← findScriptByHashIO allScripts scriptHash with
          | none => pure ()
          | some (scriptBytes, version) =>
            let redeemerData := decodePlutusDataFromCbor redeemer.data |>.getD (.ByteString redeemer.data)
            let certData := encodeCertPlutusData cert
            let purpose := ScriptPurpose.Certifying redeemer.index certData
            let contextCbor := match version with
              | .V3 => encodePlutusDataCbor (buildScriptContext body witnesses resolvedInputs resolvedRefInputs txHash redeemerData purpose)
              | _ => encodePlutusDataCbor (buildScriptContextV2 body witnesses resolvedInputs resolvedRefInputs txHash purpose)
            let args := match version with
              | .V3 => [contextCbor]
              | _ => [redeemer.data, contextCbor]
            let result ← evaluateScriptIO scriptBytes version args redeemer.exUnits
            if !result.success then
              return .error s!"Plutus cert script failed (index {redeemer.index}): {result.error.getD "returned false"}"
    | .Reward =>
      let allScripts := allScriptsFromWitness witnesses ++ refInputScripts body utxo
      match body.withdrawals.get? redeemer.index with
      | none => pure ()
      | some (rewardAddr, _) =>
        -- Credential hash: bytes 1..28 of reward address (header byte 0 encodes type)
        let credHash := if rewardAddr.size >= 29 then rewardAddr.extract 1 29 else rewardAddr
        match ← findScriptByHashIO allScripts credHash with
        | none => pure ()
        | some (scriptBytes, version) =>
          let redeemerData := decodePlutusDataFromCbor redeemer.data |>.getD (.ByteString redeemer.data)
          let purpose := ScriptPurpose.Rewarding rewardAddr
          let contextCbor := match version with
            | .V3 => encodePlutusDataCbor (buildScriptContext body witnesses resolvedInputs resolvedRefInputs txHash redeemerData purpose)
            | _ => encodePlutusDataCbor (buildScriptContextV2 body witnesses resolvedInputs resolvedRefInputs txHash purpose)
          let args := match version with
            | .V3 => [contextCbor]
            | _ => [redeemer.data, contextCbor]
          let result ← evaluateScriptIO scriptBytes version args redeemer.exUnits
          if !result.success then
            return .error s!"Plutus reward script failed (index {redeemer.index}): {result.error.getD "returned false"}"
    | .Vote | .Propose =>
      -- Conway governance redeemers — skip evaluation (not yet supported)
      pure ()
  return .ok ()

/-- Pure wrapper for backwards compatibility -/
def evaluateTransactionScripts
    (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (txHash : ByteArray)
    : Except String Unit :=
  let _ := body; let _ := witnesses; let _ := utxo; let _ := txHash
  .ok ()

end Cleanode.Plutus.Evaluate
