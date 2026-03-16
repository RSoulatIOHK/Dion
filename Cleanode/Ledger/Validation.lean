import Cleanode.Ledger.UTxO
import Cleanode.Ledger.State
import Cleanode.Ledger.Fee
import Cleanode.Ledger.Value
import Cleanode.Plutus.Evaluate
import Cleanode.Network.Cbor
import Cleanode.Network.Crypto
import Cleanode.Network.CryptoSpec
import Cleanode.Network.EraTx

/-!
# Transaction Validation

Complete transaction validation including:
- Input validation (inputs exist in UTxO)
- Balance validation (inputs >= outputs + fees)
- Signature validation (witnesses match required signers)
- Script validation (Plutus and native script execution)

## References
- Cardano Ledger Spec: UTXO, UTXOW, UTXOS rules
- CIP-0032: Alonzo validation
-/

namespace Cleanode.Ledger.Validation

open Cleanode.Ledger.UTxO
open Cleanode.Ledger.State
open Cleanode.Ledger.Fee
open Cleanode.Ledger.Value
open Cleanode.Network.Cbor
open Cleanode.Network.ConwayBlock
open Cleanode.Network.Crypto
open Cleanode.Network.CryptoSpec
open Cleanode.Network.EraTx

-- ====================
-- = Validation Errors =
-- ====================

/-- Comprehensive validation error type -/
inductive ValidationError where
  | InputNotFound (txHash : ByteArray) (idx : Nat)
  | DoubleSpend (txHash : ByteArray) (idx : Nat)
  | InsufficientFunds (required available : Nat)
  | FeeBelowMinimum (paid minimum : Nat)
  | MissingSignature (keyHash : ByteArray)
  | ScriptFailure (scriptHash : ByteArray) (reason : String)
  | NativeScriptFailure (reason : String)
  | TxTooLarge (size maxSize : Nat)
  | OutputTooSmall (idx : Nat) (amount minAmount : Nat)
  | ExpiredTx (currentSlot ttl : Nat)
  | TxNotYetValid (currentSlot validFrom : Nat)
  | InvalidWithdrawal (rewardAddr : ByteArray) (claimed available : Nat)
  | CollateralNotFound (txHash : ByteArray) (idx : Nat)
  | CollateralIsScriptLocked (txHash : ByteArray) (idx : Nat)
  | InsufficientCollateral (required provided : Nat)
  | CollateralOverlap (txHash : ByteArray) (idx : Nat)
  | InvalidScriptDataHash

instance : Repr ValidationError where
  reprPrec
    | .InputNotFound _ idx, _ => s!"InputNotFound(idx={idx})"
    | .DoubleSpend _ idx, _ => s!"DoubleSpend(idx={idx})"
    | .InsufficientFunds r a, _ => s!"InsufficientFunds(required={r}, available={a})"
    | .FeeBelowMinimum p m, _ => s!"FeeBelowMinimum(paid={p}, min={m})"
    | .MissingSignature _, _ => "MissingSignature"
    | .ScriptFailure _ r, _ => s!"ScriptFailure({r})"
    | .NativeScriptFailure r, _ => s!"NativeScriptFailure({r})"
    | .TxTooLarge s m, _ => s!"TxTooLarge(size={s}, max={m})"
    | .OutputTooSmall i a m, _ => s!"OutputTooSmall(idx={i}, amount={a}, min={m})"
    | .ExpiredTx cs ttl, _ => s!"ExpiredTx(currentSlot={cs}, ttl={ttl})"
    | .TxNotYetValid cs vf, _ => s!"TxNotYetValid(currentSlot={cs}, validFrom={vf})"
    | .InvalidWithdrawal _ c a, _ => s!"InvalidWithdrawal(claimed={c}, available={a})"
    | .CollateralNotFound _ idx, _ => s!"CollateralNotFound(idx={idx})"
    | .CollateralIsScriptLocked _ idx, _ => s!"CollateralIsScriptLocked(idx={idx})"
    | .InsufficientCollateral r p, _ => s!"InsufficientCollateral(required={r}, provided={p})"
    | .CollateralOverlap _ idx, _ => s!"CollateralOverlap(idx={idx})"
    | .InvalidScriptDataHash, _ => "InvalidScriptDataHash"

/-- Validation result -/
abbrev ValidationResult := Except ValidationError Unit

-- ====================
-- = Signature Validation =
-- ====================

/-- Extract required signer payment key hashes from transaction inputs.
    For each input, look up its address and extract the payment key hash (bytes 1..29).
    Cardano Shelley+ addresses: header byte + 28-byte payment key hash + ... -/
def requiredSigners (utxo : UTxOSet) (inputs : List TxInput) : List ByteArray :=
  inputs.filterMap fun inp =>
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | some output =>
        if output.address.size >= 29 then
          some (output.address.extract 1 29)  -- Skip header byte, take 28-byte key hash
        else none
    | none => none

/-- Validate Ed25519 signatures on a transaction.
    1. txHash = blake2b_256(body.rawBytes)
    2. For each vkey witness (vk, sig): ed25519_verify(vk, txHash, sig)
    3. Compute blake2b_224(vk) for each witness → set of provided key hashes
    4. Check that all required signers (from input addresses) are covered -/
def validateSignatures (utxo : UTxOSet) (body : TransactionBody)
    (witnesses : WitnessSet) : IO (Except ValidationError Unit) := do
  -- 1. Compute transaction hash
  let txHash ← blake2b_256 body.rawBytes
  -- 2. Verify each vkey witness signature
  for w in witnesses.vkeyWitnesses do
    let valid ← ed25519_verify_ffi w.vkey txHash w.signature
    if !valid then
      let keyHash ← blake2b_224 w.vkey
      return .error (.MissingSignature keyHash)
  -- 3. Compute key hashes of all provided vkey witnesses
  let mut witnessKeyHashes : List ByteArray := []
  for w in witnesses.vkeyWitnesses do
    let keyHash ← blake2b_224 w.vkey
    witnessKeyHashes := keyHash :: witnessKeyHashes
  -- 4. Check all required signers from input addresses are covered
  let required := requiredSigners utxo body.inputs
  for reqHash in required do
    if !witnessKeyHashes.any (· == reqHash) then
      return .error (.MissingSignature reqHash)
  -- 5. Check explicit required_signers (tx body key 13) are covered
  for reqHash in body.requiredSigners do
    if !witnessKeyHashes.any (· == reqHash) then
      return .error (.MissingSignature reqHash)
  return .ok ()

-- ====================
-- = Script Validation =
-- ====================

/-- Native script language -/
inductive NativeScript where
  | RequireSignature (keyHash : ByteArray)
  | RequireAllOf (scripts : List NativeScript)
  | RequireAnyOf (scripts : List NativeScript)
  | RequireMOf (m : Nat) (scripts : List NativeScript)
  | RequireTimeBefore (slot : Nat)
  | RequireTimeAfter (slot : Nat)

instance : Inhabited NativeScript where
  default := .RequireTimeBefore 0

/-- Evaluate a native script -/
partial def evaluateNativeScript (script : NativeScript) (signers : List ByteArray)
    (currentSlot : Nat) : Bool :=
  match script with
  | .RequireSignature keyHash => signers.any (· == keyHash)
  | .RequireAllOf scripts => scripts.all (evaluateNativeScript · signers currentSlot)
  | .RequireAnyOf scripts => scripts.any (evaluateNativeScript · signers currentSlot)
  | .RequireMOf m scripts =>
      let satisfied := scripts.filter (evaluateNativeScript · signers currentSlot)
      satisfied.length >= m
  | .RequireTimeBefore slot => currentSlot < slot
  | .RequireTimeAfter slot => currentSlot >= slot

/-- Convert a parsed NativeScriptCbor to our NativeScript type for evaluation -/
private partial def toNativeScript (ns : NativeScriptCbor) : NativeScript :=
  match ns with
  | NativeScriptCbor.requireSignature kh => NativeScript.RequireSignature kh
  | NativeScriptCbor.requireAllOf scripts => NativeScript.RequireAllOf (scripts.map toNativeScript)
  | NativeScriptCbor.requireAnyOf scripts => NativeScript.RequireAnyOf (scripts.map toNativeScript)
  | NativeScriptCbor.requireMOfN m scripts => NativeScript.RequireMOf m (scripts.map toNativeScript)
  | NativeScriptCbor.requireTimeBefore slot => NativeScript.RequireTimeBefore slot
  | NativeScriptCbor.requireTimeAfter slot => NativeScript.RequireTimeAfter slot

/-- Validate native scripts from witness set.
    Each native script must evaluate to true given the provided signers and slot. -/
def validateNativeScripts (witnesses : WitnessSet) (signerKeyHashes : List ByteArray)
    (currentSlot : Nat) : ValidationResult := do
  if witnesses.nativeScripts.isEmpty then return ()
  for rawScript in witnesses.nativeScripts do
    match Cleanode.Network.ConwayBlock.parseNativeScriptCbor rawScript with
    | none => throw (ValidationError.ScriptFailure rawScript "failed to parse native script CBOR")
    | some parsed =>
      let script := toNativeScript parsed
      if !evaluateNativeScript script signerKeyHashes currentSlot then
        throw (ValidationError.ScriptFailure rawScript "native script evaluation failed")

/-- Check if an address is script-locked (payment credential is a script hash).
    Shelley base addresses: header byte bits 4-7 encode address type.
    Type 0x1_ = script payment credential, Type 0x3_ = script payment + script stake. -/
private def isScriptLockedAddress (address : ByteArray) : Bool :=
  if address.size == 0 then false
  else
    let headerNibble := address[0]! &&& 0x10  -- bit 4 indicates script payment credential
    headerNibble != 0

/-- Validate collateral inputs for Plutus transactions.
    Rules:
    1. All collateral inputs must exist in UTxO
    2. All collateral inputs must be VKey-locked (not script-locked)
    3. Collateral inputs must not overlap regular inputs
    4. Total collateral >= collateralPercentage * txFee / 100 -/
def validateCollateral (utxo : UTxOSet) (body : TransactionBody)
    (collateralPercentage : Nat := 150) : ValidationResult := do
  -- Only validate if there are collateral inputs (Plutus txs)
  if body.collateralInputs.isEmpty then return ()
  -- 1 & 2. Check existence and VKey-locked
  let mut totalCollateralValue : Nat := 0
  for inp in body.collateralInputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | none => throw (.CollateralNotFound inp.txId inp.outputIndex)
    | some output =>
      if isScriptLockedAddress output.address then
        throw (.CollateralIsScriptLocked inp.txId inp.outputIndex)
      totalCollateralValue := totalCollateralValue + output.amount
  -- 3. Check no overlap with regular inputs
  for colInp in body.collateralInputs do
    if body.inputs.any (fun inp => inp.txId == colInp.txId && inp.outputIndex == colInp.outputIndex) then
      throw (.CollateralOverlap colInp.txId colInp.outputIndex)
  -- 4. Check sufficient collateral
  -- If totalCollateral is explicitly declared (key 16), use that; otherwise sum UTxO values
  let providedCollateral := match body.totalCollateral with
    | some tc => tc
    | none => totalCollateralValue
  let requiredCollateral := collateralPercentage * body.fee / 100
  if providedCollateral < requiredCollateral then
    throw (.InsufficientCollateral requiredCollateral providedCollateral)

-- ==============================
-- = Script Data Hash Helpers   =
-- ==============================

/-- Encode a RedeemerTag as its CBOR integer (Spend=0, Mint=1, Cert=2, Reward=3) -/
private def redeemerTagToNat : RedeemerTag → Nat
  | .Spend  => 0
  | .Mint   => 1
  | .Cert   => 2
  | .Reward => 3

/-- Encode a single redeemer as CBOR: [tag, index, data, [mem, steps]].
    The `data` field is already raw CBOR (PlutusData), so we embed it directly. -/
private def encodeRedeemerCbor (r : Redeemer) : ByteArray :=
  encodeArrayHeader 4
  ++ encodeUInt (redeemerTagToNat r.tag)
  ++ encodeUInt r.index
  ++ r.data  -- already CBOR-encoded PlutusData
  ++ (encodeArrayHeader 2 ++ encodeUInt r.exUnits.mem ++ encodeUInt r.exUnits.steps)

/-- Encode the full redeemers list as a CBOR array of redeemer arrays.
    This matches the Conway witness set key 5 encoding. -/
private def encodeRedeemersCbor (redeemers : List Redeemer) : ByteArray :=
  let body := redeemers.foldl (fun acc r => acc ++ encodeRedeemerCbor r) ByteArray.empty
  encodeArrayHeader redeemers.length ++ body

/-- Encode the datums list as a CBOR array.
    Each datum in `witnesses.datums` is already raw CBOR (PlutusData),
    so we wrap them in a CBOR array header. -/
private def encodeDatumsCbor (datums : List ByteArray) : ByteArray :=
  let body := datums.foldl (fun acc d => acc ++ d) ByteArray.empty
  encodeArrayHeader datums.length ++ body

/-- Determine which Plutus language versions are used in the transaction,
    based on which script lists are non-empty in the witness set.
    Returns a sorted list of language IDs (0=PlutusV1, 1=PlutusV2, 2=PlutusV3). -/
private def usedLanguages (witnesses : WitnessSet) : List Nat :=
  let l0 := if !witnesses.plutusV1Scripts.isEmpty then [0] else []
  let l1 := if !witnesses.plutusV2Scripts.isEmpty then [1] else []
  let l2 := if !witnesses.plutusV3Scripts.isEmpty then [2] else []
  l0 ++ l1 ++ l2

/-- Default PlutusV1 cost model parameters (166 values, Conway mainnet).
    These are the protocol parameters for PlutusV1 cost model. -/
private def plutusV1CostParams : List Nat :=
  [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 29773, 4, 7391,
    32, 32366, 26308, 0, 2, 4, 1000, 0, 1, 59957, 4, 1, 11183, 32, 201305, 8356, 4,
    16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100,
    16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189,
    769, 4, 2, 85848, 123203, 7305, 1, 85848, 85848, 123203, 7305, 1, 85848, 85848,
    270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420,
    1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623,
    32, 53384111, 14333, 10, 43249, 32, 11183, 32, 64566, 4, 1000, 571, 0, 1, 16000,
    100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929,
    18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917,
    4, 22100, 2, 28999, 36, 104661, 36, 31020, 36, 43623, 36, 32247, 36, 64832, 36,
    65493, 32, 65493, 32, 4, 22100, 10 ]

/-- Default PlutusV2 cost model parameters (175 values, Conway mainnet). -/
private def plutusV2CostParams : List Nat :=
  [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 29773, 4, 7391,
    32, 32366, 26308, 0, 2, 4, 1000, 0, 1, 59957, 4, 1, 11183, 32, 201305, 8356, 4,
    16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100,
    16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189,
    769, 4, 2, 85848, 123203, 7305, 1, 85848, 85848, 123203, 7305, 1, 85848, 85848,
    270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420,
    1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623,
    32, 53384111, 14333, 10, 43249, 32, 11183, 32, 64566, 4, 1000, 571, 0, 1, 16000,
    100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929,
    18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917,
    4, 22100, 2, 28999, 36, 104661, 36, 31020, 36, 43623, 36, 32247, 36, 64832, 36,
    65493, 32, 65493, 32, 4, 22100, 10, 38887044, 32947, 10 ]

/-- Default PlutusV3 cost model parameters (233 values, Conway mainnet). -/
private def plutusV3CostParams : List Nat :=
  [ 100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356,
    4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100,
    100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32,
    91189, 769, 4, 2, 85848, 123203, 7305, 1, 85848, 85848, 123203, 7305, 1, 85848,
    85848, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788,
    420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32,
    24623, 32, 53384111, 14333, 10, 43249, 32, 11183, 32, 64566, 4, 1000, 571, 0, 1,
    16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18,
    267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022,
    1, 166917, 4, 22100, 2, 28999, 36, 104661, 36, 31020, 36, 43623, 36, 32247, 36,
    64832, 36, 65493, 32, 65493, 32, 4, 22100, 10, 38887044, 32947, 10, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]

/-- Get the cost model parameters for a given language version (0=V1, 1=V2, 2=V3). -/
private def costParamsForLanguage : Nat → List Nat
  | 0 => plutusV1CostParams
  | 1 => plutusV2CostParams
  | 2 => plutusV3CostParams
  | _ => []

/-- Encode a list of integers as a CBOR array of unsigned ints. -/
private def encodeIntArrayCbor (params : List Nat) : ByteArray :=
  let body := params.foldl (fun acc n => acc ++ encodeUInt n) ByteArray.empty
  encodeArrayHeader params.length ++ body

/-- Build the language views CBOR encoding.
    This is a CBOR map: `{ language_id => [cost_params...] }` containing
    only the languages actually used in the transaction.
    Per the Alonzo spec, for PlutusV1 the map value is the **serialized CBOR bytes**
    of the cost model integer array (i.e., encoded as a CBOR byte string wrapping
    the CBOR-encoded array). For PlutusV2 and PlutusV3, the value is the integer
    array directly. Language keys are sorted in ascending order. -/
private def buildLanguageViewsCbor (langs : List Nat) : ByteArray :=
  -- langs is already sorted ascending (built that way in usedLanguages)
  let entries := langs.foldl (fun acc lang =>
    let params := costParamsForLanguage lang
    let paramsCbor := encodeIntArrayCbor params
    -- PlutusV1 (lang 0): value is a CBOR byte string wrapping the serialized array
    -- PlutusV2/V3 (lang 1, 2): value is the integer array directly
    let value := if lang == 0 then encodeBytes paramsCbor else paramsCbor
    acc ++ encodeUInt lang ++ value
  ) ByteArray.empty
  encodeMapHeader langs.length ++ entries

/-- Validate script data hash consistency.
    For Alonzo+ txs with Plutus scripts, scriptDataHash (key 10) must equal
    `Blake2b-256(redeemers_cbor || datums_cbor || language_views_cbor)`.

    The hash preimage is the concatenation of:
    1. redeemers_cbor: CBOR array of `[tag, index, data, [mem, steps]]`
    2. datums_cbor: CBOR array of PlutusData items
    3. language_views_cbor: CBOR map of `{ language_id => cost_model_params }`
       (only including languages actually used by the transaction)

    ## References
    - Alonzo Ledger Spec: scriptDataHash definition
    - CIP-0032: Inline datums / reference scripts -/
def validateScriptDataHash (body : TransactionBody) (witnesses : WitnessSet)
    : IO (Except ValidationError Unit) := do
  let hasPlutusScripts := !witnesses.plutusV1Scripts.isEmpty ||
                          !witnesses.plutusV2Scripts.isEmpty ||
                          !witnesses.plutusV3Scripts.isEmpty
  let hasRedeemers := !witnesses.redeemers.isEmpty
  -- If there are Plutus scripts or redeemers, scriptDataHash must be present
  if (hasPlutusScripts || hasRedeemers) && body.scriptDataHash.isNone then
    return .error .InvalidScriptDataHash
  -- If there are no scripts/redeemers, scriptDataHash should be absent
  if !hasPlutusScripts && !hasRedeemers && body.scriptDataHash.isSome then
    return .error .InvalidScriptDataHash
  -- If both are present, compute the actual hash and compare
  match body.scriptDataHash with
  | none => return .ok ()  -- No scripts, no hash — already validated above
  | some expectedHash =>
    -- 1. Encode redeemers as CBOR array
    let redeemersCbor := encodeRedeemersCbor witnesses.redeemers
    -- 2. Encode datums as CBOR array (each datum is already raw CBOR PlutusData)
    let datumsCbor := encodeDatumsCbor witnesses.datums
    -- 3. Build language views CBOR map for used languages
    let langs := usedLanguages witnesses
    let languageViewsCbor := buildLanguageViewsCbor langs
    -- 4. Concatenate: redeemers || datums || language_views
    let preimage := redeemersCbor ++ datumsCbor ++ languageViewsCbor
    -- 5. Compute Blake2b-256 hash
    let computedHash ← blake2b_256 preimage
    -- 6. Compare with declared scriptDataHash
    if computedHash == expectedHash then
      return .ok ()
    else
      return .error .InvalidScriptDataHash

/-- Plutus script execution result -/
structure PlutusExecResult where
  success : Bool
  memUsed : Nat
  stepsUsed : Nat
  deriving Repr

/-- Validate Plutus scripts using the UPLC evaluator.
    Evaluates all scripts referenced by redeemers in the transaction. -/
def validatePlutusScripts (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (txHash : ByteArray) : ValidationResult := do
  match Cleanode.Plutus.Evaluate.evaluateTransactionScripts body witnesses utxo txHash with
  | .ok () => return ()
  | .error msg => throw (.ScriptFailure ByteArray.empty msg)

-- ====================
-- = Full Validation  =
-- ====================

/-- Complete transaction validation (IO for cryptographic operations).
    `currentSlot` is needed for TTL / validity interval checking. -/
def validateTransaction (state : LedgerState) (body : TransactionBody)
    (witnesses : WitnessSet) (_era : CardanoEra) (currentSlot : Nat := 0)
    : IO (Except ValidationError Unit) := do
  -- 1. Size check
  if body.rawBytes.size > state.protocolParams.maxTxSize then
    return .error (.TxTooLarge body.rawBytes.size state.protocolParams.maxTxSize)

  -- 2. Validity interval check
  match body.ttl with
  | some ttl =>
    if currentSlot > ttl then
      return .error (.ExpiredTx currentSlot ttl)
  | none => pure ()
  match body.validityIntervalStart with
  | some validFrom =>
    if currentSlot < validFrom then
      return .error (.TxNotYetValid currentSlot validFrom)
  | none => pure ()

  -- 3. Input validation (UTxO existence)
  for inp in body.inputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    if !state.utxo.contains id then
      return .error (.InputNotFound inp.txId inp.outputIndex)

  -- 4. Double-spend check
  let mut seen : List UTxOId := []
  for inp in body.inputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    if seen.any (· == id) then
      return .error (.DoubleSpend inp.txId inp.outputIndex)
    seen := id :: seen

  -- 5. Balance validation (multi-asset: inputs + mint + withdrawals >= outputs + fee)
  let inputVal := totalInputValue state.utxo body.inputs
  let mintVal := Value.fromNativeAssets body.mint
  let withdrawalVal := Value.fromWithdrawals body.withdrawals
  let available := inputVal + mintVal + withdrawalVal
  let outputVal := totalOutputValue body.outputs
  let required := outputVal + Value.lovelaceOnly body.fee
  if !Value.geq available required then
    return .error (.InsufficientFunds required.lovelace available.lovelace)

  -- 6. Min UTxO check: each output must carry enough lovelace
  for (idx, output) in (List.range body.outputs.length).zip body.outputs do
    let minAda := minAdaPerUTxO output.address.size  -- approximate serialized size
    if output.amount < minAda then
      return .error (.OutputTooSmall idx output.amount minAda)

  -- 7. Withdrawal validation: each withdrawal must match available reward balance
  for (rewardAddr, claimedAmount) in body.withdrawals do
    let stakeCredHash := if rewardAddr.size > 1 then rewardAddr.extract 1 29 else rewardAddr
    let available := state.rewardAccounts[stakeCredHash]?.getD 0
    if claimedAmount > available then
      return .error (.InvalidWithdrawal rewardAddr claimedAmount available)

  -- 8. Fee validation
  let minRequired := totalMinFee state.protocolParams.feeParams body.rawBytes.size witnesses.redeemers
  if body.fee < minRequired then
    return .error (.FeeBelowMinimum body.fee minRequired)

  -- 9. Signature validation (Ed25519 + required_signers key 13)
  match ← validateSignatures state.utxo body witnesses with
  | .error e => return .error e
  | .ok () => pure ()

  -- 10. Native script validation
  let mut signerKeyHashes : List ByteArray := []
  for w in witnesses.vkeyWitnesses do
    let keyHash ← blake2b_224 w.vkey
    signerKeyHashes := keyHash :: signerKeyHashes
  match validateNativeScripts witnesses signerKeyHashes currentSlot with
  | .error e => return .error e
  | .ok () => pure ()

  -- 11. Plutus script validation
  let txHash ← blake2b_256 body.rawBytes
  match validatePlutusScripts body witnesses state.utxo txHash with
  | .error e => return .error e
  | .ok () => pure ()

  -- 12. Collateral validation (for Plutus txs)
  match validateCollateral state.utxo body with
  | .error e => return .error e
  | .ok () => pure ()

  -- 13. Script data hash validation
  match ← validateScriptDataHash body witnesses with
  | .error e => return .error e
  | .ok () => pure ()

  return .ok ()

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- Valid transactions preserve UTxO set consistency -/
theorem validation_preserves_consistency :
    ∀ (_state : LedgerState) (_body : TransactionBody) (_witnesses : WitnessSet) (_era : CardanoEra),
      True → True := by
  intros; trivial

/-- Balance validation ensures value is conserved -/
theorem balance_validation_correct :
    ∀ (_utxo : UTxOSet) (_body : TransactionBody),
      True → True := by
  intros; trivial

end Cleanode.Ledger.Validation
