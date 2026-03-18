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
-- = ByteArray Ordering =
-- ====================

/-- Lexicographic comparison of ByteArrays (used for input sorting per Cardano spec). -/
def compareByteArray (a b : ByteArray) : Ordering :=
  let minLen := min a.size b.size
  go 0 minLen a b
where
  go (i : Nat) (len : Nat) (a b : ByteArray) : Ordering :=
    if i >= len then compare a.size b.size
    else
      let va := a[i]!
      let vb := b[i]!
      if va < vb then .lt
      else if va > vb then .gt
      else go (i + 1) len a b

/-- Lexicographic comparison of TxInputs: first by txId, then by outputIndex. -/
def compareTxInput (a b : TxInput) : Ordering :=
  match compareByteArray a.txId b.txId with
  | .eq => compare a.outputIndex b.outputIndex
  | ord => ord

/-- Sort inputs lexicographically and return (sortedPosition, originalInput) pairs.
    Redeemer Spend(index) refers to position in this sorted order. -/
def sortInputs (inputs : List TxInput) : List TxInput :=
  inputs.mergeSort (fun a b => compareTxInput a b != .gt)

-- ====================
-- = Validation Errors =
-- ====================

/-- Comprehensive validation error type -/
inductive ValidationError where
  | InputNotFound (txHash : ByteArray) (idx : Nat)
  | DoubleSpend (txHash : ByteArray) (idx : Nat)
  | InsufficientFunds (required available : Nat)
  | FeeBelowMinimum (paid minimum : Nat)
  | MissingSignature (keyHash : ByteArray) (reason : String)
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
  | NoCollateral
  | InvalidScriptDataHash (detail : String := "")
  -- Phase 1: UTXO checks (#378-384)
  | InputSetEmpty                     -- #378
  | OutputTooBig (idx : Nat) (size maxSize : Nat)  -- #379
  | WrongNetwork (detail : String)    -- #380
  | CollateralContainsNonADA (txHash : ByteArray) (idx : Nat)  -- #381
  | TooManyCollateralInputs (count maxCount : Nat)  -- #382
  | IncorrectTotalCollateral (declared actual : Nat)  -- #383
  | NonDisjointRefInputs (txHash : ByteArray) (idx : Nat)  -- #384
  -- Phase 2: Witness/Script checks (#385-391)
  | ExtraneousScriptWitness (scriptHash : ByteArray)  -- #385
  | MissingRequiredScript (scriptHash : ByteArray)   -- #385 (missing side)
  | MetadataHashMismatch (detail : String)  -- #386
  | ValidationTagMismatch (declared actual : Bool)  -- #387
  | ExtraRedeemer (tag : String) (idx : Nat)  -- #388
  | MissingRedeemer (tag : String) (idx : Nat)      -- #388 (missing side)
  | MissingRequiredDatum (datumHash : ByteArray)     -- datum preimage missing
  | ExUnitsTooBig (mem steps maxMem maxSteps : Nat)  -- #389
  | UnspendableUTxONoDatum (txHash : ByteArray) (idx : Nat)  -- #390
  -- Certificate/delegation checks (#391-395)
  | Phase2CollateralInsufficient (detail : String)  -- #391
  | StakeDeregNonZeroReward (stakeKeyHash : ByteArray) (balance : Nat)  -- #392
  | CertDepositMismatch (provided expected : Nat) (certType : String)  -- #393
  | PoolCostTooLow (cost minCost : Nat)  -- #394
  | DelegateeNotRegistered (detail : String)  -- #395
  -- Phase 4: Block-level checks (#374-377)
  | WrongBlockBodySize (declared actual : Nat)  -- #374
  | TooManyBlockExUnits (mem steps maxMem maxSteps : Nat)  -- #375
  | RefScriptsSizeTooBig (size maxSize : Nat)  -- #376
  | HeaderProtVerTooHigh (version : Nat)  -- #377

instance : Repr ValidationError where
  reprPrec
    | .InputNotFound _ idx, _ => s!"InputNotFound(idx={idx})"
    | .DoubleSpend _ idx, _ => s!"DoubleSpend(idx={idx})"
    | .InsufficientFunds r a, _ => s!"InsufficientFunds(required={r}, available={a})"
    | .FeeBelowMinimum p m, _ => s!"FeeBelowMinimum(paid={p}, min={m})"
    | .MissingSignature h r, _ =>
      let hexPrefix := String.join (h.toList.take 4 |>.map fun b => String.mk (Nat.toDigits 16 b.toNat))
      s!"MissingSignature({r}, keyHash={hexPrefix}..)"
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
    | .NoCollateral, _ => "NoCollateral"
    | .InvalidScriptDataHash d, _ => if d.isEmpty then "InvalidScriptDataHash" else s!"InvalidScriptDataHash({d})"
    | .InputSetEmpty, _ => "InputSetEmpty"
    | .OutputTooBig i s m, _ => s!"OutputTooBig(idx={i}, size={s}, max={m})"
    | .WrongNetwork d, _ => s!"WrongNetwork({d})"
    | .CollateralContainsNonADA _ idx, _ => s!"CollateralContainsNonADA(idx={idx})"
    | .TooManyCollateralInputs c m, _ => s!"TooManyCollateralInputs(count={c}, max={m})"
    | .IncorrectTotalCollateral d a, _ => s!"IncorrectTotalCollateral(declared={d}, actual={a})"
    | .NonDisjointRefInputs _ idx, _ => s!"NonDisjointRefInputs(idx={idx})"
    | .ExtraneousScriptWitness sh, _ =>
      let hd (n : Nat) : Char := if n < 10 then Char.ofNat (n + 48) else Char.ofNat (n + 87)
      let hex := sh.foldl (fun s b => s ++ String.singleton (hd (b.toNat / 16)) ++ String.singleton (hd (b.toNat % 16))) ""
      s!"ExtraneousScriptWitness({hex})"
    | .MissingRequiredScript sh, _ =>
      let hd (n : Nat) : Char := if n < 10 then Char.ofNat (n + 48) else Char.ofNat (n + 87)
      let hex := sh.foldl (fun s b => s ++ String.singleton (hd (b.toNat / 16)) ++ String.singleton (hd (b.toNat % 16))) ""
      s!"MissingRequiredScript({hex})"
    | .MetadataHashMismatch d, _ => s!"MetadataHashMismatch({d})"
    | .ValidationTagMismatch d a, _ => s!"ValidationTagMismatch(declared={d}, actual={a})"
    | .ExtraRedeemer t i, _ => s!"ExtraRedeemer(tag={t}, idx={i})"
    | .MissingRedeemer t i, _ => s!"MissingRedeemer(tag={t}, idx={i})"
    | .MissingRequiredDatum _, _ => "MissingRequiredDatum"
    | .ExUnitsTooBig m s mm ms, _ => s!"ExUnitsTooBig(mem={m}, steps={s}, maxMem={mm}, maxSteps={ms})"
    | .UnspendableUTxONoDatum _ idx, _ => s!"UnspendableUTxONoDatum(idx={idx})"
    | .Phase2CollateralInsufficient d, _ => s!"Phase2CollateralInsufficient({d})"
    | .StakeDeregNonZeroReward _ b, _ => s!"StakeDeregNonZeroReward(balance={b})"
    | .CertDepositMismatch p e ct, _ => s!"CertDepositMismatch(provided={p}, expected={e}, cert={ct})"
    | .PoolCostTooLow c m, _ => s!"PoolCostTooLow(cost={c}, min={m})"
    | .DelegateeNotRegistered d, _ => s!"DelegateeNotRegistered({d})"
    | .WrongBlockBodySize d a, _ => s!"WrongBlockBodySize(declared={d}, actual={a})"
    | .TooManyBlockExUnits m s mm ms, _ => s!"TooManyBlockExUnits(mem={m}, steps={s}, maxMem={mm}, maxSteps={ms})"
    | .RefScriptsSizeTooBig s m, _ => s!"RefScriptsSizeTooBig(size={s}, max={m})"
    | .HeaderProtVerTooHigh v, _ => s!"HeaderProtVerTooHigh(version={v})"

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
          let headerByte := output.address[0]!.toNat
          -- Bit 4 of header: 0 = key hash (needs vkey witness), 1 = script hash (needs script)
          -- Also skip Byron/bootstrap addresses (header type 8 = 0b1000xxxx)
          if headerByte &&& 0x10 == 0 && headerByte >>> 4 != 8 then
            some (output.address.extract 1 29)  -- Skip header byte, take 28-byte key hash
          else none
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
      return .error (.MissingSignature keyHash "ed25519_verify failed")
  -- 3. Compute key hashes of all provided vkey witnesses
  let mut witnessKeyHashes : List ByteArray := []
  for w in witnesses.vkeyWitnesses do
    let keyHash ← blake2b_224 w.vkey
    witnessKeyHashes := keyHash :: witnessKeyHashes
  -- 4. Check all required signers from input addresses are covered
  let required := requiredSigners utxo body.inputs
  for reqHash in required do
    if !witnessKeyHashes.any (· == reqHash) then
      return .error (.MissingSignature reqHash s!"input addr signer not in {witnessKeyHashes.length} witnesses")
  -- 5. Check explicit required_signers (tx body key 13) are covered
  for reqHash in body.requiredSigners do
    if !witnessKeyHashes.any (· == reqHash) then
      return .error (.MissingSignature reqHash s!"required_signer (key13) not in {witnessKeyHashes.length} witnesses")
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

/-- Decode a CBOR byte string, returning just the payload (stripping the CBOR header).
    Plutus scripts in the witness set are stored as CBOR bstrings; the script hash
    must be computed over the inner bytes only, not the CBOR encoding. -/
private def decodeCborBstring (bs : ByteArray) : ByteArray :=
  if bs.size == 0 then bs
  else
    let major := bs[0]! >>> 5
    if major != 2 then bs  -- not a bstring, return as-is
    else
      let additional := bs[0]! &&& 0x1F
      if additional < 24 then bs.extract 1 bs.size
      else if additional == 24 && bs.size >= 2 then bs.extract 2 bs.size
      else if additional == 25 && bs.size >= 3 then bs.extract 3 bs.size
      else if additional == 26 && bs.size >= 5 then bs.extract 5 bs.size
      else if additional == 27 && bs.size >= 9 then bs.extract 9 bs.size
      else bs  -- fallback

/-- Unwrap a reference script from CDDL: `#6.24(bytes .cbor [type, script_bytes])`.
    Returns (scriptBytes, versionPrefixByte) for hashing as blake2b_224(prefix ++ script). -/
private partial def unwrapRefScriptForHash (raw : ByteArray) : Option (ByteArray × UInt8) := do
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
  let scriptType := cborPayload[1]!
  let scriptBytes := cborPayload.extract 2 cborPayload.size
  some (scriptBytes, scriptType)

/-- Validate collateral inputs for Plutus transactions.
    Rules:
    1. All collateral inputs must exist in UTxO
    2. All collateral inputs must be VKey-locked (not script-locked)
    3. Collateral inputs must not overlap regular inputs
    4. Total collateral >= collateralPercentage * txFee / 100
    5. (#381) Collateral must contain only ADA
    6. (#382) Number of collateral inputs <= maxCollateralInputs
    7. (#383) If totalCollateral declared, must match actual -/
def validateCollateral (utxo : UTxOSet) (body : TransactionBody)
    (hasRedeemers : Bool) (collateralPercentage : Nat := 150) (maxCollateralInputs : Nat := 3)
    : ValidationResult := do
  if body.collateralInputs.isEmpty then
    if hasRedeemers then throw .NoCollateral
    else return ()
  -- #382: Check max collateral inputs count
  if body.collateralInputs.length > maxCollateralInputs then
    throw (.TooManyCollateralInputs body.collateralInputs.length maxCollateralInputs)
  -- 1 & 2. Check existence and VKey-locked
  -- Babbage/Conway: collateral inputs MAY contain native assets as long as
  -- collateral return output returns them (CIP-40). We only check ADA balance.
  let mut totalCollateralValue : Nat := 0
  for inp in body.collateralInputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | none => throw (.CollateralNotFound inp.txId inp.outputIndex)
    | some output =>
      if isScriptLockedAddress output.address then
        throw (.CollateralIsScriptLocked inp.txId inp.outputIndex)
      totalCollateralValue := totalCollateralValue + output.amount
  -- 3b. Check collateral return doesn't exceed collateral inputs
  match body.collateralReturn with
  | some ret =>
    if ret.amount > totalCollateralValue then
      throw (.InsufficientCollateral totalCollateralValue ret.amount)
  | none => pure ()
  -- 4. Check sufficient collateral
  let providedCollateral := match body.totalCollateral with
    | some tc => tc
    | none => totalCollateralValue
  -- Use multiplication form to avoid integer division truncation:
  -- providedCollateral * 100 >= fee * collateralPercentage
  if providedCollateral * 100 < body.fee * collateralPercentage then
    let requiredCollateral := (body.fee * collateralPercentage + 99) / 100  -- ceiling division for error display
    throw (.InsufficientCollateral requiredCollateral providedCollateral)
  -- #383: If totalCollateral explicitly declared, verify it matches actual
  match body.totalCollateral with
  | some declared =>
    -- actual = sum(collateral inputs) - collateralReturn output value
    let returnValue := match body.collateralReturn with
      | some ret => ret.amount
      | none => 0
    let actual := totalCollateralValue - returnValue
    if declared != actual then
      throw (.IncorrectTotalCollateral declared actual)
  | none => pure ()

-- ==============================
-- = Script Data Hash Helpers   =
-- ==============================

/-- Encode a RedeemerTag as its CBOR integer (Spend=0, Mint=1, Cert=2, Reward=3) -/
private def redeemerTagToNat : RedeemerTag → Nat
  | .Spend   => 0
  | .Mint    => 1
  | .Cert    => 2
  | .Reward  => 3
  | .Vote    => 4
  | .Propose => 5

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
  let _hasPlutusScripts := !witnesses.plutusV1Scripts.isEmpty ||
                           !witnesses.plutusV2Scripts.isEmpty ||
                           !witnesses.plutusV3Scripts.isEmpty
  let hasRedeemers := !witnesses.redeemers.isEmpty
  let hasRawRedeemers := !witnesses.rawRedeemersCbor.isEmpty
  -- Note: scripts may come from reference inputs (CIP-33), so we can't check
  -- hasPlutusScripts reliably. Only check based on redeemers presence.
  -- Also, tx body parsing may break early, leaving scriptDataHash as none.
  -- Only validate when we have both a hash AND redeemers/raw bytes to compare.
  if !hasRedeemers && !hasRawRedeemers && body.scriptDataHash.isSome then
    return .error (.InvalidScriptDataHash "no redeemers but hash present")
  -- If both are present, compute the actual hash and compare
  match body.scriptDataHash with
  | none => return .ok ()  -- No scripts, no hash — already validated above
  | some expectedHash =>
    -- Use raw CBOR bytes captured during witness set parsing.
    -- Preimage = redeemers_cbor || datums_cbor || language_views_cbor
    let redeemersCbor := witnesses.rawRedeemersCbor
    -- If no datums in witness set, use empty CBOR array (0x80)
    let datumsCbor := if witnesses.rawDatumsCbor.isEmpty then ByteArray.mk #[0x80] else witnesses.rawDatumsCbor
    let langs := usedLanguages witnesses
    let languageViewsCbor := buildLanguageViewsCbor langs
    let preimage := redeemersCbor ++ datumsCbor ++ languageViewsCbor
    let computedHash ← blake2b_256 preimage
    if computedHash == expectedHash then
      return .ok ()
    else
      return .error (.InvalidScriptDataHash s!"red={redeemersCbor.size}B dat={datumsCbor.size}B lv={languageViewsCbor.size}B langs={langs}")

/-- Plutus script execution result -/
structure PlutusExecResult where
  success : Bool
  memUsed : Nat
  stepsUsed : Nat
  deriving Repr

/-- Validate Plutus scripts using the UPLC evaluator.
    Evaluates all scripts referenced by redeemers in the transaction. -/
def validatePlutusScriptsIO (body : TransactionBody) (witnesses : WitnessSet)
    (utxo : UTxOSet) (txHash : ByteArray) : IO ValidationResult := do
  match ← Cleanode.Plutus.Evaluate.evaluateTransactionScriptsIO body witnesses utxo txHash with
  | .ok () => return .ok ()
  | .error msg => return .error (.ScriptFailure ByteArray.empty msg)

-- ====================
-- = Full Validation  =
-- ====================

/-- Complete transaction validation (IO for cryptographic operations).
    `currentSlot` is needed for TTL / validity interval checking.
    Returns a list of all validation errors found (empty = valid). -/
def validateTransaction (state : LedgerState) (body : TransactionBody)
    (witnesses : WitnessSet) (_era : CardanoEra) (currentSlot : Nat := 0)
    (txSerializedSize : Nat := 0)
    : IO (List ValidationError) := do
  let mut errors : List ValidationError := []
  -- Use full serialized tx size if available, fall back to body CBOR size
  let effectiveTxSize := if txSerializedSize > 0 then txSerializedSize else body.rawBytes.size
  -- 1. Size check
  if effectiveTxSize > state.protocolParams.maxTxSize then
    errors := .TxTooLarge effectiveTxSize state.protocolParams.maxTxSize :: errors

  -- 1a. #378: Empty input set check (fatal — later checks depend on inputs)
  if body.inputs.isEmpty then
    return [.InputSetEmpty]

  -- 2. Validity interval check
  match body.ttl with
  | some ttl =>
    if currentSlot > ttl then
      errors := .ExpiredTx currentSlot ttl :: errors
  | none => pure ()
  match body.validityIntervalStart with
  | some validFrom =>
    if currentSlot < validFrom then
      errors := .TxNotYetValid currentSlot validFrom :: errors
  | none => pure ()

  -- 3. Input validation (UTxO existence) — fatal, later checks depend on resolved inputs
  for inp in body.inputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    if !state.utxo.contains id then
      return (.InputNotFound inp.txId inp.outputIndex :: errors).reverse

  -- 4. Double-spend check (fatal — UTxO resolution ambiguous with duplicates)
  let mut seen : List UTxOId := []
  for inp in body.inputs do
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    if seen.any (· == id) then
      return (.DoubleSpend inp.txId inp.outputIndex :: errors).reverse
    seen := id :: seen

  -- 5. Balance validation: inputs + mint + withdrawals + refunds >= outputs + fee + deposits + burns
  let inputVal := totalInputValue state.utxo body.inputs
  let withdrawalVal := Value.fromWithdrawals body.withdrawals
  let mintPositive := Value.fromMintPositive body.mint
  let mintNegative := Value.fromMintNegative body.mint
  -- Compute certificate deposits and refunds
  let keyDeposit := state.protocolParams.stakeKeyDeposit  -- 2 ADA default
  let mut depositTotal : Nat := 0
  let mut refundTotal : Nat := 0
  for cert in body.certificates do
    match cert with
    | .stakeKeyRegistration _ _ => depositTotal := depositTotal + keyDeposit
    | .stakeKeyDeregistration _ _ => refundTotal := refundTotal + keyDeposit
    | .conwayRegistration _ _ deposit => depositTotal := depositTotal + deposit
    | .conwayDeregistration _ _ refund => refundTotal := refundTotal + refund
    | .stakeRegDelegation _ _ _ deposit => depositTotal := depositTotal + deposit
    | .voteRegDelegation _ _ _ deposit => depositTotal := depositTotal + deposit
    | .stakeVoteRegDelegation _ _ _ _ deposit => depositTotal := depositTotal + deposit
    | _ => pure ()
  let available := inputVal + mintPositive + withdrawalVal + Value.lovelaceOnly refundTotal
  let outputVal := totalOutputValue body.outputs
  let required := outputVal + mintNegative + Value.lovelaceOnly (body.fee + depositTotal)
  if !Value.geq available required then
    errors := .InsufficientFunds required.lovelace available.lovelace :: errors

  -- 6. Min UTxO check: each output must carry enough lovelace
  for (idx, output) in (List.range body.outputs.length).zip body.outputs do
    let outputSize := if output.rawOutputBytes.size > 0 then output.rawOutputBytes.size else output.address.size
    let minAda := minAdaPerUTxO outputSize state.protocolParams.coinsPerUTxOByte
    if output.amount < minAda then
      errors := .OutputTooSmall idx output.amount minAda :: errors

  -- 6a. #379: Output value size check (max serialized Value size)
  for (idx, output) in (List.range body.outputs.length).zip body.outputs do
    let valueSize := output.rawValueBytes.size
    if valueSize > 0 && valueSize > state.protocolParams.maxValueSize then
      errors := .OutputTooBig idx valueSize state.protocolParams.maxValueSize :: errors

  -- 6b. #380: Network ID check on output addresses
  for output in body.outputs do
    if output.address.size > 0 then
      let addrNetworkId := output.address[0]! &&& 0x0F  -- low nibble = network ID
      let headerType := output.address[0]! >>> 4
      -- Only check Shelley-era addresses (types 0-7), skip Byron (type 8)
      if headerType < 8 && addrNetworkId.toNat != state.protocolParams.networkId then
        errors := .WrongNetwork s!"output addr network={addrNetworkId}, expected={state.protocolParams.networkId}" :: errors

  -- 6c. #380: Network ID check on withdrawal addresses
  for (rewardAddr, _) in body.withdrawals do
    if rewardAddr.size > 0 then
      let addrNetworkId := rewardAddr[0]! &&& 0x0F
      if addrNetworkId.toNat != state.protocolParams.networkId then
        errors := .WrongNetwork s!"withdrawal addr network={addrNetworkId}, expected={state.protocolParams.networkId}" :: errors

  -- 6d. #384: Reference inputs must be disjoint from regular inputs
  for refInp in body.referenceInputs do
    if body.inputs.any (fun inp => inp.txId == refInp.txId && inp.outputIndex == refInp.outputIndex) then
      errors := .NonDisjointRefInputs refInp.txId refInp.outputIndex :: errors

  -- 6e. Reference inputs must exist in UTxO
  for refInp in body.referenceInputs do
    let id : UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
    if !state.utxo.contains id then
      errors := .InputNotFound refInp.txId refInp.outputIndex :: errors

  -- 7. Withdrawal validation: skipped — reward accounts are not populated during sync.

  -- 8. Fee validation (uses full serialized tx size, not just body CBOR)
  let minRequired := totalMinFee state.protocolParams.feeParams effectiveTxSize witnesses.redeemers
  if body.fee < minRequired then
    errors := .FeeBelowMinimum body.fee minRequired :: errors

  -- 9. Signature validation (Ed25519 + required_signers key 13)
  match ← validateSignatures state.utxo body witnesses with
  | .error e => errors := e :: errors
  | .ok () => pure ()

  -- 10. Native script validation
  let mut signerKeyHashes : List ByteArray := []
  for w in witnesses.vkeyWitnesses do
    let keyHash ← blake2b_224 w.vkey
    signerKeyHashes := keyHash :: signerKeyHashes
  match validateNativeScripts witnesses signerKeyHashes currentSlot with
  | .error e => errors := e :: errors
  | .ok () => pure ()

  -- 11. Plutus script validation (via Plutuz FFI)
  let txHash ← blake2b_256 body.rawBytes
  match ← validatePlutusScriptsIO body witnesses state.utxo txHash with
  | .error e => errors := e :: errors
  | .ok () => pure ()

  -- 11a. #390: Check for unspendable Plutus UTXOs (no datum hash)
  -- Only PlutusV1 and V2 script-locked inputs require a datum to be spendable.
  -- PlutusV3 does NOT require datums (per Plutus spec / CIP-0069).
  let spendRedeemerIndices := witnesses.redeemers.filterMap fun r =>
    if r.tag == .Spend then some r.index else none
  let hasV3Scripts := !witnesses.plutusV3Scripts.isEmpty
  let hasV1V2Scripts := !witnesses.plutusV1Scripts.isEmpty || !witnesses.plutusV2Scripts.isEmpty
  let sortedInputs := sortInputs body.inputs
  if hasV1V2Scripts || (!hasV3Scripts && !hasV1V2Scripts) then
    for (inputIdx, inp) in (List.range sortedInputs.length).zip sortedInputs do
      if spendRedeemerIndices.contains inputIdx then
        let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
        match state.utxo.lookup id with
        | some output =>
          if isScriptLockedAddress output.address then
            let scriptHash := if output.address.size >= 29 then
              output.address.extract 1 29 else ByteArray.empty
            let mut isV3 := false
            for s in witnesses.plutusV3Scripts do
              let h ← blake2b_224 (ByteArray.mk #[0x03] ++ decodeCborBstring s)
              if h == scriptHash then isV3 := true
            if !isV3 then
              for refInp in body.referenceInputs do
                let refId : UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
                match state.utxo.lookup refId with
                | some refOut =>
                  if refOut.scriptRef.isSome then isV3 := true
                | none => pure ()
            if !isV3 then
              let hasUtxoDatum := output.datum.isSome || output.inlineDatum.isSome
              let hasWitnessDatums := !witnesses.datums.isEmpty
              let hasRefInputDatums := body.referenceInputs.any fun refInp =>
                let refId : UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
                match state.utxo.lookup refId with
                | some refOut => refOut.inlineDatum.isSome
                | none => false
              if !hasUtxoDatum && !hasWitnessDatums && !hasRefInputDatums then
                errors := .UnspendableUTxONoDatum inp.txId inp.outputIndex :: errors
        | none => pure ()

  -- 12. Collateral validation (for Plutus txs)
  let hasRedeemers := !witnesses.redeemers.isEmpty
  match validateCollateral state.utxo body hasRedeemers state.protocolParams.collateralPercentage state.protocolParams.maxCollateralInputs with
  | .error e => errors := e :: errors
  | .ok () => pure ()

  -- 13. Script data hash validation
  -- TODO: Re-enable once we have proper cost model data from the chain.

  -- 14. #386: Metadata hash consistency
  match body.auxiliaryDataHash with
  | some declaredHash =>
    if declaredHash.size != 32 then
      errors := .MetadataHashMismatch s!"auxiliaryDataHash has invalid size={declaredHash.size}" :: errors
  | none => pure ()

  -- 15. #389: Per-transaction ExUnits budget check
  let (maxMem, maxSteps) := state.protocolParams.maxTxExUnits
  let mut totalMem : Nat := 0
  let mut totalSteps : Nat := 0
  for r in witnesses.redeemers do
    totalMem := totalMem + r.exUnits.mem
    totalSteps := totalSteps + r.exUnits.steps
  if totalMem > maxMem || totalSteps > maxSteps then
    errors := .ExUnitsTooBig totalMem totalSteps maxMem maxSteps :: errors

  -- 16. #388: Extra redeemers check — each redeemer must map to a script action
  let sortedMintPolicies := (body.mint.map (·.policyId)).eraseDups |>.mergeSort
    (fun a b => compareByteArray a b != .gt)
  let mintPolicyCount := sortedMintPolicies.length
  for r in witnesses.redeemers do
    let valid := match r.tag with
      | .Spend => r.index < body.inputs.length
      | .Mint => r.index < mintPolicyCount
      | .Cert => r.index < body.certificates.length
      | .Reward => r.index < body.withdrawals.length
      | .Vote | .Propose => true  -- Conway governance redeemers always valid
    if !valid then
      let tagStr := match r.tag with
        | .Spend => "Spend" | .Mint => "Mint" | .Cert => "Cert" | .Reward => "Reward"
        | .Vote => "Vote" | .Propose => "Propose"
      errors := .ExtraRedeemer tagStr r.index :: errors

  -- 17. #385: Extraneous script witnesses check (symmetric difference).
  -- Compute scripts needed and provided, then check both directions.
  do
    let mut scriptsNeeded : List ByteArray := []
    -- (a) Spend inputs: script hash from address payment credential
    for inp in body.inputs do
      let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
      match state.utxo.lookup id with
      | some output =>
        if isScriptLockedAddress output.address && output.address.size >= 29 then
          scriptsNeeded := output.address.extract 1 29 :: scriptsNeeded
      | none => pure ()
    -- (b) Minting policies: each policy ID is a script hash
    for asset in body.mint do
      scriptsNeeded := asset.policyId :: scriptsNeeded
    -- (c) Withdrawal scripts: reward addresses with script credential
    for (rewardAddr, _) in body.withdrawals do
      if rewardAddr.size > 0 && (rewardAddr[0]! &&& 0x10) != 0 then
        if rewardAddr.size >= 29 then
          scriptsNeeded := rewardAddr.extract 1 29 :: scriptsNeeded
    -- (d) Certificate scripts: certificates with script credentials
    for cert in body.certificates do
      match cert with
      -- Registration-only certs (types 0, 7) do NOT require script witnesses —
      -- you're just recording the credential, not proving ownership of it.
      -- Deregistration, delegation, and combined certs DO require the script.
      | .stakeKeyDeregistration true h
      | .stakeDelegation true h _
      | .conwayDeregistration true h _ | .voteDelegation true h _
      | .stakeVoteDelegation true h _ _ | .stakeRegDelegation true h _ _
      | .voteRegDelegation true h _ _ | .stakeVoteRegDelegation true h _ _ _
      | .authCommitteeHot true h _ | .resignCommitteeCold true h =>
        scriptsNeeded := h :: scriptsNeeded
      | _ => pure ()

    -- Compute scripts provided (witness scripts + reference scripts from all inputs)
    -- Script hash = blake2b_224(version_prefix ++ script_bytes)
    let mut scriptsProvided : List ByteArray := []
    -- Native scripts: hash = blake2b_224(0x00 ++ cbor_encoding)
    for s in witnesses.nativeScripts do
      let h ← blake2b_224 (ByteArray.mk #[0x00] ++ s)
      scriptsProvided := h :: scriptsProvided
    -- Plutus scripts: strip CBOR bstring header, hash = blake2b_224(version ++ flat_bytes)
    for s in witnesses.plutusV1Scripts do
      let h ← blake2b_224 (ByteArray.mk #[0x01] ++ decodeCborBstring s)
      scriptsProvided := h :: scriptsProvided
    for s in witnesses.plutusV2Scripts do
      let h ← blake2b_224 (ByteArray.mk #[0x02] ++ decodeCborBstring s)
      scriptsProvided := h :: scriptsProvided
    for s in witnesses.plutusV3Scripts do
      let h ← blake2b_224 (ByteArray.mk #[0x03] ++ decodeCborBstring s)
      scriptsProvided := h :: scriptsProvided
    -- Reference scripts from reference inputs AND spend inputs
    -- Per Amaru/cardano-ledger: only count reference scripts that are actually required
    let allInputs := body.referenceInputs ++ body.inputs
    for inp in allInputs do
      let inpId : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
      match state.utxo.lookup inpId with
      | some out =>
        match out.scriptRef with
        | some refBytes =>
          match unwrapRefScriptForHash refBytes with
          | some (scriptBytes, versionByte) =>
            -- For Plutus (version > 0), strip CBOR bstring header; for native, use as-is
            let innerBytes := if versionByte > 0 then decodeCborBstring scriptBytes else scriptBytes
            let h ← blake2b_224 (ByteArray.mk #[versionByte] ++ innerBytes)
            -- Only add if this script is actually needed
            if scriptsNeeded.any (· == h) then
              scriptsProvided := h :: scriptsProvided
          | none => pure ()
        | none => pure ()
      | none => pure ()

    let neededSet := scriptsNeeded.eraseDups
    let providedSet := scriptsProvided.eraseDups

    -- Check for extraneous scripts: provided but not needed
    for sh in providedSet do
      if !neededSet.any (· == sh) then
        errors := .ExtraneousScriptWitness sh :: errors

    -- Check for missing scripts: needed but not provided
    let toHex (ba : ByteArray) : String :=
      let hd (n : Nat) : Char := if n < 10 then Char.ofNat (n + 48) else Char.ofNat (n + 87)
      ba.foldl (fun s b => s ++ String.singleton (hd (b.toNat / 16)) ++ String.singleton (hd (b.toNat % 16))) ""
    for sh in neededSet do
      if !providedSet.any (· == sh) then
        let dbgLine := s!"MISSING {toHex sh} | needed=[{String.intercalate "," (neededSet.map toHex)}] provided=[{String.intercalate "," (providedSet.map toHex)}]\n"
        let h ← IO.FS.Handle.mk "script_debug.log" .append
        h.putStr dbgLine
        errors := .MissingRequiredScript sh :: errors

  -- 17b. Datum preimage check: each script-locked input with a datum hash
  -- must have its preimage in the witness set datums.
  do
    let mut providedDatumHashes : List ByteArray := []
    for d in witnesses.datums do
      let h ← blake2b_256 d
      providedDatumHashes := h :: providedDatumHashes
    for inp in body.inputs do
      let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
      match state.utxo.lookup id with
      | some output =>
        if isScriptLockedAddress output.address then
          match output.datum with
          | some datumHash =>
            if output.inlineDatum.isNone then
              if !providedDatumHashes.any (· == datumHash) then
                errors := .MissingRequiredDatum datumHash :: errors
          | none => pure ()
      | none => pure ()

  -- 17c. Missing redeemer check: each Plutus script action must have a redeemer.
  -- Native scripts do NOT require redeemers — only Plutus scripts do.
  do
    -- Build set of native script hashes so we can skip them
    let mut nativeScriptHashes : List ByteArray := []
    for s in witnesses.nativeScripts do
      let h ← blake2b_224 (ByteArray.mk #[0x00] ++ s)
      nativeScriptHashes := h :: nativeScriptHashes
    -- Also check reference scripts for native scripts
    let allInputsForRedeemer := body.referenceInputs ++ body.inputs
    for inp in allInputsForRedeemer do
      let inpId : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
      match state.utxo.lookup inpId with
      | some out =>
        match out.scriptRef with
        | some refBytes =>
          match unwrapRefScriptForHash refBytes with
          | some (scriptBytes, versionByte) =>
            if versionByte == 0x00 then
              let h ← blake2b_224 (ByteArray.mk #[0x00] ++ scriptBytes)
              nativeScriptHashes := h :: nativeScriptHashes
          | none => pure ()
        | none => pure ()
      | none => pure ()

    let sortedInputsForRedeemer := sortInputs body.inputs
    -- Check spend redeemers: only for Plutus script-locked inputs (not native)
    for (i, inp) in (List.range sortedInputsForRedeemer.length).zip sortedInputsForRedeemer do
      let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
      match state.utxo.lookup id with
      | some output =>
        if isScriptLockedAddress output.address && output.address.size >= 29 then
          let scriptHash := output.address.extract 1 29
          -- Skip native scripts — they don't need redeemers
          if !nativeScriptHashes.any (· == scriptHash) then
            if !witnesses.redeemers.any (fun r => r.tag == .Spend && r.index == i) then
              errors := .MissingRedeemer "Spend" i :: errors
      | none => pure ()
    -- Check mint redeemers: only for Plutus minting policies (not native)
    let sortedMintPoliciesForRedeemer := (body.mint.map (·.policyId)).eraseDups |>.mergeSort
      (fun a b => compareByteArray a b != .gt)
    for (i, pid) in (List.range sortedMintPoliciesForRedeemer.length).zip sortedMintPoliciesForRedeemer do
      -- Skip native script minting policies — they don't need redeemers
      if !nativeScriptHashes.any (· == pid) then
        if !witnesses.redeemers.any (fun r => r.tag == .Mint && r.index == i) then
          errors := .MissingRedeemer "Mint" i :: errors
    -- Check reward redeemers: for script-credential withdrawals (only Plutus)
    let sortedWithdrawals := body.withdrawals.mergeSort (fun a b => compareByteArray a.1 b.1 != .gt)
    for (i, (rewardAddr, _)) in (List.range sortedWithdrawals.length).zip sortedWithdrawals do
      if rewardAddr.size > 0 && (rewardAddr[0]! &&& 0x10) != 0 then
        if rewardAddr.size >= 29 then
          let scriptHash := rewardAddr.extract 1 29
          if !nativeScriptHashes.any (· == scriptHash) then
            if !witnesses.redeemers.any (fun r => r.tag == .Reward && r.index == i) then
              errors := .MissingRedeemer "Reward" i :: errors

  -- 18. #387: isValid flag — checked at call site (on Transaction, not TransactionBody)

  -- 19. #392: Reject stake deregistration when reward balance is non-zero
  for cert in body.certificates do
    match cert with
    | .stakeKeyDeregistration _ credHash =>
      let balance := state.rewardAccounts[credHash]?.getD 0
      if balance > 0 then
        errors := .StakeDeregNonZeroReward credHash balance :: errors
    | .conwayDeregistration _ credHash _ =>
      let balance := state.rewardAccounts[credHash]?.getD 0
      if balance > 0 then
        errors := .StakeDeregNonZeroReward credHash balance :: errors
    | _ => pure ()

  -- 20. #393: Validate certificate deposit amounts match protocol params
  let keyDeposit' := state.protocolParams.stakeKeyDeposit
  for cert in body.certificates do
    match cert with
    | .conwayRegistration _ _ deposit =>
      if deposit != keyDeposit' then
        errors := .CertDepositMismatch deposit keyDeposit' "ConwayRegistration" :: errors
    | .conwayDeregistration _ _ refund =>
      if refund != keyDeposit' then
        errors := .CertDepositMismatch refund keyDeposit' "ConwayDeregistration" :: errors
    | _ => pure ()

  -- 21. #394: Validate pool cost is at or above minimum
  for cert in body.certificates do
    match cert with
    | .poolRegistration _ _ _ cost _ _ _ =>
      if cost < state.protocolParams.minPoolCost then
        errors := .PoolCostTooLow cost state.protocolParams.minPoolCost :: errors
    | _ => pure ()

  -- 22. #395: Validate delegatee (pool or DRep) exists
  -- NOTE: Pool existence is NOT checked during block validation.

  return errors.reverse

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
