import Cleanode.Ledger.Validation
import Cleanode.Ledger.State
import Cleanode.Network.ConwayBlock
import Cleanode.Network.Crypto

/-!
# Block Body Validation (BBODY Rules)

Validates block-level constraints beyond individual transaction validation:
- Block body hash matches header declaration (#374)
- Total execution units within block limits (#375)
- Reference script size within limits (#376)
- Protocol version validity (#377)

## References
- Shelley BBODY rule: WrongBlockBodySizeBBODY, InvalidBodyHashBBODY
- Alonzo BBODY rule: TooManyExUnits
- Conway BBODY rule: BodyRefScriptsSizeTooBig, HeaderProtVerTooHigh
-/

namespace Cleanode.Ledger.BlockValidation

open Cleanode.Ledger.Validation
open Cleanode.Ledger.State
open Cleanode.Ledger.UTxO
open Cleanode.Network.ConwayBlock
open Cleanode.Network.Crypto

-- ====================
-- = Block Errors     =
-- ====================

/-- Block-level validation error -/
inductive BlockError where
  | WrongBlockBodySize (declared actual : Nat)
  | InvalidBodyHash (detail : String)
  | TooManyExUnits (totalMem totalSteps maxMem maxSteps : Nat)
  | RefScriptsSizeTooBig (totalSize maxSize : Nat)
  | HeaderProtVerTooHigh (version maxVersion : Nat)
  | TxValidationFailed (txIndex : Nat) (err : ValidationError)

instance : Repr BlockError where
  reprPrec
    | .WrongBlockBodySize d a, _ => s!"WrongBlockBodySize(declared={d}, actual={a})"
    | .InvalidBodyHash d, _ => s!"InvalidBodyHash({d})"
    | .TooManyExUnits m s mm ms, _ => s!"TooManyExUnits(mem={m}, steps={s}, maxMem={mm}, maxSteps={ms})"
    | .RefScriptsSizeTooBig s m, _ => s!"RefScriptsSizeTooBig(size={s}, max={m})"
    | .HeaderProtVerTooHigh v m, _ => s!"HeaderProtVerTooHigh(version={v}, max={m})"
    | .TxValidationFailed i e, _ => s!"TxValidationFailed(tx={i}, {repr e})"

-- ====================
-- = Validation       =
-- ====================

/-- #374: Validate block body size matches header declaration -/
def validateBlockBodySize (declaredSize : Nat) (actualBodyBytes : ByteArray)
    : Except BlockError Unit := do
  if declaredSize != actualBodyBytes.size then
    throw (.WrongBlockBodySize declaredSize actualBodyBytes.size)

/-- #374: Validate block body hash matches header declaration -/
def validateBlockBodyHash (declaredHash : ByteArray) (actualBodyBytes : ByteArray)
    : IO (Except BlockError Unit) := do
  let computedHash ← blake2b_256 actualBodyBytes
  if computedHash != declaredHash then
    return .error (.InvalidBodyHash s!"declared={declaredHash.size}B computed={computedHash.size}B")
  return .ok ()

/-- #375: Validate total block execution units don't exceed limits -/
def validateBlockExUnits (txWitnesses : List WitnessSet) (params : ProtocolParamsState)
    : Except BlockError Unit := do
  let (maxMem, maxSteps) := params.maxBlockExUnits
  let mut totalMem : Nat := 0
  let mut totalSteps : Nat := 0
  for ws in txWitnesses do
    for r in ws.redeemers do
      totalMem := totalMem + r.exUnits.mem
      totalSteps := totalSteps + r.exUnits.steps
  if totalMem > maxMem || totalSteps > maxSteps then
    throw (.TooManyExUnits totalMem totalSteps maxMem maxSteps)

/-- #376: Validate total reference script size in a block (Conway) -/
def validateBlockRefScriptsSize (txBodies : List TransactionBody) (utxo : UTxOSet)
    (maxRefScriptSize : Nat := 204800)  -- 200KB default
    : Except BlockError Unit := do
  let mut totalSize : Nat := 0
  for body in txBodies do
    for refInp in body.referenceInputs do
      let id : UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
      match utxo.lookup id with
      | some output =>
        match output.scriptRef with
        | some scriptRef => totalSize := totalSize + scriptRef.size
        | none => pure ()
      | none => pure ()
  if totalSize > maxRefScriptSize then
    throw (.RefScriptsSizeTooBig totalSize maxRefScriptSize)

/-- #377: Validate protocol version in block header is not too high -/
def validateProtocolVersion (headerProtVer : Nat) (maxProtVer : Nat := 10)
    : Except BlockError Unit := do
  if headerProtVer > maxProtVer then
    throw (.HeaderProtVerTooHigh headerProtVer maxProtVer)

/-- Full block body validation: combines all BBODY checks -/
def validateBlockBody
    (declaredBodySize : Nat)
    (declaredBodyHash : ByteArray)
    (actualBodyBytes : ByteArray)
    (txBodies : List TransactionBody)
    (txWitnesses : List WitnessSet)
    (utxo : UTxOSet)
    (params : ProtocolParamsState)
    (headerProtVer : Nat := 10)
    : IO (Except BlockError Unit) := do
  -- #374: Body size
  match validateBlockBodySize declaredBodySize actualBodyBytes with
  | .error e => return .error e
  | .ok () => pure ()
  -- #374: Body hash
  match ← validateBlockBodyHash declaredBodyHash actualBodyBytes with
  | .error e => return .error e
  | .ok () => pure ()
  -- #375: Total ExUnits
  match validateBlockExUnits txWitnesses params with
  | .error e => return .error e
  | .ok () => pure ()
  -- #376: Reference scripts size
  match validateBlockRefScriptsSize txBodies utxo with
  | .error e => return .error e
  | .ok () => pure ()
  -- #377: Protocol version
  match validateProtocolVersion headerProtVer with
  | .error e => return .error e
  | .ok () => pure ()
  return .ok ()

end Cleanode.Ledger.BlockValidation
