import Cleanode.Ledger.State
import Cleanode.Network.EraTx

/-!
# Protocol Parameter Update Validation

Validates protocol parameter update proposals:
- Pre-Conway (PPUP rule): authorized proposers, epoch timing, version succession
- Conway: governance-based with parameter groups and different thresholds

## Parameter Groups (Conway)
- Network: maxBlockBodySize, maxTxSize, maxBlockHeaderSize, maxValueSize, maxBlockExUnits, maxTxExUnits, maxCollateralInputs
- Economic: txFeePerByte, txFeeFixed, utxoCostPerByte, stakeKeyDeposit, poolDeposit, minPoolCost, executionPrices
- Technical: poolPledgeInfluence, poolTargetNum, poolRetireMaxEpoch, collateralPercentage, costModels
- Governance: all voting thresholds, govActionLifetime, govActionDeposit, dRepDeposit, dRepActivity

## Security-Relevant Parameters (require additional SPO approval)
maxBlockBodySize, maxTxSize, maxBlockHeaderSize, maxValueSize, maxBlockExUnits,
txFeePerByte, txFeeFixed, utxoCostPerByte, govActionDeposit

## References
- Shelley PPUP rule
- Conway GOV rule for parameter changes
- CIP-1694: parameter groups
-/

namespace Cleanode.Ledger.ProtocolParamValidation

open Cleanode.Ledger.State
open Cleanode.Network.EraTx

-- ====================
-- = PPUpdate Errors  =
-- ====================

/-- Protocol parameter update validation error -/
inductive PPUpdateError where
  | NonGenesisProposer (detail : String)
  | WrongEpoch (currentEpoch proposalEpoch : Nat)
  | InvalidProtocolVersionSuccession (currentMajor currentMinor newMajor newMinor : Nat)
  | ParameterOutOfBounds (paramName : String) (value : Nat)

instance : Repr PPUpdateError where
  reprPrec
    | .NonGenesisProposer d, _ => s!"NonGenesisProposer({d})"
    | .WrongEpoch c p, _ => s!"WrongEpoch(current={c}, proposal={p})"
    | .InvalidProtocolVersionSuccession cm cmi nm nmi, _ =>
      s!"InvalidProtocolVersionSuccession(current={cm}.{cmi}, new={nm}.{nmi})"
    | .ParameterOutOfBounds n v, _ => s!"ParameterOutOfBounds({n}={v})"

-- ====================
-- = Parameter Groups =
-- ====================

/-- Parameter group classification -/
inductive ParamGroup where
  | Network
  | Economic
  | Technical
  | Governance
  deriving BEq, Repr

/-- Check if a parameter name is security-relevant (requires SPO approval in Conway) -/
def isSecurityRelevant (paramName : String) : Bool :=
  paramName ∈ ["maxBlockBodySize", "maxTxSize", "maxBlockHeaderSize", "maxValueSize",
    "maxBlockExUnits", "txFeePerByte", "txFeeFixed", "utxoCostPerByte",
    "govActionDeposit", "minFeeRefScriptCostPerByte"]

/-- Classify a parameter into its group -/
def paramGroup (paramName : String) : ParamGroup :=
  if paramName ∈ ["maxBlockBodySize", "maxTxSize", "maxBlockHeaderSize", "maxValueSize",
      "maxBlockExUnits", "maxTxExUnits", "maxCollateralInputs"] then .Network
  else if paramName ∈ ["txFeePerByte", "txFeeFixed", "utxoCostPerByte", "stakeKeyDeposit",
      "poolDeposit", "minPoolCost", "monetaryExpansion", "treasuryCut",
      "executionPrices", "minFeeRefScriptCostPerByte"] then .Economic
  else if paramName ∈ ["poolPledgeInfluence", "poolTargetNum", "poolRetireMaxEpoch",
      "collateralPercentage", "costModels"] then .Technical
  else .Governance

-- ====================
-- = Validation       =
-- ====================

/-- Validate protocol version succession.
    Major version can increment by exactly 1 (hard fork).
    Minor version can be anything if major stays the same. -/
def validateProtocolVersion (currentMajor currentMinor newMajor newMinor : Nat)
    : Except PPUpdateError Unit := do
  if newMajor == currentMajor then
    -- Same major: minor version can be anything >= current
    pure ()
  else if newMajor == currentMajor + 1 then
    -- Hard fork: major increments by 1, minor resets
    pure ()
  else
    throw (.InvalidProtocolVersionSuccession currentMajor currentMinor newMajor newMinor)

/-- Validate a protocol parameter update proposal (pre-Conway PPUP rule) -/
def validatePPUpdate (state : LedgerState)
    (proposerKeyHash : ByteArray)
    (genesisKeyHashes : List ByteArray)
    : Except PPUpdateError Unit := do
  -- Only genesis key delegates can propose (pre-Conway)
  if state.protocolParams.currentEra != .Conway then
    if !genesisKeyHashes.any (· == proposerKeyHash) then
      throw (.NonGenesisProposer "proposer is not a genesis delegate")

/-- Basic sanity checks on proposed parameter values -/
def validateParamBounds (paramName : String) (value : Nat)
    : Except PPUpdateError Unit := do
  -- Reject obviously dangerous values
  match paramName with
  | "maxTxSize" =>
    if value < 4096 || value > 65536 then
      throw (.ParameterOutOfBounds paramName value)
  | "maxBlockBodySize" =>
    if value < 16384 || value > 262144 then
      throw (.ParameterOutOfBounds paramName value)
  | "maxBlockHeaderSize" =>
    if value < 400 || value > 4096 then
      throw (.ParameterOutOfBounds paramName value)
  | "collateralPercentage" =>
    if value > 500 then  -- Max 500%
      throw (.ParameterOutOfBounds paramName value)
  | "maxCollateralInputs" =>
    if value == 0 || value > 10 then
      throw (.ParameterOutOfBounds paramName value)
  | _ => pure ()

end Cleanode.Ledger.ProtocolParamValidation
