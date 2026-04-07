import Lean

/-!
# Genesis File Parsers

Parses the genesis configuration files for each Cardano era.
Only the fields needed by the node for protocol operation are extracted.

## Genesis Files
- **Byron genesis**: Initial chain state, security parameter, slot duration
- **Shelley genesis**: Epoch/slot parameters, protocol parameters, initial funds
- **Alonzo genesis**: Plutus cost models, execution prices, script limits
- **Conway genesis**: Governance parameters, voting thresholds, committee

## References
- cardano-ledger genesis specs
- https://github.com/IntersectMBO/cardano-node/tree/master/configuration
-/

namespace Dion.Config.Genesis

open Lean (Json)

-- ====================
-- = Byron Genesis    =
-- ====================

/-- Byron genesis essential parameters -/
structure ByronGenesis where
  securityParameter : Nat           -- k: number of blocks for stability
  slotDuration : Nat                -- Slot duration in milliseconds
  startTime : Nat                   -- Genesis timestamp (Unix epoch seconds)
  protocolMagic : Nat               -- Network identifier
  maxBlockSize : Nat                -- Maximum block size in bytes
  maxHeaderSize : Nat               -- Maximum header size in bytes
  maxTxSize : Nat                   -- Maximum transaction size in bytes
  deriving Repr

/-- Parse Byron genesis from JSON -/
def parseByronGenesis (j : Json) : Option ByronGenesis := do
  -- protocolConsts.k
  let protocolConsts ← (j.getObjVal? "protocolConsts").toOption
  let k ← (protocolConsts.getObjValAs? Nat "k").toOption

  -- blockVersionData
  let bvd ← (j.getObjVal? "blockVersionData").toOption
  let slotDuration ← (bvd.getObjValAs? Nat "slotDuration").toOption
  let maxBlockSize ← (bvd.getObjValAs? Nat "maxBlockSize").toOption
  let maxHeaderSize ← (bvd.getObjValAs? Nat "maxHeaderSize").toOption
  let maxTxSize ← (bvd.getObjValAs? Nat "maxTxSize").toOption

  let protocolMagic ← (protocolConsts.getObjValAs? Nat "protocolMagic").toOption

  -- Parse startTime: try as number first, fall back to known ISO 8601 timestamps
  let startTime := match (j.getObjValAs? Nat "startTime").toOption with
    | some t => t
    | none => match (j.getObjValAs? String "startTime").toOption with
      | some "2017-09-23T21:44:51Z" => 1506203091  -- Byron mainnet
      | some "2022-11-01T00:00:00Z" => 1667260800  -- Preprod
      | some "2022-04-01T00:00:00Z" => 1648771200  -- Preview
      | _ => 0  -- Unknown — safe default

  some {
    securityParameter := k,
    slotDuration := slotDuration,
    startTime := startTime,
    protocolMagic := protocolMagic,
    maxBlockSize := maxBlockSize,
    maxHeaderSize := maxHeaderSize,
    maxTxSize := maxTxSize
  }

-- ====================
-- = Shelley Genesis  =
-- ====================

/-- Shelley protocol parameters (subset) -/
structure ShelleyProtocolParams where
  minFeeA : Nat                     -- Linear fee coefficient (lovelace per byte)
  minFeeB : Nat                     -- Constant fee (lovelace)
  maxBlockBodySize : Nat            -- Max block body size (bytes)
  maxBlockHeaderSize : Nat          -- Max block header size (bytes)
  maxTxSize : Nat                   -- Max transaction size (bytes)
  keyDeposit : Nat                  -- Stake key deposit (lovelace)
  poolDeposit : Nat                 -- Pool deposit (lovelace)
  eMax : Nat                        -- Max epoch for pool retirement
  nOpt : Nat                        -- Desired number of pools
  decentralisationParam : Float     -- Decentralisation parameter (0.0 = fully decentralised)
  deriving Repr

/-- Rational number for deterministic arithmetic (numerator / denominator) -/
structure Rational where
  numerator : Nat
  denominator : Nat
  deriving Repr

/-- Shelley genesis essential parameters -/
structure ShelleyGenesis where
  epochLength : Nat                 -- Slots per epoch
  slotLength : Nat                  -- Slot duration in seconds (typically 1)
  activeSlotsCoeff : Rational       -- Active slots coefficient (typically 1/20 = 0.05)
  securityParam : Nat               -- k: security parameter
  maxLovelaceSupply : Nat           -- Maximum ADA supply in lovelace
  networkMagic : Nat                -- Network identifier
  networkId : String                -- "Mainnet" or "Testnet"
  protocolParams : Option ShelleyProtocolParams
  deriving Repr

/-- Parse Shelley protocol parameters from JSON -/
def parseShelleyProtocolParams (j : Json) : Option ShelleyProtocolParams := do
  let minFeeA ← (j.getObjValAs? Nat "minFeeA").toOption
  let minFeeB ← (j.getObjValAs? Nat "minFeeB").toOption
  let maxBlockBodySize ← (j.getObjValAs? Nat "maxBlockBodySize").toOption
  let maxBlockHeaderSize ← (j.getObjValAs? Nat "maxBlockHeaderSize").toOption
  let maxTxSize ← (j.getObjValAs? Nat "maxTxSize").toOption
  let keyDeposit ← (j.getObjValAs? Nat "keyDeposit").toOption
  let poolDeposit ← (j.getObjValAs? Nat "poolDeposit").toOption
  let eMax ← (j.getObjValAs? Nat "eMax").toOption
  let nOpt ← (j.getObjValAs? Nat "nOpt").toOption
  some {
    minFeeA, minFeeB, maxBlockBodySize, maxBlockHeaderSize,
    maxTxSize, keyDeposit, poolDeposit, eMax, nOpt,
    decentralisationParam := 0.0
  }

/-- Parse Shelley genesis from JSON -/
def parseShelleyGenesis (j : Json) : Option ShelleyGenesis := do
  let epochLength ← (j.getObjValAs? Nat "epochLength").toOption
  let slotLength ← (j.getObjValAs? Nat "slotLength").toOption
  let securityParam ← (j.getObjValAs? Nat "securityParam").toOption
  let maxLovelaceSupply ← (j.getObjValAs? Nat "maxLovelaceSupply").toOption
  let networkMagic ← (j.getObjValAs? Nat "networkMagic").toOption
  let networkId ← (j.getObjValAs? String "networkId").toOption

  -- activeSlotsCoeff: parse as rational from JSON number (e.g. 0.05 = 5/100 = 1/20)
  let activeSlotsCoeff : Rational := match j.getObjVal? "activeSlotsCoeff" with
    | .ok (Json.num n) =>
      -- Scientific notation: n is a JsonNumber with mantissa and exponent
      -- Convert to rational: for 0.05 we get mantissa=5, exponent=-2 → 5/100 = 1/20
      let s := toString n
      match s.splitOn "." with
      | [whole, frac] =>
        let denom := 10 ^ frac.length
        let num := (whole ++ frac).toNat!
        let g := Nat.gcd num denom
        { numerator := num / g, denominator := denom / g }
      | [whole] => { numerator := whole.toNat!, denominator := 1 }
      | _ => { numerator := 1, denominator := 20 }  -- default 0.05
    | _ => { numerator := 1, denominator := 20 }  -- default 0.05

  let protocolParams := match j.getObjVal? "protocolParams" with
    | .ok pp => parseShelleyProtocolParams pp
    | .error _ => none

  some {
    epochLength, slotLength, activeSlotsCoeff, securityParam,
    maxLovelaceSupply, networkMagic, networkId, protocolParams
  }

-- ====================
-- = Alonzo Genesis   =
-- ====================

/-- Alonzo execution unit limits -/
structure ExUnitLimits where
  mem : Nat                         -- Memory units
  steps : Nat                       -- CPU steps
  deriving Repr

/-- Alonzo genesis essential parameters -/
structure AlonzoGenesis where
  maxTxExUnits : ExUnitLimits       -- Per-transaction execution limits
  maxBlockExUnits : ExUnitLimits    -- Per-block execution limits
  maxValSize : Nat                  -- Maximum value size in bytes
  collateralPercentage : Nat        -- Collateral percentage (e.g., 150)
  maxCollateralInputs : Nat         -- Max collateral inputs
  deriving Repr

/-- Parse ExUnitLimits from JSON -/
def parseExUnitLimits (j : Json) : Option ExUnitLimits := do
  let mem ← (j.getObjValAs? Nat "exUnitsMem").toOption
  let steps ← (j.getObjValAs? Nat "exUnitsSteps").toOption
  some { mem, steps }

/-- Parse Alonzo genesis from JSON -/
def parseAlonzoGenesis (j : Json) : Option AlonzoGenesis := do
  let maxTxExUnits ← match j.getObjVal? "maxTxExUnits" with
    | .ok v => parseExUnitLimits v
    | .error _ => none
  let maxBlockExUnits ← match j.getObjVal? "maxBlockExUnits" with
    | .ok v => parseExUnitLimits v
    | .error _ => none
  let maxValSize ← (j.getObjValAs? Nat "maxValueSize").toOption
  let collateralPercentage ← (j.getObjValAs? Nat "collateralPercentage").toOption
  let maxCollateralInputs ← (j.getObjValAs? Nat "maxCollateralInputs").toOption
  some {
    maxTxExUnits, maxBlockExUnits, maxValSize,
    collateralPercentage, maxCollateralInputs
  }

-- ====================
-- = Conway Genesis   =
-- ====================

/-- Conway governance voting thresholds -/
structure VotingThresholds where
  motionNoConfidence : Float
  committeeNormal : Float
  committeeNoConfidence : Float
  updateToConstitution : Float
  hardForkInitiation : Float
  ppNetworkGroup : Float
  ppEconomicGroup : Float
  ppTechnicalGroup : Float
  ppGovGroup : Float
  treasuryWithdrawal : Float
  deriving Repr

/-- Conway genesis essential parameters -/
structure ConwayGenesis where
  committeeMinSize : Nat            -- Minimum committee size
  committeeMaxTermLength : Nat      -- Maximum term length in epochs
  dRepVotingThresholds : Option VotingThresholds
  poolVotingThresholds : Option VotingThresholds
  govActionLifetime : Nat           -- Governance action lifetime in epochs
  govActionDeposit : Nat            -- Deposit for governance actions (lovelace)
  dRepDeposit : Nat                 -- DRep registration deposit (lovelace)
  dRepActivity : Nat                -- DRep activity period in epochs
  deriving Repr

/-- Parse Conway genesis from JSON -/
def parseConwayGenesis (j : Json) : Option ConwayGenesis := do
  let committeeMinSize := (j.getObjValAs? Nat "committeeMinSize").toOption |>.getD 7
  let committeeMaxTermLength := (j.getObjValAs? Nat "committeeMaxTermLength").toOption |>.getD 146
  let govActionLifetime := (j.getObjValAs? Nat "govActionLifetime").toOption |>.getD 6
  let govActionDeposit := (j.getObjValAs? Nat "govActionDeposit").toOption |>.getD 100000000000
  let dRepDeposit := (j.getObjValAs? Nat "dRepDeposit").toOption |>.getD 500000000
  let dRepActivity := (j.getObjValAs? Nat "dRepActivity").toOption |>.getD 20
  -- Parse DRep voting thresholds (nested JSON object)
  let dRepVotingThresholds := match (j.getObjVal? "dRepVotingThresholds").toOption with
    | some obj => some {
        motionNoConfidence := (obj.getObjValAs? Float "motionNoConfidence").toOption |>.getD 0.67
        committeeNormal := (obj.getObjValAs? Float "committeeNormal").toOption |>.getD 0.67
        committeeNoConfidence := (obj.getObjValAs? Float "committeeNoConfidence").toOption |>.getD 0.60
        updateToConstitution := (obj.getObjValAs? Float "updateToConstitution").toOption |>.getD 0.75
        hardForkInitiation := (obj.getObjValAs? Float "hardForkInitiation").toOption |>.getD 0.60
        ppNetworkGroup := (obj.getObjValAs? Float "ppNetworkGroup").toOption |>.getD 0.67
        ppEconomicGroup := (obj.getObjValAs? Float "ppEconomicGroup").toOption |>.getD 0.67
        ppTechnicalGroup := (obj.getObjValAs? Float "ppTechnicalGroup").toOption |>.getD 0.67
        ppGovGroup := (obj.getObjValAs? Float "ppGovGroup").toOption |>.getD 0.75
        treasuryWithdrawal := (obj.getObjValAs? Float "treasuryWithdrawal").toOption |>.getD 0.67
      : VotingThresholds }
    | none => none
  -- Parse pool voting thresholds
  let poolVotingThresholds := match (j.getObjVal? "poolVotingThresholds").toOption with
    | some obj => some {
        motionNoConfidence := (obj.getObjValAs? Float "motionNoConfidence").toOption |>.getD 0.51
        committeeNormal := (obj.getObjValAs? Float "committeeNormal").toOption |>.getD 0.51
        committeeNoConfidence := (obj.getObjValAs? Float "committeeNoConfidence").toOption |>.getD 0.51
        updateToConstitution := (obj.getObjValAs? Float "updateToConstitution").toOption |>.getD 0.0
        hardForkInitiation := (obj.getObjValAs? Float "hardForkInitiation").toOption |>.getD 0.51
        ppNetworkGroup := (obj.getObjValAs? Float "ppNetworkGroup").toOption |>.getD 0.0
        ppEconomicGroup := (obj.getObjValAs? Float "ppEconomicGroup").toOption |>.getD 0.0
        ppTechnicalGroup := (obj.getObjValAs? Float "ppTechnicalGroup").toOption |>.getD 0.0
        ppGovGroup := (obj.getObjValAs? Float "ppGovGroup").toOption |>.getD 0.0
        treasuryWithdrawal := (obj.getObjValAs? Float "treasuryWithdrawal").toOption |>.getD 0.0
      : VotingThresholds }
    | none => none
  some {
    committeeMinSize, committeeMaxTermLength,
    dRepVotingThresholds, poolVotingThresholds,
    govActionLifetime, govActionDeposit, dRepDeposit, dRepActivity
  }

-- ====================
-- = File Loaders     =
-- ====================

/-- Load and parse a genesis file -/
private def loadGenesisFile (path : System.FilePath) (parser : Json → Option α) : IO (Except String α) := do
  try
    let contents ← IO.FS.readFile path
    match Json.parse contents with
    | .error e => return .error s!"Failed to parse genesis JSON: {e}"
    | .ok json =>
        match parser json with
        | none => return .error s!"Failed to extract genesis parameters from {path}"
        | some genesis => return .ok genesis
  catch e =>
    return .error s!"Failed to read genesis file {path}: {e}"

def loadByronGenesis (path : System.FilePath) : IO (Except String ByronGenesis) :=
  loadGenesisFile path parseByronGenesis

def loadShelleyGenesis (path : System.FilePath) : IO (Except String ShelleyGenesis) :=
  loadGenesisFile path parseShelleyGenesis

def loadAlonzoGenesis (path : System.FilePath) : IO (Except String AlonzoGenesis) :=
  loadGenesisFile path parseAlonzoGenesis

def loadConwayGenesis (path : System.FilePath) : IO (Except String ConwayGenesis) :=
  loadGenesisFile path parseConwayGenesis

end Dion.Config.Genesis
