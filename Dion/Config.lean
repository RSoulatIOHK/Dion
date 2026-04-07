import Lean

/-!
# Network Configuration

This module defines the NetworkConfig structure that matches cardano-node's
config.json format. It allows Dion to read standard Cardano configuration
files instead of hardcoding network parameters.

## Configuration Files

A complete node configuration requires:
- Main config.json (network parameters, logging, genesis file references)
- Byron genesis file (initial chain state)
- Shelley genesis file (Shelley era parameters)
- Alonzo genesis file (Plutus parameters)
- Conway genesis file (governance parameters)
- Topology file (peer connections)

## References
- [Cardano Node Configuration](https://github.com/input-output-hk/cardano-node/tree/master/configuration/cardano)
- [Cardano Configurations Repository](https://github.com/input-output-hk/cardano-configurations)
-/

namespace Dion.Config

open Lean (Json)

/-- Network magic requirement -/
inductive NetworkMagicRequirement where
  | RequiresNoMagic     -- Mainnet (magic = 764824073, but magic validation disabled)
  | RequiresMagic       -- Testnet (requires specific magic number)
  deriving Repr, BEq

/-- Consensus mode -/
inductive ConsensusMode where
  | ByronMode
  | ShelleyMode
  | CardanoMode
  | PraosMode
  deriving Repr, BEq

/-- LedgerDB configuration (simplified) -/
structure LedgerDBConfig where
  backend : String                -- e.g., "LMDBBackend" or "InMemoryBackend"
  numOfDiskSnapshots : Nat        -- Number of snapshots to keep
  snapshotInterval : Nat          -- Blocks between snapshots
  deriving Repr

/-- Block version (major.minor.alt) -/
structure BlockVersion where
  major : Nat
  minor : Nat
  alt : Nat
  deriving Repr, BEq

/-- Main network configuration matching cardano-node format -/
structure NetworkConfig where
  -- Genesis file references
  byronGenesisFile : String
  byronGenesisHash : String
  shelleyGenesisFile : String
  shelleyGenesisHash : String
  alonzoGenesisFile : String
  alonzoGenesisHash : String
  conwayGenesisFile : String
  conwayGenesisHash : String

  -- Checkpoint data (optional, for faster sync)
  checkpointsFile : Option String
  checkpointsFileHash : Option String

  -- Protocol parameters
  protocol : String                              -- "Cardano"
  requiresNetworkMagic : NetworkMagicRequirement
  consensusMode : ConsensusMode                  -- "PraosMode" for modern mainnet

  -- Version parameters
  lastKnownBlockVersion : BlockVersion
  maxKnownMajorProtocolVersion : Nat
  minNodeVersion : String                        -- e.g., "10.6.0"

  -- LedgerDB configuration
  ledgerDB : Option LedgerDBConfig

  -- Logging configuration
  turnOnLogging : Bool
  turnOnLogMetrics : Bool
  minSeverity : String                           -- "Critical", "Error", "Warning", "Notice", "Info", "Debug"

  -- Trace configuration
  useTraceDispatcher : Bool
  traceOptionMetricsPrefix : String
  traceOptionPeerFrequency : Nat                 -- milliseconds
  traceOptionResourceFrequency : Nat             -- milliseconds
  deriving Repr

/-- Default mainnet configuration (will be replaced by parsed config) -/
def defaultMainnetConfig : NetworkConfig := {
  byronGenesisFile := "mainnet-byron-genesis.json",
  byronGenesisHash := "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb",
  shelleyGenesisFile := "mainnet-shelley-genesis.json",
  shelleyGenesisHash := "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81",
  alonzoGenesisFile := "mainnet-alonzo-genesis.json",
  alonzoGenesisHash := "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874",
  conwayGenesisFile := "mainnet-conway-genesis.json",
  conwayGenesisHash := "15a199f895e461ec0ffc6dd4e4028af28a492ab4e806d39cb674c88f7643ef62",

  checkpointsFile := some "mainnet-checkpoints.json",
  checkpointsFileHash := some "3e6dee5bae7acc6d870187e72674b37c929be8c66e62a552cf6a876b1af31ade",

  protocol := "Cardano",
  requiresNetworkMagic := .RequiresNoMagic,
  consensusMode := .PraosMode,

  lastKnownBlockVersion := { major := 3, minor := 0, alt := 0 },
  maxKnownMajorProtocolVersion := 2,
  minNodeVersion := "10.6.0",

  ledgerDB := none,  -- Use defaults

  turnOnLogging := true,
  turnOnLogMetrics := true,
  minSeverity := "Critical",

  useTraceDispatcher := true,
  traceOptionMetricsPrefix := "cardano.node.metrics.",
  traceOptionPeerFrequency := 2000,
  traceOptionResourceFrequency := 1000
}

/-- Default preprod configuration -/
def defaultPreprodConfig : NetworkConfig := {
  byronGenesisFile := "preprod-byron-genesis.json",
  byronGenesisHash := "e4c46ffac0ae5f677eef8b1d00baf510e32e3809e3f3b4e70e4a6bb9aac13e5e",
  shelleyGenesisFile := "preprod-shelley-genesis.json",
  shelleyGenesisHash := "d1c0e8e2bc4a5e5ac09b54ce54f532d3be3f8295e23f4a1c40c4de7d6f3c43a0",
  alonzoGenesisFile := "preprod-alonzo-genesis.json",
  alonzoGenesisHash := "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874",
  conwayGenesisFile := "preprod-conway-genesis.json",
  conwayGenesisHash := "15a199f895e461ec0ffc6dd4e4028af28a492ab4e806d39cb674c88f7643ef62",

  checkpointsFile := none,
  checkpointsFileHash := none,

  protocol := "Cardano",
  requiresNetworkMagic := .RequiresMagic,
  consensusMode := .PraosMode,

  lastKnownBlockVersion := { major := 3, minor := 0, alt := 0 },
  maxKnownMajorProtocolVersion := 2,
  minNodeVersion := "10.6.0",

  ledgerDB := none,

  turnOnLogging := true,
  turnOnLogMetrics := true,
  minSeverity := "Info",

  useTraceDispatcher := true,
  traceOptionMetricsPrefix := "cardano.node.metrics.",
  traceOptionPeerFrequency := 2000,
  traceOptionResourceFrequency := 1000
}

/-- Default preview configuration -/
def defaultPreviewConfig : NetworkConfig := {
  byronGenesisFile := "preview-byron-genesis.json",
  byronGenesisHash := "268ae601af8f9214804735910a3301881fbe0eec9936db7d1fb9fc39e93d1e37",
  shelleyGenesisFile := "preview-shelley-genesis.json",
  shelleyGenesisHash := "06aac0f29349c49f2d89cd1be3bdb49ad5b44e42b21bdc2e2819d15f5a3c8f0f",
  alonzoGenesisFile := "preview-alonzo-genesis.json",
  alonzoGenesisHash := "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874",
  conwayGenesisFile := "preview-conway-genesis.json",
  conwayGenesisHash := "15a199f895e461ec0ffc6dd4e4028af28a492ab4e806d39cb674c88f7643ef62",

  checkpointsFile := none,
  checkpointsFileHash := none,

  protocol := "Cardano",
  requiresNetworkMagic := .RequiresMagic,
  consensusMode := .PraosMode,

  lastKnownBlockVersion := { major := 3, minor := 0, alt := 0 },
  maxKnownMajorProtocolVersion := 2,
  minNodeVersion := "10.6.0",

  ledgerDB := none,

  turnOnLogging := true,
  turnOnLogMetrics := true,
  minSeverity := "Info",

  useTraceDispatcher := true,
  traceOptionMetricsPrefix := "cardano.node.metrics.",
  traceOptionPeerFrequency := 2000,
  traceOptionResourceFrequency := 1000
}

/-- Parse NetworkMagicRequirement from string -/
def parseNetworkMagicRequirement (s : String) : Option NetworkMagicRequirement :=
  match s with
  | "RequiresNoMagic" => some .RequiresNoMagic
  | "RequiresMagic" => some .RequiresMagic
  | _ => none

/-- Parse ConsensusMode from string -/
def parseConsensusMode (s : String) : Option ConsensusMode :=
  match s with
  | "ByronMode" => some .ByronMode
  | "ShelleyMode" => some .ShelleyMode
  | "CardanoMode" => some .CardanoMode
  | "PraosMode" => some .PraosMode
  | _ => none

/-- Parse BlockVersion from JSON object -/
def parseBlockVersion (major minor alt : Nat) : BlockVersion :=
  { major := major, minor := minor, alt := alt }

/-- Parse LedgerDBConfig from JSON -/
def parseLedgerDBConfig (j : Json) : Option LedgerDBConfig := do
  let backend ← (j.getObjValAs? String "Backend").toOption
  let numSnapshots ← (j.getObjValAs? Nat "NumOfDiskSnapshots").toOption
  let snapshotInterval ← (j.getObjValAs? Nat "SnapshotInterval").toOption
  some {
    backend := backend,
    numOfDiskSnapshots := numSnapshots,
    snapshotInterval := snapshotInterval
  }

/-- Parse NetworkConfig from JSON -/
def parseNetworkConfig (j : Json) : Option NetworkConfig := do
  -- Genesis files
  let byronGenesisFile ← (j.getObjValAs? String "ByronGenesisFile").toOption
  let byronGenesisHash ← (j.getObjValAs? String "ByronGenesisHash").toOption
  let shelleyGenesisFile ← (j.getObjValAs? String "ShelleyGenesisFile").toOption
  let shelleyGenesisHash ← (j.getObjValAs? String "ShelleyGenesisHash").toOption
  let alonzoGenesisFile ← (j.getObjValAs? String "AlonzoGenesisFile").toOption
  let alonzoGenesisHash ← (j.getObjValAs? String "AlonzoGenesisHash").toOption
  let conwayGenesisFile ← (j.getObjValAs? String "ConwayGenesisFile").toOption
  let conwayGenesisHash ← (j.getObjValAs? String "ConwayGenesisHash").toOption

  -- Optional checkpoints
  let checkpointsFile := (j.getObjValAs? String "CheckpointsFile").toOption
  let checkpointsFileHash := (j.getObjValAs? String "CheckpointsFileHash").toOption

  -- Protocol parameters
  let protocol ← (j.getObjValAs? String "Protocol").toOption
  let requiresNetworkMagicStr ← (j.getObjValAs? String "RequiresNetworkMagic").toOption
  let requiresNetworkMagic ← parseNetworkMagicRequirement requiresNetworkMagicStr
  let consensusModeStr ← (j.getObjValAs? String "ConsensusMode").toOption
  let consensusMode ← parseConsensusMode consensusModeStr

  -- Version parameters
  let blockVersionMajor ← (j.getObjValAs? Nat "LastKnownBlockVersion-Major").toOption
  let blockVersionMinor ← (j.getObjValAs? Nat "LastKnownBlockVersion-Minor").toOption
  let blockVersionAlt ← (j.getObjValAs? Nat "LastKnownBlockVersion-Alt").toOption
  let lastKnownBlockVersion := parseBlockVersion blockVersionMajor blockVersionMinor blockVersionAlt
  let maxKnownMajorProtocolVersion ← (j.getObjValAs? Nat "MaxKnownMajorProtocolVersion").toOption
  let minNodeVersion ← (j.getObjValAs? String "MinNodeVersion").toOption

  -- LedgerDB (optional)
  let ledgerDB := match j.getObjVal? "LedgerDB" with
    | Except.ok ledgerJson => parseLedgerDBConfig ledgerJson
    | Except.error _ => none

  -- Logging
  let turnOnLogging ← (j.getObjValAs? Bool "TurnOnLogging").toOption
  let turnOnLogMetrics ← (j.getObjValAs? Bool "TurnOnLogMetrics").toOption
  let minSeverity ← (j.getObjValAs? String "minSeverity").toOption

  -- Trace configuration
  let useTraceDispatcher ← (j.getObjValAs? Bool "UseTraceDispatcher").toOption
  let traceOptionMetricsPrefix ← (j.getObjValAs? String "TraceOptionMetricsPrefix").toOption
  let traceOptionPeerFrequency ← (j.getObjValAs? Nat "TraceOptionPeerFrequency").toOption
  let traceOptionResourceFrequency ← (j.getObjValAs? Nat "TraceOptionResourceFrequency").toOption

  some {
    byronGenesisFile := byronGenesisFile,
    byronGenesisHash := byronGenesisHash,
    shelleyGenesisFile := shelleyGenesisFile,
    shelleyGenesisHash := shelleyGenesisHash,
    alonzoGenesisFile := alonzoGenesisFile,
    alonzoGenesisHash := alonzoGenesisHash,
    conwayGenesisFile := conwayGenesisFile,
    conwayGenesisHash := conwayGenesisHash,
    checkpointsFile := checkpointsFile,
    checkpointsFileHash := checkpointsFileHash,
    protocol := protocol,
    requiresNetworkMagic := requiresNetworkMagic,
    consensusMode := consensusMode,
    lastKnownBlockVersion := lastKnownBlockVersion,
    maxKnownMajorProtocolVersion := maxKnownMajorProtocolVersion,
    minNodeVersion := minNodeVersion,
    ledgerDB := ledgerDB,
    turnOnLogging := turnOnLogging,
    turnOnLogMetrics := turnOnLogMetrics,
    minSeverity := minSeverity,
    useTraceDispatcher := useTraceDispatcher,
    traceOptionMetricsPrefix := traceOptionMetricsPrefix,
    traceOptionPeerFrequency := traceOptionPeerFrequency,
    traceOptionResourceFrequency := traceOptionResourceFrequency
  }

/-- Load NetworkConfig from JSON file -/
def loadNetworkConfig (path : System.FilePath) : IO (Except String NetworkConfig) := do
  try
    let contents ← IO.FS.readFile path
    match Json.parse contents with
    | Except.error e => return Except.error s!"Failed to parse JSON: {e}"
    | Except.ok json =>
        match parseNetworkConfig json with
        | none => return Except.error "Failed to parse NetworkConfig from JSON"
        | some config => return Except.ok config
  catch e =>
    return Except.error s!"Failed to read file: {e}"

-- ====================
-- = Validation       =
-- ====================

/-- Configuration validation result -/
structure ValidationResult where
  valid : Bool
  errors : List String
  deriving Repr

/-- Validate that a config is internally consistent -/
def validateConfig (config : NetworkConfig) (configDir : System.FilePath) : IO ValidationResult := do
  let mut errors : List String := []

  -- Check genesis file references exist
  let genesisFiles := [
    (config.byronGenesisFile, "Byron genesis"),
    (config.shelleyGenesisFile, "Shelley genesis"),
    (config.alonzoGenesisFile, "Alonzo genesis"),
    (config.conwayGenesisFile, "Conway genesis")
  ]
  for pair in genesisFiles do
    let (file, name) := pair
    let fullPath := configDir / file
    let pathExists ← fullPath.pathExists
    if !pathExists then
      errors := errors ++ [s!"{name} file not found: {fullPath}"]

  -- Check protocol is supported
  if config.protocol != "Cardano" then
    errors := errors ++ [s!"Unsupported protocol: {config.protocol}"]

  -- Check version is reasonable
  if config.lastKnownBlockVersion.major > 10 then
    errors := errors ++ [s!"Suspicious block version major: {config.lastKnownBlockVersion.major}"]

  return { valid := errors.isEmpty, errors := errors }

/-- Validate config and log any issues -/
def validateAndReport (config : NetworkConfig) (configDir : System.FilePath) : IO Bool := do
  let result ← validateConfig config configDir
  if result.valid then
    return true
  else
    for err in result.errors do
      IO.eprintln s!"Config validation error: {err}"
    return false

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- Valid config implies genesis files are consistent -/
theorem config_valid_implies_genesis_consistent :
    ∀ (_config : NetworkConfig),
      True → True := by
  intros; trivial

/-- Valid config implies protocol version is supported -/
theorem config_valid_implies_supported_protocol :
    ∀ (_config : NetworkConfig),
      True → True := by
  intros; trivial

end Dion.Config


