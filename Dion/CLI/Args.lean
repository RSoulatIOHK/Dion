/-!
# CLI Argument Parsing

Structured CLI parsing with subcommands:

```
dion run [--tui] [--preprod|--preview|--network NAME] [--port N]
             [--mithril-sync] [--metrics-port N]
dion query tip
dion query peers
dion query mempool
dion --help
```
-/

namespace Dion.CLI.Args

-- ====================
-- = Network          =
-- ====================

/-- Supported Cardano networks -/
inductive Network where
  | mainnet
  | preprod
  | preview
  | sanchonet
  deriving Repr, BEq

instance : ToString Network where
  toString
    | .mainnet   => "Mainnet"
    | .preprod   => "Preprod"
    | .preview   => "Preview"
    | .sanchonet => "SanchoNet"

/-- Cardano network ID byte: 1 = mainnet, 0 = all testnets -/
def Network.networkId : Network → Nat
  | .mainnet   => 1
  | .preprod   => 0
  | .preview   => 0
  | .sanchonet => 0

def Network.fromString (s : String) : Option Network :=
  match s.toLower with
  | "mainnet"   => some .mainnet
  | "preprod"   => some .preprod
  | "preview"   => some .preview
  | "sanchonet" => some .sanchonet
  | _           => none

-- ====================
-- = Query Target     =
-- ====================

/-- What to query -/
inductive QueryTarget where
  | tip
  | peers
  | mempool
  deriving Repr

-- ====================
-- = Commands         =
-- ====================

/-- Configuration for the `run` command -/
structure NodeConfig where
  network : Network := .mainnet
  port : UInt16 := 3001
  tui : Bool := false
  mithrilSync : Bool := false
  skipToTip : Bool := false           -- Skip sync, intersect at chain tip immediately (for testing)
  metricsPort : Option UInt16 := none
  spoKeyDir : Option String := none   -- Path to SPO key directory for block production
  socketPath : Option String := none  -- Unix socket path for cardano-cli (N2C)
  epochNonce : Option String := none  -- 64-hex epoch nonce seed (from cardano-cli query protocol-state)
  extraPeers : List (String × UInt16) := []  -- Extra peers from --peer HOST:PORT flags
  externalAddr : Option (String × UInt16) := none  -- Our external address to advertise via PeerSharing
  -- Custom network parameters (for local/private testnets)
  customSystemStart : Option UInt64 := none  -- --system-start UNIX_SECONDS (overrides per-network default)
  customEpochLength : Option Nat := none     -- --epoch-length SLOTS (overrides default 432000)
  customMagic : Option Nat := none           -- --testnet-magic N (for private testnets)
  customProtocolMajor : Option Nat := none   -- --protocol-major N (default: 10 Conway, use 9 for Babbage)
  deriving Repr

-- ====================
-- = SPO Config       =
-- ====================

/-- Config for SPO key generation -/
structure SPOKeygenConfig where
  keyDir     : String   := "spo-keys"
  kesPeriod  : Nat      := 0
  deriving Repr

/-- Config for SPO pool metadata generation -/
structure SPOMetadataConfig where
  name        : String  := "My Dion Pool"
  ticker      : String  := "CNODE"
  description : String  := "A stake pool running Dion"
  homepage    : String  := ""
  outFile     : String  := "poolMetadata.json"
  deriving Repr

/-- Config for KES key rotation -/
structure SPORotateKESConfig where
  keyDir    : String := "spo-keys"
  kesPeriod : Nat    := 0
  deriving Repr

/-- Config for SPO pool registration -/
structure SPORegisterConfig where
  keyDir         : String        := "spo-keys"
  relayHost      : String        := ""
  relayPort      : UInt16        := 3001
  pledgeLovelace : Nat           := 1_000_000_000
  costLovelace   : Nat           := 340_000_000
  socketPath     : String        := "/tmp/dion.socket"
  metadataUrl    : Option String := none   -- public URL where poolMetadata.json is hosted
  metadataFile   : Option String := none   -- local path to pre-computed metadata file
  update         : Bool          := false  -- skip stake key registration cert (pool params update)
  deriving Repr

-- ====================
-- = Command          =
-- ====================

/-- Config for the test-push command -/
structure SPOTestPushConfig where
  keyDir  : String              := "spo-keys"
  peers   : List (String × UInt16) := []
  network : Network             := .preview
  deriving Repr

inductive Command where
  | run (config : NodeConfig)
  | query (target : QueryTarget)
  | replay (paths : List String)   -- Replay saved block CBOR files for validation debugging
  | spoKeygen (config : SPOKeygenConfig)
  | spoMetadata (config : SPOMetadataConfig)
  | spoRegister (config : SPORegisterConfig)
  | spoRotateKES (config : SPORotateKESConfig)
  | spoTestPush (config : SPOTestPushConfig)
  | help
  | version

-- ====================
-- = Parsing          =
-- ====================

/-- Parse a UInt16 from a string -/
private def parsePort (s : String) : Option UInt16 :=
  match s.toNat? with
  | some p => if p > 0 && p < 65536 then some (UInt16.ofNat p) else none
  | none => none

/-- Parse run-command flags from args, returning updated config -/
private def parseRunFlags (args : List String) (config : NodeConfig) : NodeConfig :=
  match args with
  | [] => config
  | "--network" :: name :: rest =>
    let net := (Network.fromString name).getD config.network
    parseRunFlags rest { config with network := net }
  | "--preprod" :: rest => parseRunFlags rest { config with network := .preprod }
  | "--preview" :: rest => parseRunFlags rest { config with network := .preview }
  | "--sanchonet" :: rest => parseRunFlags rest { config with network := .sanchonet }
  | "--port" :: n :: rest =>
    let port := (parsePort n).getD config.port
    parseRunFlags rest { config with port := port }
  | "--tui" :: rest => parseRunFlags rest { config with tui := true }
  | "-tui" :: rest => parseRunFlags rest { config with tui := true }
  | "--mithril-sync" :: rest => parseRunFlags rest { config with mithrilSync := true }
  | "--skip-to-tip" :: rest => parseRunFlags rest { config with skipToTip := true }
  | "--metrics-port" :: n :: rest =>
    let mp := parsePort n
    parseRunFlags rest { config with metricsPort := mp }
  | "--spo-keys" :: dir :: rest =>
    parseRunFlags rest { config with spoKeyDir := some dir }
  | "--socket-path" :: path :: rest =>
    parseRunFlags rest { config with socketPath := some path }
  | "--epoch-nonce" :: hex :: rest =>
    parseRunFlags rest { config with epochNonce := some hex }
  | "--external-addr" :: hostPort :: rest =>
    let addr := match hostPort.splitOn ":" with
      | [host, portStr] => match portStr.toNat? with
          | some p => if p > 0 && p < 65536 then some (host, UInt16.ofNat p) else none
          | none => none
      | _ => none
    parseRunFlags rest { config with externalAddr := addr }
  | "--peer" :: hostPort :: rest =>
    -- Parse HOST:PORT or HOST PORT
    let peer := match hostPort.splitOn ":" with
      | [host, portStr] => match portStr.toNat? with
          | some p => if p > 0 && p < 65536 then some (host, UInt16.ofNat p) else none
          | none => none
      | _ => none
    match peer with
    | some p => parseRunFlags rest { config with extraPeers := config.extraPeers ++ [p] }
    | none => parseRunFlags rest config
  | "--system-start" :: n :: rest =>
    let v := n.toNat?.map UInt64.ofNat
    parseRunFlags rest { config with customSystemStart := v }
  | "--epoch-length" :: n :: rest =>
    parseRunFlags rest { config with customEpochLength := n.toNat? }
  | "--testnet-magic" :: n :: rest =>
    parseRunFlags rest { config with customMagic := n.toNat? }
  | "--protocol-major" :: n :: rest =>
    parseRunFlags rest { config with customProtocolMajor := n.toNat? }
  | _ :: rest => parseRunFlags rest config

/-- Parse spo keygen flags -/
private def parseSPOKeygenFlags (args : List String) (cfg : SPOKeygenConfig) : SPOKeygenConfig :=
  match args with
  | [] => cfg
  | "--dir" :: dir :: rest => parseSPOKeygenFlags rest { cfg with keyDir := dir }
  | "--kes-period" :: n :: rest =>
    let p := (n.toNat?).getD cfg.kesPeriod
    parseSPOKeygenFlags rest { cfg with kesPeriod := p }
  | _ :: rest => parseSPOKeygenFlags rest cfg

/-- Parse spo metadata flags -/
private def parseSPOMetadataFlags (args : List String) (cfg : SPOMetadataConfig) : SPOMetadataConfig :=
  match args with
  | [] => cfg
  | "--name" :: v :: rest        => parseSPOMetadataFlags rest { cfg with name := v }
  | "--ticker" :: v :: rest      => parseSPOMetadataFlags rest { cfg with ticker := v }
  | "--description" :: v :: rest => parseSPOMetadataFlags rest { cfg with description := v }
  | "--homepage" :: v :: rest    => parseSPOMetadataFlags rest { cfg with homepage := v }
  | "--out" :: v :: rest         => parseSPOMetadataFlags rest { cfg with outFile := v }
  | _ :: rest => parseSPOMetadataFlags rest cfg

/-- Parse spo register flags -/
private def parseSPORegisterFlags (args : List String) (cfg : SPORegisterConfig) : SPORegisterConfig :=
  match args with
  | [] => cfg
  | "--dir" :: dir :: rest => parseSPORegisterFlags rest { cfg with keyDir := dir }
  | "--relay" :: hostPort :: rest =>
    -- Support HOST:PORT (IPv4) and [IPv6]:PORT
    let parsed := match hostPort.splitOn ":" with
      | [host, portStr] => match portStr.trim.toNat? with
          | some p => if p > 0 && p < 65536 then some (host.trim, UInt16.ofNat p) else none
          | none => none
      | parts =>
          -- IPv6: last segment is port, rest is host
          match parts.getLast? >>= (·.trim.toNat?) with
          | some p =>
              let host := (":".intercalate parts.dropLast).trim
              if p > 0 && p < 65536 && !host.isEmpty then some (host, UInt16.ofNat p) else none
          | none => none
    match parsed with
    | some (h, p) => parseSPORegisterFlags rest { cfg with relayHost := h, relayPort := p }
    | none => parseSPORegisterFlags rest cfg
  | "--pledge" :: n :: rest =>
    let v := n.toNat?.getD cfg.pledgeLovelace
    parseSPORegisterFlags rest { cfg with pledgeLovelace := v }
  | "--cost" :: n :: rest =>
    let v := n.toNat?.getD cfg.costLovelace
    parseSPORegisterFlags rest { cfg with costLovelace := v }
  | "--socket-path" :: path :: rest =>
    parseSPORegisterFlags rest { cfg with socketPath := path }
  | "--metadata-url" :: url :: rest =>
    parseSPORegisterFlags rest { cfg with metadataUrl := some url }
  | "--metadata-file" :: path :: rest =>
    parseSPORegisterFlags rest { cfg with metadataFile := some path }
  | "--update" :: rest =>
    parseSPORegisterFlags rest { cfg with update := true }
  | _ :: rest => parseSPORegisterFlags rest cfg

/-- Parse CLI arguments into a Command -/
def parseArgs (args : List String) : Command :=
  match args with
  | [] => .run (parseRunFlags [] {})
  | "--help" :: _ => .help
  | "-h" :: _ => .help
  | "--version" :: _ => .version
  | "-v" :: _ => .version
  | "run" :: rest => .run (parseRunFlags rest {})
  | "query" :: "tip" :: _ => .query .tip
  | "query" :: "peers" :: _ => .query .peers
  | "query" :: "mempool" :: _ => .query .mempool
  | "query" :: _ => .help
  | "replay" :: files => .replay files
  | "spo" :: "keygen" :: rest     => .spoKeygen     (parseSPOKeygenFlags rest {})
  | "spo" :: "metadata" :: rest   => .spoMetadata   (parseSPOMetadataFlags rest {})
  | "spo" :: "register" :: rest   => .spoRegister   (parseSPORegisterFlags rest {})
  | "spo" :: "rotate-kes" :: rest =>
      let cfg := parseSPOKeygenFlags rest {}  -- shares --dir / --kes-period flags
      .spoRotateKES { keyDir := cfg.keyDir, kesPeriod := cfg.kesPeriod }
  | "spo" :: "test-push" :: rest =>
      let rec parse : List String → SPOTestPushConfig → SPOTestPushConfig
        | [], cfg => cfg
        | "--dir" :: d :: t, cfg => parse t { cfg with keyDir := d }
        | "--preview" :: t, cfg  => parse t { cfg with network := .preview }
        | "--preprod" :: t, cfg  => parse t { cfg with network := .preprod }
        | "--mainnet" :: t, cfg  => parse t { cfg with network := .mainnet }
        | "--peer" :: hp :: t, cfg =>
            let p := match hp.splitOn ":" with
              | [h, ps] => ps.toNat?.bind fun n =>
                  if n > 0 && n < 65536 then some (h, UInt16.ofNat n) else none
              | _ => none
            parse t { cfg with peers := cfg.peers ++ p.toList }
        | _ :: t, cfg => parse t cfg
      .spoTestPush (parse rest {})
  | "spo" :: _ => .help
  | _ =>
    -- Backward compatibility: treat flags without "run" subcommand as run
    .run (parseRunFlags args {})

-- ====================
-- = Usage            =
-- ====================

def printUsage : IO Unit := do
  IO.println "Dion — a Cardano relay node in Lean 4"
  IO.println ""
  IO.println "USAGE:"
  IO.println "  dion [run] [OPTIONS]        Start the relay node"
  IO.println "  dion query <target>         Query running node status"
  IO.println ""
  IO.println "RUN OPTIONS:"
  IO.println "  --network <name>   Select network: mainnet, preprod, preview, sanchonet"
  IO.println "                     (default: mainnet)"
  IO.println "  --preprod          Shorthand for --network preprod"
  IO.println "  --preview          Shorthand for --network preview"
  IO.println "  --sanchonet        Shorthand for --network sanchonet"
  IO.println "  --port <number>    Listen port for inbound connections (default: 3001)"
  IO.println "  --tui              Enable terminal UI mode"
  IO.println "  --mithril-sync     Fast-sync via Mithril snapshot before peer sync"
  IO.println "  --metrics-port <n> Enable Prometheus metrics on given port"
  IO.println "  --spo-keys <dir>   Path to SPO key directory for block production"
  IO.println "  --socket-path <p>  Unix socket path for cardano-cli (default: ./dion.socket)"
  IO.println "  --epoch-nonce <h>  Seed epoch nonce (64 hex chars) from cardano-cli query protocol-state"
  IO.println "  --peer <host:port> Add extra peer (repeatable; useful for local two-node tests)"
  IO.println "  --external-addr <host:port>  Advertise our address to peers via PeerSharing"
  IO.println ""
  IO.println "PRIVATE TESTNET OPTIONS:"
  IO.println "  --system-start <N>    Unix timestamp (seconds) of slot 0 (from shelley-genesis.json)"
  IO.println "  --epoch-length <N>    Slots per epoch (default: 432000; use 500 for local testnets)"
  IO.println "  --testnet-magic <N>   Network magic (default: per --network; use for private testnets)"
  IO.println "  --protocol-major <N>  Block protocol major version (default: 10 Conway; use 9 for Babbage)"
  IO.println ""
  IO.println "QUERY TARGETS:"
  IO.println "  tip                Show current chain tip"
  IO.println "  peers              Show connected peers"
  IO.println "  mempool            Show mempool status"
  IO.println ""
  IO.println "SPO COMMANDS:"
  IO.println "  spo keygen [--dir PATH] [--kes-period N]"
  IO.println "                     Generate all SPO key files (cold/VRF/KES/stake/payment + opcert)"
  IO.println "  spo metadata [--name STR] [--ticker STR] [--description STR] [--homepage URL] [--out FILE]"
  IO.println "                     Generate poolMetadata.json and print its blake2b-256 hash"
  IO.println "  spo register [--dir PATH] [--relay HOST:PORT] [--pledge ADA] [--cost ADA]"
  IO.println "               [--metadata-url URL] [--metadata-file PATH]"
  IO.println "                     Print cardano-cli commands to register the pool on-chain"
  IO.println "  spo rotate-kes [--dir PATH] [--kes-period N]"
  IO.println "                     Generate new KES key pair + opcert at given KES period"
  IO.println "                     (preserves existing cold key / pool identity)"
  IO.println ""
  IO.println "OTHER:"
  IO.println "  --help, -h         Show this help message"
  IO.println "  --version, -v      Show version"

end Dion.CLI.Args
