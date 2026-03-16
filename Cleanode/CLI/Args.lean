/-!
# CLI Argument Parsing

Structured CLI parsing with subcommands:

```
cleanode run [--tui] [--preprod|--preview|--network NAME] [--port N]
             [--mithril-sync] [--metrics-port N]
cleanode query tip
cleanode query peers
cleanode query mempool
cleanode --help
```
-/

namespace Cleanode.CLI.Args

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
  metricsPort : Option UInt16 := none
  spoKeyDir : Option String := none   -- Path to SPO key directory for block production
  socketPath : Option String := none  -- Unix socket path for cardano-cli (N2C)
  deriving Repr

/-- Top-level CLI command -/
inductive Command where
  | run (config : NodeConfig)
  | query (target : QueryTarget)
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
  | "--metrics-port" :: n :: rest =>
    let mp := parsePort n
    parseRunFlags rest { config with metricsPort := mp }
  | "--spo-keys" :: dir :: rest =>
    parseRunFlags rest { config with spoKeyDir := some dir }
  | "--socket-path" :: path :: rest =>
    parseRunFlags rest { config with socketPath := some path }
  | _ :: rest => parseRunFlags rest config

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
  | "query" :: _ => .help  -- unknown query target
  | _ =>
    -- Backward compatibility: treat flags without "run" subcommand as run
    .run (parseRunFlags args {})

-- ====================
-- = Usage            =
-- ====================

def printUsage : IO Unit := do
  IO.println "Cleanode — a Cardano relay node in Lean 4"
  IO.println ""
  IO.println "USAGE:"
  IO.println "  cleanode [run] [OPTIONS]        Start the relay node"
  IO.println "  cleanode query <target>         Query running node status"
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
  IO.println "  --socket-path <p>  Unix socket path for cardano-cli (default: ./cleanode.socket)"
  IO.println ""
  IO.println "QUERY TARGETS:"
  IO.println "  tip                Show current chain tip"
  IO.println "  peers              Show connected peers"
  IO.println "  mempool            Show mempool status"
  IO.println ""
  IO.println "OTHER:"
  IO.println "  --help, -h         Show this help message"
  IO.println "  --version, -v      Show version"

end Cleanode.CLI.Args
