/-!
# Logging and Metrics

Structured logging system for the Dion node with support for:
- Log levels (Debug, Info, Warn, Error)
- Structured context (key-value pairs)
- Metrics tracking (block count, sync speed, peer latency)

## References
- iohk-monitoring-framework (Haskell Cardano node)
-/

namespace Dion.Network.Logging

-- ====================
-- = Log Levels       =
-- ====================

/-- Log severity levels -/
inductive LogLevel where
  | Debug | Info | Warn | Error
  deriving Repr, BEq

def LogLevel.toNat : LogLevel → Nat
  | .Debug => 0
  | .Info  => 1
  | .Warn  => 2
  | .Error => 3

instance : Ord LogLevel where
  compare a b := compare a.toNat b.toNat

/-- Log level display string -/
def LogLevel.toString : LogLevel → String
  | .Debug => "DEBUG"
  | .Info  => "INFO "
  | .Warn  => "WARN "
  | .Error => "ERROR"

-- ====================
-- = Log Entries      =
-- ====================

/-- Structured log context (key-value metadata) -/
abbrev LogContext := List (String × String)

/-- A single log entry -/
structure LogEntry where
  level : LogLevel
  component : String         -- Module/component name
  message : String
  context : LogContext        -- Structured context
  deriving Repr

/-- Format a log entry for display -/
def LogEntry.format (entry : LogEntry) : String :=
  let ctx := if entry.context.isEmpty then ""
    else " " ++ String.intercalate " " (entry.context.map (fun (k, v) => s!"{k}={v}"))
  s!"[{entry.level.toString}] [{entry.component}] {entry.message}{ctx}"

-- ====================
-- = Logger           =
-- ====================

/-- Logger configuration -/
structure LoggerConfig where
  minLevel : LogLevel := .Info
  component : String := "node"
  deriving Repr

/-- Logger state -/
structure Logger where
  config : LoggerConfig
  entryCount : Nat := 0
  deriving Repr

/-- Create a new logger -/
def Logger.new (config : LoggerConfig := {}) : Logger :=
  { config := config }

/-- Create a child logger with a sub-component -/
def Logger.child (logger : Logger) (subComponent : String) : Logger :=
  { logger with config := { logger.config with component := s!"{logger.config.component}.{subComponent}" } }

/-- Log a message if it meets the minimum level -/
def Logger.log (logger : Logger) (level : LogLevel) (msg : String)
    (ctx : LogContext := []) : IO Logger := do
  if level.toNat >= logger.config.minLevel.toNat then
    let entry : LogEntry := {
      level := level,
      component := logger.config.component,
      message := msg,
      context := ctx
    }
    IO.println entry.format
  return { logger with entryCount := logger.entryCount + 1 }

/-- Log debug message -/
def Logger.debug (logger : Logger) (msg : String) (ctx : LogContext := []) : IO Logger :=
  logger.log .Debug msg ctx

/-- Log info message -/
def Logger.info (logger : Logger) (msg : String) (ctx : LogContext := []) : IO Logger :=
  logger.log .Info msg ctx

/-- Log warning message -/
def Logger.warn (logger : Logger) (msg : String) (ctx : LogContext := []) : IO Logger :=
  logger.log .Warn msg ctx

/-- Log error message -/
def Logger.error (logger : Logger) (msg : String) (ctx : LogContext := []) : IO Logger :=
  logger.log .Error msg ctx

-- ====================
-- = Metrics          =
-- ====================

/-- Node metrics for monitoring -/
structure NodeMetrics where
  blocksReceived : Nat := 0
  blocksFetched : Nat := 0
  txsProcessed : Nat := 0
  rollbacks : Nat := 0
  peerCount : Nat := 0
  avgLatencyMs : Nat := 0
  syncProgress : Nat := 0      -- Percentage (0-100)
  currentSlot : Nat := 0
  currentBlockNo : Nat := 0
  utxoSetSize : Nat := 0
  deriving Repr

/-- Update metrics with a new block -/
def NodeMetrics.onBlock (m : NodeMetrics) (slot blockNo : Nat) : NodeMetrics :=
  { m with
    blocksReceived := m.blocksReceived + 1,
    currentSlot := slot,
    currentBlockNo := blockNo }

/-- Update metrics with a fetched block -/
def NodeMetrics.onBlockFetch (m : NodeMetrics) : NodeMetrics :=
  { m with blocksFetched := m.blocksFetched + 1 }

/-- Update metrics with processed transactions -/
def NodeMetrics.onTxs (m : NodeMetrics) (count : Nat) : NodeMetrics :=
  { m with txsProcessed := m.txsProcessed + count }

/-- Update metrics on rollback -/
def NodeMetrics.onRollback (m : NodeMetrics) : NodeMetrics :=
  { m with rollbacks := m.rollbacks + 1 }

/-- Format metrics for display -/
def NodeMetrics.format (m : NodeMetrics) : String :=
  s!"blocks={m.blocksReceived} fetched={m.blocksFetched} txs={m.txsProcessed} " ++
  s!"rollbacks={m.rollbacks} slot={m.currentSlot} blockNo={m.currentBlockNo} " ++
  s!"sync={m.syncProgress}%"

end Dion.Network.Logging
