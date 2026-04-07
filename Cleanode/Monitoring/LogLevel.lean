/-!
# Structured Logging with Levels

Provides log levels and a configurable logger that routes messages
to stdout and/or TUI state based on severity.

## Log Levels
- Debug: Verbose internal state (VRF details, CBOR parsing)
- Info: Normal operations (block synced, peer connected)
- Warn: Recoverable issues (peer timeout, reconnecting)
- Error: Failures requiring attention (handshake failed, DB error)
-/

namespace Dion.Monitoring.LogLevel

/-- Log severity levels -/
inductive LogLevel where
  | debug
  | info
  | warn
  | error
  deriving Repr, BEq, Ord

instance : ToString LogLevel where
  toString
    | .debug => "DEBUG"
    | .info  => "INFO"
    | .warn  => "WARN"
    | .error => "ERROR"

/-- Compare log levels for filtering -/
instance : LE LogLevel where
  le a b := compare a b != .gt

/-- Check if a message at `level` should be logged given `minLevel` -/
def shouldLog (minLevel level : LogLevel) : Bool :=
  compare minLevel level != .gt

/-- Format a log message with timestamp prefix -/
def formatLogMessage (level : LogLevel) (component : String) (msg : String) : IO String := do
  let now ← IO.monoMsNow
  let secs := now / 1000
  let ms := now % 1000
  let msStr := toString ms
  let msPadded := String.mk (List.replicate (3 - msStr.length) '0') ++ msStr
  return s!"[{secs}.{msPadded}] [{level}] [{component}] {msg}"

/-- Logger configuration -/
structure LogConfig where
  minLevel : LogLevel := .info
  deriving Repr

/-- Log a message if it meets the minimum level -/
def logMsg (config : LogConfig) (level : LogLevel) (component : String) (msg : String) : IO Unit := do
  if shouldLog config.minLevel level then
    let formatted ← formatLogMessage level component msg
    if level == .error then
      IO.eprintln formatted
    else
      IO.println formatted

/-- Convenience functions -/
def logDebug (config : LogConfig) (component msg : String) : IO Unit :=
  logMsg config .debug component msg

def logInfo (config : LogConfig) (component msg : String) : IO Unit :=
  logMsg config .info component msg

def logWarn (config : LogConfig) (component msg : String) : IO Unit :=
  logMsg config .warn component msg

def logError (config : LogConfig) (component msg : String) : IO Unit :=
  logMsg config .error component msg

end Dion.Monitoring.LogLevel
