import Cleanode.TUI.State
import Cleanode.Monitoring.Metrics

/-!
# Metrics HTTP Server

Starts a background HTTP server serving Prometheus metrics on a configurable port.
Uses C FFI for the POSIX socket server, with a Lean callback to generate metrics text.
-/

namespace Cleanode.Monitoring.Server

open Cleanode.TUI.State
open Cleanode.Monitoring.Metrics

-- ====================
-- = FFI Bindings     =
-- ====================

/-- Register the metrics callback (called before starting the server) -/
@[extern "cleanode_metrics_set_callback"]
opaque metricsSetCallback (callback : Unit → IO String) : IO Unit

/-- Start the HTTP server on the given port (blocks forever — run in a Task) -/
@[extern "cleanode_metrics_server_start"]
opaque metricsServerStart (port : UInt16) : IO (Except String Unit)

-- ====================
-- = Server Lifecycle =
-- ====================

/-- Start the Prometheus metrics server in a background task.
    Returns the spawned task handle. -/
def startMetricsServer (port : UInt16) (tuiRef : IO.Ref TUIState) : IO (Task (Except IO.Error Unit)) := do
  -- Register callback that reads TUIState and renders Prometheus format
  metricsSetCallback fun () => do
    let state ← tuiRef.get
    let nowMs ← IO.monoMsNow
    return renderPrometheus state nowMs

  IO.println s!"[metrics] Starting Prometheus metrics server on port {port}"

  -- Spawn the blocking server in a background task
  let task ← IO.asTask do
    let result ← metricsServerStart port
    match result with
    | .ok () => pure ()
    | .error msg => IO.eprintln s!"[metrics] Server error: {msg}"
  return task

end Cleanode.Monitoring.Server
