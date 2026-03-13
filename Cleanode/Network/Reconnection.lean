import Cleanode.Network.Socket
import Cleanode.Network.Logging

/-!
# Connection Reconnection

Handles automatic reconnection when a peer connection drops.
Implements exponential backoff with jitter.

## Strategy
1. On connection failure, wait with exponential backoff
2. Retry connection to same or different peer
3. After max retries, report failure

## References
- Ouroboros Network: Connection Manager
-/

namespace Cleanode.Network.Reconnection

open Cleanode.Network.Socket
open Cleanode.Network.Logging

-- ====================
-- = Configuration    =
-- ====================

/-- Reconnection strategy configuration -/
structure ReconnectConfig where
  maxRetries : Nat := 10            -- Maximum reconnection attempts
  initialDelayMs : Nat := 1000     -- Initial delay (1 second)
  maxDelayMs : Nat := 60000        -- Maximum delay (60 seconds)
  backoffMultiplier : Nat := 2     -- Exponential backoff factor
  deriving Repr

-- ====================
-- = Reconnect State  =
-- ====================

/-- Connection state for reconnection tracking -/
inductive ConnectionState where
  | Connected                        -- Active connection
  | Disconnected                     -- Cleanly disconnected
  | Reconnecting (attempt : Nat)     -- Attempting reconnection
  | Failed (reason : String)         -- Permanently failed
  deriving Repr

/-- Reconnection manager state -/
structure ReconnectManager where
  config : ReconnectConfig
  state : ConnectionState
  currentHost : String
  currentPort : UInt16
  totalReconnects : Nat := 0
  logger : Logger
  deriving Repr

-- ====================
-- = Reconnect Logic  =
-- ====================

/-- Calculate delay for a given attempt (exponential backoff) -/
def calculateDelay (config : ReconnectConfig) (attempt : Nat) : Nat :=
  let delay := config.initialDelayMs * (config.backoffMultiplier ^ attempt)
  min delay config.maxDelayMs

/-- Create a reconnection manager -/
def ReconnectManager.new (host : String) (port : UInt16)
    (config : ReconnectConfig := {}) : ReconnectManager :=
  { config := config,
    state := .Disconnected,
    currentHost := host,
    currentPort := port,
    logger := Logger.new { component := "reconnect" } }

/-- Attempt to connect (or reconnect) -/
def ReconnectManager.connect (mgr : ReconnectManager) : IO (Except SocketError Socket × ReconnectManager) := do
  let logger ← mgr.logger.info s!"Connecting to {mgr.currentHost}:{mgr.currentPort}"
  let result ← socket_connect mgr.currentHost mgr.currentPort
  match result with
  | .ok sock =>
      let logger ← logger.info "Connected successfully"
      return (.ok sock, { mgr with state := .Connected, logger := logger })
  | .error e =>
      let logger ← logger.error s!"Connection failed: {repr e}"
      return (.error e, { mgr with state := .Disconnected, logger := logger })

/-- Attempt reconnection with backoff -/
partial def ReconnectManager.reconnect (mgr : ReconnectManager) : IO (Except SocketError Socket × ReconnectManager) := do
  let mut currentMgr := mgr
  for attempt in List.range mgr.config.maxRetries do
    currentMgr := { currentMgr with state := .Reconnecting attempt }
    let delay := calculateDelay currentMgr.config attempt
    let logger ← currentMgr.logger.info s!"Reconnection attempt {attempt + 1}/{currentMgr.config.maxRetries}, waiting {delay}ms"
    currentMgr := { currentMgr with logger := logger }

    -- Wait with backoff
    IO.sleep (UInt32.ofNat delay)

    -- Try to connect
    let (result, newMgr) ← currentMgr.connect
    currentMgr := newMgr
    match result with
    | .ok sock =>
        currentMgr := { currentMgr with totalReconnects := currentMgr.totalReconnects + 1 }
        return (.ok sock, currentMgr)
    | .error _ => continue

  -- All retries exhausted
  let logger ← currentMgr.logger.error "All reconnection attempts failed"
  currentMgr := { currentMgr with state := .Failed "max retries exceeded", logger := logger }
  return (.error (.ConnectionFailed "Reconnection failed after max retries"), currentMgr)

/-- Handle a connection error (decide whether to reconnect) -/
def ReconnectManager.onError (mgr : ReconnectManager) (err : SocketError) : IO (Except SocketError Socket × ReconnectManager) := do
  let logger ← mgr.logger.warn s!"Connection error: {repr err}"
  let mgr := { mgr with state := .Disconnected, logger := logger }
  mgr.reconnect

end Cleanode.Network.Reconnection
