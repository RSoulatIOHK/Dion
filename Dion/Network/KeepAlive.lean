import Dion.Network.Cbor
import Dion.Network.Multiplexer
import Dion.Network.Socket

/-!
# Keep-Alive Mini-Protocol

The Keep-Alive protocol maintains connection health between peers.
The client periodically sends cookies and measures round-trip latency.
The server responds with the same cookie.

## Protocol Flow
1. Client sends MsgKeepAlive with a 16-bit cookie
2. Server responds with MsgKeepAliveResponse echoing the cookie
3. Client measures round-trip time for latency estimation

## Timeout Detection
If no response is received within the timeout window, the connection
is considered unhealthy. After a threshold of missed responses,
the connection should be terminated.

## References
- Ouroboros Network Spec Section 3.5 (Keep-Alive)
- Protocol ID: 0x0008
-/

namespace Dion.Network.KeepAlive

open Dion.Network.Cbor
open Dion.Network.Multiplexer
open Dion.Network.Socket

-- ====================
-- = Types            =
-- ====================

/-- Keep-Alive protocol messages -/
inductive KeepAliveMessage where
  | MsgKeepAlive (cookie : UInt16)          -- [0, cookie]: Client sends cookie
  | MsgKeepAliveResponse (cookie : UInt16)  -- [1, cookie]: Server echoes cookie
  deriving Repr, BEq

/-- Keep-Alive client state for tracking connection health -/
structure KeepAliveState where
  lastSentCookie : UInt16           -- Last cookie we sent
  lastReceivedCookie : UInt16       -- Last cookie echoed back
  lastRoundTripMs : Option Nat      -- Last measured round-trip time (milliseconds)
  missedResponses : Nat             -- Consecutive missed responses
  totalSent : Nat                   -- Total keep-alives sent
  totalReceived : Nat               -- Total responses received
  deriving Repr

/-- Default initial state -/
def KeepAliveState.initial : KeepAliveState :=
  { lastSentCookie := 0
  , lastReceivedCookie := 0
  , lastRoundTripMs := none
  , missedResponses := 0
  , totalSent := 0
  , totalReceived := 0 }

/-- Configuration for keep-alive behavior -/
structure KeepAliveConfig where
  intervalMs : Nat := 10000         -- Send interval (default 10s)
  timeoutMs : Nat := 30000          -- Response timeout (default 30s)
  maxMissed : Nat := 3              -- Max missed before disconnect
  deriving Repr

-- ====================
-- = Encoding         =
-- ====================

/-- Encode KeepAlive message as CBOR -/
def encodeKeepAliveMessage : KeepAliveMessage → ByteArray
  | .MsgKeepAlive cookie =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 0
      let cookieEnc := encodeUInt cookie.toNat
      arr ++ msgId ++ cookieEnc
  | .MsgKeepAliveResponse cookie =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 1
      let cookieEnc := encodeUInt cookie.toNat
      arr ++ msgId ++ cookieEnc

-- ====================
-- = Decoding         =
-- ====================

/-- Decode KeepAlive message from CBOR -/
def decodeKeepAliveMessage (bs : ByteArray) : Option KeepAliveMessage := do
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none
  let r2 ← decodeUInt r1.remaining
  let r3 ← decodeUInt r2.remaining
  let cookie := UInt16.ofNat r3.value
  match r2.value with
  | 0 => some (.MsgKeepAlive cookie)
  | 1 => some (.MsgKeepAliveResponse cookie)
  | _ => none

-- ====================
-- = Client API       =
-- ====================

/-- Send a KeepAlive message over socket -/
def sendKeepAlive (sock : Socket) (msg : KeepAliveMessage) : IO (Except SocketError Unit) := do
  let payload := encodeKeepAliveMessage msg
  let frame ← createFrame .KeepAlive .Initiator payload
  socket_send sock (encodeMuxFrame frame)

/-- Receive a KeepAlive message from socket -/
def receiveKeepAlive (sock : Socket) : IO (Except SocketError (Option KeepAliveMessage)) := do
  match ← socket_receive_exact sock 8 with
  | .error e => return .error e
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none => return .ok none
      | some header => do
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error e
          | .ok payload => return .ok (decodeKeepAliveMessage payload)

/-- Send a keep-alive cookie and return the updated state -/
def keepAliveClient (sock : Socket) (state : KeepAliveState) : IO (Except SocketError KeepAliveState) := do
  let cookie := state.lastSentCookie + 1
  match ← sendKeepAlive sock (.MsgKeepAlive cookie) with
  | .error e => return .error e
  | .ok () =>
      return .ok { state with
        lastSentCookie := cookie
        totalSent := state.totalSent + 1
      }

/-- Handle an incoming KeepAlive message as a server (respond to cookies) -/
def keepAliveServer (sock : Socket) (msg : KeepAliveMessage) : IO (Except SocketError Unit) := do
  match msg with
  | .MsgKeepAlive cookie =>
      sendKeepAlive sock (.MsgKeepAliveResponse cookie)
  | .MsgKeepAliveResponse _ =>
      return .ok ()  -- Unexpected as server, ignore

/-- Process a received KeepAlive response and update state -/
def processResponse (state : KeepAliveState) (msg : KeepAliveMessage) : KeepAliveState :=
  match msg with
  | .MsgKeepAliveResponse cookie =>
      { state with
        lastReceivedCookie := cookie
        missedResponses := 0
        totalReceived := state.totalReceived + 1 }
  | .MsgKeepAlive _ => state  -- Not a response

/-- Record a missed response (timeout) -/
def recordMissed (state : KeepAliveState) : KeepAliveState :=
  { state with missedResponses := state.missedResponses + 1 }

/-- Check if the connection should be considered dead -/
def isConnectionDead (config : KeepAliveConfig) (state : KeepAliveState) : Bool :=
  state.missedResponses ≥ config.maxMissed

-- ====================
-- = Latency          =
-- ====================

/-- Timestamp in milliseconds (from IO.monoMsNow) -/
abbrev TimestampMs := Nat

/-- Extended keep-alive state with latency tracking -/
structure KeepAliveLatencyState where
  base : KeepAliveState
  sentTimestamp : Option TimestampMs     -- When last cookie was sent
  latencyHistory : List Nat              -- Recent RTT measurements (ms)
  maxHistorySize : Nat := 10             -- Keep last N measurements
  deriving Repr

/-- Initial latency state -/
def KeepAliveLatencyState.initial : KeepAliveLatencyState :=
  { base := KeepAliveState.initial
  , sentTimestamp := none
  , latencyHistory := [] }

/-- Send a keep-alive with timestamp recording -/
def sendKeepAliveWithLatency (sock : Socket) (state : KeepAliveLatencyState)
    : IO (Except SocketError KeepAliveLatencyState) := do
  let now ← IO.monoMsNow
  let cookie := state.base.lastSentCookie + 1
  match ← sendKeepAlive sock (.MsgKeepAlive cookie) with
  | .error e => return .error e
  | .ok () =>
      return .ok { state with
        base := { state.base with
          lastSentCookie := cookie
          totalSent := state.base.totalSent + 1 }
        sentTimestamp := some now }

/-- Process a response with latency measurement -/
def processResponseWithLatency (state : KeepAliveLatencyState) (msg : KeepAliveMessage)
    : IO KeepAliveLatencyState := do
  match msg with
  | .MsgKeepAliveResponse cookie =>
      let now ← IO.monoMsNow
      let rtt := match state.sentTimestamp with
        | some sent => now - sent
        | none => 0
      let history := (rtt :: state.latencyHistory).take state.maxHistorySize
      return { state with
        base := { state.base with
          lastReceivedCookie := cookie
          missedResponses := 0
          totalReceived := state.base.totalReceived + 1
          lastRoundTripMs := some rtt }
        sentTimestamp := none
        latencyHistory := history }
  | .MsgKeepAlive _ => return state

/-- Get average latency from history -/
def averageLatencyMs (state : KeepAliveLatencyState) : Option Nat :=
  if state.latencyHistory.isEmpty then none
  else
    let sum := state.latencyHistory.foldl (· + ·) 0
    some (sum / state.latencyHistory.length)

/-- Record a timeout (no response received within deadline) -/
def recordTimeout (state : KeepAliveLatencyState) : KeepAliveLatencyState :=
  { state with
    base := recordMissed state.base
    sentTimestamp := none }

/-- Check if we're waiting for a response and it has timed out -/
def checkTimeout (config : KeepAliveConfig) (state : KeepAliveLatencyState)
    : IO (Bool × KeepAliveLatencyState) := do
  match state.sentTimestamp with
  | none => return (false, state)
  | some sent =>
      let now ← IO.monoMsNow
      if now - sent > config.timeoutMs then
        return (true, recordTimeout state)
      else
        return (false, state)

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- KeepAlive protocol: response cookie matches request cookie -/
theorem keepalive_cookie_echo :
    ∀ (cookie : UInt16),
      decodeKeepAliveMessage (encodeKeepAliveMessage (.MsgKeepAliveResponse cookie)) =
        some (.MsgKeepAliveResponse cookie) := by
  sorry

/-- Timeout detection is monotonic: missed count only increases on timeout -/
theorem timeout_increases_missed (state : KeepAliveState) :
    (recordMissed state).missedResponses = state.missedResponses + 1 := by
  simp [recordMissed]

end Dion.Network.KeepAlive
