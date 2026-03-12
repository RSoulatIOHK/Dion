import Cleanode.Network.Cbor
import Cleanode.Network.Multiplexer
import Cleanode.Network.Socket

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

namespace Cleanode.Network.KeepAlive

open Cleanode.Network.Cbor
open Cleanode.Network.Multiplexer
open Cleanode.Network.Socket

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

end Cleanode.Network.KeepAlive
