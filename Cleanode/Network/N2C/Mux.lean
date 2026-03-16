import Cleanode.Network.Socket
import Cleanode.Network.N2C.MiniProtocolId
import Cleanode.Network.Multiplexer

/-!
# Node-to-Client Multiplexer

MUX frame encode/decode for Node-to-Client connections. Uses the same 8-byte
header format as Node-to-Node but with N2C protocol IDs. Reuses encoding
helpers from the N2N `Multiplexer` module.

## References
- Ouroboros Network Spec Section 3 (Multiplexing)
-/

namespace Cleanode.Network.N2C.Mux

open Cleanode.Network.Socket
open Cleanode.Network.N2C.MiniProtocolId
open Cleanode.Network.Multiplexer (Mode encodeUInt32BE encodeUInt16BE
  decodeUInt32BE decodeUInt16BE getCurrentTimestamp)

-- ====================
-- = Header/Frame     =
-- ====================

/-- N2C multiplexer header (8 bytes, same wire format as N2N) -/
structure N2CMuxHeader where
  timestamp : UInt32
  protocolId : N2CMiniProtocolId
  mode : Mode
  payloadLength : UInt16
  deriving Repr

/-- N2C multiplexer frame -/
structure N2CMuxFrame where
  header : N2CMuxHeader
  payload : ByteArray

-- ====================
-- = Encoding         =
-- ====================

/-- Encode N2C MUX header (8 bytes) -/
def encodeN2CMuxHeader (h : N2CMuxHeader) : ByteArray :=
  let ts := encodeUInt32BE h.timestamp
  let pid := h.protocolId.toUInt16
  let modeAndPid := (h.mode.toBit <<< 15) ||| pid
  let pidBytes := encodeUInt16BE modeAndPid
  let lenBytes := encodeUInt16BE h.payloadLength
  ts ++ pidBytes ++ lenBytes

/-- Encode a complete N2C MUX frame -/
def encodeN2CMuxFrame (frame : N2CMuxFrame) : ByteArray :=
  encodeN2CMuxHeader frame.header ++ frame.payload

-- ====================
-- = Decoding         =
-- ====================

/-- Decode N2C MUX header from 8 bytes -/
def decodeN2CMuxHeader (bs : ByteArray) : Option N2CMuxHeader := do
  if bs.size < 8 then none

  let timestamp ← decodeUInt32BE (bs.extract 0 4)
  let modeAndPid ← decodeUInt16BE (bs.extract 4 6)
  let payloadLength ← decodeUInt16BE (bs.extract 6 8)

  let mode := Mode.fromBit (modeAndPid >>> 15)
  let pid := modeAndPid &&& 0x7FFF
  let protocolId ← N2CMiniProtocolId.fromUInt16 pid

  some {
    timestamp := timestamp,
    protocolId := protocolId,
    mode := mode,
    payloadLength := payloadLength
  }

-- ====================
-- = Socket I/O       =
-- ====================

/-- Read exactly one N2C MUX frame from a socket -/
def receiveN2CMuxFrame (sock : Socket) : IO (Except SocketError N2CMuxFrame) := do
  match ← socket_receive_exact sock 8 with
  | .error e => return .error e
  | .ok headerBytes =>
    match decodeN2CMuxHeader headerBytes with
    | none => return .error (.ReceiveFailed "Failed to decode N2C MUX header")
    | some header => do
      match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
      | .error e => return .error e
      | .ok payload =>
        return .ok { header := header, payload := payload }

/-- Send a payload as an N2C MUX frame -/
def sendN2CPayload (sock : Socket) (protocolId : N2CMiniProtocolId) (mode : Mode)
    (payload : ByteArray) : IO (Except SocketError Unit) := do
  let timestamp ← getCurrentTimestamp
  let frame : N2CMuxFrame := {
    header := {
      timestamp := timestamp,
      protocolId := protocolId,
      mode := mode,
      payloadLength := UInt16.ofNat payload.size
    },
    payload := payload
  }
  socket_send sock (encodeN2CMuxFrame frame)

end Cleanode.Network.N2C.Mux
