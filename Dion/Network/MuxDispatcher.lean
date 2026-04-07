import Dion.Network.Multiplexer
import Dion.Network.Socket
import Dion.Network.ChainSync
import Dion.Network.BlockFetch
import Dion.Network.TxSubmission2
import Dion.Network.KeepAlive
import Dion.Network.PeerSharing

/-!
# MUX Frame Dispatcher

Generalized MUX frame reader that dispatches incoming frames to the
appropriate mini-protocol decoder based on protocol ID. Replaces the
ad-hoc `receiveChainSyncFrame` pattern with a unified event loop.

## Design
- Reads one MUX frame (8-byte header + payload)
- Decodes payload based on protocol ID
- Returns a typed `MuxEvent` discriminated union
- Optionally handles KeepAlive transparently (auto-respond)
-/

namespace Dion.Network.MuxDispatcher

open Dion.Network.Multiplexer
open Dion.Network.Socket
open Dion.Network.ChainSync
open Dion.Network.BlockFetch
open Dion.Network.TxSubmission2
open Dion.Network.KeepAlive
open Dion.Network.PeerSharing

-- ====================
-- = MUX Events       =
-- ====================

/-- Result of reading and decoding one MUX frame -/
inductive MuxEvent where
  | ChainSyncMsg (msg : ChainSyncMessage)
  | BlockFetchMsg (msg : BlockFetchMessage)
  | TxSubmission2Msg (msg : TxSubmission2Message)
  | KeepAliveMsg (msg : KeepAliveMessage)
  | PeerSharingMsg (msg : PeerSharingMessage)
  | UnknownProtocol (protocolId : UInt16) (payload : ByteArray)
  | DecodeError (protocolId : MiniProtocolId) (payload : ByteArray)

instance : Repr MuxEvent where
  reprPrec e _ := match e with
    | .ChainSyncMsg msg => s!"ChainSync({repr msg})"
    | .BlockFetchMsg msg => s!"BlockFetch({repr msg})"
    | .TxSubmission2Msg msg => s!"TxSubmission2({repr msg})"
    | .KeepAliveMsg msg => s!"KeepAlive({repr msg})"
    | .PeerSharingMsg msg => s!"PeerSharing({repr msg})"
    | .UnknownProtocol pid _ => s!"UnknownProtocol({pid})"
    | .DecodeError pid _ => s!"DecodeError({repr pid})"

-- ====================
-- = Frame Reading    =
-- ====================

/-- Read exactly one MUX frame from the socket -/
def receiveMuxFrame (sock : Socket) : IO (Except SocketError MuxFrame) := do
  match ← socket_receive_exact sock 8 with
  | .error e => return .error e
  | .ok headerBytes =>
      match decodeMuxHeader headerBytes with
      | none => return .error (.ReceiveFailed "Failed to decode MUX header")
      | some header => do
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error e
          | .ok payload =>
              return .ok { header := header, payload := payload }

/-- Read one MUX frame and decode it into a typed event -/
def receiveMuxEvent (sock : Socket) : IO (Except SocketError MuxEvent) := do
  match ← receiveMuxFrame sock with
  | .error e => return .error e
  | .ok frame =>
      match frame.header.protocolId with
      | .ChainSync =>
          match decodeChainSyncMessage frame.payload with
          | some msg => return .ok (.ChainSyncMsg msg)
          | none => return .ok (.DecodeError .ChainSync frame.payload)
      | .BlockFetch =>
          match decodeBlockFetchMessage frame.payload with
          | some result => return .ok (.BlockFetchMsg result.value)
          | none => return .ok (.DecodeError .BlockFetch frame.payload)
      | .TxSubmission2 =>
          match decodeTxSubmission2Message frame.payload with
          | some msg => return .ok (.TxSubmission2Msg msg)
          | none => return .ok (.DecodeError .TxSubmission2 frame.payload)
      | .KeepAlive =>
          match decodeKeepAliveMessage frame.payload with
          | some msg => return .ok (.KeepAliveMsg msg)
          | none => return .ok (.DecodeError .KeepAlive frame.payload)
      | .PeerSharing =>
          match decodePeerSharingMessage frame.payload with
          | some msg => return .ok (.PeerSharingMsg msg)
          | none => return .ok (.DecodeError .PeerSharing frame.payload)
      | .Handshake =>
          -- Handshake frames after initial handshake are unexpected
          return .ok (.UnknownProtocol 0 frame.payload)

-- ====================
-- = KeepAlive Helper =
-- ====================

/-- Send a KeepAlive response for a given cookie -/
def sendKeepAliveResponse (sock : Socket) (cookie : UInt16) : IO (Except SocketError Unit) := do
  let payload := encodeKeepAliveMessage (.MsgKeepAliveResponse cookie)
  let frame ← createFrame .KeepAlive .Initiator payload
  socket_send sock (encodeMuxFrame frame)

/-- Read MUX events, automatically responding to KeepAlive.
    Returns the first non-KeepAlive event. -/
partial def receiveMuxEventAutoKeepAlive (sock : Socket) : IO (Except SocketError MuxEvent) := do
  match ← receiveMuxEvent sock with
  | .error e => return .error e
  | .ok (.KeepAliveMsg (.MsgKeepAlive cookie)) => do
      -- Auto-respond and continue reading
      match ← sendKeepAliveResponse sock cookie with
      | .error e => return .error e
      | .ok () => receiveMuxEventAutoKeepAlive sock
  | .ok event => return .ok event

-- ====================
-- = Dispatch Loop    =
-- ====================

/-- Run a MUX dispatch loop. Reads frames and calls the handler for each event.
    The handler returns `true` to continue or `false` to stop.
    KeepAlive is handled transparently. -/
partial def muxDispatchLoop (sock : Socket)
    (handler : MuxEvent → IO Bool) : IO (Except SocketError Unit) := do
  match ← receiveMuxEventAutoKeepAlive sock with
  | .error e => return .error e
  | .ok event => do
      let continue_ ← handler event
      if continue_ then
        muxDispatchLoop sock handler
      else
        return .ok ()

end Dion.Network.MuxDispatcher
