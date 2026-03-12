import Cleanode.Network.Socket

/-!
# Ouroboros Network Multiplexer

The multiplexer (MUX) allows multiple mini-protocols to share a single TCP connection.
Each message is framed with a header containing:
- Timestamp (32-bit)
- Mini-protocol ID (16-bit)
- Mode (1 bit: 0=initiator, 1=responder)
- Payload length (16-bit)
- Payload data

## References
- Ouroboros Network Spec Section 3 (Multiplexing)
- https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf
-/

namespace Cleanode.Network.Multiplexer

open Cleanode.Network.Socket

/-- Mini-protocol identifiers for node-to-node communication -/
inductive MiniProtocolId where
  | Handshake      -- 0x0000: Version negotiation
  | ChainSync      -- 0x0002: Block header synchronization
  | BlockFetch     -- 0x0003: Block body retrieval
  | TxSubmission2  -- 0x0004: Transaction propagation
  | KeepAlive      -- 0x0008: Connection health
  | PeerSharing    -- 0x000a: P2P peer discovery
  deriving Repr, BEq

def MiniProtocolId.toUInt16 : MiniProtocolId → UInt16
  | .Handshake     => 0x0000
  | .ChainSync     => 0x0002
  | .BlockFetch    => 0x0003
  | .TxSubmission2 => 0x0004
  | .KeepAlive     => 0x0008
  | .PeerSharing   => 0x000a

def MiniProtocolId.fromUInt16 (n : UInt16) : Option MiniProtocolId :=
  match n with
  | 0x0000 => some .Handshake
  | 0x0002 => some .ChainSync
  | 0x0003 => some .BlockFetch
  | 0x0004 => some .TxSubmission2
  | 0x0008 => some .KeepAlive
  | 0x000a => some .PeerSharing
  | _      => none

/-- Mode of communication: initiator or responder -/
inductive Mode where
  | Initiator   -- 0: We initiated this mini-protocol
  | Responder   -- 1: Remote peer initiated this mini-protocol
  deriving Repr, BEq

def Mode.toBit : Mode → UInt16
  | .Initiator => 0
  | .Responder => 1

def Mode.fromBit (n : UInt16) : Mode :=
  if n == 0 then .Initiator else .Responder

/-- Multiplexer header (8 bytes) -/
structure MuxHeader where
  timestamp : UInt32      -- Transmission time (microseconds)
  protocolId : MiniProtocolId
  mode : Mode
  payloadLength : UInt16  -- Size of payload in bytes
  deriving Repr

/-- Complete multiplexer frame (header + payload) -/
structure MuxFrame where
  header : MuxHeader
  payload : ByteArray

-- ==============
-- = Encoding   =
-- ==============

/-- Encode UInt32 as 4-byte big-endian -/
def encodeUInt32BE (n : UInt32) : ByteArray :=
  let b0 := UInt8.ofNat ((n >>> 24).toNat % 256)
  let b1 := UInt8.ofNat ((n >>> 16).toNat % 256)
  let b2 := UInt8.ofNat ((n >>>  8).toNat % 256)
  let b3 := UInt8.ofNat (n.toNat % 256)
  ⟨#[b0, b1, b2, b3]⟩

/-- Encode UInt16 as 2-byte big-endian -/
def encodeUInt16BE (n : UInt16) : ByteArray :=
  let b0 := UInt8.ofNat ((n >>> 8).toNat % 256)
  let b1 := UInt8.ofNat (n.toNat % 256)
  ⟨#[b0, b1]⟩

/-- Encode multiplexer header (8 bytes) -/
def encodeMuxHeader (h : MuxHeader) : ByteArray :=
  let ts := encodeUInt32BE h.timestamp
  let pid := h.protocolId.toUInt16
  let modeAndPid := (h.mode.toBit <<< 15) ||| pid  -- mode in high bit
  let pidBytes := encodeUInt16BE modeAndPid
  let lenBytes := encodeUInt16BE h.payloadLength
  ts ++ pidBytes ++ lenBytes

/-- Encode complete MUX frame (header + payload) -/
def encodeMuxFrame (frame : MuxFrame) : ByteArray :=
  encodeMuxHeader frame.header ++ frame.payload

-- ==============
-- = Decoding   =
-- ==============

/-- Decode UInt32 from 4-byte big-endian -/
def decodeUInt32BE (bs : ByteArray) : Option UInt32 :=
  if bs.size < 4 then none
  else
    let b0 := bs[0]!.toNat
    let b1 := bs[1]!.toNat
    let b2 := bs[2]!.toNat
    let b3 := bs[3]!.toNat
    some (UInt32.ofNat (b0 * 256^3 + b1 * 256^2 + b2 * 256 + b3))

/-- Decode UInt16 from 2-byte big-endian -/
def decodeUInt16BE (bs : ByteArray) : Option UInt16 :=
  if bs.size < 2 then none
  else
    let b0 := bs[0]!.toNat
    let b1 := bs[1]!.toNat
    some (UInt16.ofNat (b0 * 256 + b1))

/-- Decode multiplexer header (8 bytes) -/
def decodeMuxHeader (bs : ByteArray) : Option MuxHeader := do
  if bs.size < 8 then none

  let timestamp ← decodeUInt32BE (bs.extract 0 4)
  let modeAndPid ← decodeUInt16BE (bs.extract 4 6)
  let payloadLength ← decodeUInt16BE (bs.extract 6 8)

  let mode := Mode.fromBit (modeAndPid >>> 15)
  let pid := modeAndPid &&& 0x7FFF  -- Mask out mode bit
  let protocolId ← MiniProtocolId.fromUInt16 pid

  some {
    timestamp := timestamp,
    protocolId := protocolId,
    mode := mode,
    payloadLength := payloadLength
  }

/-- Decode complete MUX frame (header + payload) -/
def decodeMuxFrame (bs : ByteArray) : Option MuxFrame := do
  let header ← decodeMuxHeader bs
  let payloadStart := 8
  let payloadEnd := payloadStart + header.payloadLength.toNat

  if bs.size < payloadEnd then none

  let payload := bs.extract payloadStart payloadEnd
  some { header := header, payload := payload }

/-- Get current timestamp in microseconds (placeholder) -/
def getCurrentTimestamp : IO UInt32 := do
  -- TODO: Implement proper microsecond timestamp
  -- For now, return a placeholder
  pure 0

/-- Create a MUX frame for sending -/
def createFrame (protocolId : MiniProtocolId) (mode : Mode) (payload : ByteArray) : IO MuxFrame := do
  let timestamp ← getCurrentTimestamp
  pure {
    header := {
      timestamp := timestamp,
      protocolId := protocolId,
      mode := mode,
      payloadLength := UInt16.ofNat payload.size
    },
    payload := payload
  }

-- ==============================
-- = Flow Control & Buffering   =
-- ==============================

/-- Maximum MUX segment payload size (per Ouroboros spec) -/
def maxSegmentSize : Nat := 65535

/-- Per-protocol receive buffer for reassembling multi-frame messages -/
structure MuxChannel where
  protocolId : MiniProtocolId
  recvBuffer : ByteArray           -- Accumulated received bytes
  expectedSize : Option Nat         -- Expected total size (if known)

/-- Create an empty channel -/
def MuxChannel.empty (pid : MiniProtocolId) : MuxChannel :=
  { protocolId := pid, recvBuffer := ByteArray.empty, expectedSize := none }

/-- Append data to a channel's receive buffer -/
def MuxChannel.append (ch : MuxChannel) (data : ByteArray) : MuxChannel :=
  { ch with recvBuffer := ch.recvBuffer ++ data }

/-- Clear a channel's receive buffer -/
def MuxChannel.clear (ch : MuxChannel) : MuxChannel :=
  { ch with recvBuffer := ByteArray.empty, expectedSize := none }

/-- Multiplexer state managing multiple protocol channels -/
structure MuxState where
  channels : List MuxChannel

/-- Create initial MUX state with empty channels for all protocols -/
def MuxState.initial : MuxState :=
  { channels := [
      MuxChannel.empty .Handshake,
      MuxChannel.empty .ChainSync,
      MuxChannel.empty .BlockFetch,
      MuxChannel.empty .TxSubmission2,
      MuxChannel.empty .KeepAlive,
      MuxChannel.empty .PeerSharing
    ] }

/-- Get the channel for a specific protocol -/
def MuxState.getChannel (s : MuxState) (pid : MiniProtocolId) : MuxChannel :=
  s.channels.find? (fun ch => ch.protocolId == pid) |>.getD (MuxChannel.empty pid)

/-- Update the channel for a specific protocol -/
def MuxState.updateChannel (s : MuxState) (ch : MuxChannel) : MuxState :=
  { channels := s.channels.map fun c =>
      if c.protocolId == ch.protocolId then ch else c }

/-- Segment a large payload into MUX-sized frames -/
partial def segmentPayload (payload : ByteArray) (maxSize : Nat := maxSegmentSize) : List ByteArray :=
  if payload.size ≤ maxSize then [payload]
  else
    let rec go (offset : Nat) (acc : List ByteArray) : List ByteArray :=
      if offset ≥ payload.size then acc.reverse
      else
        let chunkEnd := min (offset + max maxSize 1) payload.size
        let segment := payload.extract offset chunkEnd
        go chunkEnd (segment :: acc)
    go 0 []

/-- Send a payload that may need to be split across multiple MUX frames -/
def sendSegmented (sock : Socket) (protocolId : MiniProtocolId) (mode : Mode)
    (payload : ByteArray) : IO (Except SocketError Unit) := do
  let segments := segmentPayload payload
  for segment in segments do
    let frame ← createFrame protocolId mode segment
    match ← socket_send sock (encodeMuxFrame frame) with
    | .error e => return .error e
    | .ok () => pure ()
  return .ok ()

end Cleanode.Network.Multiplexer
