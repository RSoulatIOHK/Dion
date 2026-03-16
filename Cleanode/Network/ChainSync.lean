import Cleanode.Network.Cbor
import Cleanode.Network.ByteArrayBuilder
import Cleanode.Network.Multiplexer
import Cleanode.Network.Socket

/-!
# Chain Sync Mini-Protocol

The Chain Sync protocol is used to synchronize blockchain headers between nodes.
In node-to-node mode, only headers are transferred (full blocks come via BlockFetch).

## Protocol Flow
1. Client finds intersection with server's chain using MsgFindIntersect
2. Client requests next update with MsgRequestNext
3. Server responds with:
   - MsgRollForward (new header) or
   - MsgRollBackward (rollback to common point) or
   - MsgAwaitReply (caught up, wait for new blocks)

## State Machine
- StIdle: Client can send MsgRequestNext, MsgFindIntersect, or MsgDone
- StCanAwait: Server can send MsgAwaitReply, MsgRollForward, or MsgRollBackward
- StMustReply: Server must send MsgRollForward or MsgRollBackward (no await)
- StIntersect: Server responds to intersection request

## References
- Ouroboros Network Spec Section 3.7 (Chain Sync)
- Protocol number: 2 (node-to-node)
-/

namespace Cleanode.Network.ChainSync

open Cleanode.Network.Cbor
open Cleanode.Network.Multiplexer
open Cleanode.Network.Socket

-- ==============
-- = Core Types =
-- ==============

/-- A point on the blockchain (slot number + block hash) -/
structure Point where
  slot : UInt64
  hash : ByteArray  -- Block hash (32 bytes for Cardano)

instance : Repr Point where
  reprPrec p _ := s!"Point(slot={p.slot}, hash={p.hash.size}B)"

/-- Special point representing the genesis block -/
def Point.genesis : Point :=
  { slot := 0, hash := ⟨#[]⟩ }

/-- The tip of the chain (point + block number) -/
structure Tip where
  point : Point
  blockNo : UInt64

instance : Repr Tip where
  reprPrec t _ := s!"Tip(slot={t.point.slot}, blockNo={t.blockNo}, hash={t.point.hash.size}B)"

/-- Block header (era-wrapped format) -/
structure Header where
  era : Nat  -- Era tag: 0=Byron, 1=Shelley, 2=Allegra, 3=Mary, 4=Alonzo, 5=Babbage, 6=Conway
  headerBytes : ByteArray  -- Raw CBOR bytes of the actual header (era-specific)
  -- TODO: Parse era-specific header formats

instance : Repr Header where
  reprPrec h _ := s!"Header(era={h.era}, bytes={h.headerBytes.size}B)"

-- ===================
-- = Protocol Messages =
-- ===================

/-- ChainSync protocol messages -/
inductive ChainSyncMessage where
  | MsgRequestNext                              -- [0] Request next update
  | MsgAwaitReply                               -- [1] Wait for next block
  | MsgRollForward (header : Header) (tip : Tip)  -- [2] New block
  | MsgRollBackward (point : Point) (tip : Tip)   -- [3] Rollback to point
  | MsgFindIntersect (points : List Point)         -- [4] Find common point
  | MsgIntersectFound (point : Point) (tip : Tip)  -- [5] Intersection found
  | MsgIntersectNotFound (tip : Tip)               -- [6] No intersection
  | MsgDone                                      -- [7] Terminate protocol
  deriving Repr

-- ==============
-- = Encoding   =
-- ==============

/-- Encode Point as CBOR array [slot, hash] -/
def encodePoint (p : Point) : ByteArray :=
  let header := encodeArrayHeader 2
  let slotEncoded := encodeUInt p.slot.toNat
  let hashEncoded := encodeBytes p.hash
  header ++ slotEncoded ++ hashEncoded

/-- Encode Tip as CBOR array [point, blockNo] -/
def encodeTip (t : Tip) : ByteArray :=
  let header := encodeArrayHeader 2
  let pointEncoded := encodePoint t.point
  let blockNoEncoded := encodeUInt t.blockNo.toNat
  header ++ pointEncoded ++ blockNoEncoded

/-- Encode Header (era-wrapped format) -/
def encodeHeader (h : Header) : ByteArray :=
  let header := encodeArrayHeader 2
  let eraEncoded := encodeUInt h.era
  header ++ eraEncoded ++ h.headerBytes

/-- Encode ChainSync message -/
def encodeChainSyncMessage : ChainSyncMessage → ByteArray
  | .MsgRequestNext =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 0
      arr ++ msgId
  | .MsgAwaitReply =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 1
      arr ++ msgId
  | .MsgRollForward header tip =>
      let arr := encodeArrayHeader 3
      let msgId := encodeUInt 2
      let headerEnc := encodeHeader header
      let tipEnc := encodeTip tip
      arr ++ msgId ++ headerEnc ++ tipEnc
  | .MsgRollBackward point tip =>
      let arr := encodeArrayHeader 3
      let msgId := encodeUInt 3
      let pointEnc := encodePoint point
      let tipEnc := encodeTip tip
      arr ++ msgId ++ pointEnc ++ tipEnc
  | .MsgFindIntersect points =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 4
      let pointsArr := encodeArrayHeader points.length
      let pointsEnc := ByteArrayBuilder.foldEncode points encodePoint
      arr ++ msgId ++ pointsArr ++ pointsEnc
  | .MsgIntersectFound point tip =>
      let arr := encodeArrayHeader 3
      let msgId := encodeUInt 5
      let pointEnc := encodePoint point
      let tipEnc := encodeTip tip
      arr ++ msgId ++ pointEnc ++ tipEnc
  | .MsgIntersectNotFound tip =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 6
      let tipEnc := encodeTip tip
      arr ++ msgId ++ tipEnc
  | .MsgDone =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 7
      arr ++ msgId

-- ==============
-- = Decoding   =
-- ==============

/-- Decode Point from CBOR (handles both normal points and origin/genesis as []) -/
def decodePoint (bs : ByteArray) : Option (DecodeResult Point) := do
  let r1 ← decodeArrayHeader bs

  -- Origin/genesis is encoded as empty array []
  if r1.value == 0 then
    some {
      value := Point.genesis,
      remaining := r1.remaining
    }
  else if r1.value == 2 then do
    let r2 ← decodeUInt r1.remaining
    let slot := UInt64.ofNat r2.value

    let r3 ← decodeBytes r2.remaining
    let hash := r3.value

    some {
      value := { slot := slot, hash := hash },
      remaining := r3.remaining
    }
  else
    none

/-- Decode Tip from CBOR -/
def decodeTip (bs : ByteArray) : Option (DecodeResult Tip) := do
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  let r2 ← decodePoint r1.remaining
  let point := r2.value

  let r3 ← decodeUInt r2.remaining
  let blockNo := UInt64.ofNat r3.value

  some {
    value := { point := point, blockNo := blockNo },
    remaining := r3.remaining
  }

/-- Decode Header from CBOR (era-wrapped format: [era, wrappedHeader]) -/
def decodeHeader (bs : ByteArray) : Option (DecodeResult Header) := do
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  let r2 ← decodeUInt r1.remaining
  let era := r2.value

  -- Calculate how many bytes the header consumes
  let headerStart := r2.remaining

  -- Skip over the wrapped header data (which varies by era)
  let afterHeader ← skipCborValue r2.remaining

  -- Extract just the wrapped header bytes (not including the era prefix)
  let headerLen := headerStart.size - afterHeader.size
  let headerBytes := headerStart.extract 0 headerLen

  some {
    value := { era := era, headerBytes := headerBytes },
    remaining := afterHeader
  }

/-- Decode ChainSync message -/
def decodeChainSyncMessage (bs : ByteArray) : Option ChainSyncMessage := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining

  match r2.value with
  | 0 => if r1.value == 1 then some .MsgRequestNext else none
  | 1 => if r1.value == 1 then some .MsgAwaitReply else none
  | 2 => do  -- MsgRollForward
      if r1.value != 3 then none
      let r3 ← decodeHeader r2.remaining
      let r4 ← decodeTip r3.remaining
      some (.MsgRollForward r3.value r4.value)
  | 3 => do  -- MsgRollBackward
      if r1.value != 3 then none
      let r3 ← decodePoint r2.remaining
      let r4 ← decodeTip r3.remaining
      some (.MsgRollBackward r3.value r4.value)
  | 4 => do  -- MsgFindIntersect
      if r1.value != 2 then none
      let r3 ← decodeArrayHeader r2.remaining
      let numPoints := r3.value
      let mut points : List Point := []
      let mut remaining := r3.remaining
      for _ in List.range numPoints do
        match decodePoint remaining with
        | some result =>
            points := points ++ [result.value]
            remaining := result.remaining
        | none => break
      some (.MsgFindIntersect points)
  | 5 => do  -- MsgIntersectFound
      if r1.value != 3 then none
      let r3 ← decodePoint r2.remaining
      let r4 ← decodeTip r3.remaining
      some (.MsgIntersectFound r3.value r4.value)
  | 6 => do  -- MsgIntersectNotFound
      if r1.value != 2 then none
      let r3 ← decodeTip r2.remaining
      some (.MsgIntersectNotFound r3.value)
  | 7 => if r1.value == 1 then some .MsgDone else none
  | _ => none

-- ==============
-- = Client API =
-- ==============

/-- Send ChainSync message over socket -/
def sendChainSync (sock : Socket) (msg : ChainSyncMessage) : IO (Except SocketError Unit) := do
  let payload := encodeChainSyncMessage msg
  let frame ← createFrame .ChainSync .Initiator payload
  let frameBytes := encodeMuxFrame frame
  socket_send sock frameBytes

/-- Receive ChainSync message from socket -/
def receiveChainSync (sock : Socket) : IO (Except SocketError (Option ChainSyncMessage)) := do
  -- Receive MUX frame header first
  match ← socket_receive sock 8 with
  | .error e => return .error e
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none => return .ok none
      | some header => do
          -- Receive payload
          match ← socket_receive sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error e
          | .ok payload => return .ok (decodeChainSyncMessage payload)

/-- Find intersection with genesis (bootstrap from scratch) -/
def findIntersectGenesis : ChainSyncMessage :=
  .MsgFindIntersect [Point.genesis]

/-- Helper: Convert single hex char to value -/
def hexCharToNat (c : Char) : Nat :=
  if c.isDigit then c.toNat - '0'.toNat
  else if c.toLower >= 'a' && c.toLower <= 'f' then c.toLower.toNat - 'a'.toNat + 10
  else 0

/-- Helper: Convert hex string to ByteArray -/
def hexToBytes (hex : String) : ByteArray :=
  let chars := hex.toList
  let rec loop : List Char → List UInt8
    | [] => []
    | [_] => []  -- Odd length, ignore last char
    | hi :: lo :: rest =>
        let hiVal := hexCharToNat hi
        let loVal := hexCharToNat lo
        UInt8.ofNat (hiVal * 16 + loVal) :: loop rest
  ByteArray.mk (loop chars).toArray

/-- Create a checkpoint point from slot and hex hash -/
def createCheckpoint (slot : UInt64) (hashHex : String) : Point :=
  { slot := slot, hash := hexToBytes hashHex }

/-- Find intersection from a known checkpoint (with genesis fallback) -/
def findIntersectFromCheckpoint (checkpoint : Point) : ChainSyncMessage :=
  .MsgFindIntersect [checkpoint, Point.genesis]

/-- Find intersection at a specific point (e.g. the tip) -/
def findIntersectAt (point : Point) : ChainSyncMessage :=
  .MsgFindIntersect [point]

/-- Query tip by sending FindIntersect with empty points (server responds with MsgIntersectNotFound + tip) -/
def findIntersectTip : ChainSyncMessage :=
  .MsgFindIntersect []

/-- Request next block -/
def requestNext : ChainSyncMessage :=
  .MsgRequestNext

end Cleanode.Network.ChainSync
