import Cleanode.Network.Cbor
import Cleanode.Network.Multiplexer
import Cleanode.Network.Socket

/-!
# PeerSharing Mini-Protocol

The PeerSharing protocol allows nodes to exchange peer addresses for
decentralized peer discovery. The client requests a number of peers
from the server, which responds with known peer addresses.

## Protocol Flow
1. Client sends MsgShareRequest with desired peer count
2. Server responds with MsgSharePeers containing peer addresses
3. Repeat or terminate with MsgDone

## State Machine
- StIdle: Client can send MsgShareRequest or MsgDone
- StBusy: Server must respond with MsgSharePeers
- StDone: Protocol terminated

## References
- Ouroboros Network Spec (Peer Sharing)
- Protocol number: 10 (0x000a, node-to-node)
-/

namespace Cleanode.Network.PeerSharing

open Cleanode.Network.Cbor
open Cleanode.Network.Multiplexer
open Cleanode.Network.Socket

-- ==============
-- = Core Types =
-- ==============

/-- A peer address for sharing between nodes -/
structure PeerAddress where
  host : String
  port : UInt16
  deriving Repr, BEq

instance : ToString PeerAddress where
  toString p := s!"{p.host}:{p.port}"

-- ===================
-- = Protocol Messages =
-- ===================

/-- PeerSharing protocol messages -/
inductive PeerSharingMessage where
  | MsgShareRequest (amount : UInt8)            -- [0, amount] Client requests peers
  | MsgSharePeers (peers : List PeerAddress)    -- [1, peers]  Server responds with peers
  | MsgDone                                     -- [2]         Terminate protocol
  deriving Repr

-- ==============
-- = Encoding   =
-- ==============

/-- Encode a PeerAddress as CBOR array [port, host_bytes] -/
def encodePeerAddress (addr : PeerAddress) : ByteArray :=
  let header := encodeArrayHeader 2
  let portEnc := encodeUInt addr.port.toNat
  let hostEnc := encodeBytes addr.host.toUTF8
  header ++ portEnc ++ hostEnc

/-- Encode PeerSharing message -/
def encodePeerSharingMessage : PeerSharingMessage → ByteArray
  | .MsgShareRequest amount =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 0
      let amountEnc := encodeUInt amount.toNat
      arr ++ msgId ++ amountEnc
  | .MsgSharePeers peers =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 1
      let peersArr := encodeArrayHeader peers.length
      let peersEnc := peers.foldl (fun acc p => acc ++ encodePeerAddress p) ⟨#[]⟩
      arr ++ msgId ++ peersArr ++ peersEnc
  | .MsgDone =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 2
      arr ++ msgId

-- ==============
-- = Decoding   =
-- ==============

/-- Convert a 32-bit integer to an IPv4 dotted-decimal string -/
def uint32ToIPv4 (n : Nat) : String :=
  let a := (n / (256 * 256 * 256)) % 256
  let b := (n / (256 * 256)) % 256
  let c := (n / 256) % 256
  let d := n % 256
  s!"{a}.{b}.{c}.{d}"

/-- Convert four 32-bit integers to an IPv6 colon-hex string -/
def uint32x4ToIPv6 (a b c d : Nat) : String :=
  let fmt (n : Nat) : String :=
    let hi := (n / 65536) % 65536
    let lo := n % 65536
    s!"{Nat.toDigits 16 hi |>.asString}:{Nat.toDigits 16 lo |>.asString}"
  s!"{fmt a}:{fmt b}:{fmt c}:{fmt d}"

/-- Decode a PeerAddress from CBOR in Cardano wire format.
    IPv4: [0, uint32_ip, port]  (array of 3)
    IPv6: [1, uint32_a, uint32_b, uint32_c, uint32_d, port]  (array of 6) -/
def decodePeerAddress (bs : ByteArray) : Option (DecodeResult PeerAddress) := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining  -- address type: 0=IPv4, 1=IPv6
  match r2.value with
  | 0 => do  -- IPv4: [0, uint32, port]
      if r1.value != 3 then none
      let r3 ← decodeUInt r2.remaining  -- IP as uint32
      let r4 ← decodeUInt r3.remaining  -- port
      let host := uint32ToIPv4 r3.value
      let port := UInt16.ofNat r4.value
      some { value := { host := host, port := port }, remaining := r4.remaining }
  | 1 => do  -- IPv6: [1, u32, u32, u32, u32, port]
      if r1.value != 6 then none
      let ra ← decodeUInt r2.remaining
      let rb ← decodeUInt ra.remaining
      let rc ← decodeUInt rb.remaining
      let rd ← decodeUInt rc.remaining
      let rPort ← decodeUInt rd.remaining
      let host := uint32x4ToIPv6 ra.value rb.value rc.value rd.value
      let port := UInt16.ofNat rPort.value
      some { value := { host := host, port := port }, remaining := rPort.remaining }
  | _ => none

/-- Decode a list of PeerAddresses from either a fixed or indefinite-length CBOR array.
    Cardano uses indefinite-length arrays (0x9F ... 0xFF) for peer lists. -/
def decodePeerAddressList (bs : ByteArray) : Option (DecodeResult (List PeerAddress)) := do
  if bs.size == 0 then none
  if bs[0]! == 0x9F then
    -- Indefinite-length array: read until 0xFF break code
    let mut remaining := bs.extract 1 bs.size
    let mut peers : List PeerAddress := []
    let mut done := false
    while !done do
      if remaining.size == 0 then
        done := true
      else if remaining[0]! == 0xFF then
        remaining := remaining.extract 1 remaining.size
        done := true
      else
        match decodePeerAddress remaining with
        | some r =>
            peers := peers ++ [r.value]
            remaining := r.remaining
        | none => done := true  -- skip unparseable entries
    some { value := peers, remaining := remaining }
  else
    -- Fixed-length array
    let r1 ← decodeArrayHeader bs
    let count := r1.value
    let mut remaining := r1.remaining
    let mut peers : List PeerAddress := []
    for _ in List.range count do
      let r ← decodePeerAddress remaining
      peers := peers ++ [r.value]
      remaining := r.remaining
    some { value := peers, remaining := remaining }

/-- Decode PeerSharing message -/
def decodePeerSharingMessage (bs : ByteArray) : Option PeerSharingMessage := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining

  match r2.value with
  | 0 => do  -- MsgShareRequest
      if r1.value != 2 then none
      let r3 ← decodeUInt r2.remaining
      some (.MsgShareRequest (UInt8.ofNat r3.value))
  | 1 => do  -- MsgSharePeers
      if r1.value != 2 then none
      let r3 ← decodePeerAddressList r2.remaining
      some (.MsgSharePeers r3.value)
  | 2 => if r1.value == 1 then some .MsgDone else none
  | _ => none

-- ==============
-- = Client API =
-- ==============

/-- Send PeerSharing message over socket -/
def sendPeerSharing (sock : Socket) (msg : PeerSharingMessage) : IO (Except SocketError Unit) := do
  let payload := encodePeerSharingMessage msg
  let frame ← createFrame .PeerSharing .Initiator payload
  let frameBytes := encodeMuxFrame frame
  socket_send sock frameBytes

/-- Receive PeerSharing message from socket -/
def receivePeerSharing (sock : Socket) : IO (Except SocketError (Option PeerSharingMessage)) := do
  match ← socket_receive sock 8 with
  | .error e => return .error e
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none => return .ok none
      | some header => do
          match ← socket_receive sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error e
          | .ok payload => return .ok (decodePeerSharingMessage payload)

/-- Request peers from a connected node -/
def requestPeers (amount : UInt8 := 10) : PeerSharingMessage :=
  .MsgShareRequest amount

end Cleanode.Network.PeerSharing
