import Cleanode.Network.Cbor
import Cleanode.Network.Multiplexer
import Cleanode.Network.Socket

/-!
# Ouroboros Handshake Protocol

The handshake protocol performs version negotiation when two nodes connect.
It's the first mini-protocol that runs on a new connection.

## Protocol Flow
1. Client sends ProposeVersions with supported versions and parameters
2. Server responds with either:
   - AcceptVersion (one of the proposed versions)
   - Refuse (rejection reason)

## Version Parameters
For node-to-node communication:
- NetworkMagic: identifies the network (mainnet = 764824073)
- InitiatorAndResponderDiffusionMode: P2P mode flag
- PeerSharing: enable peer discovery
- Query: enable ledger queries

## References
- Ouroboros Network Spec Section 4.1 (Handshake)
- https://github.com/IntersectMBO/ouroboros-network/tree/master/ouroboros-network-protocols
-/

namespace Cleanode.Network.Handshake

open Cleanode.Network.Cbor
open Cleanode.Network.Multiplexer
open Cleanode.Network.Socket

/-- Cardano network magic numbers -/
inductive NetworkMagic where
  | Mainnet    -- 764824073
  | Preprod    -- 1
  | Preview    -- 2
  | Custom (n : Nat)
  deriving Repr, BEq

def NetworkMagic.toNat : NetworkMagic → Nat
  | .Mainnet => 764824073
  | .Preprod => 1
  | .Preview => 2
  | .Custom n => n

/-- Protocol version number -/
structure VersionNumber where
  value : Nat
  deriving Repr, BEq

/-- Node-to-node version parameters -/
structure VersionData where
  networkMagic : NetworkMagic
  initiatorAndResponderDiffusionMode : Bool  -- P2P mode
  peerSharing : Nat                          -- 0 or 1: peer discovery
  query : Bool                               -- Enable ledger queries
  deriving Repr

/-- Handshake protocol messages -/
inductive HandshakeMessage where
  | ProposeVersions (versions : List (VersionNumber × VersionData))
  | AcceptVersion (version : VersionNumber) (versionData : VersionData)
  | Refuse (reason : String)
  deriving Repr

-- ==============
-- = Encoding   =
-- ==============

/-- Encode version parameters as CBOR array (v14+ format) -/
def encodeVersionData (vd : VersionData) : ByteArray :=
  -- CBOR array with 4 elements: [networkMagic, diffusionMode, peerSharing, query]
  let header := encodeArrayHeader 4
  let val1 := encodeUInt vd.networkMagic.toNat
  let val2 := encodeBool vd.initiatorAndResponderDiffusionMode
  let val3 := encodeUInt vd.peerSharing
  let val4 := encodeBool vd.query
  header ++ val1 ++ val2 ++ val3 ++ val4

/-- Encode a single version proposal (version number + version data) -/
def encodeVersionProposal (vn : VersionNumber) (vd : VersionData) : ByteArray :=
  let vnEncoded := encodeUInt vn.value
  let vdEncoded := encodeVersionData vd
  vnEncoded ++ vdEncoded

/-- Encode ProposeVersions message -/
def encodeProposeVersions (versions : List (VersionNumber × VersionData)) : ByteArray :=
  -- Message tag 0 (ProposeVersions)
  let tag := encodeArrayHeader 2
  let msgId := encodeUInt 0

  -- Encode version map
  let mapHeader := encodeMapHeader versions.length
  let versionsEncoded := versions.foldl (fun acc (vn, vd) =>
    acc ++ encodeVersionProposal vn vd
  ) ⟨#[]⟩

  (((tag ++ msgId) ++ mapHeader) ++ versionsEncoded)

/-- Encode AcceptVersion message -/
def encodeAcceptVersion (vn : VersionNumber) (vd : VersionData) : ByteArray :=
  -- Message tag 1 (AcceptVersion)
  let tag := encodeArrayHeader 2
  let msgId := encodeUInt 1
  let vnEncoded := encodeUInt vn.value
  let vdEncoded := encodeVersionData vd

  tag ++ msgId ++ vnEncoded ++ vdEncoded

/-- Encode Refuse message -/
def encodeRefuse (reason : String) : ByteArray :=
  -- Message tag 2 (Refuse)
  let tag := encodeArrayHeader 2
  let msgId := encodeUInt 2
  let reasonBytes := reason.toUTF8
  let reasonEncoded := encodeBytes reasonBytes

  tag ++ msgId ++ reasonEncoded

/-- Encode handshake message -/
def encodeHandshakeMessage : HandshakeMessage → ByteArray
  | .ProposeVersions versions => encodeProposeVersions versions
  | .AcceptVersion vn vd => encodeAcceptVersion vn vd
  | .Refuse reason => encodeRefuse reason

-- ==============
-- = Decoding   =
-- ==============

/-- Decode version data from CBOR array (v14+ format) -/
partial def decodeVersionData (bs : ByteArray) : Option (DecodeResult VersionData) := do
  -- Decode array header (expecting 4 elements)
  let r1 ← decodeArrayHeader bs
  if r1.value != 4 then none

  -- NetworkMagic
  let r2 ← decodeUInt r1.remaining
  let networkMagic := NetworkMagic.Custom r2.value

  -- DiffusionMode
  let r3 ← decodeBool r2.remaining
  let diffusionMode := r3.value

  -- PeerSharing (0 or 1)
  let r4 ← decodeUInt r3.remaining
  let peerSharing := r4.value

  -- Query
  let r5 ← decodeBool r4.remaining
  let query := r5.value

  some {
    value := {
      networkMagic := networkMagic,
      initiatorAndResponderDiffusionMode := diffusionMode,
      peerSharing := peerSharing,
      query := query
    },
    remaining := r5.remaining
  }

/-- Decode handshake message (v14+ format) -/
partial def decodeHandshakeMessage (bs : ByteArray) : Option HandshakeMessage := do
  -- Decode array header
  let r1 ← decodeArrayHeader bs

  -- Decode message ID
  let r2 ← decodeUInt r1.remaining

  match r2.value with
  | 0 => none  -- ProposeVersions (complex, TODO)
  | 1 => do    -- AcceptVersion: [1, versionNumber, versionData]
      -- For v14+, this is a 3-element array
      if r1.value != 3 then none
      let r3 ← decodeUInt r2.remaining
      let vn := VersionNumber.mk r3.value
      let r4 ← decodeVersionData r3.remaining
      some (.AcceptVersion vn r4.value)
  | 2 => do    -- Refuse: [2, refuseReason]
      -- For v14+, this is a 2-element array
      if r1.value != 2 then none
      -- Refuse format: [2, refuseReason]
      -- refuseReason can be: [0, versions] | [1, version, text] | [2, version, text]
      -- For now, just decode as a generic refusal
      some (.Refuse "Version negotiation failed - versions not supported by remote peer")
  | _ => none

-- ==============
-- = Helpers    =
-- ==============

/-- Create a handshake proposal for a specific network -/
def createProposal (network : NetworkMagic) : HandshakeMessage :=
  let versionData : VersionData := {
    networkMagic := network,
    initiatorAndResponderDiffusionMode := false,  -- False = full duplex (both initiator and responder)
    peerSharing := 1,  -- 1 = engage in peer sharing
    query := false
  }
  let versions := [
    (VersionNumber.mk 14, versionData),  -- Try versions 14 and 15 (>= v14 encoding)
    (VersionNumber.mk 15, versionData)
  ]
  .ProposeVersions versions

/-- Create a standard mainnet handshake proposal -/
def createMainnetProposal : HandshakeMessage :=
  createProposal .Mainnet

/-- Create a preprod handshake proposal -/
def createPreprodProposal : HandshakeMessage :=
  createProposal .Preprod

/-- Create a preview handshake proposal -/
def createPreviewProposal : HandshakeMessage :=
  createProposal .Preview

/-- Send handshake message over a socket -/
def sendHandshake (sock : Socket) (msg : HandshakeMessage) : IO (Except SocketError Unit) := do
  let payload := encodeHandshakeMessage msg
  let frame ← createFrame .Handshake .Initiator payload
  let frameBytes := encodeMuxFrame frame
  socket_send sock frameBytes

/-- Receive handshake response -/
def receiveHandshake (sock : Socket) : IO (Except SocketError (Option HandshakeMessage)) := do
  -- Receive MUX frame (8-byte header first)
  match ← socket_receive sock 8 with
  | .error e => return .error e
  | .ok headerBytes => do
      -- Decode header to get payload length
      match decodeMuxHeader headerBytes with
      | none => return .ok none
      | some header => do
          -- Receive payload
          match ← socket_receive sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error e
          | .ok payload => return .ok (decodeHandshakeMessage payload)

end Cleanode.Network.Handshake
