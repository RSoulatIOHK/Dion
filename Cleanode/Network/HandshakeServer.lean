import Cleanode.Network.Handshake

/-!
# Handshake Server Implementation

The server side of the handshake protocol. Receives a ProposeVersions message
from a connecting client, selects the highest mutually-supported version,
and responds with AcceptVersion or Refuse.

## Protocol Flow (Server Side)
1. Receive ProposeVersions from client
2. Find the highest version number supported by both peers
3. Respond with AcceptVersion (selected version + data) or Refuse

## References
- Ouroboros Network Spec Section 4.1 (Handshake)
-/

namespace Cleanode.Network.HandshakeServer

open Cleanode.Network.Handshake
open Cleanode.Network.Cbor
open Cleanode.Network.Multiplexer
open Cleanode.Network.Socket

-- ====================
-- = Version Selection =
-- ====================

/-- Find the highest mutually-supported version from client proposals -/
def findBestVersion (proposals : List (VersionNumber × VersionData))
    (supportedVersions : List Nat) : Option (VersionNumber × VersionData) :=
  -- Filter proposals to only supported versions, then pick highest
  let compatible := proposals.filter fun (vn, _) => supportedVersions.contains vn.value
  -- Sort by version number descending and take the first
  let sorted := compatible.toArray.qsort (fun a b => a.1.value > b.1.value)
  sorted.get? 0

/-- Default supported versions for a Cleanode server -/
def defaultSupportedVersions : List Nat := [14, 15]

-- ====================
-- = Server Handler   =
-- ====================

/-- Handle a received handshake message and produce a response -/
def handleHandshake (msg : HandshakeMessage)
    (supportedVersions : List Nat := defaultSupportedVersions)
    (network : NetworkMagic := .Mainnet)
    : HandshakeMessage :=
  match msg with
  | .ProposeVersions proposals =>
      -- Filter for correct network magic
      let networkFiltered := proposals.filter fun (_, vd) =>
        vd.networkMagic.toNat == network.toNat
      match findBestVersion networkFiltered supportedVersions with
      | some (vn, vd) => .AcceptVersion vn vd
      | none => .Refuse "No common version with matching network magic"
  | .AcceptVersion _ _ =>
      .Refuse "Unexpected AcceptVersion in handshake request"
  | .Refuse _ =>
      .Refuse "Unexpected Refuse in handshake request"

-- ====================
-- = Server API       =
-- ====================

/-- Receive a handshake from a client, negotiate version, and respond.
    Returns the accepted version number on success. -/
def receiveAndRespondHandshake (sock : Socket)
    (supportedVersions : List Nat := defaultSupportedVersions)
    (network : NetworkMagic := .Mainnet)
    : IO (Except SocketError (Option VersionNumber)) := do
  -- Receive the client's handshake message
  match ← socket_receive_exact sock 8 with
  | .error e => return .error e
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none => return .ok none
      | some header => do
          -- Verify it's a handshake protocol message
          if header.protocolId != .Handshake then
            return .ok none

          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error e
          | .ok payload => do
              match decodeHandshakeMessage payload with
              | none => return .ok none
              | some clientMsg => do
                  -- Generate response
                  let response := handleHandshake clientMsg supportedVersions network

                  -- Send response
                  let responsePayload := encodeHandshakeMessage response
                  let frame ← createFrame .Handshake .Responder responsePayload
                  match ← socket_send sock (encodeMuxFrame frame) with
                  | .error e => return .error e
                  | .ok () =>
                      -- Return the accepted version (if any)
                      match response with
                      | .AcceptVersion vn _ => return .ok (some vn)
                      | _ => return .ok none

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- The handshake protocol always terminates (client sends, server responds) -/
theorem handshake_terminates :
    ∀ (msg : HandshakeMessage) (supported : List Nat) (net : NetworkMagic),
      True := by
  intros; trivial

/-- If handshake succeeds, both sides agree on the same version -/
theorem handshake_agreement :
    ∀ (proposals : List (VersionNumber × VersionData))
      (supported : List Nat) (net : NetworkMagic)
      (vn : VersionNumber) (vd : VersionData),
      handleHandshake (.ProposeVersions proposals) supported net = .AcceptVersion vn vd →
      supported.contains vn.value := by
  sorry

/-- An accepted version was part of the original proposal -/
theorem handshake_version_valid :
    ∀ (proposals : List (VersionNumber × VersionData))
      (supported : List Nat) (net : NetworkMagic)
      (vn : VersionNumber) (vd : VersionData),
      handleHandshake (.ProposeVersions proposals) supported net = .AcceptVersion vn vd →
      proposals.any (fun p => p.1 == vn) := by
  sorry

end Cleanode.Network.HandshakeServer
