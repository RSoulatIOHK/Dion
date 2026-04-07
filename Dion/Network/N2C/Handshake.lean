import Dion.Network.Cbor
import Dion.Network.Handshake
import Dion.Network.N2C.Mux

/-!
# Node-to-Client Handshake

The N2C handshake uses the same CBOR message format as N2N but:
- Supported versions: 16, 17, 18 (not 14, 15)
- Version data is simpler: [networkMagic, query] (no diffusionMode, peerSharing)

## Protocol Flow
1. `cardano-cli` sends ProposeVersions with N2C versions {16, 17, 18}
2. Dion responds with AcceptVersion (highest mutual version)

## References
- Ouroboros Network Spec Section 4.1
-/

namespace Dion.Network.N2C.Handshake

open Dion.Network.Cbor
open Dion.Network.Handshake (NetworkMagic VersionNumber)
open Dion.Network.N2C.Mux
open Dion.Network.N2C.MiniProtocolId
open Dion.Network.Multiplexer (Mode)
open Dion.Network.Socket

-- ====================
-- = Types            =
-- ====================

/-- N2C version data: simpler than N2N (only networkMagic + query) -/
structure N2CVersionData where
  networkMagic : Nat
  query : Bool
  deriving Repr

/-- N2C wire version offset (0x8000) — N2C versions are encoded as 32768 + logicalVersion -/
def n2cVersionOffset : Nat := 32768

/-- Default N2C supported versions (wire format: 32768 + logical version) -/
def n2cSupportedVersions : List Nat := [32784, 32785, 32786]  -- v16, v17, v18

-- ====================
-- = Encoding         =
-- ====================

/-- Encode N2C version data as CBOR: [networkMagic, query] -/
def encodeN2CVersionData (vd : N2CVersionData) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt vd.networkMagic ++ encodeBool vd.query

/-- Encode AcceptVersion message: [1, version, versionData] -/
def encodeN2CAcceptVersion (version : Nat) (vd : N2CVersionData) : ByteArray :=
  encodeArrayHeader 3 ++ encodeUInt 1 ++ encodeUInt version ++ encodeN2CVersionData vd

/-- Encode Refuse message: [2, [reasonTag, reasonText]] -/
def encodeN2CRefuse (reason : String) : ByteArray :=
  let refuseReason := encodeArrayHeader 2 ++ encodeUInt 0 ++ encodeBytes reason.toUTF8
  encodeArrayHeader 2 ++ encodeUInt 2 ++ refuseReason

-- ====================
-- = Decoding         =
-- ====================

/-- Decode N2C version data from CBOR.
    v16+: [networkMagic, query] (2-element array)
    v9-v15: just networkMagic (bare uint) -/
partial def decodeN2CVersionData (bs : ByteArray) : Option (DecodeResult N2CVersionData) := do
  if bs.size > 0 && bs[0]! == 0x82 then
    -- Array format (v16+): [networkMagic, query]
    let r1 ← decodeArrayHeader bs
    let r2 ← decodeUInt r1.remaining
    let r3 ← decodeBool r2.remaining
    some { value := { networkMagic := r2.value, query := r3.value }, remaining := r3.remaining }
  else
    -- Bare uint format (v9-v15): just networkMagic
    let r1 ← decodeUInt bs
    some { value := { networkMagic := r1.value, query := false }, remaining := r1.remaining }

/-- Decode a single N2C version proposal (version number + version data) -/
partial def decodeN2CVersionProposal (bs : ByteArray)
    : Option (DecodeResult (Nat × N2CVersionData)) := do
  let r1 ← decodeUInt bs
  let r2 ← decodeN2CVersionData r1.remaining
  some { value := (r1.value, r2.value), remaining := r2.remaining }

/-- Decode N2C ProposeVersions: [0, {version: versionData, ...}] -/
partial def decodeN2CProposeVersions (bs : ByteArray)
    : Option (List (Nat × N2CVersionData)) := do
  -- [arrayHeader(2), msgId(0), mapHeader(n), ...proposals]
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining
  if r2.value != 0 then none  -- Must be MsgProposeVersions (tag 0)
  let r3 ← decodeMapHeader r2.remaining
  let count := r3.value
  let mut remaining := r3.remaining
  let mut proposals : List (Nat × N2CVersionData) := []
  let mut failed := false
  for _ in List.range count do
    if !failed then
      match decodeN2CVersionProposal remaining with
      | some r =>
        proposals := r.value :: proposals
        remaining := r.remaining
      | none => failed := true
  if failed then none else some proposals.reverse

-- ====================
-- = Server Handler   =
-- ====================

/-- Find the highest mutually-supported N2C version with matching network magic -/
def findBestN2CVersion (proposals : List (Nat × N2CVersionData))
    (supportedVersions : List Nat) (networkMagic : Nat)
    : Option (Nat × N2CVersionData) :=
  let compatible := proposals.filter fun (v, vd) =>
    supportedVersions.contains v && vd.networkMagic == networkMagic
  let sorted := compatible.toArray.qsort (fun a b => a.1 > b.1)
  sorted[0]?

/-- Receive N2C handshake from cardano-cli and respond.
    Returns the accepted version on success. -/
def receiveAndRespondN2CHandshake (sock : Socket)
    (network : NetworkMagic)
    : IO (Except SocketError (Option Nat)) := do
  -- Read the MUX frame (handshake is on protocol 0)
  match ← receiveN2CMuxFrame sock with
  | .error e => return .error e
  | .ok frame =>
    if frame.header.protocolId != .Handshake then
      return .ok none

    -- Decode the ProposeVersions
    IO.eprintln s!"[n2c-hs] Received handshake payload: {frame.payload.size} bytes"
    match decodeN2CProposeVersions frame.payload with
    | none =>
      IO.eprintln s!"[n2c-hs] Failed to decode ProposeVersions"
      -- Dump first 32 bytes as hex for debugging
      let hexBytes := frame.payload.toList.take 32 |>.map fun b =>
        let hi := b.toNat / 16
        let lo := b.toNat % 16
        let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
        String.mk [toHex hi, toHex lo]
      IO.eprintln s!"[n2c-hs] Hex: {String.intercalate " " hexBytes}"
      return .ok none
    | some proposals =>
      IO.eprintln s!"[n2c-hs] Decoded {proposals.length} version proposals"
      for (v, vd) in proposals do
        IO.eprintln s!"[n2c-hs]   version={v}, magic={vd.networkMagic}, query={vd.query}"
      -- Find best version
      IO.eprintln s!"[n2c-hs] Looking for versions {n2cSupportedVersions} with magic {network.toNat}"
      match findBestN2CVersion proposals n2cSupportedVersions network.toNat with
      | none =>
        IO.eprintln "[n2c-hs] No common version found, refusing"
        -- Refuse
        let payload := encodeN2CRefuse "No common N2C version with matching network magic"
        match ← sendN2CPayload sock .Handshake .Responder payload with
        | .error e => return .error e
        | .ok () => return .ok none
      | some (version, vd) =>
        -- Accept
        let payload := encodeN2CAcceptVersion version vd
        match ← sendN2CPayload sock .Handshake .Responder payload with
        | .error e => return .error e
        | .ok () => return .ok (some version)

end Dion.Network.N2C.Handshake
