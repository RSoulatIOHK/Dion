import Lean

/-!
# Cardano TextEnvelope Key File Parser

Cardano key files use the "TextEnvelope" JSON format:
```json
{
    "type": "VrfSigningKey_PraosVRF",
    "description": "VRF Signing Key",
    "cborHex": "5820..."
}
```

The `cborHex` field contains CBOR-wrapped key material.

## Key Types
- `PaymentSigningKeyShelley_ed25519` — Ed25519 signing key (32 bytes)
- `PaymentVerificationKeyShelley_ed25519` — Ed25519 verification key (32 bytes)
- `StakePoolSigningKey_ed25519` — Cold pool signing key (32 bytes)
- `StakePoolVerificationKey_ed25519` — Cold pool verification key (32 bytes)
- `VrfSigningKey_PraosVRF` — VRF signing key (32 bytes seed + 32 bytes vkey = 64 bytes)
- `VrfVerificationKey_PraosVRF` — VRF verification key (32 bytes)
- `KesSigningKey_ed25519_kes_2^6` — KES signing key (Sum-KES depth 6)
- `KesVerificationKey_ed25519_kes_2^6` — KES verification key (32 bytes)
- `NodeOperationalCertificate` — Operational certificate

## References
- cardano-api: Cardano.Api.SerialiseTextEnvelope
- https://github.com/IntersectMBO/cardano-node/wiki/Key-types
-/

namespace Cleanode.Crypto.TextEnvelope

open Lean (Json)

-- ====================
-- = Types            =
-- ====================

/-- Supported key types from TextEnvelope files -/
inductive KeyType where
  | paymentSigningKey
  | paymentVerificationKey
  | stakePoolSigningKey
  | stakePoolVerificationKey
  | vrfSigningKey
  | vrfVerificationKey
  | kesSigningKey
  | kesVerificationKey
  | operationalCertificate
  | unknown (typeName : String)
  deriving Repr, BEq

instance : ToString KeyType where
  toString
    | .paymentSigningKey => "PaymentSigningKeyShelley_ed25519"
    | .paymentVerificationKey => "PaymentVerificationKeyShelley_ed25519"
    | .stakePoolSigningKey => "StakePoolSigningKey_ed25519"
    | .stakePoolVerificationKey => "StakePoolVerificationKey_ed25519"
    | .vrfSigningKey => "VrfSigningKey_PraosVRF"
    | .vrfVerificationKey => "VrfVerificationKey_PraosVRF"
    | .kesSigningKey => "KesSigningKey_ed25519_kes_2^6"
    | .kesVerificationKey => "KesVerificationKey_ed25519_kes_2^6"
    | .operationalCertificate => "NodeOperationalCertificate"
    | .unknown s => s

/-- Parse a key type string -/
def KeyType.fromString (s : String) : KeyType :=
  if s == "PaymentSigningKeyShelley_ed25519" then .paymentSigningKey
  else if s == "PaymentVerificationKeyShelley_ed25519" then .paymentVerificationKey
  else if s == "StakePoolSigningKey_ed25519" then .stakePoolSigningKey
  else if s == "StakePoolVerificationKey_ed25519" then .stakePoolVerificationKey
  else if s == "VrfSigningKey_PraosVRF" then .vrfSigningKey
  else if s == "VrfVerificationKey_PraosVRF" then .vrfVerificationKey
  else if s == "KesSigningKey_ed25519_kes_2^6" then .kesSigningKey
  else if s == "KesVerificationKey_ed25519_kes_2^6" then .kesVerificationKey
  else if s == "NodeOperationalCertificate" then .operationalCertificate
  else .unknown s

/-- Parsed TextEnvelope -/
structure TextEnvelope where
  keyType : KeyType
  description : String
  rawBytes : ByteArray      -- Decoded hex, still CBOR-wrapped

-- ====================
-- = Hex Decoding     =
-- ====================

/-- Decode a hex character to its nibble value -/
private def hexCharToNibble (c : Char) : Option UInt8 :=
  if c >= '0' && c <= '9' then some (c.toNat - '0'.toNat).toUInt8
  else if c >= 'a' && c <= 'f' then some (c.toNat - 'a'.toNat + 10).toUInt8
  else if c >= 'A' && c <= 'F' then some (c.toNat - 'A'.toNat + 10).toUInt8
  else none

/-- Decode a hex string to ByteArray -/
def decodeHex (s : String) : Option ByteArray := do
  let chars := s.toList
  if chars.length % 2 != 0 then none
  else
    let mut result := ByteArray.emptyWithCapacity (chars.length / 2)
    let mut i := 0
    while h : i + 1 < chars.length do
      let hi ← hexCharToNibble chars[i]
      let lo ← hexCharToNibble chars[i + 1]
      result := result.push (hi * 16 + lo)
      i := i + 2
    some result

-- ====================
-- = CBOR Unwrapping  =
-- ====================

/-- Unwrap a single layer of CBOR byte string wrapping.
    TextEnvelope cborHex is typically: CBOR(bytes(key_material))
    e.g., 5820<32 bytes> means major type 2 (bytes), length 32.
    Some keys have nested CBOR wrapping. -/
def unwrapCborBytes (data : ByteArray) : Option ByteArray := do
  if data.size < 1 then none
  let initial := data[0]!.toNat
  let major := initial / 32
  let additional := initial % 32
  -- Must be major type 2 (byte string)
  if major != 2 then none
  if additional <= 23 then
    let len := additional
    if data.size < 1 + len then none
    some (data.extract 1 (1 + len))
  else if additional == 24 then
    if data.size < 2 then none
    let len := data[1]!.toNat
    if data.size < 2 + len then none
    some (data.extract 2 (2 + len))
  else if additional == 25 then
    if data.size < 3 then none
    let len := data[1]!.toNat * 256 + data[2]!.toNat
    if data.size < 3 + len then none
    some (data.extract 3 (3 + len))
  else
    none

-- ====================
-- = Parsing          =
-- ====================

/-- Parse a TextEnvelope JSON file -/
def parseTextEnvelope (json : Json) : Option TextEnvelope := do
  let typeName ← (json.getObjValAs? String "type").toOption
  let description ← (json.getObjValAs? String "description").toOption
  let cborHex ← (json.getObjValAs? String "cborHex").toOption
  let rawBytes ← decodeHex cborHex
  some {
    keyType := KeyType.fromString typeName
    description
    rawBytes
  }

/-- Load and parse a TextEnvelope key file -/
def loadTextEnvelope (path : String) : IO (Except String TextEnvelope) := do
  try
    let content ← IO.FS.readFile path
    match Json.parse content with
    | .error e => return .error s!"JSON parse error: {e}"
    | .ok json =>
      match parseTextEnvelope json with
      | some te => return .ok te
      | none => return .error s!"Failed to parse TextEnvelope from {path}"
  catch e =>
    return .error s!"Failed to read file {path}: {e}"

/-- Load a TextEnvelope and unwrap CBOR to get raw key bytes -/
def loadKeyBytes (path : String) : IO (Except String (KeyType × ByteArray)) := do
  let result ← loadTextEnvelope path
  match result with
  | .error e => return .error e
  | .ok te =>
    match unwrapCborBytes te.rawBytes with
    | some bytes => return .ok (te.keyType, bytes)
    | none => return .error s!"Failed to unwrap CBOR from {path} (raw size: {te.rawBytes.size})"

-- ====================
-- = Typed Loaders    =
-- ====================

/-- Load a VRF signing key (expects 64 bytes: 32-byte seed + 32-byte public key) -/
def loadVrfSigningKey (path : String) : IO (Except String (ByteArray × ByteArray)) := do
  let result ← loadKeyBytes path
  match result with
  | .error e => return .error e
  | .ok (keyType, bytes) =>
    unless keyType == .vrfSigningKey do
      return .error s!"Expected VrfSigningKey, got {keyType}"
    if bytes.size == 64 then
      -- First 32 bytes = seed/signing key, last 32 = verification key
      return .ok (bytes.extract 0 32, bytes.extract 32 64)
    else if bytes.size == 32 then
      -- Some formats have just the seed
      return .ok (bytes, ByteArray.emptyWithCapacity 0)
    else
      return .error s!"VRF signing key unexpected size: {bytes.size} (expected 32 or 64)"

/-- Load a VRF verification key (expects 32 bytes) -/
def loadVrfVerificationKey (path : String) : IO (Except String ByteArray) := do
  let result ← loadKeyBytes path
  match result with
  | .error e => return .error e
  | .ok (keyType, bytes) =>
    unless keyType == .vrfVerificationKey do
      return .error s!"Expected VrfVerificationKey, got {keyType}"
    if bytes.size != 32 then
      return .error s!"VRF verification key unexpected size: {bytes.size} (expected 32)"
    return .ok bytes

/-- Load a KES signing key -/
def loadKesSigningKey (path : String) : IO (Except String ByteArray) := do
  let result ← loadKeyBytes path
  match result with
  | .error e => return .error e
  | .ok (keyType, bytes) =>
    unless keyType == .kesSigningKey do
      return .error s!"Expected KesSigningKey, got {keyType}"
    return .ok bytes

/-- Load a KES verification key (expects 32 bytes) -/
def loadKesVerificationKey (path : String) : IO (Except String ByteArray) := do
  let result ← loadKeyBytes path
  match result with
  | .error e => return .error e
  | .ok (keyType, bytes) =>
    unless keyType == .kesVerificationKey do
      return .error s!"Expected KesVerificationKey, got {keyType}"
    if bytes.size != 32 then
      return .error s!"KES verification key unexpected size: {bytes.size} (expected 32)"
    return .ok bytes

/-- Load a pool cold signing key (Ed25519, expects 32 bytes) -/
def loadPoolSigningKey (path : String) : IO (Except String ByteArray) := do
  let result ← loadKeyBytes path
  match result with
  | .error e => return .error e
  | .ok (keyType, bytes) =>
    unless keyType == .stakePoolSigningKey do
      return .error s!"Expected StakePoolSigningKey, got {keyType}"
    if bytes.size != 32 then
      return .error s!"Pool signing key unexpected size: {bytes.size} (expected 32)"
    return .ok bytes

/-- Load a pool cold verification key (Ed25519, expects 32 bytes) -/
def loadPoolVerificationKey (path : String) : IO (Except String ByteArray) := do
  let result ← loadKeyBytes path
  match result with
  | .error e => return .error e
  | .ok (keyType, bytes) =>
    unless keyType == .stakePoolVerificationKey do
      return .error s!"Expected StakePoolVerificationKey, got {keyType}"
    if bytes.size != 32 then
      return .error s!"Pool verification key unexpected size: {bytes.size} (expected 32)"
    return .ok bytes

-- ====================
-- = Operational Cert =
-- ====================

/-- Parse an operational certificate from CBOR bytes.
    Format: CBOR array [hotVKey(32), seqNum, kesPeriod, coldKeySig(64)] -/
def parseOperationalCertBytes (data : ByteArray) : Option (ByteArray × Nat × Nat × ByteArray) := do
  -- Simple CBOR array parsing for the 4-element cert
  if data.size < 1 then none
  let initial := data[0]!.toNat
  let major := initial / 32
  let count := initial % 32
  if major != 4 || count != 4 then none  -- Must be 4-element array

  -- Parse hotVKey (bytes, 32)
  let pos := 1
  if data.size < pos + 1 then none
  let b0 := data[pos]!.toNat
  if b0 / 32 != 2 then none  -- Must be bytes
  let hotVKeyLen := b0 % 32
  if hotVKeyLen > 23 then none  -- Simple short form only
  let hotVKey := data.extract (pos + 1) (pos + 1 + hotVKeyLen)
  let pos := pos + 1 + hotVKeyLen

  -- Parse seqNum (uint)
  if data.size < pos + 1 then none
  let s0 := data[pos]!.toNat
  let (seqNum, pos) :=
    if s0 / 32 == 0 && s0 % 32 <= 23 then (s0 % 32, pos + 1)
    else if s0 == 0x18 then
      if data.size < pos + 2 then (0, pos)
      else (data[pos + 1]!.toNat, pos + 2)
    else (0, pos + 1)

  -- Parse kesPeriod (uint)
  if data.size < pos + 1 then none
  let k0 := data[pos]!.toNat
  let (kesPeriod, pos) :=
    if k0 / 32 == 0 && k0 % 32 <= 23 then (k0 % 32, pos + 1)
    else if k0 == 0x18 then
      if data.size < pos + 2 then (0, pos)
      else (data[pos + 1]!.toNat, pos + 2)
    else if k0 == 0x19 then
      if data.size < pos + 3 then (0, pos)
      else (data[pos + 1]!.toNat * 256 + data[pos + 2]!.toNat, pos + 3)
    else (0, pos + 1)

  -- Parse coldKeySig (bytes, 64)
  if data.size < pos + 1 then none
  let c0 := data[pos]!.toNat
  if c0 / 32 != 2 then none
  let sigLen := if c0 % 32 == 24 then
    if data.size < pos + 2 then 0 else data[pos + 1]!.toNat
  else c0 % 32
  let sigStart := if c0 % 32 == 24 then pos + 2 else pos + 1
  let coldSig := data.extract sigStart (sigStart + sigLen)

  some (hotVKey, seqNum, kesPeriod, coldSig)

/-- Load an operational certificate -/
def loadOperationalCert (path : String) : IO (Except String (ByteArray × Nat × Nat × ByteArray)) := do
  let result ← loadTextEnvelope path
  match result with
  | .error e => return .error e
  | .ok te =>
    unless te.keyType == .operationalCertificate do
      return .error s!"Expected NodeOperationalCertificate, got {te.keyType}"
    -- OpCert is not byte-string wrapped — it's a CBOR array directly
    match parseOperationalCertBytes te.rawBytes with
    | some cert => return .ok cert
    | none =>
      -- Try unwrapping first
      match unwrapCborBytes te.rawBytes >>= parseOperationalCertBytes with
      | some cert => return .ok cert
      | none => return .error s!"Failed to parse operational certificate CBOR (size: {te.rawBytes.size})"

end Cleanode.Crypto.TextEnvelope
