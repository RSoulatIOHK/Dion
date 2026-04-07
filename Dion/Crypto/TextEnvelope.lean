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

namespace Dion.Crypto.TextEnvelope

open Lean (Json)

-- ====================
-- = Types            =
-- ====================

/-- Supported key types from TextEnvelope files -/
inductive KeyType where
  | paymentSigningKey
  | paymentVerificationKey
  | stakeSigningKey
  | stakeVerificationKey
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
    | .stakeSigningKey => "StakeSigningKeyShelley_ed25519"
    | .stakeVerificationKey => "StakeVerificationKeyShelley_ed25519"
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
  else if s == "StakeSigningKeyShelley_ed25519" then .stakeSigningKey
  else if s == "StakeVerificationKeyShelley_ed25519" then .stakeVerificationKey
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

/-- Parse a CBOR bytestring at position, returning (bytes, newPos) -/
private def parseCborBytes (data : ByteArray) (pos : Nat) : Option (ByteArray × Nat) := do
  if data.size <= pos then none
  let b0 := data[pos]!.toNat
  if b0 / 32 != 2 then none  -- Must be major type 2 (bytes)
  let info := b0 % 32
  if info <= 23 then
    some (data.extract (pos + 1) (pos + 1 + info), pos + 1 + info)
  else if info == 24 then
    if data.size < pos + 2 then none
    let len := data[pos + 1]!.toNat
    some (data.extract (pos + 2) (pos + 2 + len), pos + 2 + len)
  else if info == 25 then
    if data.size < pos + 3 then none
    let len := data[pos + 1]!.toNat * 256 + data[pos + 2]!.toNat
    some (data.extract (pos + 3) (pos + 3 + len), pos + 3 + len)
  else none

/-- Parse a CBOR unsigned integer at position, returning (value, newPos) -/
private def parseCborUint (data : ByteArray) (pos : Nat) : Option (Nat × Nat) := do
  if data.size <= pos then none
  let b0 := data[pos]!.toNat
  if b0 / 32 != 0 then none  -- Must be major type 0 (uint)
  let info := b0 % 32
  if info <= 23 then some (info, pos + 1)
  else if info == 24 then
    if data.size < pos + 2 then none
    some (data[pos + 1]!.toNat, pos + 2)
  else if info == 25 then
    if data.size < pos + 3 then none
    some (data[pos + 1]!.toNat * 256 + data[pos + 2]!.toNat, pos + 3)
  else if info == 26 then
    if data.size < pos + 5 then none
    let v := data[pos + 1]!.toNat * 16777216 + data[pos + 2]!.toNat * 65536 +
             data[pos + 3]!.toNat * 256 + data[pos + 4]!.toNat
    some (v, pos + 5)
  else none

/-- Parse an operational certificate from CBOR bytes.
    Format: either [cert4, coldVKey] where cert4 = [hotVKey, seqNum, kesPeriod, coldSig]
    or flat [hotVKey, seqNum, kesPeriod, coldSig] -/
def parseOperationalCertBytes (data : ByteArray) : Option (ByteArray × Nat × Nat × ByteArray) := do
  if data.size < 1 then none
  let initial := data[0]!.toNat
  let major := initial / 32
  let count := initial % 32
  -- Determine start position of the 4-element cert array
  let startPos ←
    if major == 4 && count == 2 && data.size >= 2 then
      -- Wrapped format: [cert4_array, coldVKey]
      let inner := data[1]!.toNat
      if inner / 32 == 4 && inner % 32 == 4 then some 2 else none
    else if major == 4 && count == 4 then some 1
    else none

  let (hotVKey, pos) ← parseCborBytes data startPos
  let (seqNum, pos) ← parseCborUint data pos
  let (kesPeriod, pos) ← parseCborUint data pos
  let (coldSig, _) ← parseCborBytes data pos
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

-- ====================
-- = Writing          =
-- ====================

/-- Encode bytes as lowercase hex string -/
def encodeHex (bytes : ByteArray) : String :=
  let toHex := fun (b : UInt8) =>
    let hi := b.toNat / 16
    let lo := b.toNat % 16
    let hexDigit n := if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
    String.mk [hexDigit hi, hexDigit lo]
  String.join (bytes.toList.map toHex)

/-- CBOR-wrap raw bytes as a byte string.
    Produces: 0x58 <len> <bytes> for 24≤len≤255,
              0x59 <hi> <lo> <bytes> for 256≤len≤65535,
              0x40+len <bytes> for len≤23. -/
def cborWrapBytes (bytes : ByteArray) : ByteArray :=
  let n := bytes.size
  let header : ByteArray :=
    if n <= 23 then ByteArray.mk #[(0x40 + n).toUInt8]
    else if n <= 255 then ByteArray.mk #[0x58, n.toUInt8]
    else ByteArray.mk #[0x59, (n / 256).toUInt8, (n % 256).toUInt8]
  header ++ bytes

/-- Build a TextEnvelope JSON string -/
def makeTextEnvelope (keyType : KeyType) (description : String) (rawKeyBytes : ByteArray) : String :=
  let cborWrapped := cborWrapBytes rawKeyBytes
  let hex := encodeHex cborWrapped
  let typeName := toString keyType
  s!"\{\n    \"type\": \"{typeName}\",\n    \"description\": \"{description}\",\n    \"cborHex\": \"{hex}\"\n}\n"

/-- Write a TextEnvelope key file -/
def writeTextEnvelope (path : String) (keyType : KeyType) (description : String) (rawKeyBytes : ByteArray) : IO Unit := do
  let content := makeTextEnvelope keyType description rawKeyBytes
  IO.FS.writeFile path content

/-- Build and write an operational certificate TextEnvelope.
    OpCert CBOR format: [[hotVKey(58), seqNum(uint), kesPeriod(uint), coldSig(58)], coldVKey(58)]
    (2-element outer array wrapping the 4-element cert array + cold VK) -/
def writeOperationalCert (path : String)
    (hotVKey : ByteArray) (seqNum : Nat) (kesPeriod : Nat) (coldSig : ByteArray)
    (coldVKey : ByteArray) : IO Unit := do
  -- Encode the cert as CBOR: [[hotVKey, seqNum, kesPeriod, coldSig], coldVKey]
  let encUInt (n : Nat) : ByteArray :=
    if n <= 23 then ByteArray.mk #[n.toUInt8]
    else if n <= 255 then ByteArray.mk #[0x18, n.toUInt8]
    else ByteArray.mk #[0x19, (n / 256).toUInt8, (n % 256).toUInt8]
  let inner :=
    ByteArray.mk #[0x84]  -- 4-element array
    ++ cborWrapBytes hotVKey
    ++ encUInt seqNum
    ++ encUInt kesPeriod
    ++ cborWrapBytes coldSig
  let outer :=
    ByteArray.mk #[0x82]  -- 2-element array
    ++ inner
    ++ cborWrapBytes coldVKey
  let hex := encodeHex outer
  let content := s!"\{\n    \"type\": \"NodeOperationalCertificate\",\n    \"description\": \"Operational Certificate\",\n    \"cborHex\": \"{hex}\"\n}\n"
  IO.FS.writeFile path content

/-- Write the OpCert issue counter file -/
def writeOpCertCounter (path : String) (seqNum : Nat) (coldVKey : ByteArray) : IO Unit := do
  -- Counter format: [seqNum, coldVKey]
  let encUInt (n : Nat) : ByteArray :=
    if n <= 23 then ByteArray.mk #[n.toUInt8]
    else if n <= 255 then ByteArray.mk #[0x18, n.toUInt8]
    else ByteArray.mk #[0x19, (n / 256).toUInt8, (n % 256).toUInt8]
  let cbor := ByteArray.mk #[0x82] ++ encUInt seqNum ++ cborWrapBytes coldVKey
  let hex := encodeHex cbor
  let content := s!"\{\n    \"type\": \"NodeOpCertCounterFile\",\n    \"description\": \"Next Certificate Issue Number: {seqNum}\",\n    \"cborHex\": \"{hex}\"\n}\n"
  IO.FS.writeFile path content

end Dion.Crypto.TextEnvelope
