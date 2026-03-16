/-!
# CBOR Encoding/Decoding for Ouroboros Network Protocol

This module provides lightweight CBOR encoding/decoding specifically for
the Ouroboros network protocol. It works with ByteArray for efficiency.

## References
- RFC 8949: Concise Binary Object Representation (CBOR)
- Ouroboros Network Spec Section 3.1 (CBOR framing)
- Adapted from PlutusCore.Cbor
-/

namespace Cleanode.Network.Cbor

/-- CBOR major types (3-bit value in initial byte) -/
inductive MajorType where
  | UnsignedInt   -- 0: unsigned integer
  | NegativeInt   -- 1: negative integer
  | ByteString    -- 2: byte string
  | TextString    -- 3: text string
  | Array         -- 4: array of data items
  | Map           -- 5: map of pairs of data items
  | Tag           -- 6: semantic tag
  | Simple        -- 7: simple value (bool, null, float)
  deriving Repr, BEq

def MajorType.toNat : MajorType → Nat
  | .UnsignedInt => 0
  | .NegativeInt => 1
  | .ByteString  => 2
  | .TextString  => 3
  | .Array       => 4
  | .Map         => 5
  | .Tag         => 6
  | .Simple      => 7

-- ==============
-- = Encoding   =
-- ==============

/-- Get the i-th byte of n in big-endian order -/
def getByte (n : Nat) (i : Nat) : UInt8 :=
  UInt8.ofNat ((n / (256 ^ i)) % 256)

/-- Encode unsigned integer as 1-byte big-endian -/
def encodeUInt8 (n : Nat) : ByteArray :=
  ByteArray.mk #[getByte n 0]

/-- Encode unsigned integer as 2-byte big-endian -/
def encodeUInt16 (n : Nat) : ByteArray :=
  ByteArray.mk #[getByte n 1, getByte n 0]

/-- Encode unsigned integer as 4-byte big-endian -/
def encodeUInt32 (n : Nat) : ByteArray :=
  ByteArray.mk #[getByte n 3, getByte n 2, getByte n 1, getByte n 0]

/-- Encode unsigned integer as 8-byte big-endian -/
def encodeUInt64 (n : Nat) : ByteArray :=
  ByteArray.mk #[getByte n 7, getByte n 6, getByte n 5, getByte n 4,
    getByte n 3, getByte n 2, getByte n 1, getByte n 0]

/-- Encode CBOR initial byte + additional info -/
def encodeHead (major : MajorType) (n : Nat) : ByteArray :=
  let m := major.toNat
  if n ≤ 23 then
    ByteArray.mk #[UInt8.ofNat (32 * m + n)]
  else if n ≤ 255 then
    ByteArray.mk #[UInt8.ofNat (32 * m + 24)] ++ encodeUInt8 n
  else if n ≤ 65535 then
    ByteArray.mk #[UInt8.ofNat (32 * m + 25)] ++ encodeUInt16 n
  else if n ≤ 4294967295 then
    ByteArray.mk #[UInt8.ofNat (32 * m + 26)] ++ encodeUInt32 n
  else
    ByteArray.mk #[UInt8.ofNat (32 * m + 27)] ++ encodeUInt64 n

/-- Encode unsigned integer in CBOR format -/
def encodeUInt (n : Nat) : ByteArray :=
  encodeHead .UnsignedInt n

/-- Convert a Nat to big-endian byte array (for bignum encoding) -/
partial def natToBigEndianBytes (n : Nat) : ByteArray :=
  if n == 0 then ByteArray.mk #[]
  else
    let rec go (n : Nat) (acc : List UInt8) : List UInt8 :=
      if n == 0 then acc
      else go (n / 256) (UInt8.ofNat (n % 256) :: acc)
    ByteArray.mk (go n []).toArray

/-- Encode a large non-negative integer in CBOR format.
    Uses standard uint for values ≤ 2^64-1, CBOR bignum (tag 2) for larger values. -/
def encodeBigUInt (n : Nat) : ByteArray :=
  if n ≤ 18446744073709551615 then
    encodeUInt n
  else
    -- Positive bignum: tag(2) + byte string (big-endian)
    encodeHead .Tag 2 ++ encodeHead .ByteString (natToBigEndianBytes n).size ++ natToBigEndianBytes n

/-- Encode byte string in CBOR format -/
def encodeBytes (bs : ByteArray) : ByteArray :=
  encodeHead .ByteString bs.size ++ bs

/-- Encode a CBOR text string (major type 3) from UTF-8 bytes -/
def encodeTextString (s : String) : ByteArray :=
  let utf8 := s.toUTF8
  encodeHead .TextString utf8.size ++ utf8

/-- Encode array header (number of items) -/
def encodeArrayHeader (n : Nat) : ByteArray :=
  encodeHead .Array n

/-- Encode map header (number of key-value pairs) -/
def encodeMapHeader (n : Nat) : ByteArray :=
  encodeHead .Map n

/-- Encode boolean value (CBOR simple values) -/
def encodeBool (b : Bool) : ByteArray :=
  if b then ByteArray.mk #[UInt8.ofNat (32 * 7 + 21)]  -- simple value 21 (true)
       else ByteArray.mk #[UInt8.ofNat (32 * 7 + 20)]  -- simple value 20 (false)

/-- Encode CBOR break byte (0xFF) - terminates indefinite-length items -/
def encodeBreak : ByteArray :=
  ByteArray.mk #[0xff]

/-- Encode indefinite-length array header (major type 4, additional 31) -/
def encodeIndefiniteArrayHeader : ByteArray :=
  ByteArray.mk #[0x9f]

/-- Encode indefinite-length map header (major type 5, additional 31) -/
def encodeIndefiniteMapHeader : ByteArray :=
  ByteArray.mk #[0xbf]

/-- Encode indefinite-length byte string header (major type 2, additional 31) -/
def encodeIndefiniteBytesHeader : ByteArray :=
  ByteArray.mk #[0x5f]

/-- Encode indefinite-length text string header (major type 3, additional 31) -/
def encodeIndefiniteTextHeader : ByteArray :=
  ByteArray.mk #[0x7f]

/-- Encode tagged value (major type 6)

CBOR tags provide semantic annotations for data items. The tag is encoded
using major type 6 followed by the tagged data item.

Common tags:
- Tag 24: Encoded CBOR data item (used in Byron block headers)
- Tag 32: URI
- Tag 258: Set

Example:
  encodeTagged 24 (encodeBytes data)  -- Tag 24 wrapping a bytestring
-/
def encodeTagged (tag : Nat) (value : ByteArray) : ByteArray :=
  encodeHead .Tag tag ++ value

-- ==============
-- = Decoding   =
-- ==============

structure DecodeResult (α : Type) where
  value : α
  remaining : ByteArray

def DecodeResult.map {α β : Type} (f : α → β) (r : DecodeResult α) : DecodeResult β :=
  { value := f r.value, remaining := r.remaining }

/-- Decode single byte -/
def decodeUInt8 (bs : ByteArray) : Option (DecodeResult UInt8) :=
  if bs.size ≥ 1 then
    some { value := bs[0]!, remaining := bs.extract 1 bs.size }
  else
    none

/-- Decode 2-byte big-endian integer -/
def decodeUInt16 (bs : ByteArray) : Option (DecodeResult Nat) :=
  if bs.size ≥ 2 then
    let n := bs[0]!.toNat * 256 + bs[1]!.toNat
    some { value := n, remaining := bs.extract 2 bs.size }
  else
    none

/-- Decode 4-byte big-endian integer -/
def decodeUInt32 (bs : ByteArray) : Option (DecodeResult Nat) :=
  if bs.size ≥ 4 then
    let n := bs[0]!.toNat * 256^3 + bs[1]!.toNat * 256^2 +
             bs[2]!.toNat * 256   + bs[3]!.toNat
    some { value := n, remaining := bs.extract 4 bs.size }
  else
    none

/-- Decode 8-byte big-endian integer -/
def decodeUInt64 (bs : ByteArray) : Option (DecodeResult Nat) :=
  if bs.size ≥ 8 then
    let n := bs[0]!.toNat * 256^7 + bs[1]!.toNat * 256^6 +
             bs[2]!.toNat * 256^5 + bs[3]!.toNat * 256^4 +
             bs[4]!.toNat * 256^3 + bs[5]!.toNat * 256^2 +
             bs[6]!.toNat * 256   + bs[7]!.toNat
    some { value := n, remaining := bs.extract 8 bs.size }
  else
    none

/-- Decode CBOR head (major type + additional info) -/
def decodeHead (bs : ByteArray) : Option (DecodeResult (Nat × Nat)) := do
  if bs.size < 1 then none

  let initial := bs[0]!.toNat
  let major := initial / 32
  let additional := initial % 32

  if additional ≤ 23 then
    some { value := (major, additional), remaining := bs.extract 1 bs.size }
  else if additional == 24 then
    let r ← decodeUInt8 (bs.extract 1 bs.size)
    some { value := (major, r.value.toNat), remaining := r.remaining }
  else if additional == 25 then
    let r ← decodeUInt16 (bs.extract 1 bs.size)
    some { value := (major, r.value), remaining := r.remaining }
  else if additional == 26 then
    let r ← decodeUInt32 (bs.extract 1 bs.size)
    some { value := (major, r.value), remaining := r.remaining }
  else if additional == 27 then
    let r ← decodeUInt64 (bs.extract 1 bs.size)
    some { value := (major, r.value), remaining := r.remaining }
  else
    none

/-- Decode unsigned integer from CBOR -/
def decodeUInt (bs : ByteArray) : Option (DecodeResult Nat) := do
  let r ← decodeHead bs
  let (major, value) := r.value
  if major == 0 then  -- UnsignedInt
    some { value := value, remaining := r.remaining }
  else
    none

/-- Decode byte string from CBOR -/
def decodeBytes (bs : ByteArray) : Option (DecodeResult ByteArray) := do
  let r ← decodeHead bs
  let (major, len) := r.value
  if major == 2 && r.remaining.size ≥ len then  -- ByteString
    some {
      value := r.remaining.extract 0 len,
      remaining := r.remaining.extract len r.remaining.size
    }
  else
    none

/-- Decode array header (returns number of items) -/
def decodeArrayHeader (bs : ByteArray) : Option (DecodeResult Nat) := do
  let r ← decodeHead bs
  let (major, count) := r.value
  if major == 4 then  -- Array
    some { value := count, remaining := r.remaining }
  else
    none

/-- Decode map header (returns number of key-value pairs) -/
def decodeMapHeader (bs : ByteArray) : Option (DecodeResult Nat) := do
  let r ← decodeHead bs
  let (major, count) := r.value
  if major == 5 then  -- Map
    some { value := count, remaining := r.remaining }
  else
    none

/-- Decode and skip CBOR tag, returning the tagged content.
    Tags are major type 6 followed by tag number, then the tagged value.
    Common tags: 258 = set (used in Cardano for transaction inputs) -/
def skipTag (bs : ByteArray) : Option (DecodeResult Nat) := do
  let r ← decodeHead bs
  let (major, tagNum) := r.value
  if major == 6 then  -- Tag
    -- Return tag number and remaining bytes (the tagged value)
    some { value := tagNum, remaining := r.remaining }
  else
    none

/-- Decode boolean value -/
def decodeBool (bs : ByteArray) : Option (DecodeResult Bool) := do
  if bs.size < 1 then none
  let initial := bs[0]!.toNat
  if initial == (32 * 7 + 20) then  -- false
    some { value := false, remaining := bs.extract 1 bs.size }
  else if initial == (32 * 7 + 21) then  -- true
    some { value := true, remaining := bs.extract 1 bs.size }
  else
    none

/-- Decode tagged value (major type 6) - returns (tag, remaining) without decoding inner value -/
def decodeTag (bs : ByteArray) : Option (DecodeResult Nat) := do
  let r ← decodeHead bs
  let (major, tag) := r.value
  if major == 6 then  -- Tag
    some { value := tag, remaining := r.remaining }
  else
    none

mutual
  /-- Skip over a CBOR value without decoding it (returns remaining bytes) -/
  partial def skipCborValue (bs : ByteArray) : Option ByteArray := do
    if bs.size == 0 then none

    let major := bs[0]! >>> 5
    let additional := bs[0]! &&& 0x1f

    -- Handle indefinite-length items (additional == 31)
    if additional == 31 then
      let afterHead := bs.extract 1 bs.size
      match major with
      | 2 | 3 => skipIndefiniteChunks afterHead  -- indefinite bytes/text
      | 4 => skipIndefiniteItems afterHead         -- indefinite array
      | 5 => skipIndefiniteItems afterHead         -- indefinite map (pairs)
      | 7 => some afterHead                        -- break byte (0xFF) handled by caller
      | _ => none
    else
      -- Determine argument size and decode the argument if needed
      let (argSize, argValue) :=
        if additional < 24 then (0, additional.toNat)
        else if additional == 24 then
          if bs.size < 2 then (0, 0) else (1, bs[1]!.toNat)
        else if additional == 25 then
          if bs.size < 3 then (0, 0) else (2, bs[1]!.toNat * 256 + bs[2]!.toNat)
        else if additional == 26 then
          if bs.size < 5 then (0, 0) else
            (4, bs[1]!.toNat * 256^3 + bs[2]!.toNat * 256^2 + bs[3]!.toNat * 256 + bs[4]!.toNat)
        else if additional == 27 then
          if bs.size < 9 then (0, 0) else
            (8, bs[1]!.toNat * 256^7 + bs[2]!.toNat * 256^6 + bs[3]!.toNat * 256^5 + bs[4]!.toNat * 256^4 +
                bs[5]!.toNat * 256^3 + bs[6]!.toNat * 256^2 + bs[7]!.toNat * 256 + bs[8]!.toNat)
        else (0, 0)

      let afterHead := bs.extract (1 + argSize) bs.size

      match major with
      | 0 | 1 | 7 => some afterHead  -- uint, nint, simple
      | 2 | 3 => do  -- bytes, text - skip length bytes
          if afterHead.size < argValue then none
          else some (afterHead.extract argValue afterHead.size)
      | 4 | 5 => do  -- array, map - skip items
          let itemCount := if major == 5 then argValue * 2 else argValue
          skipCborValues itemCount afterHead
      | 6 => skipCborValue afterHead  -- tag - skip the tagged value
      | _ => none

  /-- Helper to skip multiple CBOR values -/
  partial def skipCborValues (n : Nat) (bs : ByteArray) : Option ByteArray :=
    if n == 0 then some bs
    else match skipCborValue bs with
      | some next => skipCborValues (n - 1) next
      | none => none

  /-- Skip indefinite-length chunks (byte strings or text strings) until break -/
  partial def skipIndefiniteChunks (bs : ByteArray) : Option ByteArray :=
    if bs.size == 0 then none
    else if bs[0]! == 0xff then some (bs.extract 1 bs.size)  -- break byte
    else match skipCborValue bs with
      | some next => skipIndefiniteChunks next
      | none => none

  /-- Skip indefinite-length items (array elements or map key-value pairs) until break -/
  partial def skipIndefiniteItems (bs : ByteArray) : Option ByteArray :=
    if bs.size == 0 then none
    else if bs[0]! == 0xff then some (bs.extract 1 bs.size)  -- break byte
    else match skipCborValue bs with
      | some next => skipIndefiniteItems next
      | none => none
end

end Cleanode.Network.Cbor
