import Dion.Network.Cbor
import Dion.Network.ByteArrayBuilder

/-!
# CBOR Value Type and Generic Codec

This module defines a unified `CborValue` inductive type that represents
any CBOR data item, along with generic encode/decode functions.

## Specification Propositions

Formal specifications for CBOR encoding/decoding correctness are defined
as propositions, to be proved in `Dion.Proofs.CborProofs`.

## References
- RFC 8949: Concise Binary Object Representation (CBOR)
- Ouroboros Network Spec Section 3.1 (CBOR framing)
-/

namespace Dion.Network.CborValue

open Dion.Network.Cbor

-- ====================
-- = CBOR Value Type  =
-- ====================

/-- A unified CBOR value representing any CBOR data item per RFC 8949 -/
inductive CborValue where
  | uint (n : Nat)                                -- Major type 0: unsigned integer
  | nint (n : Nat)                                -- Major type 1: negative integer (-1 - n)
  | bytes (bs : ByteArray)                        -- Major type 2: byte string
  | text (s : String)                             -- Major type 3: text string
  | array (items : List CborValue)                -- Major type 4: array
  | map (pairs : List (CborValue × CborValue))    -- Major type 5: map
  | tag (t : Nat) (value : CborValue)             -- Major type 6: semantic tag
  | bool (b : Bool)                               -- Major type 7, simple 20/21
  | null                                          -- Major type 7, simple 22
  | undefined                                     -- Major type 7, simple 23
  | float64 (bits : UInt64)                       -- Major type 7, additional 27 (IEEE 754)
  deriving BEq

-- ====================
-- = Encoding         =
-- ====================

/-- Encode a CborValue to its CBOR binary representation -/
partial def encodeCborValue : CborValue → ByteArray
  | .uint n => encodeHead .UnsignedInt n
  | .nint n => encodeHead .NegativeInt n
  | .bytes bs => encodeHead .ByteString bs.size ++ bs
  | .text s =>
      let utf8 := s.toUTF8
      encodeHead .TextString utf8.size ++ utf8
  | .array items =>
      let header := encodeHead .Array items.length
      let b := items.foldl (fun b v => b.append (encodeCborValue v)) (ByteArrayBuilder.Builder.empty.append header)
      b.toByteArray
  | .map pairs =>
      let header := encodeHead .Map pairs.length
      let b := pairs.foldl (fun b (k, v) => (b.append (encodeCborValue k)).append (encodeCborValue v)) (ByteArrayBuilder.Builder.empty.append header)
      b.toByteArray
  | .tag t value =>
      encodeHead .Tag t ++ encodeCborValue value
  | .bool b =>
      if b then ByteArray.mk #[0xf5]  -- simple value 21 (true)
           else ByteArray.mk #[0xf4]  -- simple value 20 (false)
  | .null => ByteArray.mk #[0xf6]     -- simple value 22
  | .undefined => ByteArray.mk #[0xf7] -- simple value 23
  | .float64 bits =>
      let header := ByteArray.mk #[0xfb]  -- major 7, additional 27
      let b0 := UInt8.ofNat ((bits >>> 56).toNat % 256)
      let b1 := UInt8.ofNat ((bits >>> 48).toNat % 256)
      let b2 := UInt8.ofNat ((bits >>> 40).toNat % 256)
      let b3 := UInt8.ofNat ((bits >>> 32).toNat % 256)
      let b4 := UInt8.ofNat ((bits >>> 24).toNat % 256)
      let b5 := UInt8.ofNat ((bits >>> 16).toNat % 256)
      let b6 := UInt8.ofNat ((bits >>> 8).toNat % 256)
      let b7 := UInt8.ofNat (bits.toNat % 256)
      header ++ ByteArray.mk #[b0, b1, b2, b3, b4, b5, b6, b7]

-- ====================
-- = Decoding         =
-- ====================

/-- Decode a text string from raw bytes (UTF-8) -/
private def decodeTextString (bs : ByteArray) : Option (DecodeResult String) := do
  let r ← decodeHead bs
  let (major, len) := r.value
  if major != 3 then none
  if r.remaining.size < len then none
  let textBytes := r.remaining.extract 0 len
  let remaining := r.remaining.extract len r.remaining.size
  some { value := String.fromUTF8! textBytes, remaining := remaining }

/-- Decode a negative integer from CBOR (major type 1, value = -1 - n) -/
private def decodeNegInt (bs : ByteArray) : Option (DecodeResult Nat) := do
  let r ← decodeHead bs
  let (major, value) := r.value
  if major != 1 then none
  some { value := value, remaining := r.remaining }

/-- Decode a float64 from 8 bytes big-endian -/
private def decodeFloat64Bits (bs : ByteArray) : Option (DecodeResult UInt64) := do
  if bs.size < 8 then none
  let bits := (bs[0]!.toNat.toUInt64 <<< 56) ||| (bs[1]!.toNat.toUInt64 <<< 48) |||
              (bs[2]!.toNat.toUInt64 <<< 40) ||| (bs[3]!.toNat.toUInt64 <<< 32) |||
              (bs[4]!.toNat.toUInt64 <<< 24) ||| (bs[5]!.toNat.toUInt64 <<< 16) |||
              (bs[6]!.toNat.toUInt64 <<< 8)  ||| bs[7]!.toNat.toUInt64
  some { value := bits, remaining := bs.extract 8 bs.size }

mutual
  /-- Decode a generic CborValue from a byte array -/
  partial def decodeCborValue (bs : ByteArray) : Option (DecodeResult CborValue) := do
    if bs.size == 0 then none

    let initial := bs[0]!.toNat
    let major := initial / 32
    let additional := initial % 32

    match major with
    | 0 => do -- Unsigned integer
        let r ← decodeUInt bs
        some { value := .uint r.value, remaining := r.remaining }
    | 1 => do -- Negative integer
        let r ← decodeNegInt bs
        some { value := .nint r.value, remaining := r.remaining }
    | 2 => do -- Byte string
        if additional == 31 then
          let r ← decodeIndefiniteBytes (bs.extract 1 bs.size)
          some { value := .bytes r.value, remaining := r.remaining }
        else
          let r ← decodeBytes bs
          some { value := .bytes r.value, remaining := r.remaining }
    | 3 => do -- Text string
        if additional == 31 then
          let r ← decodeIndefiniteText (bs.extract 1 bs.size)
          some { value := .text r.value, remaining := r.remaining }
        else
          let r ← decodeTextString bs
          some { value := .text r.value, remaining := r.remaining }
    | 4 => do -- Array
        if additional == 31 then
          let r ← decodeIndefiniteArray (bs.extract 1 bs.size)
          some { value := .array r.value, remaining := r.remaining }
        else
          let r ← decodeArrayHeader bs
          let items ← decodeNItems r.value r.remaining
          some { value := .array items.value, remaining := items.remaining }
    | 5 => do -- Map
        if additional == 31 then
          let r ← decodeIndefiniteMap (bs.extract 1 bs.size)
          some { value := .map r.value, remaining := r.remaining }
        else
          let r ← decodeMapHeader bs
          let pairs ← decodeNPairs r.value r.remaining
          some { value := .map pairs.value, remaining := pairs.remaining }
    | 6 => do -- Tag
        let r ← decodeTag bs
        let inner ← decodeCborValue r.remaining
        some { value := .tag r.value inner.value, remaining := inner.remaining }
    | 7 => -- Simple values and floats
        if additional == 20 then
          some { value := .bool false, remaining := bs.extract 1 bs.size }
        else if additional == 21 then
          some { value := .bool true, remaining := bs.extract 1 bs.size }
        else if additional == 22 then
          some { value := .null, remaining := bs.extract 1 bs.size }
        else if additional == 23 then
          some { value := .undefined, remaining := bs.extract 1 bs.size }
        else if additional == 27 then do
          let r ← decodeFloat64Bits (bs.extract 1 bs.size)
          some { value := .float64 r.value, remaining := r.remaining }
        else
          none
    | _ => none

  /-- Decode N CborValues sequentially -/
  partial def decodeNItems (n : Nat) (bs : ByteArray) : Option (DecodeResult (List CborValue)) :=
    if n == 0 then some { value := [], remaining := bs }
    else do
      let r ← decodeCborValue bs
      let rest ← decodeNItems (n - 1) r.remaining
      some { value := r.value :: rest.value, remaining := rest.remaining }

  /-- Decode N key-value pairs -/
  partial def decodeNPairs (n : Nat) (bs : ByteArray) : Option (DecodeResult (List (CborValue × CborValue))) :=
    if n == 0 then some { value := [], remaining := bs }
    else do
      let rk ← decodeCborValue bs
      let rv ← decodeCborValue rk.remaining
      let rest ← decodeNPairs (n - 1) rv.remaining
      some { value := (rk.value, rv.value) :: rest.value, remaining := rest.remaining }

  /-- Decode indefinite-length byte string chunks until break -/
  partial def decodeIndefiniteBytes (bs : ByteArray) : Option (DecodeResult ByteArray) :=
    if bs.size == 0 then none
    else if bs[0]! == 0xff then
      some { value := ByteArray.empty, remaining := bs.extract 1 bs.size }
    else do
      let chunk ← decodeBytes bs
      let rest ← decodeIndefiniteBytes chunk.remaining
      some { value := chunk.value ++ rest.value, remaining := rest.remaining }

  /-- Decode indefinite-length text string chunks until break -/
  partial def decodeIndefiniteText (bs : ByteArray) : Option (DecodeResult String) :=
    if bs.size == 0 then none
    else if bs[0]! == 0xff then
      some { value := "", remaining := bs.extract 1 bs.size }
    else do
      let chunk ← decodeTextString bs
      let rest ← decodeIndefiniteText chunk.remaining
      some { value := chunk.value ++ rest.value, remaining := rest.remaining }

  /-- Decode indefinite-length array items until break -/
  partial def decodeIndefiniteArray (bs : ByteArray) : Option (DecodeResult (List CborValue)) :=
    if bs.size == 0 then none
    else if bs[0]! == 0xff then
      some { value := [], remaining := bs.extract 1 bs.size }
    else do
      let item ← decodeCborValue bs
      let rest ← decodeIndefiniteArray item.remaining
      some { value := item.value :: rest.value, remaining := rest.remaining }

  /-- Decode indefinite-length map pairs until break -/
  partial def decodeIndefiniteMap (bs : ByteArray) : Option (DecodeResult (List (CborValue × CborValue))) :=
    if bs.size == 0 then none
    else if bs[0]! == 0xff then
      some { value := [], remaining := bs.extract 1 bs.size }
    else do
      let rk ← decodeCborValue bs
      let rv ← decodeCborValue rk.remaining
      let rest ← decodeIndefiniteMap rv.remaining
      some { value := (rk.value, rv.value) :: rest.value, remaining := rest.remaining }
end

-- ============================
-- = Specification Propositions =
-- ============================

/-- A byte array contains a valid CBOR encoding per RFC 8949 -/
def ValidCborEncoding (_bs : ByteArray) : Prop :=
  ∃ v : CborValue, ∃ r : DecodeResult CborValue,
    decodeCborValue _bs = some r ∧ r.value = v

/-- v encodes to bs under CBOR encoding -/
def CborEncodes (v : CborValue) (bs : ByteArray) : Prop :=
  encodeCborValue v = bs

/-- bs decodes to v under CBOR decoding -/
def CborDecodes (bs : ByteArray) (v : CborValue) : Prop :=
  ∃ r : DecodeResult CborValue,
    decodeCborValue bs = some r ∧ r.value = v

end Dion.Network.CborValue
