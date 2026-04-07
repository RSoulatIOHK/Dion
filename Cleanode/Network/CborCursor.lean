/-!
# Zero-Copy CBOR Cursor Decoder

A position-based CBOR decoder that avoids ByteArray.extract allocations.
Instead of slicing the underlying data on every decode, we advance a `pos : Nat`
offset into the original ByteArray. Only leaf values (byte strings needed by
the caller) trigger an `extract`.

This eliminates ~100+ allocations per block compared to the Cbor.lean decoder.
-/

namespace Dion.Network.CborCursor

-- ========================
-- = Core Types           =
-- ========================

/-- A cursor into a ByteArray. `data` is never copied; only `pos` advances. -/
structure Cursor where
  data : ByteArray
  pos  : Nat

/-- Result of a cursor-based decode: the decoded value and the advanced cursor. -/
structure CResult (α : Type) where
  value  : α
  cursor : Cursor

def CResult.map {α β : Type} (f : α → β) (r : CResult α) : CResult β :=
  { value := f r.value, cursor := r.cursor }

/-- Create a cursor at the start of a ByteArray -/
def Cursor.mk' (bs : ByteArray) : Cursor := { data := bs, pos := 0 }

/-- Remaining bytes from cursor position to end -/
@[inline] def Cursor.remaining (c : Cursor) : Nat :=
  if c.pos ≤ c.data.size then c.data.size - c.pos else 0

/-- Read byte at current position (unchecked) -/
@[inline] def Cursor.peek (c : Cursor) : UInt8 :=
  c.data[c.pos]!

/-- Read byte at offset from current position (unchecked) -/
@[inline] def Cursor.peekAt (c : Cursor) (offset : Nat) : UInt8 :=
  c.data[c.pos + offset]!

/-- Advance cursor by n bytes -/
@[inline] def Cursor.advance (c : Cursor) (n : Nat) : Cursor :=
  { c with pos := c.pos + n }

/-- Extract a sub-array from cursor (only for leaf values that need the actual bytes) -/
@[inline] def Cursor.extract (c : Cursor) (len : Nat) : ByteArray :=
  c.data.extract c.pos (c.pos + len)

-- ========================
-- = Primitive Decoders   =
-- ========================

/-- Decode single byte -/
@[inline] def decodeUInt8 (c : Cursor) : Option (CResult UInt8) :=
  if c.remaining ≥ 1 then
    some { value := c.peek, cursor := c.advance 1 }
  else
    none

/-- Decode 2-byte big-endian integer -/
@[inline] def readUInt16 (c : Cursor) : Nat :=
  (c.peekAt 0).toNat * 256 + (c.peekAt 1).toNat

/-- Decode 4-byte big-endian integer -/
@[inline] def readUInt32 (c : Cursor) : Nat :=
  (c.peekAt 0).toNat * 256^3 + (c.peekAt 1).toNat * 256^2 +
  (c.peekAt 2).toNat * 256   + (c.peekAt 3).toNat

/-- Decode 8-byte big-endian integer -/
@[inline] def readUInt64 (c : Cursor) : Nat :=
  (c.peekAt 0).toNat * 256^7 + (c.peekAt 1).toNat * 256^6 +
  (c.peekAt 2).toNat * 256^5 + (c.peekAt 3).toNat * 256^4 +
  (c.peekAt 4).toNat * 256^3 + (c.peekAt 5).toNat * 256^2 +
  (c.peekAt 6).toNat * 256   + (c.peekAt 7).toNat

-- ========================
-- = CBOR Head Decoder    =
-- ========================

/-- Decode CBOR head: returns (majorType, argument) and advanced cursor.
    This is the core primitive — all other decoders build on it. -/
def decodeHead (c : Cursor) : Option (CResult (Nat × Nat)) := do
  if c.remaining < 1 then none
  let initial := c.peek.toNat
  let major := initial / 32
  let additional := initial % 32
  let c1 := c.advance 1

  if additional ≤ 23 then
    some { value := (major, additional), cursor := c1 }
  else if additional == 24 then
    if c1.remaining < 1 then none
    else some { value := (major, c1.peek.toNat), cursor := c1.advance 1 }
  else if additional == 25 then
    if c1.remaining < 2 then none
    else some { value := (major, readUInt16 c1), cursor := c1.advance 2 }
  else if additional == 26 then
    if c1.remaining < 4 then none
    else some { value := (major, readUInt32 c1), cursor := c1.advance 4 }
  else if additional == 27 then
    if c1.remaining < 8 then none
    else some { value := (major, readUInt64 c1), cursor := c1.advance 8 }
  else
    none

-- ========================
-- = Typed Decoders       =
-- ========================

/-- Decode unsigned integer (major type 0) -/
def decodeUInt (c : Cursor) : Option (CResult Nat) := do
  let r ← decodeHead c
  let (major, value) := r.value
  if major == 0 then some { value := value, cursor := r.cursor }
  else none

/-- Decode byte string (major type 2) — extracts the actual bytes -/
def decodeBytes (c : Cursor) : Option (CResult ByteArray) := do
  let r ← decodeHead c
  let (major, len) := r.value
  if major == 2 && r.cursor.remaining ≥ len then
    some { value := r.cursor.extract len, cursor := r.cursor.advance len }
  else
    none

/-- Decode array header (major type 4) — returns element count.
    For indefinite-length arrays (0x9F), returns a large sentinel value. -/
def decodeArrayHeader (c : Cursor) : Option (CResult Nat) := do
  if c.remaining < 1 then none
  let initial := c.peek.toNat
  -- Check for indefinite-length array (0x9F)
  if initial == 0x9F then
    some { value := 9999, cursor := c.advance 1 }  -- sentinel: parse until 0xFF break
  else
    let r ← decodeHead c
    let (major, count) := r.value
    if major == 4 then some { value := count, cursor := r.cursor }
    -- Handle CBOR tag 258 (set semantics) wrapping an array — used in Conway era
    else if major == 6 && count == 258 then
      -- Tag 258 wraps the actual array; decode the inner array header
      let inner := r.cursor
      if inner.remaining < 1 then none
      let innerInitial := inner.peek.toNat
      if innerInitial == 0x9F then
        some { value := 9999, cursor := inner.advance 1 }
      else
        let r2 ← decodeHead inner
        let (major2, count2) := r2.value
        if major2 == 4 then some { value := count2, cursor := r2.cursor }
        else none
    else none

/-- Decode map header (major type 5) — returns pair count.
    For indefinite-length maps (0xBF), returns a large sentinel value. -/
def decodeMapHeader (c : Cursor) : Option (CResult Nat) := do
  if c.remaining < 1 then none
  let initial := c.peek.toNat
  -- Check for indefinite-length map (0xBF)
  if initial == 0xBF then
    some { value := 9999, cursor := c.advance 1 }  -- sentinel: parse until 0xFF break
  else
    let r ← decodeHead c
    let (major, count) := r.value
    if major == 5 then some { value := count, cursor := r.cursor }
    else none

/-- Check if cursor points to CBOR break code (0xFF) -/
def isBreak (c : Cursor) : Bool :=
  c.remaining ≥ 1 && c.peek.toNat == 0xFF

/-- Skip past break code (0xFF), returns cursor after -/
def skipBreak (c : Cursor) : Option Cursor :=
  if isBreak c then some (c.advance 1) else none

/-- Decode and skip CBOR tag (major type 6) — returns tag number -/
def skipTag (c : Cursor) : Option (CResult Nat) := do
  let r ← decodeHead c
  let (major, tagNum) := r.value
  if major == 6 then some { value := tagNum, cursor := r.cursor }
  else none

-- ========================
-- = Skip (zero-copy)     =
-- ========================

mutual
  /-- Skip over a CBOR value without allocating. Returns advanced cursor. -/
  partial def skipValue (c : Cursor) : Option Cursor := do
    if c.remaining == 0 then none

    let major := c.peek >>> 5
    let additional := c.peek &&& 0x1f

    -- Indefinite-length items (additional == 31)
    if additional == 31 then
      let c1 := c.advance 1
      match major with
      | 2 | 3 => skipIndefiniteChunks c1
      | 4 | 5 => skipIndefiniteItems c1
      | 7 => some c1  -- break byte handled by caller
      | _ => none
    else
      -- Decode argument (how many bytes the argument takes)
      let (argSize, argValue) :=
        if additional < 24 then (0, additional.toNat)
        else if additional == 24 then
          if c.remaining < 2 then (0, 0) else (1, (c.peekAt 1).toNat)
        else if additional == 25 then
          if c.remaining < 3 then (0, 0)
          else (2, (c.peekAt 1).toNat * 256 + (c.peekAt 2).toNat)
        else if additional == 26 then
          if c.remaining < 5 then (0, 0)
          else (4, (c.peekAt 1).toNat * 256^3 + (c.peekAt 2).toNat * 256^2 +
                   (c.peekAt 3).toNat * 256   + (c.peekAt 4).toNat)
        else if additional == 27 then
          if c.remaining < 9 then (0, 0)
          else (8, (c.peekAt 1).toNat * 256^7 + (c.peekAt 2).toNat * 256^6 +
                   (c.peekAt 3).toNat * 256^5 + (c.peekAt 4).toNat * 256^4 +
                   (c.peekAt 5).toNat * 256^3 + (c.peekAt 6).toNat * 256^2 +
                   (c.peekAt 7).toNat * 256   + (c.peekAt 8).toNat)
        else (0, 0)

      let afterHead := c.advance (1 + argSize)

      match major with
      | 0 | 1 | 7 => some afterHead  -- uint, nint, simple: no payload
      | 2 | 3 =>  -- bytes, text: skip `argValue` bytes
          if afterHead.remaining < argValue then none
          else some (afterHead.advance argValue)
      | 4 | 5 =>  -- array, map: skip items
          let itemCount := if major == 5 then argValue * 2 else argValue
          skipValues itemCount afterHead
      | 6 => skipValue afterHead  -- tag: skip the tagged value
      | _ => none

  /-- Skip n CBOR values in sequence -/
  partial def skipValues (n : Nat) (c : Cursor) : Option Cursor :=
    if n == 0 then some c
    else match skipValue c with
      | some next => skipValues (n - 1) next
      | none => none

  /-- Skip indefinite-length byte/text chunks until break -/
  partial def skipIndefiniteChunks (c : Cursor) : Option Cursor :=
    if c.remaining == 0 then none
    else if c.peek == 0xff then some (c.advance 1)
    else match skipValue c with
      | some next => skipIndefiniteChunks next
      | none => none

  /-- Skip indefinite-length array/map items until break -/
  partial def skipIndefiniteItems (c : Cursor) : Option Cursor :=
    if c.remaining == 0 then none
    else if c.peek == 0xff then some (c.advance 1)
    else match skipValue c with
      | some next => skipIndefiniteItems next
      | none => none
end

/-- Extract raw CBOR bytes spanning from cursor c1 to c2 (for rawBytes fields) -/
@[inline] def extractBetween (c1 c2 : Cursor) : ByteArray :=
  c1.data.extract c1.pos c2.pos

end Dion.Network.CborCursor
