import Cleanode.Plutus.UPLC.Term
import Cleanode.Plutus.ScriptContext

/-!
# Flat Binary Deserializer for UPLC

Plutus scripts are serialized in the "Flat" binary format — a bit-level encoding.
This is NOT CBOR. The Flat format packs data at the bit level for compactness.

Adapted from the formal specification at:
  https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf
  (Appendix C — Flat Encoding)

And from the sc-fvt reference implementation in PlutusCore/UPLC/FlatEncoding/Basic.lean.
-/

namespace Cleanode.Plutus.UPLC.Flat

open Cleanode.Plutus.UPLC
open Cleanode.Plutus.ScriptContext

-- ====================
-- = Bit Reader       =
-- ====================

/-- State for the bit-level reader — efficient ByteArray-backed bit stream -/
structure BitReader where
  bytes : ByteArray
  bytePos : Nat    -- current byte position
  bitPos : Nat     -- current bit within byte (0=MSB, 7=LSB)

instance : Repr BitReader where
  reprPrec r _ := s!"BitReader(pos={r.bytePos}, bit={r.bitPos}, size={r.bytes.size})"

/-- Create a BitReader from raw bytes -/
def BitReader.mk' (data : ByteArray) : BitReader :=
  { bytes := data, bytePos := 0, bitPos := 0 }

/-- Total bits remaining -/
def BitReader.bitsRemaining (r : BitReader) : Nat :=
  if r.bytePos >= r.bytes.size then 0
  else (r.bytes.size - r.bytePos) * 8 - r.bitPos

/-- Read a single bit, advance reader -/
def BitReader.readBit (r : BitReader) : Option (Bool × BitReader) :=
  if r.bytePos >= r.bytes.size then none
  else
    let byte := r.bytes[r.bytePos]!
    let bit := (byte >>> (7 - r.bitPos).toUInt8) &&& 1 != 0
    let r' := if r.bitPos >= 7 then
      { r with bytePos := r.bytePos + 1, bitPos := 0 }
    else
      { r with bitPos := r.bitPos + 1 }
    some (bit, r')

/-- Read n bits as a Nat (MSB first) — Spec C.2.1 fixed-width natural numbers -/
partial def BitReader.readBits (r : BitReader) (n : Nat) : Option (Nat × BitReader) :=
  let rec go (remaining : Nat) (acc : Nat) (reader : BitReader) : Option (Nat × BitReader) :=
    if remaining == 0 then some (acc, reader)
    else match reader.readBit with
      | none => none
      | some (b, reader') => go (remaining - 1) (acc * 2 + if b then 1 else 0) reader'
  go n 0 r

/-- Read a byte (8 bits) as UInt8 -/
def BitReader.readByte (r : BitReader) : Option (UInt8 × BitReader) :=
  match r.readBits 8 with
  | some (n, r') => some (n.toUInt8, r')
  | none => none

-- ====================
-- = Flat Primitives  =
-- ====================

/-- Spec C.1.1: Remove padding — find and consume the padding 1-bit and trailing zeros.
    Padding format: 0...01 (zero or more 0s followed by a 1) to align to byte boundary. -/
def unpad (r : BitReader) : Option BitReader :=
  -- Skip padding bits: find the `1` padding marker, then skip to byte boundary
  -- In Flat, padding is: some number of 0 bits followed by a single 1 bit,
  -- such that we end up byte-aligned. Just advance to the next byte boundary.
  if r.bitPos == 0 then some r
  else some { r with bytePos := r.bytePos + 1, bitPos := 0 }

/-- Spec C.2.2: Decode a list — each element preceded by a 1-bit, terminated by 0-bit.
    `f` decodes a single element from the bit stream. -/
partial def decodeList (f : BitReader → Option (α × BitReader)) (r : BitReader) :
    Option (List α × BitReader) := do
  let (cont, r') ← r.readBit
  if !cont then return ([], r')
  else
    let (x, r'') ← f r'
    let (xs, r''') ← decodeList f r''
    return (x :: xs, r''')

/-- Spec C.2.1: Decode a fixed-width natural number (n bits, MSB first). -/
def decodeFixedNat (n : Nat) (r : BitReader) : Option (Nat × BitReader) :=
  r.readBits n

/-- Spec C.2.3: Decode a variable-width natural number.
    Format: a list of 7-bit chunks (continuation-bit list encoding),
    followed by one final 7-bit chunk. Combined as little-endian groups. -/
def decodeNat (r : BitReader) : Option (Nat × BitReader) := do
  let (chunks, r') ← decodeList (decodeFixedNat 7) r
  let (final, r'') ← decodeFixedNat 7 r'
  let allChunks := chunks ++ [final]
  let value := allChunks.foldl (fun (acc, i) ki => (acc + ki * (2 ^ (7 * i)), i + 1)) (0, 0) |>.1
  return (value, r'')

/-- Spec C.2.4: Decode an integer (zigzag-encoded natural). -/
def decodeInt (r : BitReader) : Option (Int × BitReader) := do
  let (n, r') ← decodeNat r
  let value := if n % 2 == 0 then Int.ofNat (n / 2) else Int.negSucc (n / 2)
  return (value, r')

/-- Spec C.2.5: Decode a chunk of n bytes (D_C^n).
    Reads n bytes as 8-bit fixed nats. -/
partial def decodeChunk (count : Nat) (r : BitReader) (acc : ByteArray) :
    Option (ByteArray × BitReader) :=
  if count == 0 then some (acc, r)
  else do
    let (byte, r') ← decodeFixedNat 8 r
    decodeChunk (count - 1) r' (acc.push byte.toUInt8)

/-- Spec C.2.5: Decode one chunk group (D_C): 8-bit length prefix, then that many bytes. -/
def decodeChunks (r : BitReader) : Option (ByteArray × BitReader) := do
  let (len, r') ← decodeFixedNat 8 r
  decodeChunk len r' ByteArray.empty

/-- Spec C.2.5: Decode a sequence of chunk groups (D_C*):
    Read chunks until an empty chunk (length 0) is encountered. -/
partial def decodeChunksStar (r : BitReader) (acc : ByteArray) :
    Option (ByteArray × BitReader) := do
  let (chunk, r') ← decodeChunks r
  if chunk.size == 0 then return (acc, r')
  else decodeChunksStar r' (acc ++ chunk)

/-- Spec C.2.5: Decode a bytestring (D_B*).
    Removes padding first, then reads chunked bytes. -/
def decodeByteString (r : BitReader) : Option (ByteArray × BitReader) := do
  let r' ← unpad r
  decodeChunksStar r' ByteArray.empty

/-- Spec C.2.6: Decode a Unicode string (UTF-8 encoded bytestring). -/
def decodeString (r : BitReader) : Option (String × BitReader) := do
  let (bs, r') ← decodeByteString r
  return (String.fromUTF8! bs, r')

-- ====================
-- = Term Tag         =
-- ====================

/-- Read a 4-bit term tag -/
def readTermTag (r : BitReader) : Option (Nat × BitReader) :=
  r.readBits 4

/-- Read a de Bruijn index (variable-width natural) -/
def readDeBruijn (r : BitReader) : Option (Nat × BitReader) :=
  decodeNat r

/-- Read a builtin function index (7 bits fixed) -/
def readBuiltinIndex (r : BitReader) : Option (Nat × BitReader) :=
  decodeFixedNat 7 r

-- ====================
-- = Constant Types   =
-- ====================

/-- Spec C.3: Decode constant type tags.
    Type encoding uses a list of 4-bit tags (via Flat list encoding).
    The tag sequence is then interpreted structurally. -/
partial def decodeConstTypeFromTags (tags : List Nat) : Option (ConstType × List Nat) :=
  match tags with
  | 0 :: rest => some (.Integer, rest)
  | 1 :: rest => some (.ByteString, rest)
  | 2 :: rest => some (.String, rest)
  | 3 :: rest => some (.Unit, rest)
  | 4 :: rest => some (.Bool, rest)
  | 7 :: 5 :: rest => do  -- List
    let (inner, rest') ← decodeConstTypeFromTags rest
    return (.List inner, rest')
  | 7 :: 7 :: 6 :: rest => do  -- Pair
    let (a, rest') ← decodeConstTypeFromTags rest
    let (b, rest'') ← decodeConstTypeFromTags rest'
    return (.Pair a b, rest'')
  | 8 :: rest => some (.Data, rest)
  | _ => none

def readConstType (r : BitReader) : Option (ConstType × BitReader) := do
  -- Read the list of 4-bit type tags
  let (tags, r') ← decodeList (decodeFixedNat 4) r
  let (typ, remaining) ← decodeConstTypeFromTags tags
  -- All tags should be consumed
  if remaining.isEmpty then return (typ, r')
  else none

-- ====================
-- = Constant Values  =
-- ====================

/-- Read a constant value given its type -/
partial def readConstValue (r : BitReader) (typ : ConstType) : Option (Constant × BitReader) :=
  match typ with
  | .Integer => do
    let (v, r') ← decodeInt r
    return (.Integer v, r')
  | .ByteString => do
    let (bs, r') ← decodeByteString r
    return (.ByteString bs, r')
  | .String => do
    let (s, r') ← decodeString r
    return (.Str s, r')
  | .Unit => return (.Unit, r)
  | .Bool => do
    let (b, r') ← r.readBit
    return (.Bool b, r')
  | .Data => do
    -- Data constants are CBOR-encoded bytestrings decoded to PlutusData
    let (bs, r') ← decodeByteString r
    match decodePlutusDataCbor bs with
    | some d => return (.Data d, r')
    | none => none
  | .List inner => do
    let (items, r') ← decodeListValues r inner
    return (.List inner items, r')
  | .Pair a b => do
    let (va, r') ← readConstValue r a
    let (vb, r'') ← readConstValue r' b
    return (.Pair va vb, r'')
  | .Apply _ _ => none  -- Should not appear at value level
where
  decodeListValues (r : BitReader) (elemType : ConstType) : Option (List Constant × BitReader) := do
    let (hasMore, r') ← r.readBit
    if !hasMore then return ([], r')
    else
      let (item, r'') ← readConstValue r' elemType
      let (rest, r''') ← decodeListValues r'' elemType
      return (item :: rest, r''')
  /-- Decode PlutusData from CBOR bytes (for Data constants).
      Plutus Data constants are first Flat-decoded as bytestrings, then CBOR-decoded. -/
  decodePlutusDataCbor (_bs : ByteArray) : Option PlutusData :=
    -- TODO: implement full CBOR → PlutusData decoding
    -- For now, return a stub — most scripts don't embed Data constants inline
    some (PlutusData.Integer 0)

-- ====================
-- = Term Decoding    =
-- ====================

/-- Decode a UPLC term from the Flat bit stream -/
partial def decodeTerm (r : BitReader) : Option (Term × BitReader) := do
  let (tag, r') ← readTermTag r
  match tag with
  | 0 => -- Var
    let (idx, r'') ← readDeBruijn r'
    return (.Var idx, r'')
  | 1 => -- Delay
    let (t, r'') ← decodeTerm r'
    return (.Delay t, r'')
  | 2 => -- LamAbs
    let (body, r'') ← decodeTerm r'
    return (.LamAbs body, r'')
  | 3 => -- Apply
    let (fun_, r'') ← decodeTerm r'
    let (arg, r''') ← decodeTerm r''
    return (.Apply fun_ arg, r''')
  | 4 => -- Constant
    let (typ, r'') ← readConstType r'
    let (val, r''') ← readConstValue r'' typ
    return (.Constant val, r''')
  | 5 => -- Force
    let (t, r'') ← decodeTerm r'
    return (.Force t, r'')
  | 6 => -- Error
    return (.Error, r')
  | 7 => -- Builtin
    let (idx, r'') ← readBuiltinIndex r'
    match BuiltinFun.fromIndex idx with
    | some b => return (.Builtin b, r'')
    | none => none
  | 8 => -- Constr (PlutusV3)
    let (ctag, r'') ← decodeNat r'
    let (args, r''') ← decodeTermList r''
    return (.Constr ctag args, r''')
  | 9 => -- Case (PlutusV3)
    let (scrut, r'') ← decodeTerm r'
    let (cases, r''') ← decodeTermList r''
    return (.Case scrut cases, r''')
  | _ => none
where
  decodeTermList (r : BitReader) : Option (List Term × BitReader) :=
    decodeList decodeTerm r

-- ====================
-- = Program Decoding =
-- ====================

/-- Decode a full UPLC program from Flat-encoded bytes, with diagnostic on failure. -/
def decodeProgramDiag (data : ByteArray) : Except String Program :=
  let r := BitReader.mk' data
  match decodeNat r with
  | none => .error "failed to decode version major"
  | some (major, r') =>
  match decodeNat r' with
  | none => .error "failed to decode version minor"
  | some (minor, r'') =>
  match decodeNat r'' with
  | none => .error "failed to decode version patch"
  | some (patch, r''') =>
  match decodeTerm r''' with
  | none => .error s!"v{major}.{minor}.{patch} term decode failed at byte={r'''.bytePos} bit={r'''.bitPos} remaining={r'''.bitsRemaining}"
  | some (term, _) =>
    .ok { versionMajor := major, versionMinor := minor, versionPatch := patch, term }

/-- Decode a full UPLC program from Flat-encoded bytes.
    Format: version (3 naturals) + term + padding -/
def decodeProgram (data : ByteArray) : Option Program :=
  match decodeProgramDiag data with
  | .ok p => some p
  | .error _ => none

-- ====================
-- = CBOR Unwrapping  =
-- ====================

/-- Decode a definite-length CBOR byte string header.
    Returns (payloadStart, payloadLen) or (0, 0) on failure. -/
private def decodeCborBsHeader (bs : ByteArray) (off : Nat) : Nat × Nat :=
  if off >= bs.size then (0, 0)
  else
    let firstByte := bs[off]!
    let additionalInfo := firstByte &&& 0x1F
    if additionalInfo < 24 then (off + 1, additionalInfo.toNat)
    else if additionalInfo == 24 then
      if off + 1 >= bs.size then (0, 0)
      else (off + 2, bs[off + 1]!.toNat)
    else if additionalInfo == 25 then
      if off + 2 >= bs.size then (0, 0)
      else (off + 3, bs[off + 1]!.toNat * 256 + bs[off + 2]!.toNat)
    else if additionalInfo == 26 then
      if off + 4 >= bs.size then (0, 0)
      else (off + 5, bs[off + 1]!.toNat * 16777216 + bs[off + 2]!.toNat * 65536 +
                      bs[off + 3]!.toNat * 256 + bs[off + 4]!.toNat)
    else (0, 0)

/-- Collect chunks from an indefinite-length CBOR byte string.
    Starts at `pos` (after the 0x5F opener), reads definite-length chunks until 0xFF. -/
private partial def collectCborChunks (bs : ByteArray) (pos : Nat) (acc : ByteArray) : Option ByteArray :=
  if pos >= bs.size then none
  else if bs[pos]! == 0xFF then some acc  -- break code
  else
    let chunkByte := bs[pos]!
    let chunkMajor := chunkByte >>> 5
    if chunkMajor != 2 then none  -- chunks must be byte strings
    else
      let (payloadStart, payloadLen) := decodeCborBsHeader bs pos
      if payloadStart == 0 then none
      else
        let chunk := bs.extract payloadStart (payloadStart + payloadLen)
        collectCborChunks bs (payloadStart + payloadLen) (acc ++ chunk)

/-- Strip one CBOR bytestring wrapper (major type 2) from bytes.
    Handles both definite-length and indefinite-length (0x5F ... chunks ... 0xFF) encodings. -/
def stripCborByteString (bs : ByteArray) : Option ByteArray :=
  if bs.size < 2 then none
  else
    let firstByte := bs[0]!
    let majorType := firstByte >>> 5
    if majorType != 2 then none
    else
      let additionalInfo := firstByte &&& 0x1F
      if additionalInfo == 31 then
        -- Indefinite-length byte string (0x5F): concatenate chunks until 0xFF break
        collectCborChunks bs 1 ByteArray.empty
      else
        -- Definite-length byte string
        let (payloadStart, payloadLen) := decodeCborBsHeader bs 0
        if payloadStart == 0 then none
        else some (bs.extract payloadStart (payloadStart + payloadLen))

/-- Decode a Plutus script, returning either the Program or a diagnostic string.
    Handles double CBOR-encoded scripts (witness → CBOR bstr → CBOR bstr → Flat). -/
def decodePlutusScriptDiag (cborBytes : ByteArray) : Except String Program :=
  match stripCborByteString cborBytes with
  | none =>
    match decodeProgramDiag cborBytes with
    | .ok p => .ok p
    | .error msg => .error s!"no CBOR wrapper: {msg}"
  | some inner =>
    match stripCborByteString inner with
    | some innerInner =>
      match decodeProgramDiag innerInner with
      | .ok p => .ok p
      | .error msg => .error s!"double CBOR (flatSize={innerInner.size}): {msg}"
    | none =>
      match decodeProgramDiag inner with
      | .ok p => .ok p
      | .error msg => .error s!"single CBOR (innerSize={inner.size}): {msg}"

def decodePlutusScript (cborBytes : ByteArray) : Option Program :=
  match decodePlutusScriptDiag cborBytes with
  | .ok p => some p
  | .error _ => none

end Cleanode.Plutus.UPLC.Flat
