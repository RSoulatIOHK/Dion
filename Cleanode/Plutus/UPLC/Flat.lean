import Cleanode.Plutus.UPLC.Term
import Cleanode.Plutus.ScriptContext

/-!
# Flat Binary Deserializer for UPLC

Plutus scripts are serialized in the "Flat" binary format — a bit-level encoding.
This is NOT CBOR. The Flat format packs data at the bit level for compactness.

## Encoding Summary
- Terms use tag bits: 0=Var, 1=Delay, 2=LamAbs, 3=Apply, 4=Constant, 5=Force, 6=Builtin, 7=Error, 8=Constr, 9=Case
- Integers use a variable-length encoding (7 bits per byte, high bit = continuation)
- ByteStrings: length prefix then raw bytes (padded to byte boundary)
- Constants use type tags followed by values

## References
- Flat encoding specification
- plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Core/Instance/Flat.hs
-/

namespace Cleanode.Plutus.UPLC.Flat

open Cleanode.Plutus.UPLC
open Cleanode.Plutus.ScriptContext

-- ====================
-- = Bit Reader       =
-- ====================

/-- State for the bit-level reader -/
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

/-- Read a single bit (0 or 1), advance reader -/
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

/-- Read n bits as a Nat (MSB first) -/
partial def BitReader.readBits (r : BitReader) (n : Nat) : Option (Nat × BitReader) :=
  let rec go (remaining : Nat) (acc : Nat) (reader : BitReader) : Option (Nat × BitReader) :=
    if remaining == 0 then some (acc, reader)
    else match reader.readBit with
      | none => none
      | some (b, reader') => go (remaining - 1) (acc * 2 + if b then 1 else 0) reader'
  go n 0 r

/-- Read a byte (8 bits) -/
def BitReader.readByte (r : BitReader) : Option (UInt8 × BitReader) :=
  match r.readBits 8 with
  | some (n, r') => some (n.toUInt8, r')
  | none => none

/-- Skip padding to next byte boundary -/
def BitReader.skipPadding (r : BitReader) : BitReader :=
  if r.bitPos == 0 then r
  else { r with bytePos := r.bytePos + 1, bitPos := 0 }

/-- Read raw bytes (must be byte-aligned first) -/
def BitReader.readBytes (r : BitReader) (n : Nat) : Option (ByteArray × BitReader) :=
  let r' := r.skipPadding
  if r'.bytePos + n > r'.bytes.size then none
  else
    let bytes := r'.bytes.extract r'.bytePos (r'.bytePos + n)
    some (bytes, { r' with bytePos := r'.bytePos + n })

-- ====================
-- = Flat Decoding    =
-- ====================

/-- Read a Flat-encoded natural number (unary-then-binary for small nats,
    or continuation-bit encoding).
    Flat uses a specific encoding where each group of bits is preceded by a continuation bit. -/
partial def readNatural (r : BitReader) : Option (Nat × BitReader) :=
  -- Read 8-bit chunks with high-bit continuation
  let rec go (acc : Nat) (shift : Nat) (reader : BitReader) (fuel : Nat) : Option (Nat × BitReader) :=
    if fuel == 0 then none
    else match reader.readByte with
      | none => none
      | some (byte, reader') =>
        let value := (byte &&& 0x7F).toNat
        let acc' := acc ||| (value <<< shift)
        if byte &&& 0x80 == 0 then some (acc', reader')
        else go acc' (shift + 7) reader' (fuel - 1)
  go 0 0 r 100

/-- Read a Flat-encoded integer (zigzag encoded natural) -/
def readInteger (r : BitReader) : Option (Int × BitReader) := do
  let (n, r') ← readNatural r
  -- Zigzag decode: even → positive, odd → negative
  let value := if n % 2 == 0 then Int.ofNat (n / 2) else Int.negSucc (n / 2)
  return (value, r')

/-- Read a Flat-encoded bytestring: length-prefixed chunks.
    Format: repeat { 1-byte length, then that many bytes } until length=0 -/
partial def readByteString (r : BitReader) : Option (ByteArray × BitReader) :=
  let r' := r.skipPadding  -- ByteStrings start on byte boundary
  let rec go (acc : ByteArray) (reader : BitReader) (fuel : Nat) : Option (ByteArray × BitReader) :=
    if fuel == 0 then none
    else match reader.readByte with
      | none => none
      | some (len, reader') =>
        if len == 0 then some (acc, reader')
        else match reader'.readBytes len.toNat with
          | none => none
          | some (chunk, reader'') => go (acc ++ chunk) reader'' (fuel - 1)
  go ByteArray.empty r' 1000

/-- Read a Flat-encoded string (UTF-8 bytestring) -/
def readString (r : BitReader) : Option (String × BitReader) := do
  let (bytes, r') ← readByteString r
  return (String.fromUTF8! bytes, r')

/-- Read a Flat-encoded term tag (4 bits) -/
def readTermTag (r : BitReader) : Option (Nat × BitReader) :=
  r.readBits 4

/-- Read a de Bruijn index (natural number) -/
def readDeBruijn (r : BitReader) : Option (Nat × BitReader) :=
  readNatural r

/-- Read a builtin function index (7 bits for PlutusV3) -/
def readBuiltinIndex (r : BitReader) : Option (Nat × BitReader) :=
  r.readBits 7

-- ====================
-- = Constant Types   =
-- ====================

/-- Read a constant type tag from the Flat stream.
    Type tags use a list encoding: sequence of 1-prefixed 4-bit tags, terminated by 0. -/
partial def readConstType (r : BitReader) : Option (ConstType × BitReader) := do
  let (tag, r') ← r.readBits 4
  match tag with
  | 0 => return (.Integer, r')
  | 1 => return (.ByteString, r')
  | 2 => return (.String, r')
  | 3 => return (.Unit, r')
  | 4 => return (.Bool, r')
  | 5 =>
    let (inner, r'') ← readConstTypeList r'
    match inner with
    | [t] => return (.List t, r'')
    | _ => none
  | 6 =>
    let (inner, r'') ← readConstTypeList r'
    match inner with
    | [a, b] => return (.Pair a b, r'')
    | _ => none
  | 7 => return (.Data, r')
  | _ => none
where
  readConstTypeList (r : BitReader) : Option (List ConstType × BitReader) := do
    let (cont, r') ← r.readBit
    if !cont then return ([], r')
    else
      let (t, r'') ← readConstType r'
      let (rest, r''') ← readConstTypeList r''
      return (t :: rest, r''')

-- ====================
-- = Constant Values  =
-- ====================

/-- Read a constant value given its type -/
partial def readConstValue (r : BitReader) (typ : ConstType) : Option (Constant × BitReader) :=
  match typ with
  | .Integer => do
    let (v, r') ← readInteger r
    return (.Integer v, r')
  | .ByteString => do
    let (bs, r') ← readByteString r
    return (.ByteString bs, r')
  | .String => do
    let (s, r') ← readString r
    return (.Str s, r')
  | .Unit => return (.Unit, r)
  | .Bool => do
    let (b, r') ← r.readBit
    return (.Bool b, r')
  | .Data => do
    let (d, r') ← readPlutusData r
    return (.Data d, r')
  | .List inner => do
    let (items, r') ← readList r inner
    return (.List inner items, r')
  | .Pair a b => do
    let (va, r') ← readConstValue r a
    let (vb, r'') ← readConstValue r' b
    return (.Pair va vb, r'')
  | .Apply _ _ => none  -- Should not appear at value level
where
  readList (r : BitReader) (elemType : ConstType) : Option (List Constant × BitReader) := do
    let (hasMore, r') ← r.readBit
    if !hasMore then return ([], r')
    else
      let (item, r'') ← readConstValue r' elemType
      let (rest, r''') ← readList r'' elemType
      return (item :: rest, r''')
  readPlutusData (r : BitReader) : Option (PlutusData × BitReader) := do
    let (tag, r') ← r.readBits 3
    match tag with
    | 0 => -- Constr
      let (ctag, r'') ← readNatural r'
      let (fields, r''') ← readDataList r''
      return (.Constr ctag fields, r''')
    | 1 => -- Map
      let (entries, r'') ← readDataPairs r'
      return (.Map entries, r'')
    | 2 => -- List
      let (items, r'') ← readDataList r'
      return (.List items, r'')
    | 3 => -- Integer
      let (v, r'') ← readInteger r'
      return (.Integer v, r'')
    | 4 => -- ByteString
      let (bs, r'') ← readByteString r'
      return (.ByteString bs, r'')
    | _ => none
  readDataList (r : BitReader) : Option (List PlutusData × BitReader) := do
    let (hasMore, r') ← r.readBit
    if !hasMore then return ([], r')
    else
      let (item, r'') ← readPlutusData r'
      let (rest, r''') ← readDataList r''
      return (item :: rest, r''')
  readDataPairs (r : BitReader) : Option (List (PlutusData × PlutusData) × BitReader) := do
    let (hasMore, r') ← r.readBit
    if !hasMore then return ([], r')
    else
      let (k, r'') ← readPlutusData r'
      let (v, r''') ← readPlutusData r''
      let (rest, r'''') ← readDataPairs r'''
      return ((k, v) :: rest, r'''')

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
  | 6 => -- Builtin
    let (idx, r'') ← readBuiltinIndex r'
    match BuiltinFun.fromIndex idx with
    | some b => return (.Builtin b, r'')
    | none => none
  | 7 => -- Error
    return (.Error, r')
  | 8 => -- Constr (PlutusV3)
    let (ctag, r'') ← readNatural r'
    let (args, r''') ← decodeTermList r''
    return (.Constr ctag args, r''')
  | 9 => -- Case (PlutusV3)
    let (scrut, r'') ← decodeTerm r'
    let (cases, r''') ← decodeTermList r''
    return (.Case scrut cases, r''')
  | _ => none
where
  decodeTermList (r : BitReader) : Option (List Term × BitReader) := do
    let (hasMore, r') ← r.readBit
    if !hasMore then return ([], r')
    else
      let (t, r'') ← decodeTerm r'
      let (rest, r''') ← decodeTermList r''
      return (t :: rest, r''')

-- ====================
-- = Program Decoding =
-- ====================

/-- Decode a full UPLC program from Flat-encoded bytes.
    Format: version (3 naturals) + term + padding -/
def decodeProgram (data : ByteArray) : Option Program := do
  let r := BitReader.mk' data
  let (major, r') ← readNatural r
  let (minor, r'') ← readNatural r'
  let (patch, r''') ← readNatural r''
  let (term, _) ← decodeTerm r'''
  return { versionMajor := major, versionMinor := minor, versionPatch := patch, term }

/-- Decode a Plutus script from its CBOR-wrapped bytes.
    Plutus scripts on-chain are CBOR-encoded bytestrings containing Flat-encoded programs.
    This function strips the outer CBOR wrapper and decodes the Flat payload. -/
def decodePlutusScript (cborBytes : ByteArray) : Option Program :=
  -- The outer layer is a CBOR bytestring. Check for major type 2 (bytestring).
  if cborBytes.size < 2 then none
  else
    let firstByte := cborBytes[0]!
    let majorType := firstByte >>> 5
    if majorType != 2 then
      -- Try decoding directly as Flat (some contexts don't CBOR-wrap)
      decodeProgram cborBytes
    else
      -- Extract bytestring length and payload
      let additionalInfo := firstByte &&& 0x1F
      let (payloadStart, _payloadLen) :=
        if additionalInfo < 24 then (1, additionalInfo.toNat)
        else if additionalInfo == 24 then
          if cborBytes.size < 2 then (0, 0)
          else (2, cborBytes[1]!.toNat)
        else if additionalInfo == 25 then
          if cborBytes.size < 3 then (0, 0)
          else (3, cborBytes[1]!.toNat * 256 + cborBytes[2]!.toNat)
        else (0, 0)  -- Longer lengths not expected for scripts
      if payloadStart == 0 then none
      else
        let flatBytes := cborBytes.extract payloadStart cborBytes.size
        decodeProgram flatBytes

end Cleanode.Plutus.UPLC.Flat
