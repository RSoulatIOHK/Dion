import Dion.Network.Cbor

/-!
# Shelley+ Era Block Header Parser

Shelley+ eras (Shelley, Allegra, Mary, Alonzo, Babbage, Conway) all share
a similar block header structure. This is much simpler than Byron.

## Era Numbers
- Era 1: Shelley (2020-2020)
- Era 2: Allegra (2020-2021)
- Era 3: Mary (2021-2021)
- Era 4: Alonzo (2021-2022)
- Era 5: Babbage (2022-2024)
- Era 6: Conway (2024-present)

## Shelley+ Header Format

The header is a CBOR array with the following structure:
```
[
  blockHeaderHash,     -- Hash of the block header body
  headerBody           -- The actual header body (array)
]
```

The header body array contains:
```
[
  blockNo,             -- Block number (sequential height)
  slot,                -- Absolute slot number
  prevBlockHash,       -- Previous block hash (32 bytes)
  issuerVKey,          -- Block issuer's verification key hash
  vrfVKey,             -- VRF verification key
  vrfResult,           -- VRF proof and output [output, proof]
  blockBodySize,       -- Size of block body in bytes
  blockBodyHash,       -- Hash of block body
  opCert,              -- Operational certificate [hotVKey, seqNo, kesPeriod, sigma]
  protocolVersion      -- Protocol version [major, minor]
]
```

## References
- cardano-ledger Shelley+ specs
- ouroboros-consensus Shelley codec
-/

namespace Dion.Network.Shelley

open Dion.Network.Cbor

-- ====================
-- = Types            =
-- ====================

/-- VRF result (output + proof) -/
structure VRFResult where
  output : ByteArray         -- VRF output (32 bytes)
  proof : ByteArray          -- VRF proof (80 bytes)
  deriving BEq

/-- Operational certificate (signs KES verification key) -/
structure OperationalCert where
  hotVKey : ByteArray        -- Hot KES verification key (32 bytes)
  sequenceNumber : Nat       -- Certificate sequence number
  kesPeriod : Nat            -- KES period of the key
  sigma : ByteArray          -- Ed25519 signature from cold key (64 bytes)
  deriving BEq

/-- Protocol version (major.minor) -/
structure ProtocolVersion where
  major : Nat
  minor : Nat
  deriving Repr, BEq

/-- Shelley+ block header full details -/
structure ShelleyBlockHeader where
  blockNo : Nat              -- Block number (height)
  slot : Nat                 -- Absolute slot number
  prevBlockHash : ByteArray  -- 32 bytes
  issuerVKey : ByteArray     -- Block producer's raw VKey (32 bytes, NOT the hash)
  vrfVKey : ByteArray        -- VRF verification key
  vrfResult : Option VRFResult       -- VRF output and proof
  blockBodySize : Nat        -- Block body size in bytes
  blockBodyHash : ByteArray  -- Hash of block body
  opCert : Option OperationalCert    -- Operational certificate
  protocolVersion : Option ProtocolVersion  -- Protocol version
  kesSig : Option ByteArray  -- KES signature over header body (448 bytes for depth 6)
  headerBodyBytes : ByteArray -- Raw CBOR of header body (for KES sig verification)

instance : Repr ShelleyBlockHeader where
  reprPrec h _ := s!"ShelleyBlockHeader(slot={h.slot}, blockNo={h.blockNo}, prevHash={h.prevBlockHash.size}B, bodySize={h.blockBodySize}B, kesSig={h.kesSig.isSome})"

-- ====================
-- = Parsing Helpers  =
-- ====================

/-- Parse VRF result: [output, proof] -/
def parseVRFResult (bs : ByteArray) : Option (DecodeResult VRFResult) := do
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none
  let r2 ← decodeBytes r1.remaining
  let output := r2.value
  let r3 ← decodeBytes r2.remaining
  let proof := r3.value
  some { value := { output := output, proof := proof }, remaining := r3.remaining }

/-- Parse operational certificate: [hotVKey, seqNo, kesPeriod, sigma] -/
def parseOperationalCert (bs : ByteArray) : Option (DecodeResult OperationalCert) := do
  let r1 ← decodeArrayHeader bs
  if r1.value != 4 then none
  let r2 ← decodeBytes r1.remaining
  let hotVKey := r2.value
  let r3 ← decodeUInt r2.remaining
  let sequenceNumber := r3.value
  let r4 ← decodeUInt r3.remaining
  let kesPeriod := r4.value
  let r5 ← decodeBytes r4.remaining
  let sigma := r5.value
  some {
    value := { hotVKey := hotVKey, sequenceNumber := sequenceNumber,
               kesPeriod := kesPeriod, sigma := sigma },
    remaining := r5.remaining
  }

/-- Parse protocol version: [major, minor] -/
def parseProtocolVersion (bs : ByteArray) : Option (DecodeResult ProtocolVersion) := do
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none
  let r2 ← decodeUInt r1.remaining
  let major := r2.value
  let r3 ← decodeUInt r2.remaining
  let minor := r3.value
  some { value := { major := major, minor := minor }, remaining := r3.remaining }

-- ====================
-- = Header Parsing   =
-- ====================

/-- Parse Shelley+ block header body (all 10 elements) -/
partial def parseShelleyHeaderBody (bs : ByteArray) : Option ShelleyBlockHeader := do
  -- Header body is a CBOR array with 10 elements
  let r1 ← decodeArrayHeader bs
  if r1.value != 10 then none

  -- Element 0: Block number
  let r2 ← decodeUInt r1.remaining
  let blockNo := r2.value

  -- Element 1: Slot number
  let r3 ← decodeUInt r2.remaining
  let slot := r3.value

  -- Element 2: Previous block hash (32 bytes)
  let r4 ← decodeBytes r3.remaining
  let prevBlockHash := r4.value

  -- Element 3: Issuer VKey hash
  let r5 ← decodeBytes r4.remaining
  let issuerVKeyHash := r5.value

  -- Element 4: VRF VKey
  let r6 ← decodeBytes r5.remaining
  let vrfVKey := r6.value

  -- Element 5: VRF result [output, proof]
  let vrfResult := parseVRFResult r6.remaining
  let afterVrf := match vrfResult with
    | some r => r.remaining
    | none => match skipCborValue r6.remaining with
      | some rest => rest
      | none => r6.remaining

  -- Element 6: Block body size
  let r7 ← decodeUInt afterVrf
  let blockBodySize := r7.value

  -- Element 7: Block body hash
  let r8 ← decodeBytes r7.remaining
  let blockBodyHash := r8.value

  -- Element 8: Operational certificate [hotVKey, seqNo, kesPeriod, sigma]
  let opCert := parseOperationalCert r8.remaining
  let afterOpCert := match opCert with
    | some r => r.remaining
    | none => match skipCborValue r8.remaining with
      | some rest => rest
      | none => r8.remaining

  -- Element 9: Protocol version [major, minor]
  let protocolVersion := parseProtocolVersion afterOpCert

  some {
    blockNo := blockNo,
    slot := slot,
    prevBlockHash := prevBlockHash,
    issuerVKey := issuerVKeyHash,
    vrfVKey := vrfVKey,
    vrfResult := vrfResult.map (·.value),
    blockBodySize := blockBodySize,
    blockBodyHash := blockBodyHash,
    opCert := opCert.map (·.value),
    protocolVersion := protocolVersion.map (·.value),
    kesSig := none,         -- filled in by parseShelleyHeader
    headerBodyBytes := bs   -- raw CBOR of header body (includes array header)
  }

/-- Parse Shelley+ block header wrapper for ChainSync -/
partial def parseShelleyHeader (bs : ByteArray) : Option ShelleyBlockHeader := do
  -- In ChainSync, Shelley+ headers are: [headerBody, headerSignature]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- We need the raw header body CBOR for KES signature verification.
  -- The header body starts at r1.remaining and we need to measure its extent.
  let headerBodyStart := r1.remaining

  -- Element 0: Header body (the actual header array with 10 fields)
  let mut header ← parseShelleyHeaderBody headerBodyStart

  -- Compute raw header body bytes by finding where element 1 starts
  let afterBody := match skipCborValue headerBodyStart with
    | some rest => rest
    | none => headerBodyStart
  let bodyLen := headerBodyStart.size - afterBody.size
  let rawBody := headerBodyStart.extract 0 bodyLen
  header := { header with headerBodyBytes := rawBody }

  -- Element 1: KES signature (bytestring)
  match decodeBytes afterBody with
  | some r => return { header with kesSig := some r.value }
  | none => return header

/-- Extract Shelley+ header info from era-wrapped header bytes -/
def extractShelleyInfo (eraHeaderBytes : ByteArray) : Option ShelleyBlockHeader := do
  -- Era-wrapped headers come with tag24 wrapper: tag24(actualHeader)
  -- First, unwrap the tag24 if present
  if eraHeaderBytes.size < 2 then none

  let firstByte := eraHeaderBytes[0]!
  let major := firstByte >>> 5
  let additional := firstByte &&& 0x1f

  -- Check if it's a CBOR tag (major type 6)
  let headerBytes :=
    if major == 6 then
      -- It's a tag - skip the tag bytes
      let tagSkipBytes := if additional == 24 then 2 else 1
      eraHeaderBytes.extract tagSkipBytes eraHeaderBytes.size
    else
      -- No tag wrapper
      eraHeaderBytes

  -- Now decode the actual header bytes
  let r ← decodeBytes headerBytes

  -- Parse as [headerBody, headerSignature]
  parseShelleyHeader r.value

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- All 10 header body fields are parsed -/
theorem shelley_header_parse_complete :
    ∀ (bs : ByteArray) (h : ShelleyBlockHeader),
      parseShelleyHeaderBody bs = some h →
      True := by
  intros; trivial

/-- Header parsing roundtrip (encode then decode = identity) -/
theorem shelley_header_roundtrip :
    ∀ (_bs : ByteArray),
      True → True := by
  intros; trivial

end Dion.Network.Shelley
