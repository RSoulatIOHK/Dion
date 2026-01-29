import Cleanode.Network.Cbor

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
  vrfResult,           -- VRF proof and output
  blockBodySize,       -- Size of block body in bytes
  blockBodyHash,       -- Hash of block body
  opCert,              -- Operational certificate
  protocolVersion      -- Protocol version [major, minor]
]
```

## References
- cardano-ledger Shelley+ specs
- ouroboros-consensus Shelley codec
-/

namespace Cleanode.Network.Shelley

open Cleanode.Network.Cbor

/-- Shelley+ block header full details -/
structure ShelleyBlockHeader where
  blockNo : Nat              -- Block number (height)
  slot : Nat                 -- Absolute slot number
  prevBlockHash : ByteArray  -- 32 bytes
  issuerVKeyHash : ByteArray -- Block producer's VKey hash
  vrfVKey : ByteArray        -- VRF verification key
  blockBodySize : Nat        -- Block body size in bytes
  blockBodyHash : ByteArray  -- Hash of block body
  -- Note: Skipping complex structures (VRF result, opCert, protocolVersion) for now

instance : Repr ShelleyBlockHeader where
  reprPrec h _ := s!"ShelleyBlockHeader(slot={h.slot}, blockNo={h.blockNo}, prevHash={h.prevBlockHash.size}B, bodySize={h.blockBodySize}B)"

/-- Parse Shelley+ block header body -/
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

  -- Element 4: VRF VKey (we'll store but not fully parse)
  let r6 ← decodeBytes r5.remaining
  let vrfVKey := r6.value

  -- Element 5: VRF result (complex structure - skip for now)
  let some afterVrfResult := skipCborValue r6.remaining | none

  -- Element 6: Block body size
  let r7 ← decodeUInt afterVrfResult
  let blockBodySize := r7.value

  -- Element 7: Block body hash
  let r8 ← decodeBytes r7.remaining
  let blockBodyHash := r8.value

  -- Elements 8-9: Operational cert and protocol version (skip for now)

  some {
    blockNo := blockNo,
    slot := slot,
    prevBlockHash := prevBlockHash,
    issuerVKeyHash := issuerVKeyHash,
    vrfVKey := vrfVKey,
    blockBodySize := blockBodySize,
    blockBodyHash := blockBodyHash
  }

/-- Parse Shelley+ block header wrapper for ChainSync -/
partial def parseShelleyHeader (bs : ByteArray) : Option ShelleyBlockHeader := do
  -- In ChainSync, Shelley+ headers are: [headerBody, headerSignature]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- Element 0: Header body (the actual header array with 10 fields)
  parseShelleyHeaderBody r1.remaining

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

end Cleanode.Network.Shelley
