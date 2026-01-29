import Cleanode.Network.Cbor

/-!
# Byron Era Block Header Parser

Byron was the first era of Cardano (2017-2020). Byron blocks have a specific
CBOR structure that includes the block slot, previous block hash, and consensus data.

## Byron Header Format

The Byron header wrapper is: `[[slot, blockNo], tag24(actualHeader)]`

The actual header (CBOR array) contains:
- Protocol magic
- Previous block hash (32 bytes)
- Body proof/merkle root (32 bytes)
- Consensus data
- Extra attributes

## References
- cardano-ledger Byron spec
- ouroboros-consensus Byron codec
-/

namespace Cleanode.Network.Byron

open Cleanode.Network.Cbor

/-- Byron block header full details -/
structure ByronBlockHeader where
  protocolMagic : Nat
  prevBlockHash : ByteArray  -- 32 bytes
  bodyProof : ByteArray      -- Merkle root of transactions
  consensusData : ByteArray  -- Slot leader proof
  extraData : ByteArray      -- Protocol version, software version, attributes

instance : Repr ByronBlockHeader where
  reprPrec h _ := s!"ByronBlockHeader(magic={h.protocolMagic}, prevHash={h.prevBlockHash.size}B, bodyProof={h.bodyProof.size}B, consensus={h.consensusData.size}B)"

/-- Byron block header metadata extracted from wrapper -/
structure ByronHeaderInfo where
  slot : Nat                 -- Slot number when block was produced
  blockNo : Nat              -- Sequential block number (height)
  headerBytes : ByteArray    -- Raw CBOR header for hashing
  header : Option ByronBlockHeader  -- Parsed header details

instance : Repr ByronHeaderInfo where
  reprPrec info _ :=
    match info.header with
    | none => s!"ByronHeaderInfo(slot={info.slot}, blockNo={info.blockNo}, header={info.headerBytes.size}B)"
    | some h => s!"ByronHeaderInfo(slot={info.slot}, blockNo={info.blockNo}, prevHash={h.prevBlockHash.size}B, bodyProof={h.bodyProof.size}B)"

/-- Parse the actual Byron block header CBOR structure -/
partial def parseByronBlockHeader (bs : ByteArray) : Option ByronBlockHeader := do
  -- Byron header is a CBOR array: [protocolMagic, prevHash, bodyProof, consensusData, extraData]
  let r1 ← decodeArrayHeader bs
  -- Byron blocks are size 5 (both epoch boundary and regular blocks)
  if r1.value != 5 then none

  -- Protocol magic (can be encoded as different CBOR int sizes)
  let r2 ← decodeUInt r1.remaining
  let protocolMagic := r2.value

  -- Previous block hash (32 bytes for regular blocks, might be different for epoch boundary)
  let r3 ← decodeBytes r2.remaining
  let prevBlockHash := r3.value

  -- Body proof: In epoch boundary blocks this might be encoded differently
  -- Try to decode as bytes first
  match decodeBytes r3.remaining with
  | some r4 =>
      -- Got body proof as bytes
      let bodyProof := r4.value

      -- Consensus data (complex structure - store raw for now)
      let consensusStart := r4.remaining
      let some afterConsensus := skipCborValue r4.remaining | none
      let consensusLen := consensusStart.size - afterConsensus.size
      let consensusData := consensusStart.extract 0 consensusLen

      -- Extra data (protocol version, software version, attributes)
      let extraStart := afterConsensus
      let some afterExtra := skipCborValue afterConsensus | none
      let extraLen := extraStart.size - afterExtra.size
      let extraData := extraStart.extract 0 extraLen

      some {
        protocolMagic := protocolMagic,
        prevBlockHash := prevBlockHash,
        bodyProof := bodyProof,
        consensusData := consensusData,
        extraData := extraData
      }
  | none =>
      -- Body proof might be encoded as something else (like an array or uint)
      -- For now, skip it and store empty
      let some afterBodyProof := skipCborValue r3.remaining | none
      let bodyProofLen := r3.remaining.size - afterBodyProof.size
      let bodyProof := r3.remaining.extract 0 bodyProofLen

      -- Consensus data
      let consensusStart := afterBodyProof
      let some afterConsensus := skipCborValue afterBodyProof | none
      let consensusLen := consensusStart.size - afterConsensus.size
      let consensusData := consensusStart.extract 0 consensusLen

      -- Extra data
      let extraStart := afterConsensus
      let some afterExtra := skipCborValue afterConsensus | none
      let extraLen := extraStart.size - afterExtra.size
      let extraData := extraStart.extract 0 extraLen

      some {
        protocolMagic := protocolMagic,
        prevBlockHash := prevBlockHash,
        bodyProof := bodyProof,
        consensusData := consensusData,
        extraData := extraData
      }

/-- Parse Byron header wrapper: [[slot, blockNo], tag24(headerBytes)] -/
partial def parseByronHeader (bs : ByteArray) : Option ByronHeaderInfo := do
  -- Outer array: [slotInfo, headerBytes]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- First element: [slot, blockNo]
  let r2 ← decodeArrayHeader r1.remaining
  if r2.value != 2 then none

  -- Decode slot
  let r3 ← decodeUInt r2.remaining
  let slot := r3.value

  -- Decode blockNo
  let r4 ← decodeUInt r3.remaining
  let blockNo := r4.value

  -- Second element: tag24(headerBytes) - skip tag, extract bytes
  -- Tag 24 is encoded as: 0xd8 0x18 (major type 6, additional 24, tag number 24)
  if r4.remaining.size < 2 then none
  let tagByte := r4.remaining[0]!
  let major := tagByte >>> 5
  if major != 6 then none  -- Must be a tag

  -- CBOR tag with additional=24 means the tag number follows in next byte
  let additional := tagByte &&& 0x1f
  let tagSkipBytes := if additional == 24 then 2  -- d8 18 (tag byte + tag number byte)
                      else 1

  -- Skip tag bytes and decode the bytestring
  let afterTag := r4.remaining.extract tagSkipBytes r4.remaining.size
  let r5 ← decodeBytes afterTag
  let headerBytes := r5.value

  -- Try to parse the actual header
  let parsedHeader := parseByronBlockHeader headerBytes

  some {
    slot := slot,
    blockNo := blockNo,
    headerBytes := headerBytes,
    header := parsedHeader
  }

/-- Extract Byron header info from era-wrapped header bytes -/
def extractByronInfo (eraHeaderBytes : ByteArray) : Option ByronHeaderInfo :=
  parseByronHeader eraHeaderBytes

end Cleanode.Network.Byron
