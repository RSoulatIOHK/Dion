import Cleanode.Network.ChainSync
import Cleanode.Network.Crypto

/-!
# Cardano ImmutableDB Reader

Reads the cardano-node ImmutableDB format to extract chain tip information
from Mithril snapshots.

## ImmutableDB Layout
```
db/immutable/
  NNNNN.chunk      — concatenated raw blocks (CBOR)
  NNNNN.primary    — primary index: relative slot → secondary offset
  NNNNN.secondary  — secondary index: entry → block info (hash, offset, etc.)
```

## Secondary Index Format (post-Byron / Shelley+)

Each entry in the `.secondary` file is 56 bytes (no version header):
- blockSize     : 4 bytes (Word32 BE) — size of the block in the .chunk file
- blockOffset   : 4 bytes (Word32 BE) — offset of block in .chunk file
- headerOffset  : 2 bytes (Word16 BE) — offset of header within the block
- headerSize    : 2 bytes (Word16 BE) — size of the block header
- checksum      : 4 bytes (Word32 BE) — CRC32 of the block
- headerHash    : 32 bytes            — Blake2b-256 hash of the header (= block hash)
- blockOrEBB    : 8 bytes (Word64 BE) — slot number (or EBB epoch with high bit set)

## References
- ouroboros-consensus: Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
-/

namespace Cleanode.Mithril.ImmutableDB

open Cleanode.Network.ChainSync

-- ====================
-- = Binary Helpers   =
-- ====================

/-- Read a big-endian Word64 from 8 bytes -/
def readWord64BE (bs : ByteArray) (offset : Nat) : UInt64 :=
  if offset + 8 > bs.size then 0
  else
    let b0 := bs.get! offset |>.toUInt64
    let b1 := bs.get! (offset + 1) |>.toUInt64
    let b2 := bs.get! (offset + 2) |>.toUInt64
    let b3 := bs.get! (offset + 3) |>.toUInt64
    let b4 := bs.get! (offset + 4) |>.toUInt64
    let b5 := bs.get! (offset + 5) |>.toUInt64
    let b6 := bs.get! (offset + 6) |>.toUInt64
    let b7 := bs.get! (offset + 7) |>.toUInt64
    (b0 <<< 56) ||| (b1 <<< 48) ||| (b2 <<< 40) ||| (b3 <<< 32) |||
    (b4 <<< 24) ||| (b5 <<< 16) ||| (b6 <<< 8) ||| b7

/-- Read a big-endian Word16 from 2 bytes -/
def readWord16BE (bs : ByteArray) (offset : Nat) : UInt16 :=
  if offset + 2 > bs.size then 0
  else
    let b0 := bs.get! offset |>.toUInt16
    let b1 := bs.get! (offset + 1) |>.toUInt16
    (b0 <<< 8) ||| b1

/-- Read a big-endian Word32 from 4 bytes -/
def readWord32BE (bs : ByteArray) (offset : Nat) : UInt32 :=
  if offset + 4 > bs.size then 0
  else
    let b0 := bs.get! offset |>.toUInt32
    let b1 := bs.get! (offset + 1) |>.toUInt32
    let b2 := bs.get! (offset + 2) |>.toUInt32
    let b3 := bs.get! (offset + 3) |>.toUInt32
    (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| b3

-- ====================
-- = Secondary Index  =
-- ====================

/-- A parsed secondary index entry -/
structure SecondaryEntry where
  blockSize     : UInt32      -- Size of the block in the .chunk file
  blockOffset   : UInt32      -- Offset in the .chunk file
  headerOffset  : UInt16
  headerSize    : UInt16
  checksum      : UInt32
  headerHash    : ByteArray   -- 32 bytes, Blake2b-256 of the header
  blockOrEBB    : UInt64      -- Slot number (or EBB epoch with bit 63 set)

/-- Size of a secondary index entry in bytes:
    Word32 + Word32 + Word16 + Word16 + Word32 + 32 bytes + Word64 = 56 -/
def secondaryEntrySize : Nat := 56

/-- Parse a single secondary index entry from bytes at a given offset.
    Layout: blockSize(4) + blockOffset(4) + headerOffset(2) + headerSize(2)
            + checksum(4) + headerHash(32) + blockOrEBB(8) = 56 bytes -/
def parseSecondaryEntry (bs : ByteArray) (offset : Nat) : Option SecondaryEntry := do
  if offset + secondaryEntrySize > bs.size then none
  let blockSize := readWord32BE bs offset
  let blockOffset := readWord32BE bs (offset + 4)
  let headerOffset := readWord16BE bs (offset + 8)
  let headerSize := readWord16BE bs (offset + 10)
  let checksum := readWord32BE bs (offset + 12)
  let headerHash := bs.extract (offset + 16) (offset + 48)
  let blockOrEBB := readWord64BE bs (offset + 48)
  some {
    blockSize, blockOffset, headerOffset, headerSize, checksum, headerHash, blockOrEBB
  }

/-- The EBB bit mask (bit 63) -/
private def ebbMask : UInt64 := UInt64.ofNat (1 <<< 63)

/-- Check if a secondary entry is an EBB (Epoch Boundary Block) -/
def SecondaryEntry.isEBB (e : SecondaryEntry) : Bool :=
  (e.blockOrEBB &&& ebbMask) != 0

/-- Get the slot number from a secondary entry (masks out EBB bit) -/
def SecondaryEntry.slot (e : SecondaryEntry) : UInt64 :=
  if e.isEBB then e.blockOrEBB &&& ~~~ebbMask
  else e.blockOrEBB

-- ====================
-- = Chunk Discovery  =
-- ====================

/-- Format a chunk number as a 5-digit padded string (e.g., 00042) -/
def formatChunkNumber (n : Nat) : String :=
  let s := toString n
  let padding := String.mk (List.replicate (5 - min s.length 5) '0')
  padding ++ s

/-- Find the highest numbered chunk file in the immutable directory -/
def findLastChunkNumber (immutableDir : String) : IO (Option Nat) := do
  let entries ← System.FilePath.readDir immutableDir
  let mut maxChunk : Option Nat := none
  for entry in entries do
    let name := entry.fileName
    if name.endsWith ".chunk" then
      let numStr := name.dropRight 6  -- Drop ".chunk"
      match numStr.toNat? with
      | some n =>
        maxChunk := match maxChunk with
          | none => some n
          | some m => some (max m n)
      | none => pure ()
  return maxChunk

-- ====================
-- = Tip Discovery    =
-- ====================

/-- Information about the chain tip found in the ImmutableDB -/
structure ImmutableTip where
  slot       : Nat
  headerHash : ByteArray    -- 32-byte block hash
  chunkNumber : Nat          -- Which immutable chunk file
  entryCount : Nat          -- Number of blocks in that chunk

/-- Read the last entry from a secondary index file to find the tip -/
def readLastSecondaryEntry (secondaryPath : String) : IO (Option SecondaryEntry) := do
  let contents ← IO.FS.readBinFile secondaryPath
  if contents.size < secondaryEntrySize then
    return none
  -- No version header — file is just packed entries
  let entryCount := contents.size / secondaryEntrySize
  if entryCount == 0 then return none
  -- Read the last entry
  let lastOffset := (entryCount - 1) * secondaryEntrySize
  return parseSecondaryEntry contents lastOffset

/-- Check if a path exists -/
private def pathExists (p : String) : IO Bool :=
  (⟨p⟩ : System.FilePath).pathExists

/-- Read the chain tip from the ImmutableDB.
    Finds the highest-numbered chunk file and reads its last secondary index entry. -/
def readImmutableTip (dbDir : String) : IO (Option ImmutableTip) := do
  let immutableDir := s!"{dbDir}/immutable"
  -- Check if the immutable directory exists
  let dirExists ← pathExists immutableDir
  if !dirExists then
    IO.eprintln s!"[mithril] ImmutableDB directory not found: {immutableDir}"
    return none

  -- Find the last chunk
  match ← findLastChunkNumber immutableDir with
  | none =>
    IO.eprintln "[mithril] No chunk files found in ImmutableDB"
    return none
  | some chunkNum =>
    let secondaryPath := s!"{immutableDir}/{formatChunkNumber chunkNum}.secondary"
    let secExists ← pathExists secondaryPath
    if !secExists then
      IO.eprintln s!"[mithril] Secondary index not found: {secondaryPath}"
      return none

    match ← readLastSecondaryEntry secondaryPath with
    | none =>
      IO.eprintln s!"[mithril] Failed to read secondary index: {secondaryPath}"
      return none
    | some entry =>
      -- Read the secondary file to count entries
      let contents ← IO.FS.readBinFile secondaryPath
      let entryCount := contents.size / secondaryEntrySize
      return some {
        slot := entry.slot.toNat
        headerHash := entry.headerHash
        chunkNumber := chunkNum
        entryCount := entryCount
      }

/-- Convert an ImmutableTip to a ChainSync Point -/
def ImmutableTip.toPoint (tip : ImmutableTip) : Point :=
  { slot := UInt64.ofNat tip.slot, hash := tip.headerHash }

end Cleanode.Mithril.ImmutableDB
