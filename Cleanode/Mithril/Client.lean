import Cleanode.Mithril.Types
import Cleanode.Mithril.Certificate
import Cleanode.Mithril.Zstd
import Cleanode.Mithril.Tar
import Cleanode.Mithril.ImmutableDB
import Cleanode.Network.Http
import Cleanode.Network.ChainSync

/-!
# Mithril Client

High-level client for Mithril fast-sync:
1. Discover latest snapshot from aggregator
2. Verify certificate chain back to genesis
3. Download compressed snapshot
4. Decompress and extract archive
5. Read chain tip from ImmutableDB
6. Return sync point for peer sync resumption

## Usage
```
cleanode run --mithril-sync --preprod
```

## References
- https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node
-/

namespace Cleanode.Mithril.Client

open Cleanode.Mithril.Types
open Cleanode.Mithril.Certificate
open Cleanode.Mithril.Zstd
open Cleanode.Mithril.Tar
open Cleanode.Mithril.ImmutableDB
open Cleanode.Network.Http
open Cleanode.Network.ChainSync

-- ====================
-- = Client Errors    =
-- ====================

/-- Errors during Mithril sync -/
inductive MithrilError where
  | certError (e : CertChainError)
  | downloadError (msg : String)
  | decompressError (msg : String)
  | extractError (msg : String)
  | restoreError (msg : String)
  | noSnapshotAvailable
  deriving Repr

instance : ToString MithrilError where
  toString
    | .certError e => s!"Certificate error: {e}"
    | .downloadError msg => s!"Download error: {msg}"
    | .decompressError msg => s!"Decompress error: {msg}"
    | .extractError msg => s!"Extract error: {msg}"
    | .restoreError msg => s!"Restore error: {msg}"
    | .noSnapshotAvailable => "No snapshot available from aggregator"

-- ====================
-- = Sync Progress    =
-- ====================

/-- Progress callback for Mithril sync stages -/
inductive SyncStage where
  | discoveringSnapshot
  | verifyingCertificates (depth : Nat)
  | downloading (url : String) (sizeBytes : Nat)
  | decompressing
  | extracting
  | readingTip
  | complete (epoch : Nat) (immutableFileNum : Nat) (tipSlot : Nat)
  deriving Repr

/-- Default progress reporter that prints to stdout -/
def defaultProgressReporter (stage : SyncStage) : IO Unit := do
  match stage with
  | .discoveringSnapshot => IO.println "[mithril] Discovering latest snapshot..."
  | .verifyingCertificates depth => IO.println s!"[mithril] Verifying certificate chain (depth={depth})..."
  | .downloading url size =>
    let sizeMB := size / (1024 * 1024)
    IO.println s!"[mithril] Downloading snapshot ({sizeMB} MB)..."
    IO.println s!"[mithril]   URL: {url}"
  | .decompressing => IO.println "[mithril] Decompressing snapshot (zstd)..."
  | .extracting => IO.println "[mithril] Extracting tar archive..."
  | .readingTip => IO.println "[mithril] Reading chain tip from ImmutableDB..."
  | .complete epoch imm tipSlot =>
    IO.println s!"[mithril] Fast-sync complete! Epoch={epoch}, immutable_file_number={imm}, tip_slot={tipSlot}"

-- ====================
-- = Sync Result      =
-- ====================

/-- Result of a successful Mithril sync -/
structure MithrilSyncResult where
  snapshot : MithrilSnapshot
  chain : CertificateChain
  dbPath : String               -- Path to the extracted immutable DB
  tipPoint : Point              -- Chain tip point (slot + hash) for peer intersection
  tipSlot : Nat                 -- Tip slot for display
  deriving Repr

-- ====================
-- = Client Operations =
-- ====================

/-- Discover and verify the latest snapshot.
    Returns the snapshot + verified certificate chain. -/
def discoverAndVerify (config : AggregatorConfig)
    (report : SyncStage → IO Unit := defaultProgressReporter)
    : IO (Except MithrilError (MithrilSnapshot × CertificateChain)) := do
  -- Step 1: Fetch latest snapshot
  report .discoveringSnapshot
  let snapResult ← fetchLatestSnapshot config.aggregatorUrl
  match snapResult with
  | .error e => return .error (.certError e)
  | .ok snapshot =>
    IO.println s!"[mithril] Found snapshot: epoch={snapshot.beacon.epoch}, immutable={snapshot.beacon.immutableFileNumber}"
    IO.println s!"[mithril] Size: {snapshot.size / (1024 * 1024)} MB, compression: {snapshot.compressionAlgorithm}"

    -- Step 2: Verify certificate chain
    report (.verifyingCertificates 0)
    let chainResult ← fetchAndVerifyChain config.aggregatorUrl snapshot.certificateHash
    match chainResult with
    | .error e => return .error (.certError e)
    | .ok chain =>
      report (.verifyingCertificates chain.depth)
      IO.println s!"[mithril] Certificate chain verified: depth={chain.depth}, genesis={chain.genesisHash.take 16}..."
      return .ok (snapshot, chain)

/-- Download a snapshot to a local directory.
    Returns the path to the downloaded compressed file. -/
def downloadSnapshot (snapshot : MithrilSnapshot) (downloadDir : String)
    (report : SyncStage → IO Unit := defaultProgressReporter)
    : IO (Except MithrilError String) := do
  IO.FS.createDirAll downloadDir

  match snapshot.locations.head? with
  | none => return .error (.downloadError "No download locations in snapshot")
  | some url =>
    report (.downloading url snapshot.size)
    let compressedPath := s!"{downloadDir}/snapshot_{snapshot.beacon.epoch}_{snapshot.beacon.immutableFileNumber}.zst"

    -- Check if already downloaded (resume support)
    let fileExists ← System.FilePath.pathExists compressedPath
    if fileExists then
      IO.println s!"[mithril] Using cached download: {compressedPath}"
      return .ok compressedPath

    let dlResult ← httpDownload url compressedPath
    match dlResult with
    | .error msg => return .error (.downloadError msg)
    | .ok () =>
      IO.println s!"[mithril] Download complete: {compressedPath}"
      return .ok compressedPath

/-- Decompress a downloaded snapshot.
    Returns the path to the decompressed tar file. -/
def decompressSnapshot (compressedPath : String) (outputDir : String)
    (report : SyncStage → IO Unit := defaultProgressReporter)
    : IO (Except MithrilError String) := do
  report .decompressing
  IO.FS.createDirAll outputDir

  let decompressedPath := s!"{outputDir}/snapshot.tar"

  -- Check if already decompressed
  let cached ← System.FilePath.pathExists decompressedPath
  if cached then
    IO.println s!"[mithril] Using cached decompressed file: {decompressedPath}"
    return .ok decompressedPath

  let result ← decompressFile compressedPath decompressedPath
  match result with
  | .error msg => return .error (.decompressError msg)
  | .ok () =>
    IO.println s!"[mithril] Decompression complete: {decompressedPath}"
    return .ok decompressedPath

/-- Extract the tar archive into a directory.
    Returns the path to the extracted DB directory. -/
def extractSnapshot (tarPath : String) (dbDir : String)
    (report : SyncStage → IO Unit := defaultProgressReporter)
    : IO (Except MithrilError String) := do
  report .extracting

  -- Check if already extracted (immutable directory exists)
  let immutableDir := s!"{dbDir}/immutable"
  let extracted ← System.FilePath.pathExists immutableDir
  if extracted then
    IO.println s!"[mithril] Using cached extraction: {dbDir}"
    return .ok dbDir

  let result ← extractTar tarPath dbDir
  match result with
  | .error msg => return .error (.extractError msg)
  | .ok () =>
    IO.println s!"[mithril] Extraction complete: {dbDir}"
    return .ok dbDir

/-- Full Mithril sync pipeline: discover → verify → download → decompress → extract → read tip.
    Returns the sync result with the chain tip for peer sync resumption. -/
def mithrilSync (config : AggregatorConfig) (dataDir : String)
    (report : SyncStage → IO Unit := defaultProgressReporter)
    : IO (Except MithrilError MithrilSyncResult) := do
  -- Discover and verify
  let verifyResult ← discoverAndVerify config report
  match verifyResult with
  | .error e => return .error e
  | .ok (snapshot, chain) =>

    -- Download
    let downloadDir := s!"{dataDir}/mithril/download"
    let dlResult ← downloadSnapshot snapshot downloadDir report
    match dlResult with
    | .error e => return .error e
    | .ok compressedPath =>

      -- Decompress (.zst → .tar)
      let stageDir := s!"{dataDir}/mithril/stage"
      let decompResult ← decompressSnapshot compressedPath stageDir report
      match decompResult with
      | .error e => return .error e
      | .ok tarPath =>

        -- Extract (.tar → db/)
        let dbDir := s!"{dataDir}/mithril/db"
        let extractResult ← extractSnapshot tarPath dbDir report
        match extractResult with
        | .error e => return .error e
        | .ok extractedDir =>

          -- Read chain tip from ImmutableDB
          report .readingTip
          match ← readImmutableTip extractedDir with
          | none => return .error (.restoreError "Failed to read chain tip from ImmutableDB")
          | some tip =>
            let tipPoint := tip.toPoint
            IO.println s!"[mithril] Chain tip: slot={tip.slot}, hash={tip.headerHash.size}B, chunk={tip.chunkNumber}, entries={tip.entryCount}"
            report (.complete snapshot.beacon.epoch snapshot.beacon.immutableFileNumber tip.slot)
            return .ok {
              snapshot
              chain
              dbPath := extractedDir
              tipPoint := tipPoint
              tipSlot := tip.slot
            }

end Cleanode.Mithril.Client
