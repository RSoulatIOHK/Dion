import Cleanode.Mithril.Types
import Cleanode.Mithril.Certificate
import Cleanode.Mithril.Zstd
import Cleanode.Network.Http

/-!
# Mithril Client

High-level client for Mithril fast-sync:
1. Discover latest snapshot from aggregator
2. Verify certificate chain back to genesis
3. Download compressed snapshot
4. Decompress and restore ledger state

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
open Cleanode.Network.Http

-- ====================
-- = Client Errors    =
-- ====================

/-- Errors during Mithril sync -/
inductive MithrilError where
  | certError (e : CertChainError)
  | downloadError (msg : String)
  | decompressError (msg : String)
  | restoreError (msg : String)
  | noSnapshotAvailable
  deriving Repr

instance : ToString MithrilError where
  toString
    | .certError e => s!"Certificate error: {e}"
    | .downloadError msg => s!"Download error: {msg}"
    | .decompressError msg => s!"Decompress error: {msg}"
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
  | restoring
  | complete (epoch : Nat) (immutableFileNum : Nat)
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
  | .decompressing => IO.println "[mithril] Decompressing snapshot..."
  | .restoring => IO.println "[mithril] Restoring ledger state..."
  | .complete epoch imm =>
    IO.println s!"[mithril] Sync complete! Epoch={epoch}, immutable_file_number={imm}"

-- ====================
-- = Sync Result      =
-- ====================

/-- Result of a successful Mithril sync -/
structure MithrilSyncResult where
  snapshot : MithrilSnapshot
  chain : CertificateChain
  dbPath : String               -- Path to the restored immutable DB
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

    let dlResult ← httpDownload url compressedPath
    match dlResult with
    | .error msg => return .error (.downloadError msg)
    | .ok () =>
      IO.println s!"[mithril] Download complete: {compressedPath}"
      return .ok compressedPath

/-- Decompress a downloaded snapshot.
    Returns the path to the decompressed directory. -/
def decompressSnapshot (compressedPath : String) (outputDir : String)
    (report : SyncStage → IO Unit := defaultProgressReporter)
    : IO (Except MithrilError String) := do
  report .decompressing
  IO.FS.createDirAll outputDir

  let decompressedPath := s!"{outputDir}/snapshot.tar"
  let result ← decompressFile compressedPath decompressedPath
  match result with
  | .error msg => return .error (.decompressError msg)
  | .ok () =>
    IO.println s!"[mithril] Decompression complete: {decompressedPath}"
    return .ok decompressedPath

/-- Full Mithril sync pipeline: discover → verify → download → decompress.
    Returns the sync result with paths to restored data. -/
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

      -- Decompress
      let dbDir := s!"{dataDir}/mithril/db"
      let decompResult ← decompressSnapshot compressedPath dbDir report
      match decompResult with
      | .error e => return .error e
      | .ok _decompressedPath =>
        report (.complete snapshot.beacon.epoch snapshot.beacon.immutableFileNumber)
        return .ok {
          snapshot
          chain
          dbPath := dbDir
        }

end Cleanode.Mithril.Client
