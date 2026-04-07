/-!
# Tar Archive Extraction

Extracts tar archives using the system `tar` command.
Used by the Mithril pipeline to unpack downloaded snapshots.
-/

namespace Dion.Mithril.Tar

/-- Extract a tar archive to an output directory.
    Uses the system `tar` command (available on macOS/Linux). -/
def extractTar (tarPath : String) (outputDir : String) : IO (Except String Unit) := do
  IO.FS.createDirAll outputDir
  let result ← IO.Process.output {
    cmd := "tar"
    args := #["xf", tarPath, "-C", outputDir]
  }
  if result.exitCode == 0 then
    return .ok ()
  else
    return .error s!"tar extraction failed (exit {result.exitCode}): {result.stderr}"

end Dion.Mithril.Tar
