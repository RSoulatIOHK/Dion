/-!
# Test Harness

Shared test infrastructure: result tracking, timing, hex utilities.
-/

namespace Dion.Test.TestHarness

/-- A single test result -/
structure TestResult where
  suite : String
  name : String
  passed : Bool
  durationMs : Option Float := none
  error : Option String := none

/-- Aggregate results -/
structure TestSummary where
  results : Array TestResult := #[]
  totalMs : Float := 0.0

def TestSummary.add (s : TestSummary) (r : TestResult) : TestSummary :=
  { s with results := s.results.push r }

def TestSummary.passCount (s : TestSummary) : Nat :=
  s.results.filter (·.passed) |>.size

def TestSummary.failCount (s : TestSummary) : Nat :=
  s.results.filter (fun r => !r.passed) |>.size

/-- Run a test, catching exceptions -/
def runTest (suite name : String) (test : IO Bool) : IO TestResult := do
  try
    let result ← test
    return { suite, name, passed := result }
  catch e =>
    return { suite, name, passed := false, error := some (toString e) }

/-- Run a test with timing -/
def runTestTimed (suite name : String) (test : IO Bool)
    (getTimeMs : IO UInt64) : IO TestResult := do
  try
    let start ← getTimeMs
    let result ← test
    let stop ← getTimeMs
    let elapsed := (stop - start).toNat.toFloat
    return { suite, name, passed := result, durationMs := some elapsed }
  catch e =>
    return { suite, name, passed := false, error := some (toString e) }

/-- Print a test result -/
def printResult (r : TestResult) : IO Unit := do
  let status := if r.passed then "PASS" else "FAIL"
  let timeStr := match r.durationMs with
    | some ms => s!" ({ms}ms)"
    | none => ""
  IO.println s!"  {status}: {r.name}{timeStr}"
  if let some e := r.error then
    IO.eprintln s!"       error: {e}"

/-- Print full summary -/
def printSummary (s : TestSummary) : IO Unit := do
  let total := s.results.size
  let passed := s.passCount
  let failed := s.failCount
  IO.println ""
  IO.println s!"Results: {passed}/{total} passed, {failed} failed"
  if failed > 0 then
    IO.println "FAILED tests:"
    for r in s.results do
      if !r.passed then
        IO.println s!"  {r.suite}/{r.name}"

/-- Convert hex string to ByteArray -/
def hexToBytes (hex : String) : ByteArray :=
  let hexCharToNat (c : Char) : Nat :=
    if c >= '0' && c <= '9' then c.toNat - '0'.toNat
    else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
    else if c >= 'A' && c <= 'F' then c.toNat - 'A'.toNat + 10
    else 0
  let chars := hex.toList
  let rec loop : List Char → List UInt8
    | [] => []
    | [_] => []
    | hi :: lo :: rest =>
      UInt8.ofNat (hexCharToNat hi * 16 + hexCharToNat lo) :: loop rest
  ByteArray.mk (loop chars).toArray

end Dion.Test.TestHarness
