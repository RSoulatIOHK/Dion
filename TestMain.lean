import Cleanode.Test.TestHarness
import Cleanode.Test.Blake2bTest
import Cleanode.Test.Ed25519Test
import Cleanode.Test.VRFTest
import Cleanode.Test.CborTest
import Cleanode.Test.KESTest
import Cleanode.Test.Benchmark

/-!
# Cleanode Test Runner

Runs all conformance test suites and optionally benchmarks.

```
lake build test && .lake/build/bin/test           # Run tests only
lake build test && .lake/build/bin/test --bench    # Run tests + benchmarks
```
-/

open Cleanode.Test.TestHarness
open Cleanode.Test.Blake2bTest
open Cleanode.Test.Ed25519Test
open Cleanode.Test.VRFTest
open Cleanode.Test.CborTest
open Cleanode.Test.KESTest
open Cleanode.Test.Benchmark

def main (args : List String) : IO UInt32 := do
  IO.println "╔══════════════════════════════════════════╗"
  IO.println "║  Cleanode Conformance Test Suite         ║"
  IO.println "╚══════════════════════════════════════════╝"
  IO.println ""

  let mut summary : TestSummary := {}

  -- Blake2b-256
  IO.println "--- Blake2b-256 ---"
  let blake2bResults ← runBlake2b256Tests
  for r in blake2bResults do
    printResult r
    summary := summary.add r

  -- Ed25519
  IO.println ""
  IO.println "--- Ed25519 ---"
  let ed25519Results ← runEd25519Tests
  for r in ed25519Results do
    printResult r
    summary := summary.add r

  -- VRF
  IO.println ""
  IO.println "--- VRF (ECVRF-ED25519-SHA512-Elligator2) ---"
  let vrfResults ← runVRFTests
  for r in vrfResults do
    printResult r
    summary := summary.add r

  -- CBOR
  IO.println ""
  IO.println "--- CBOR ---"
  let cborResults ← runCborTests
  for r in cborResults do
    printResult r
    summary := summary.add r

  -- KES
  IO.println ""
  IO.println "--- KES (Sum-KES-6) ---"
  let kesResults ← runKESTests
  for r in kesResults do
    printResult r
    summary := summary.add r

  -- Summary
  printSummary summary

  -- Benchmarks (optional)
  if args.contains "--bench" then
    IO.println ""
    runBenchmarks

  return if summary.failCount == 0 then 0 else 1
