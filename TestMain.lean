import Dion.Test.TestHarness
import Dion.Test.Blake2bTest
import Dion.Test.Ed25519Test
import Dion.Test.VRFTest
import Dion.Test.CborTest
import Dion.Test.KESTest
import Dion.Test.BlockRoundTripTest
import Dion.Test.PlutusConformanceTest
import Dion.Test.IntegrationTest
import Dion.Test.BLS12_381Test
import Dion.Test.Benchmark

/-!
# Dion Test Runner

Runs all conformance test suites and optionally benchmarks.

```
lake build test && .lake/build/bin/test           # Run tests only
lake build test && .lake/build/bin/test --bench    # Run tests + benchmarks
```
-/

open Dion.Test.TestHarness
open Dion.Test.Blake2bTest
open Dion.Test.Ed25519Test
open Dion.Test.VRFTest
open Dion.Test.CborTest
open Dion.Test.KESTest
open Dion.Test.BlockRoundTripTest
open Dion.Test.PlutusConformanceTest
open Dion.Test.IntegrationTest
open Dion.Test.BLS12_381Test
open Dion.Test.Benchmark

def main (args : List String) : IO UInt32 := do
  IO.println "╔══════════════════════════════════════════╗"
  IO.println "║  Dion Conformance Test Suite         ║"
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

  -- Block Round-Trip
  IO.println ""
  IO.println "--- Block CBOR Round-Trip ---"
  let blockResults ← runBlockRoundTripTests
  for r in blockResults do
    printResult r
    summary := summary.add r

  -- Plutus Conformance
  IO.println ""
  IO.println "--- Plutus Conformance (UPLC/CEK) ---"
  let plutusResults ← runPlutusConformanceTests
  for r in plutusResults do
    printResult r
    summary := summary.add r

  -- Integration (Forge → Parse Roundtrip)
  IO.println ""
  IO.println "--- Integration (Forge → Parse → Validate) ---"
  let integrationResults ← runIntegrationTests
  for r in integrationResults do
    printResult r
    summary := summary.add r

  -- BLS12-381
  IO.println ""
  IO.println "--- BLS12-381 (via blst FFI) ---"
  let blsResults ← runBLS12_381Tests
  for r in blsResults do
    printResult r
    summary := summary.add r

  -- Summary
  printSummary summary

  -- Benchmarks (optional)
  if args.contains "--bench" then
    IO.println ""
    runBenchmarks

  return if summary.failCount == 0 then 0 else 1
