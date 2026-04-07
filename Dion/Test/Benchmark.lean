import Dion.Network.Crypto
import Dion.Network.CryptoSpec
import Dion.Test.TestHarness
import Dion.Consensus.Praos.ForgeLoop

/-!
# Benchmark Timing Harness

Times cryptographic operations over multiple iterations and reports
mean/min/max per operation. Used for performance regression tracking.

## Operations Benchmarked
- Blake2b-256 (32-byte, 1KB, 64KB inputs)
- Ed25519 sign / verify
- VRF proof_to_hash
- SHA-512

## Output Format
Machine-readable lines: `bench:<name>:<iterations>:<total_ms>:<mean_us>`
-/

namespace Dion.Test.Benchmark

open Dion.Network.Crypto
open Dion.Network.CryptoSpec
open Dion.Test.TestHarness
open Dion.Consensus.Praos.ForgeLoop (getUnixTimeMs)

/-- Benchmark result for a single operation -/
structure BenchResult where
  name : String
  iterations : Nat
  totalMs : Float
  meanUs : Float

/-- Run an IO operation N times and time it -/
def bench (name : String) (iterations : Nat) (op : IO Unit) : IO BenchResult := do
  let start ← getUnixTimeMs
  for _ in List.range iterations do
    op
  let stop ← getUnixTimeMs
  let totalMs := (stop - start).toNat.toFloat
  let meanUs := totalMs * 1000.0 / iterations.toFloat
  return { name, iterations, totalMs, meanUs }

/-- Print a benchmark result -/
def printBenchResult (r : BenchResult) : IO Unit := do
  IO.println s!"  {r.name}: {r.iterations} ops in {r.totalMs}ms (mean: {r.meanUs}μs/op)"
  IO.println s!"bench:{r.name}:{r.iterations}:{r.totalMs}:{r.meanUs}"

/-- Run all benchmarks -/
def runBenchmarks : IO Unit := do
  IO.println "=== Benchmarks ==="
  IO.println ""
  let iterations := 1000

  -- Blake2b-256: 32-byte input
  let input32 := ByteArray.mk (Array.replicate 32 0xAB)
  let r ← bench "blake2b_32B" iterations do
    let _ ← blake2b_256 input32
  printBenchResult r

  -- Blake2b-256: 1KB input
  let input1k := ByteArray.mk (Array.replicate 1024 0xCD)
  let r ← bench "blake2b_1KB" iterations do
    let _ ← blake2b_256 input1k
  printBenchResult r

  -- Blake2b-256: 64KB input
  let input64k := ByteArray.mk (Array.replicate 65536 0xEF)
  let r ← bench "blake2b_64KB" iterations do
    let _ ← blake2b_256 input64k
  printBenchResult r

  -- SHA-512: 32-byte input
  let r ← bench "sha512_32B" iterations do
    let _ ← sha512 input32
  printBenchResult r

  -- Ed25519: keygen
  let r ← bench "ed25519_keygen" iterations do
    let _ ← ed25519_keypair
  printBenchResult r

  -- Ed25519: sign
  let (_, sk) ← ed25519_keypair
  let msg := "benchmark signing message of moderate length".toUTF8
  let r ← bench "ed25519_sign" iterations do
    let _ ← ed25519_sign sk msg
  printBenchResult r

  -- Ed25519: verify
  let (pk, sk2) ← ed25519_keypair
  let sig ← ed25519_sign sk2 msg
  let r ← bench "ed25519_verify" iterations do
    let _ ← ed25519_verify pk msg sig
  printBenchResult r

  -- VRF: proof_to_hash
  let vrfProof := hexToBytes "b6b4699f87d56126c9117a7da55bd0085246f4c56dbc95d20172612e9d38e8d7ca65e573a126ed88d4e30a46f80a666854d675cf3ba81de0de043c3774f061560f55edc256a787afe701677c0f602900"
  let r ← bench "vrf_proof_to_hash" iterations do
    let _ ← vrf_proof_to_hash_ffi vrfProof
  printBenchResult r

  -- VRF: verify
  let vrfPk := hexToBytes "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
  let vrfAlpha := ByteArray.empty
  let r ← bench "vrf_verify" iterations do
    let _ ← vrf_verify_ffi vrfPk vrfAlpha vrfProof
  printBenchResult r

  IO.println ""
  IO.println "=== Benchmarks complete ==="

end Dion.Test.Benchmark
