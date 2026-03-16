import Cleanode.Network.Crypto
import Cleanode.Test.TestHarness

/-!
# Blake2b-256 Conformance Tests

Test vectors from:
- RFC 7693 Appendix A (adapted to 256-bit output)
- Reference implementation known-answer tests
- Empty, short, and long inputs
-/

namespace Cleanode.Test.Blake2bTest

open Cleanode.Network.Crypto
open Cleanode.Test.TestHarness

/-- Known-answer test: verify hash matches expected hex -/
private def katTest (inputHex : String) (expectedHex : String) : IO Bool := do
  let input := hexToBytes inputHex
  let hash ← blake2b_256 input
  let expected := hexToBytes expectedHex
  return hash == expected

/-- Known-answer test with UTF-8 string input -/
private def katTestStr (input : String) (expectedHex : String) : IO Bool := do
  let hash ← blake2b_256 input.toUTF8
  let expected := hexToBytes expectedHex
  return hash == expected

def runBlake2b256Tests : IO (Array TestResult) := do
  let mut results : Array TestResult := #[]

  -- Vector 1: Empty input (verified with Python hashlib.blake2b(b'', digest_size=32))
  results := results.push (← runTest "blake2b" "empty_input" do
    katTestStr "" "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8")

  -- Vector 2: "abc"
  results := results.push (← runTest "blake2b" "abc" do
    katTestStr "abc" "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319")

  -- Vector 3: Single null byte
  results := results.push (← runTest "blake2b" "single_zero_byte" do
    let hash ← blake2b_256 (ByteArray.mk #[0])
    let expected := hexToBytes "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"
    return hash == expected)

  -- Vector 4: "The quick brown fox jumps over the lazy dog"
  results := results.push (← runTest "blake2b" "quick_brown_fox" do
    katTestStr "The quick brown fox jumps over the lazy dog"
      "01718cec35cd3d796dd00020e0bfecb473ad23457d063b75eff29c0ffa2e58a9")

  -- Vector 5: 64 bytes of 0x00
  results := results.push (← runTest "blake2b" "64_zero_bytes" do
    let input := ByteArray.mk (Array.replicate 64 0)
    let hash ← blake2b_256 input
    return hash.size == 32)

  -- Vector 6: 128 bytes of 0xFF
  results := results.push (← runTest "blake2b" "128_ff_bytes" do
    let input := ByteArray.mk (Array.replicate 128 0xFF)
    let hash ← blake2b_256 input
    return hash.size == 32)

  -- Property: Output is always 32 bytes for various lengths
  results := results.push (← runTest "blake2b" "output_always_32_bytes" do
    let lengths := [0, 1, 31, 32, 33, 64, 127, 128, 255, 256, 1000]
    let mut ok := true
    for len in lengths do
      let input := ByteArray.mk (Array.replicate len 0xAB)
      let hash ← blake2b_256 input
      if hash.size != 32 then ok := false
    return ok)

  -- Property: Deterministic
  results := results.push (← runTest "blake2b" "deterministic" do
    let input := "determinism check".toUTF8
    let h1 ← blake2b_256 input
    let h2 ← blake2b_256 input
    return h1 == h2)

  -- Property: Distinct inputs give distinct outputs
  results := results.push (← runTest "blake2b" "distinct_inputs" do
    let h1 ← blake2b_256 "hello".toUTF8
    let h2 ← blake2b_256 "world".toUTF8
    let h3 ← blake2b_256 "hello ".toUTF8
    return h1 != h2 && h1 != h3 && h2 != h3)

  -- Property: Sensitive to single-bit changes
  results := results.push (← runTest "blake2b" "avalanche_single_bit" do
    let input1 := ByteArray.mk #[0x00]
    let input2 := ByteArray.mk #[0x01]
    let h1 ← blake2b_256 input1
    let h2 ← blake2b_256 input2
    -- At least 8 bytes should differ (avalanche effect)
    let mut diffCount : Nat := 0
    for i in List.range 32 do
      if h1.get! i != h2.get! i then diffCount := diffCount + 1
    return diffCount >= 8)

  return results

end Cleanode.Test.Blake2bTest
