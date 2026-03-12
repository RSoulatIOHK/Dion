import Cleanode.Network.Crypto
import Cleanode.Network.CryptoSpec
import Cleanode.Network.ChainSync

/-!
# Cryptographic Primitives Test Vectors

Property-based tests and known-answer tests for cryptographic functions.

## Test Vectors
- Blake2b-256: RFC 7693 test vectors
- Ed25519: RFC 8032 test vectors

## References
- RFC 7693: The BLAKE2 Cryptographic Hash and Message Authentication Code
- RFC 8032: Edwards-Curve Digital Signature Algorithm (Ed25519 and Ed448)
-/

namespace Cleanode.Test.CryptoTest

open Cleanode.Network.Crypto
open Cleanode.Network.CryptoSpec
open Cleanode.Network.ChainSync (hexToBytes)

-- ====================
-- = Blake2b Tests    =
-- ====================

/-- Blake2b-256 test: empty input
    Expected: 0e5751c9c6940e9cd4e40be1e14942fbc840b5fb1f2c5fd4a8b3a0b3c0aa5642 -/
def testBlake2bEmpty : IO Bool := do
  let hash ← blake2b_256 ByteArray.empty
  let expected := hexToBytes "0e5751c9c6940e9cd4e40be1e14942fbc840b5fb1f2c5fd4a8b3a0b3c0aa5642"
  return hash == expected

/-- Blake2b-256 test: "abc"
    Expected: bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319 -/
def testBlake2bAbc : IO Bool := do
  let input := "abc".toUTF8
  let hash ← blake2b_256 input
  let expected := hexToBytes "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
  return hash == expected

/-- Run all Blake2b tests -/
def runBlake2bTests : IO (List (String × Bool)) := do
  let r1 ← testBlake2bEmpty
  let r2 ← testBlake2bAbc
  return [
    ("blake2b_empty", r1),
    ("blake2b_abc", r2)
  ]

-- ====================
-- = Ed25519 Tests    =
-- ====================

/-- Ed25519 test vector 1 from RFC 8032 Section 7.1
    Secret key (seed): 9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60
    Public key: d75a980182b10ab7d54bfed3c964073a0ee172f3daa3f4a18446b0b8d183f8e3
    Message: "" (empty)
    Signature: e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b -/
def testEd25519Vector1 : IO Bool := do
  let pk := hexToBytes "d75a980182b10ab7d54bfed3c964073a0ee172f3daa3f4a18446b0b8d183f8e3"
  let msg := ByteArray.empty
  let sig := hexToBytes "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
  ed25519_verify pk msg sig

/-- Ed25519 test vector 2 from RFC 8032 Section 7.1
    Message: 72 (single byte 0x72)
    Public key: 3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c
    Signature: 92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e159c7e94b6b3ebb4e6da81a2837e3827d16882c7d97568e5e04ab5 -/
def testEd25519Vector2 : IO Bool := do
  let pk := hexToBytes "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
  let msg := ByteArray.mk #[0x72]
  let sig := hexToBytes "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e159c7e94b6b3ebb4e6da81a2837e3827d16882c7d97568e5e04ab5"
  ed25519_verify pk msg sig

/-- Run all Ed25519 tests -/
def runEd25519Tests : IO (List (String × Bool)) := do
  let r1 ← testEd25519Vector1
  let r2 ← testEd25519Vector2
  return [
    ("ed25519_vector1_empty", r1),
    ("ed25519_vector2_0x72", r2)
  ]

-- ====================
-- = Property Tests   =
-- ====================

/-- Property: Blake2b-256 is deterministic (same input -> same output) -/
def testBlake2bDeterministic : IO Bool := do
  let input := "determinism test".toUTF8
  let h1 ← blake2b_256 input
  let h2 ← blake2b_256 input
  return h1 == h2

/-- Property: Blake2b-256 always produces 32 bytes -/
def testBlake2bOutputSize : IO Bool := do
  let inputs : List ByteArray := [
    ByteArray.empty,
    "a".toUTF8,
    "abc".toUTF8,
    ("x" ++ String.mk (List.replicate 1000 'x')).toUTF8
  ]
  let mut allOk := true
  for input in inputs do
    let hash ← blake2b_256 input
    if hash.size != 32 then
      allOk := false
  return allOk

/-- Property: Blake2b-256 produces different hashes for different inputs -/
def testBlake2bDistinct : IO Bool := do
  let h1 ← blake2b_256 "hello".toUTF8
  let h2 ← blake2b_256 "world".toUTF8
  return h1 != h2

/-- Property: Ed25519 sign then verify roundtrip -/
def testEd25519SignVerifyRoundtrip : IO Bool := do
  let (pk, sk) ← ed25519_keypair
  let msg := "test message for signing".toUTF8
  let sig ← ed25519_sign sk msg
  ed25519_verify pk msg sig

/-- Property: Ed25519 rejects wrong message -/
def testEd25519RejectWrongMessage : IO Bool := do
  let (pk, sk) ← ed25519_keypair
  let msg := "correct message".toUTF8
  let sig ← ed25519_sign sk msg
  let wrongMsg := "wrong message".toUTF8
  let result ← ed25519_verify pk wrongMsg sig
  return !result  -- Should reject

/-- Property: Ed25519 rejects wrong key -/
def testEd25519RejectWrongKey : IO Bool := do
  let (_, sk) ← ed25519_keypair
  let (pk2, _) ← ed25519_keypair
  let msg := "test message".toUTF8
  let sig ← ed25519_sign sk msg
  let result ← ed25519_verify pk2 msg sig
  return !result  -- Should reject

/-- Run all property-based tests -/
def runPropertyTests : IO (List (String × Bool)) := do
  let r1 ← testBlake2bDeterministic
  let r2 ← testBlake2bOutputSize
  let r3 ← testBlake2bDistinct
  let r4 ← testEd25519SignVerifyRoundtrip
  let r5 ← testEd25519RejectWrongMessage
  let r6 ← testEd25519RejectWrongKey
  return [
    ("blake2b_deterministic", r1),
    ("blake2b_output_32_bytes", r2),
    ("blake2b_distinct_inputs", r3),
    ("ed25519_sign_verify_roundtrip", r4),
    ("ed25519_reject_wrong_message", r5),
    ("ed25519_reject_wrong_key", r6)
  ]

-- ====================
-- = Test Runner      =
-- ====================

/-- Run all crypto tests and print results -/
def runAllCryptoTests : IO UInt32 := do
  IO.println "=== Cryptographic Primitives Tests ==="
  IO.println ""

  IO.println "--- Blake2b-256 ---"
  let blake2bResults ← runBlake2bTests
  let mut failures : Nat := 0
  for (name, passed) in blake2bResults do
    if passed then
      IO.println s!"  PASS: {name}"
    else
      IO.println s!"  FAIL: {name}"
      failures := failures + 1

  IO.println ""
  IO.println "--- Ed25519 ---"
  let ed25519Results ← runEd25519Tests
  for (name, passed) in ed25519Results do
    if passed then
      IO.println s!"  PASS: {name}"
    else
      IO.println s!"  FAIL: {name}"
      failures := failures + 1

  IO.println ""
  IO.println "--- Property-Based Tests ---"
  let propResults ← runPropertyTests
  for (name, passed) in propResults do
    if passed then
      IO.println s!"  PASS: {name}"
    else
      IO.println s!"  FAIL: {name}"
      failures := failures + 1

  IO.println ""
  let total := blake2bResults.length + ed25519Results.length + propResults.length
  let passed := total - failures
  IO.println s!"Results: {passed}/{total} passed"

  return if failures == 0 then 0 else 1

end Cleanode.Test.CryptoTest
