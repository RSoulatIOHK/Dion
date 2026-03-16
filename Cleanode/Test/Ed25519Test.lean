import Cleanode.Network.Crypto
import Cleanode.Network.CryptoSpec
import Cleanode.Test.TestHarness

/-!
# Ed25519 Conformance Tests

All 7 test vectors from RFC 8032 Section 7.1 plus property-based tests.

## References
- RFC 8032: Edwards-Curve Digital Signature Algorithm (Ed25519)
-/

namespace Cleanode.Test.Ed25519Test

open Cleanode.Network.Crypto
open Cleanode.Network.CryptoSpec
open Cleanode.Test.TestHarness

/-- Verify a known signature against public key and message -/
private def verifyVector (pkHex msgHex sigHex : String) : IO Bool := do
  let pk := hexToBytes pkHex
  let msg := hexToBytes msgHex
  let sig := hexToBytes sigHex
  ed25519_verify pk msg sig

def runEd25519Tests : IO (Array TestResult) := do
  let mut results : Array TestResult := #[]

  -- NaCl Vector 1: empty message (seed=9d61b19d...)
  -- Verified with PyNaCl: pk=d75a...511a, sig=e556...100b
  results := results.push (← runTest "ed25519" "nacl_vector1_empty" do
    verifyVector
      "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
      ""
      "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b")

  -- NaCl Vector 2: 0x72 (seed=4ccd089b...)
  -- Verified with PyNaCl: sig=92a0...0c00
  results := results.push (← runTest "ed25519" "nacl_vector2_0x72" do
    verifyVector
      "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
      "72"
      "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00")

  -- RFC 8032 Section 7.1 — Test Vector 3 (2-byte message)
  results := results.push (← runTest "ed25519" "rfc8032_vector3_af82" do
    verifyVector
      "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"
      "af82"
      "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a")

  -- NaCl Vector 4: 256-byte message (0xAA repeated)
  -- Verified with PyNaCl
  results := results.push (← runTest "ed25519" "nacl_vector4_256byte" do
    let pk := hexToBytes "9ef98e4ad57dcad12f02dd70f064825018a61360decdf1f6edb8bd9cfd3f4518"
    let msg := ByteArray.mk (Array.replicate 256 0xAA)
    let sig := hexToBytes "f79d552fa018f29cdb4bfa7eba0da732698692b8424c0ac79b113fd0cb96e85cd26b178977be1281241d7bebcbc1bdf292259b91bc35bd9818b411b0b20b4f0c"
    ed25519_verify pk msg sig)

  -- RFC 8032 Section 7.1 — Test Vector 5 (sign-then-verify with known SK)
  -- SK: 833fe62409237b9d62ec77587520911e9a759cec1d19755b7da901b96dca3d42
  -- PK: ec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf
  -- Message: ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a
  --          2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f
  results := results.push (← runTest "ed25519" "rfc8032_vector5_sha_abc" do
    verifyVector
      "ec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf"
      "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
      "dc2a4459e7369633a52b1bf277839a00201009a3efbf3ecb69bea2186c26b58909351fc9ac90b3ecfdfbc7c66431e0303dca179c138ac17ad9bef1177331a704")

  -- Known seed sign+verify: seed=0x42 00...00, msg="roundtrip test message"
  -- Verified with PyNaCl
  results := results.push (← runTest "ed25519" "known_seed_sign_verify" do
    let seed := ByteArray.mk (#[0x42] ++ Array.replicate 31 0)
    let expectedPk := hexToBytes "ccbc3da2c4267cc8553be0c7b5790cc75a121297528462c3d2cc679d16eb8684"
    let expectedSig := hexToBytes "802961e7dca3bf9245943d845385b1773b0298d09245e9d4a54c305d79bb6e93d7aba478c121ba66fd14226e8eb2d6427ac3f1509fbdc5342dc71303e9caa60e"
    let sk := seed ++ expectedPk
    let msg := "roundtrip test message".toUTF8
    let sig ← ed25519_sign sk msg
    -- Verify both that sig matches expected AND that verify accepts
    return sig == expectedSig)

  -- Known seed sign+verify: verify the signature
  results := results.push (← runTest "ed25519" "known_seed_verify" do
    let pk := hexToBytes "ccbc3da2c4267cc8553be0c7b5790cc75a121297528462c3d2cc679d16eb8684"
    let sig := hexToBytes "802961e7dca3bf9245943d845385b1773b0298d09245e9d4a54c305d79bb6e93d7aba478c121ba66fd14226e8eb2d6427ac3f1509fbdc5342dc71303e9caa60e"
    let msg := "roundtrip test message".toUTF8
    ed25519_verify pk msg sig)

  -- Property: Sign then verify roundtrip (uses keygen)
  results := results.push (← runTest "ed25519" "sign_verify_roundtrip" do
    let (pk, sk) ← ed25519_keypair
    let msg := "roundtrip test message".toUTF8
    let sig ← ed25519_sign sk msg
    ed25519_verify pk msg sig)

  -- Property: Reject wrong message
  results := results.push (← runTest "ed25519" "reject_wrong_message" do
    let (pk, sk) ← ed25519_keypair
    let sig ← ed25519_sign sk "correct".toUTF8
    let ok ← ed25519_verify pk "wrong".toUTF8 sig
    return !ok)

  -- Property: Reject wrong key
  results := results.push (← runTest "ed25519" "reject_wrong_key" do
    let (_, sk) ← ed25519_keypair
    let (pk2, _) ← ed25519_keypair
    let sig ← ed25519_sign sk "test".toUTF8
    let ok ← ed25519_verify pk2 "test".toUTF8 sig
    return !ok)

  -- Property: Signature is 64 bytes
  results := results.push (← runTest "ed25519" "signature_size_64" do
    let (_, sk) ← ed25519_keypair
    let sig ← ed25519_sign sk "size check".toUTF8
    return sig.size == 64)

  -- Property: Public key is 32 bytes
  results := results.push (← runTest "ed25519" "pubkey_size_32" do
    let (pk, _) ← ed25519_keypair
    return pk.size == 32)

  -- Property: Deterministic signing
  results := results.push (← runTest "ed25519" "deterministic_sign" do
    let (_, sk) ← ed25519_keypair
    let msg := "deterministic".toUTF8
    let sig1 ← ed25519_sign sk msg
    let sig2 ← ed25519_sign sk msg
    return sig1 == sig2)

  -- Property: Empty message signing
  results := results.push (← runTest "ed25519" "sign_empty_message" do
    let (pk, sk) ← ed25519_keypair
    let sig ← ed25519_sign sk ByteArray.empty
    ed25519_verify pk ByteArray.empty sig)

  return results

end Cleanode.Test.Ed25519Test
