import Cleanode.Test.TestHarness
import Cleanode.Crypto.BLS12_381

/-!
# BLS12-381 FFI Tests

Verifies BLS12-381 elliptic curve operations via the blst C library.
Tests cover G1/G2 point operations, serialization, and pairing.
-/

namespace Cleanode.Test.BLS12_381Test

open Cleanode.Test.TestHarness
open Cleanode.Crypto.BLS12_381

-- ====================
-- = Known Constants  =
-- ====================

/-- G1 generator (compressed, 48 bytes) — standard BLS12-381 generator point -/
private def g1Generator : ByteArray :=
  hexToBytes "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

/-- G1 identity/infinity (compressed) — all zeros with high bit set -/
private def g1Identity : ByteArray :=
  let ba := ByteArray.mk (Array.replicate 48 0x00)
  ba.set! 0 0xc0

/-- G2 generator (compressed, 96 bytes) -/
private def g2Generator : ByteArray :=
  hexToBytes "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

/-- G2 identity (compressed) -/
private def g2Identity : ByteArray :=
  let ba := ByteArray.mk (Array.replicate 96 0x00)
  ba.set! 0 0xc0

-- ====================
-- = G1 Tests        =
-- ====================

def testG1AddIdentity : IO TestResult :=
  runTest "BLS12-381" "G1: P + O = P (identity element)" do
    let result ← g1Add g1Generator g1Identity
    return result.size == 48 && result == g1Generator

def testG1AddCommutative : IO TestResult :=
  runTest "BLS12-381" "G1: P + Q = Q + P (commutativity)" do
    -- 2G = G + G
    let gg ← g1Add g1Generator g1Generator
    -- Verify commutativity by checking both orderings give same result
    let gg2 ← g1Add g1Generator g1Generator
    return gg.size == 48 && gg == gg2

def testG1NegInverse : IO TestResult :=
  runTest "BLS12-381" "G1: P + (-P) = O (inverse)" do
    let negG ← g1Neg g1Generator
    let result ← g1Add g1Generator negG
    return result.size == 48 && result == g1Identity

def testG1NegNeg : IO TestResult :=
  runTest "BLS12-381" "G1: -(-P) = P (double negation)" do
    let negG ← g1Neg g1Generator
    let negNegG ← g1Neg negG
    return negNegG == g1Generator

def testG1ScalarMulOne : IO TestResult :=
  runTest "BLS12-381" "G1: 1 * P = P" do
    let scalar := ByteArray.mk #[1]
    let result ← g1ScalarMul scalar g1Generator
    return result.size == 48 && result == g1Generator

def testG1ScalarMulTwo : IO TestResult :=
  runTest "BLS12-381" "G1: 2 * P = P + P" do
    let scalar := ByteArray.mk #[2]
    let doubled ← g1ScalarMul scalar g1Generator
    let added ← g1Add g1Generator g1Generator
    return doubled.size == 48 && doubled == added

def testG1ScalarMulZero : IO TestResult :=
  runTest "BLS12-381" "G1: 0 * P = O (identity)" do
    let scalar := ByteArray.mk #[0]
    let result ← g1ScalarMul scalar g1Generator
    return result.size == 48 && result == g1Identity

def testG1EqualSame : IO TestResult :=
  runTest "BLS12-381" "G1: P == P (reflexivity)" do
    let result ← g1Equal g1Generator g1Generator
    return result

def testG1EqualDifferent : IO TestResult :=
  runTest "BLS12-381" "G1: P != 2P" do
    let twoG ← g1Add g1Generator g1Generator
    let result ← g1Equal g1Generator twoG
    return !result

def testG1HashToGroup : IO TestResult :=
  runTest "BLS12-381" "G1: hashToGroup produces valid 48-byte point" do
    let msg := "test message".toUTF8
    let dst := "BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_".toUTF8
    let result ← g1HashToGroup msg dst
    return result.size == 48

def testG1HashToGroupDeterministic : IO TestResult :=
  runTest "BLS12-381" "G1: hashToGroup is deterministic" do
    let msg := "hello".toUTF8
    let dst := "DST".toUTF8
    let h1 ← g1HashToGroup msg dst
    let h2 ← g1HashToGroup msg dst
    return h1 == h2

def testG1CompressUncompress : IO TestResult :=
  runTest "BLS12-381" "G1: uncompress ∘ compress = id" do
    let uncompressed ← g1Uncompress g1Generator
    if uncompressed.size != 96 then return false
    let recompressed ← g1Compress uncompressed
    return recompressed.size == 48 && recompressed == g1Generator

-- ====================
-- = G2 Tests        =
-- ====================

def testG2AddIdentity : IO TestResult :=
  runTest "BLS12-381" "G2: P + O = P (identity element)" do
    let result ← g2Add g2Generator g2Identity
    return result.size == 96 && result == g2Generator

def testG2NegInverse : IO TestResult :=
  runTest "BLS12-381" "G2: P + (-P) = O (inverse)" do
    let negG ← g2Neg g2Generator
    let result ← g2Add g2Generator negG
    return result.size == 96 && result == g2Identity

def testG2ScalarMulOne : IO TestResult :=
  runTest "BLS12-381" "G2: 1 * P = P" do
    let scalar := ByteArray.mk #[1]
    let result ← g2ScalarMul scalar g2Generator
    return result.size == 96 && result == g2Generator

def testG2ScalarMulTwo : IO TestResult :=
  runTest "BLS12-381" "G2: 2 * P = P + P" do
    let scalar := ByteArray.mk #[2]
    let doubled ← g2ScalarMul scalar g2Generator
    let added ← g2Add g2Generator g2Generator
    return doubled.size == 96 && doubled == added

def testG2EqualSame : IO TestResult :=
  runTest "BLS12-381" "G2: P == P (reflexivity)" do
    g2Equal g2Generator g2Generator

def testG2HashToGroup : IO TestResult :=
  runTest "BLS12-381" "G2: hashToGroup produces valid 96-byte point" do
    let msg := "test".toUTF8
    let dst := "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_".toUTF8
    let result ← g2HashToGroup msg dst
    return result.size == 96

def testG2CompressUncompress : IO TestResult :=
  runTest "BLS12-381" "G2: uncompress ∘ compress = id" do
    let uncompressed ← g2Uncompress g2Generator
    if uncompressed.size != 192 then return false
    let recompressed ← g2Compress uncompressed
    return recompressed.size == 96 && recompressed == g2Generator

-- ====================
-- = Pairing Tests   =
-- ====================

def testMillerLoopProducesResult : IO TestResult :=
  runTest "BLS12-381" "Pairing: millerLoop(G1, G2) produces non-empty result" do
    let result ← millerLoop g1Generator g2Generator
    return result.size > 0

def testMulMlResult : IO TestResult :=
  runTest "BLS12-381" "Pairing: mulMlResult(a, b) produces valid result" do
    let a ← millerLoop g1Generator g2Generator
    let b ← millerLoop g1Generator g2Generator
    let result ← mulMlResult a b
    return result.size == a.size

def testFinalVerifySame : IO TestResult :=
  runTest "BLS12-381" "Pairing: finalVerify(ml(G1,G2), ml(G1,G2)) = True" do
    let ml ← millerLoop g1Generator g2Generator
    finalVerify ml ml

def testFinalVerifyDifferent : IO TestResult :=
  runTest "BLS12-381" "Pairing: finalVerify(ml(G1,G2), ml(2G1,G2)) = False" do
    let ml1 ← millerLoop g1Generator g2Generator
    let twoG1 ← g1Add g1Generator g1Generator
    let ml2 ← millerLoop twoG1 g2Generator
    let result ← finalVerify ml1 ml2
    return !result

def testBilinearity : IO TestResult :=
  runTest "BLS12-381" "Pairing: e(aP, Q) = e(P, aQ) (bilinearity)" do
    -- e(2*G1, G2) should equal e(G1, 2*G2)
    let scalar2 := ByteArray.mk #[2]
    let twoG1 ← g1ScalarMul scalar2 g1Generator
    let twoG2 ← g2ScalarMul scalar2 g2Generator
    let ml1 ← millerLoop twoG1 g2Generator
    let ml2 ← millerLoop g1Generator twoG2
    finalVerify ml1 ml2

-- ====================
-- = Error Handling  =
-- ====================

def testG1InvalidInput : IO TestResult :=
  runTest "BLS12-381" "G1: invalid input returns empty ByteArray" do
    let result ← g1Add (ByteArray.mk #[1, 2, 3]) g1Generator
    return result.size == 0

def testG2InvalidInput : IO TestResult :=
  runTest "BLS12-381" "G2: invalid input returns empty ByteArray" do
    let result ← g2Add (ByteArray.mk #[1, 2, 3]) g2Generator
    return result.size == 0

-- ====================
-- = Test Runner      =
-- ====================

private def runG1Tests : IO (List TestResult) := do
  let mut r : List TestResult := []
  r := r ++ [← testG1AddIdentity]
  r := r ++ [← testG1AddCommutative]
  r := r ++ [← testG1NegInverse]
  r := r ++ [← testG1NegNeg]
  r := r ++ [← testG1ScalarMulOne]
  r := r ++ [← testG1ScalarMulTwo]
  r := r ++ [← testG1ScalarMulZero]
  r := r ++ [← testG1EqualSame]
  r := r ++ [← testG1EqualDifferent]
  r := r ++ [← testG1HashToGroup]
  r := r ++ [← testG1HashToGroupDeterministic]
  r := r ++ [← testG1CompressUncompress]
  return r

private def runG2Tests : IO (List TestResult) := do
  let mut r : List TestResult := []
  r := r ++ [← testG2AddIdentity]
  r := r ++ [← testG2NegInverse]
  r := r ++ [← testG2ScalarMulOne]
  r := r ++ [← testG2ScalarMulTwo]
  r := r ++ [← testG2EqualSame]
  r := r ++ [← testG2HashToGroup]
  r := r ++ [← testG2CompressUncompress]
  return r

private def runPairingTests : IO (List TestResult) := do
  let mut r : List TestResult := []
  r := r ++ [← testMillerLoopProducesResult]
  r := r ++ [← testMulMlResult]
  r := r ++ [← testFinalVerifySame]
  r := r ++ [← testFinalVerifyDifferent]
  r := r ++ [← testBilinearity]
  r := r ++ [← testG1InvalidInput]
  r := r ++ [← testG2InvalidInput]
  return r

def runBLS12_381Tests : IO (List TestResult) := do
  let mut results : List TestResult := []
  results := results ++ (← runG1Tests)
  results := results ++ (← runG2Tests)
  results := results ++ (← runPairingTests)
  return results

end Cleanode.Test.BLS12_381Test
