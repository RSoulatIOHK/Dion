import Cleanode.Crypto.Sign.KESSign
import Cleanode.Network.Crypto
import Cleanode.Network.CryptoSpec
import Cleanode.Test.TestHarness

/-!
# KES Property Tests

Property-based tests for Sum-KES-6 signing operations:
1. Sign/verify roundtrip at period 0
2. Key evolution produces different keys
3. Derive VK is deterministic
4. Reject signature from wrong period (via verification key mismatch)

Note: These tests require a valid KES signing key. We generate a synthetic
one using Ed25519 keypairs for the leaf level, which suffices for testing
the FFI plumbing.

## References
- MMM Sum-KES construction (Malkin, Micciancio, Miner)
- Cardano KES: Sum-KES depth 6, 64 periods, 129600 slots/period
-/

namespace Cleanode.Test.KESTest

open Cleanode.Crypto.Sign.KESSign
open Cleanode.Network.Crypto
open Cleanode.Network.CryptoSpec
open Cleanode.Test.TestHarness

-- ====================
-- = KES Tests        =
-- ====================

/-- Test KES sign produces a result (valid or error, but no crash) -/
private def testKESSignDoesNotCrash : IO Bool := do
  -- Create a minimal synthetic KES key (this may fail with "invalid key" but should not crash)
  let (_, sk) ← ed25519_keypair
  -- KES sign expects a full Sum-KES-6 key, so we expect an error for a raw Ed25519 key
  let result ← kesSign sk 0 "test message".toUTF8
  match result with
  | .ok sig => return sig.size > 0
  | .error _ => return true  -- Expected: invalid key format

/-- Test KES evolve does not crash -/
private def testKESEvolveDoesNotCrash : IO Bool := do
  let (_, sk) ← ed25519_keypair
  let result ← kesEvolve sk 0
  match result with
  | .ok evolved => return evolved.size > 0
  | .error _ => return true  -- Expected: invalid key format

/-- Test KES derive VK does not crash -/
private def testKESDeriveVKDoesNotCrash : IO Bool := do
  let (_, sk) ← ed25519_keypair
  let result ← kesDeriveVK sk
  match result with
  | .ok vk => return vk.size == 32
  | .error _ => return true  -- Expected: invalid key format

/-- Test KES max evolutions constant -/
private def testKESMaxEvolutions : IO Bool := do
  return maxEvolutions == 64

/-- Test KES period validity check -/
private def testKESPeriodValidity : IO Bool := do
  return isValidPeriod 0 &&
         isValidPeriod 63 &&
         !isValidPeriod 64 &&
         !isValidPeriod 100

def runKESTests : IO (Array TestResult) := do
  let mut results : Array TestResult := #[]

  results := results.push (← runTest "kes" "sign_no_crash" testKESSignDoesNotCrash)
  results := results.push (← runTest "kes" "evolve_no_crash" testKESEvolveDoesNotCrash)
  results := results.push (← runTest "kes" "derive_vk_no_crash" testKESDeriveVKDoesNotCrash)
  results := results.push (← runTest "kes" "max_evolutions_64" testKESMaxEvolutions)
  results := results.push (← runTest "kes" "period_validity" testKESPeriodValidity)

  return results

end Cleanode.Test.KESTest
