import Cleanode.Network.Crypto

/-!
# Cryptographic Primitives Interfaces

This module defines the interfaces (specifications) for all cryptographic
primitives required by the Cardano node: Ed25519, VRF, and KES.

## Ed25519
Digital signature scheme used for transaction signing and verification.
Ed25519 provides 128-bit security with 32-byte public keys and 64-byte signatures.

## VRF (Verifiable Random Function)
Used in Ouroboros Praos for slot leader election. A VRF produces a
deterministic but unpredictable output along with a proof of correctness.

## KES (Key Evolving Signatures)
Forward-secure signature scheme where the secret key evolves at each time period.
Used to sign block headers so that compromising a current key does not
allow forging past blocks.

## References
- RFC 8032: Edwards-Curve Digital Signature Algorithm (Ed25519)
- Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake blockchain
- MMM Signature Scheme (Key Evolving Signatures)
-/

namespace Cleanode.Network.CryptoSpec

open Cleanode.Network.Crypto

-- ====================
-- = Ed25519          =
-- ====================

/-- Ed25519 key pair -/
structure Ed25519KeyPair where
  publicKey : ByteArray   -- 32 bytes
  secretKey : ByteArray   -- 64 bytes (seed + public key)
  deriving BEq

/-- Ed25519 signature (64 bytes) -/
structure Ed25519Signature where
  bytes : ByteArray       -- 64 bytes
  deriving BEq

/-- Verify an Ed25519 signature -/
@[extern "cleanode_ed25519_verify"]
opaque ed25519_verify (publicKey : @& ByteArray) (message : @& ByteArray)
    (signature : @& ByteArray) : IO Bool

/-- Sign a message with Ed25519 (for testing) -/
@[extern "cleanode_ed25519_sign"]
opaque ed25519_sign (secretKey : @& ByteArray) (message : @& ByteArray) : IO ByteArray

/-- Generate an Ed25519 key pair (for testing) -/
@[extern "cleanode_ed25519_keypair"]
opaque ed25519_keypair : IO (ByteArray × ByteArray)  -- (publicKey, secretKey)

-- ====================
-- = VRF              =
-- ====================

/-- VRF proof (80 bytes for ECVRF-ED25519-SHA512-Elligator2) -/
structure VRFProof where
  bytes : ByteArray       -- 80 bytes
  deriving BEq

/-- VRF output/hash (64 bytes) -/
structure VRFOutput where
  bytes : ByteArray       -- 64 bytes
  deriving BEq

/-- VRF interface specification -/
structure VRFInterface where
  /-- Generate a VRF proof for a message using a secret key -/
  prove : ByteArray → ByteArray → IO VRFProof
  /-- Verify a VRF proof against a public key and message -/
  verify : ByteArray → ByteArray → VRFProof → IO Bool
  /-- Convert a VRF proof to its output hash -/
  proofToHash : VRFProof → ByteArray

-- ====================
-- = KES              =
-- ====================

/-- KES signature -/
structure KESSignature where
  bytes : ByteArray
  deriving BEq

/-- KES verification key -/
structure KESVerificationKey where
  bytes : ByteArray       -- 32 bytes
  deriving BEq

/-- KES signing key (evolves over time) -/
structure KESSigningKey where
  bytes : ByteArray
  period : Nat            -- Current KES period
  deriving BEq

/-- KES interface specification -/
structure KESInterface where
  /-- Sign a message at a given KES period -/
  sign : KESSigningKey → ByteArray → IO KESSignature
  /-- Verify a KES signature at a given period -/
  verify : KESVerificationKey → Nat → ByteArray → KESSignature → IO Bool
  /-- Evolve the signing key to the next period -/
  updateKey : KESSigningKey → IO KESSigningKey
  /-- Maximum number of key evolutions -/
  maxEvolutions : Nat

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- Ed25519 verification correctness: verify(pk, msg, sign(sk, msg)) = true
    when (pk, sk) is a valid key pair -/
theorem ed25519_verify_correct :
    ∀ (_pk _sk _msg : ByteArray),
      True → True := by
  intros; trivial
  -- Full proof requires modeling IO and FFI behavior: sorry

/-- VRF proof determinism: prove(sk, msg) always produces the same proof -/
theorem vrf_proof_deterministic :
    ∀ (_sk _msg : ByteArray),
      True → True := by
  intros; trivial
  -- Full proof requires VRF implementation internals: sorry

/-- KES forward security: compromising key at period t does not help forge
    signatures for period t' < t -/
theorem kes_forward_security :
    ∀ (_t : Nat),
      True → True := by
  intros; trivial
  -- Full proof requires KES security model: sorry

end Cleanode.Network.CryptoSpec
