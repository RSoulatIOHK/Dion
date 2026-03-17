import Cleanode.Network.Bech32

/-!
# Cryptographic Functions

This module provides cryptographic functions needed for Cardano,
primarily Blake2b-256 hashing for transaction IDs and block hashes.
-/

namespace Cleanode.Network.Crypto

open Cleanode.Network.Bech32

/-- Compute Blake2b-256 hash (32 bytes) -/
@[extern "cleanode_blake2b_256"]
opaque blake2b_256 (data : @& ByteArray) : IO ByteArray

/-- Compute Blake2b-224 hash (28 bytes) — used for Cardano key hashes -/
@[extern "cleanode_blake2b_224"]
opaque blake2b_224 (data : @& ByteArray) : IO ByteArray

/-- Convert ByteArray to hex string -/
def bytesToHex (bs : ByteArray) : String :=
  String.join (bs.toList.map fun b =>
    let hi := b.toNat / 16
    let lo := b.toNat % 16
    let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
    let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
    String.mk [hiChar, loChar]
  )

/-- Compute transaction ID from transaction body CBOR bytes -/
def computeTxId (txBodyBytes : ByteArray) : IO String := do
  let hash ← blake2b_256 txBodyBytes
  return bytesToHex hash

/-- Compute block hash from block header CBOR bytes -/
def computeBlockHash (headerBytes : ByteArray) : IO String := do
  let hash ← blake2b_256 headerBytes
  return bytesToHex hash

/-- Compute SHA-512 hash (64 bytes) via FFI -/
@[extern "cleanode_sha512"]
opaque sha512 (data : @& ByteArray) : IO ByteArray

/-- Verify Ed25519 signature via FFI (returns true if valid) -/
@[extern "cleanode_ed25519_verify"]
opaque ed25519_verify_ffi (publicKey : @& ByteArray) (message : @& ByteArray)
    (signature : @& ByteArray) : IO Bool

/-- Sign a message with Ed25519 via FFI -/
@[extern "cleanode_ed25519_sign"]
opaque ed25519_sign_ffi (secretKey : @& ByteArray) (message : @& ByteArray) : IO ByteArray

/-- Generate an Ed25519 key pair via FFI -/
@[extern "cleanode_ed25519_keypair"]
opaque ed25519_keypair_ffi : IO (ByteArray × ByteArray)

/-- Verify ECVRF-ED25519-SHA512-Elligator2 proof via FFI -/
@[extern "cleanode_vrf_verify"]
opaque vrf_verify_ffi (vrfVKey : @& ByteArray) (alpha : @& ByteArray)
    (proof : @& ByteArray) : IO Bool

/-- Convert VRF proof to output hash via FFI (64 bytes) -/
@[extern "cleanode_vrf_proof_to_hash"]
opaque vrf_proof_to_hash_ffi (proof : @& ByteArray) : IO ByteArray

/-- Compute SHA-256 hash (32 bytes) via FFI -/
@[extern "cleanode_sha256"]
opaque sha256 (data : @& ByteArray) : IO ByteArray

/-- Compute SHA3-256 (Keccak-256) hash (32 bytes) via FFI -/
@[extern "cleanode_sha3_256"]
opaque sha3_256 (data : @& ByteArray) : IO ByteArray

/-- Verify ECDSA secp256k1 signature (Plutus builtin #50) -/
@[extern "cleanode_secp256k1_ecdsa_verify"]
opaque secp256k1_ecdsa_verify (publicKey : @& ByteArray) (message : @& ByteArray)
    (signature : @& ByteArray) : IO Bool

/-- Verify Schnorr secp256k1 signature, BIP-340 (Plutus builtin #51) -/
@[extern "cleanode_secp256k1_schnorr_verify"]
opaque secp256k1_schnorr_verify (publicKey : @& ByteArray) (message : @& ByteArray)
    (signature : @& ByteArray) : IO Bool

-- ====================
-- = #406: Keccak-256 =
-- ====================

/-- Compute Keccak-256 hash (32 bytes) via FFI.
    CIP-101: Ethereum-compatible hash for cross-chain interop.
    Note: Keccak-256 is NOT the same as SHA3-256 (different padding).
    Used by Plutus builtin `blake2b_256` equivalent for Ethereum data. -/
@[extern "cleanode_keccak_256"]
opaque keccak_256 (data : @& ByteArray) : IO ByteArray

-- ====================
-- = #407: RIPEMD-160 =
-- ====================

/-- Compute RIPEMD-160 hash (20 bytes) via FFI.
    CIP-127: Bitcoin address compatibility.
    Bitcoin P2PKH addresses = RIPEMD-160(SHA-256(pubkey)). -/
@[extern "cleanode_ripemd_160"]
opaque ripemd_160 (data : @& ByteArray) : IO ByteArray

-- ====================
-- = #405: BLS12-381  =
-- ====================

/-- BLS12-381 G1 point (48 bytes compressed, 96 bytes uncompressed) -/
abbrev BLS12_381_G1 := ByteArray

/-- BLS12-381 G2 point (96 bytes compressed, 192 bytes uncompressed) -/
abbrev BLS12_381_G2 := ByteArray

/-- BLS12-381 Miller loop result (pairing intermediate) -/
abbrev BLS12_381_MlResult := ByteArray

-- G1 operations
@[extern "cleanode_bls12_381_g1_add"]
opaque bls12_381_g1_add (a b : @& BLS12_381_G1) : IO BLS12_381_G1

@[extern "cleanode_bls12_381_g1_neg"]
opaque bls12_381_g1_neg (a : @& BLS12_381_G1) : IO BLS12_381_G1

@[extern "cleanode_bls12_381_g1_scalar_mul"]
opaque bls12_381_g1_scalarMul (scalar : @& ByteArray) (point : @& BLS12_381_G1) : IO BLS12_381_G1

@[extern "cleanode_bls12_381_g1_equal"]
opaque bls12_381_g1_equal (a b : @& BLS12_381_G1) : IO Bool

@[extern "cleanode_bls12_381_g1_hash_to_group"]
opaque bls12_381_g1_hashToGroup (msg dst : @& ByteArray) : IO BLS12_381_G1

@[extern "cleanode_bls12_381_g1_compress"]
opaque bls12_381_g1_compress (point : @& BLS12_381_G1) : IO ByteArray

@[extern "cleanode_bls12_381_g1_uncompress"]
opaque bls12_381_g1_uncompress (compressed : @& ByteArray) : IO BLS12_381_G1

-- G2 operations
@[extern "cleanode_bls12_381_g2_add"]
opaque bls12_381_g2_add (a b : @& BLS12_381_G2) : IO BLS12_381_G2

@[extern "cleanode_bls12_381_g2_neg"]
opaque bls12_381_g2_neg (a : @& BLS12_381_G2) : IO BLS12_381_G2

@[extern "cleanode_bls12_381_g2_scalar_mul"]
opaque bls12_381_g2_scalarMul (scalar : @& ByteArray) (point : @& BLS12_381_G2) : IO BLS12_381_G2

@[extern "cleanode_bls12_381_g2_equal"]
opaque bls12_381_g2_equal (a b : @& BLS12_381_G2) : IO Bool

@[extern "cleanode_bls12_381_g2_hash_to_group"]
opaque bls12_381_g2_hashToGroup (msg dst : @& ByteArray) : IO BLS12_381_G2

@[extern "cleanode_bls12_381_g2_compress"]
opaque bls12_381_g2_compress (point : @& BLS12_381_G2) : IO ByteArray

@[extern "cleanode_bls12_381_g2_uncompress"]
opaque bls12_381_g2_uncompress (compressed : @& ByteArray) : IO BLS12_381_G2

-- Pairing operations
@[extern "cleanode_bls12_381_miller_loop"]
opaque bls12_381_millerLoop (g1 : @& BLS12_381_G1) (g2 : @& BLS12_381_G2) : IO BLS12_381_MlResult

@[extern "cleanode_bls12_381_mul_ml_result"]
opaque bls12_381_mulMlResult (a b : @& BLS12_381_MlResult) : IO BLS12_381_MlResult

@[extern "cleanode_bls12_381_final_verify"]
opaque bls12_381_finalVerify (a b : @& BLS12_381_MlResult) : IO Bool

end Cleanode.Network.Crypto
