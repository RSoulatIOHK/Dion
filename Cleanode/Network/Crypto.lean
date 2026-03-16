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

end Cleanode.Network.Crypto
