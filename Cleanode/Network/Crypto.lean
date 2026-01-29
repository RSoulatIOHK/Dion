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

end Cleanode.Network.Crypto
