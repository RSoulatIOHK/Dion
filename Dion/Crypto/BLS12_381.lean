/-!
# BLS12-381 FFI Bindings

Lean4 FFI declarations for BLS12-381 elliptic curve operations
via the `blst` C library. Used by Plutus UPLC builtins.

## Serialization Formats
- G1 points: 48 bytes (compressed)
- G2 points: 96 bytes (compressed)
- MlResult (GT/Fp12): 576 bytes

## References
- CIP-0381: Plutus support for BLS12-381 primitives
- blst: https://github.com/supranational/blst
-/

namespace Dion.Crypto.BLS12_381

-- G1 operations (48-byte compressed points)
@[extern "dion_bls12_381_g1_add"]
opaque g1Add (a : @& ByteArray) (b : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g1_neg"]
opaque g1Neg (a : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g1_scalar_mul"]
opaque g1ScalarMul (scalar : @& ByteArray) (point : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g1_equal"]
opaque g1Equal (a : @& ByteArray) (b : @& ByteArray) : IO Bool

@[extern "dion_bls12_381_g1_hash_to_group"]
opaque g1HashToGroup (msg : @& ByteArray) (dst : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g1_compress"]
opaque g1Compress (a : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g1_uncompress"]
opaque g1Uncompress (a : @& ByteArray) : IO ByteArray

-- G2 operations (96-byte compressed points)
@[extern "dion_bls12_381_g2_add"]
opaque g2Add (a : @& ByteArray) (b : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g2_neg"]
opaque g2Neg (a : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g2_scalar_mul"]
opaque g2ScalarMul (scalar : @& ByteArray) (point : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g2_equal"]
opaque g2Equal (a : @& ByteArray) (b : @& ByteArray) : IO Bool

@[extern "dion_bls12_381_g2_hash_to_group"]
opaque g2HashToGroup (msg : @& ByteArray) (dst : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g2_compress"]
opaque g2Compress (a : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_g2_uncompress"]
opaque g2Uncompress (a : @& ByteArray) : IO ByteArray

-- Pairing operations
@[extern "dion_bls12_381_miller_loop"]
opaque millerLoop (g1 : @& ByteArray) (g2 : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_mul_ml_result"]
opaque mulMlResult (a : @& ByteArray) (b : @& ByteArray) : IO ByteArray

@[extern "dion_bls12_381_final_verify"]
opaque finalVerify (a : @& ByteArray) (b : @& ByteArray) : IO Bool

end Dion.Crypto.BLS12_381
