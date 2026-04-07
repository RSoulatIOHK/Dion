import Dion.Crypto.Sign.Ed25519.Point
import Dion.Crypto.Sign.Ed25519.Field
import Dion.Crypto.Hash.Sha512

/-!
# ECVRF-ED25519-SHA512-Elligator2

Verifiable Random Function implementation per draft-irtf-cfrg-vrf-03,
the variant used by Cardano's Ouroboros Praos consensus.

A VRF takes a secret key and a message, and produces:
- A VRF output (deterministic pseudorandom hash)
- A proof that the output was correctly computed

Anyone with the public key can verify the proof without learning the secret key.

## References
- draft-irtf-cfrg-vrf-03 (ECVRF-ED25519-SHA512-Elligator2)
- Ouroboros Praos specification
-/

namespace Dion.Crypto.VRF.ECVRF

open Dion.Crypto.Sign.Ed25519.Point
open Dion.Crypto.Sign.Ed25519.Field
open Dion.Crypto.Hash.Sha512

-- ====================
-- = VRF Types        =
-- ====================

/-- VRF proof: (Gamma point, challenge c, response s) -/
structure VRFProof where
  gamma : EdPoint         -- Gamma = sk * H(pk, message)
  challenge : Nat         -- c: 16-byte challenge scalar
  response : Nat          -- s: 32-byte response scalar

/-- VRF output: 64 bytes of pseudorandom output -/
structure VRFOutput where
  bytes : List UInt8      -- 64 bytes
  deriving BEq

-- ====================
-- = Hash to Curve    =
-- ====================

/-- Suite byte for ECVRF-ED25519-SHA512-Elligator2 -/
def suiteByte : UInt8 := 0x04

/-- Elligator2 map: deterministically map a field element to a curve point.
    This is a simplified version of the Elligator2 map for Ed25519. -/
private def elligator2 (r : Fp) : EdPoint :=
  -- Montgomery curve parameter A = 486662
  let _A := Fp.ofNat 486662
  -- u = -A / (1 + 2 * r^2)
  let r2 := Fp.square r
  let denom := Fp.add Fp.one (Fp.mul (Fp.ofNat 2) r2)
  let u := Fp.neg (Fp.div _A denom)
  -- v = u * (u^2 + A*u + 1)
  let u2 := Fp.square u
  let v := Fp.mul u (Fp.add (Fp.add u2 (Fp.mul _A u)) Fp.one)
  -- Check if v is a quadratic residue
  let e := Fp.pow v ((p - 1) / 2)
  let x := if e == Fp.one then u else Fp.sub (Fp.neg u) _A
  -- Convert from Montgomery to Edwards: map (u, v) -> (x, y)
  -- Edwards x = sqrt(-486664) * u / v
  -- Edwards y = (u - 1) / (u + 1)
  let ey := Fp.div (Fp.sub x Fp.one) (Fp.add x Fp.one)
  -- For the x coordinate, use the curve equation to recover
  let ey2 := Fp.square ey
  let d := Fp.div (Fp.ofInt (-121665)) (Fp.ofNat 121666)
  let num := Fp.sub ey2 Fp.one
  let den := Fp.add (Fp.mul d ey2) Fp.one
  let ex2 := Fp.div num den
  let ex := Fp.pow ex2 ((p + 3) / 8)
  let check := Fp.square ex
  let ex := if check != ex2 then
    let sqrt_m1 := Fp.pow (Fp.ofInt (-1)) ((p - 1) / 4)
    Fp.mul ex sqrt_m1
  else ex
  EdPoint.fromAffine ex ey

/-- Hash to curve: ECVRF_hash_to_curve using Elligator2.
    Hashes the public key and message to produce a curve point. -/
def hashToCurve (publicKey : List UInt8) (message : List UInt8) : EdPoint :=
  -- hash_string = SHA-512(suite_byte || 0x01 || pk || message)
  let hashInput := [suiteByte, 0x01] ++ publicKey ++ message
  let hashOutput := Internal.hashMessage hashInput
  let hashBytes := hashOutput.flatMap Dion.Crypto.Integer.UInt64.toUInt8BE
  -- Take first 32 bytes, clear high bit, interpret as field element
  let truncated := (hashBytes.take 32).set 31
    ((hashBytes.take 32)[31]! &&& 0x7F)
  let r := Fp.fromBytesLE truncated
  -- Map to curve using Elligator2
  let point := elligator2 r
  -- Multiply by cofactor (8 for Ed25519) to ensure point is in prime-order subgroup
  EdPoint.scalarMul 8 point

-- ====================
-- = VRF Operations   =
-- ====================

/-- Hash points: compute challenge c from a list of points.
    ECVRF_hash_points per the spec. -/
def hashPoints (points : List EdPoint) : Nat :=
  let pointBytes := points.flatMap EdPoint.compress
  let hashInput := [suiteByte, 0x02] ++ pointBytes
  let hashOutput := Internal.hashMessage hashInput
  let hashBytes := hashOutput.flatMap Dion.Crypto.Integer.UInt64.toUInt8BE
  -- Take first 16 bytes as challenge (little-endian)
  let cBytes := hashBytes.take 16
  cBytes.foldr (fun b acc => b.toNat + acc * 256) 0

/-- Decode a secret key to a scalar (using SHA-512 of the seed, clamped) -/
private def decodeScalar (seed : List UInt8) : Nat :=
  let hashOutput := Internal.hashMessage seed
  let hashBytes := hashOutput.flatMap Dion.Crypto.Integer.UInt64.toUInt8BE
  let scalar := hashBytes.take 32
  -- Clamp: clear bottom 3 bits and top bit, set second-to-top bit
  let scalar := scalar.set 0 (scalar[0]! &&& 0xF8)
  let scalar := scalar.set 31 ((scalar[31]! &&& 0x7F) ||| 0x40)
  scalar.foldr (fun b acc => b.toNat + acc * 256) 0

/-- Curve order L = 2^252 + 27742317777372353535851937790883648493 -/
def curveOrder : Nat := 2^252 + 27742317777372353535851937790883648493

/-- VRF prove: generate a VRF proof for a message.
    secretKey: 32-byte Ed25519 seed
    message: arbitrary bytes -/
def prove (secretKey : List UInt8) (message : List UInt8) : VRFProof :=
  -- Decode secret key to scalar x
  let x := decodeScalar secretKey
  -- Compute public key Y = x * B
  let pubKey := EdPoint.scalarMul x EdPoint.basePoint
  let pkBytes := EdPoint.compress pubKey
  -- Hash to curve: H = ECVRF_hash_to_curve(Y, message)
  let h := hashToCurve pkBytes message
  -- Gamma = x * H
  let gamma := EdPoint.scalarMul x h
  -- Generate nonce k (deterministic from secret key and H)
  let hBytes := EdPoint.compress h
  let hashOutput := Internal.hashMessage secretKey
  let truncHash := (hashOutput.flatMap Dion.Crypto.Integer.UInt64.toUInt8BE).drop 32
  let nonceInput := truncHash ++ hBytes
  let nonceHash := Internal.hashMessage nonceInput
  let nonceBytes := nonceHash.flatMap Dion.Crypto.Integer.UInt64.toUInt8BE
  let k := (nonceBytes.take 64).foldl (fun acc b => acc * 256 + b.toNat) 0 % curveOrder
  -- U = k * B, V = k * H
  let u := EdPoint.scalarMul k EdPoint.basePoint
  let v := EdPoint.scalarMul k h
  -- Challenge c = ECVRF_hash_points(H, Gamma, U, V)
  let c := hashPoints [h, gamma, u, v]
  -- Response s = (k - c * x) mod L
  let cx := (c * x) % curveOrder
  let s := if k >= cx then (k - cx) % curveOrder
           else (curveOrder - (cx - k) % curveOrder) % curveOrder
  { gamma := gamma, challenge := c, response := s }

/-- VRF verify: verify a VRF proof against a public key and message.
    Returns true if the proof is valid. -/
def verify (publicKey : List UInt8) (message : List UInt8) (proof : VRFProof) : Bool :=
  match EdPoint.decompress publicKey with
  | none => false
  | some pubPoint =>
    -- Hash to curve: H = ECVRF_hash_to_curve(Y, message)
    let h := hashToCurve publicKey message
    -- U = s * B + c * Y
    let sB := EdPoint.scalarMul proof.response EdPoint.basePoint
    let cY := EdPoint.scalarMul proof.challenge pubPoint
    let u := EdPoint.add sB cY
    -- V = s * H + c * Gamma
    let sH := EdPoint.scalarMul proof.response h
    let cGamma := EdPoint.scalarMul proof.challenge proof.gamma
    let v := EdPoint.add sH cGamma
    -- Recompute challenge c' = ECVRF_hash_points(H, Gamma, U, V)
    let c' := hashPoints [h, proof.gamma, u, v]
    -- Verify c == c'
    proof.challenge == c'

/-- VRF proof to hash: convert a VRF proof to its output hash.
    This is the pseudorandom output used for leader election. -/
def proofToHash (proof : VRFProof) : List UInt8 :=
  -- cofactor_Gamma = cofactor * Gamma (cofactor = 8 for Ed25519)
  let cGamma := EdPoint.scalarMul 8 proof.gamma
  let gammaBytes := EdPoint.compress cGamma
  -- output = SHA-512(suite_byte || 0x03 || Gamma_bytes)
  let hashInput := [suiteByte, 0x03] ++ gammaBytes
  let hashOutput := Internal.hashMessage hashInput
  hashOutput.flatMap Dion.Crypto.Integer.UInt64.toUInt8BE

end Dion.Crypto.VRF.ECVRF
