import Dion.Crypto.Sign.Ed25519.Signature
import Dion.Crypto.Hash.Sha512

/-!
# KES (Key Evolving Signatures) - Sum Composition

Cardano uses a Sum-KES scheme built as a binary tree over Ed25519.
At each level of the tree, two child keys form a parent key.
The key "evolves" by moving through the tree leaves.

For a tree of depth d, there are 2^d time periods.
Cardano uses d=6, giving 64 periods per operational certificate.

## Verification
To verify a KES signature at period t:
1. Parse the signature as a chain of (sigma, vk_peer, vk_left/right) from leaf to root
2. Verify the Ed25519 leaf signature
3. Verify each level's verification key matches the hash of its children
4. Check the root verification key matches the expected one

## References
- MMM Signature Scheme
- Cardano KES specification
- https://github.com/IntersectMBO/cardano-base/tree/master/cardano-crypto-class
-/

namespace Dion.Crypto.Sign.KES

open Dion.Crypto.Sign.Ed25519.Signature
open Dion.Crypto.Hash.Sha512

-- ====================
-- = KES Types        =
-- ====================

/-- Depth of the KES tree (Cardano uses 6) -/
def defaultDepth : Nat := 6

/-- Maximum evolutions = 2^depth -/
def maxEvolutions (depth : Nat := defaultDepth) : Nat := 2 ^ depth

/-- A single level of the Sum-KES signature.
    Contains the Ed25519 signature at that level plus the sibling verification key. -/
structure KESLevel where
  signature : List UInt8      -- 64-byte Ed25519 signature
  companionVK : List UInt8    -- 32-byte verification key of the sibling node

/-- Complete KES signature: a chain of levels from leaf to root -/
structure KESSignature where
  levels : List KESLevel      -- From leaf (index 0) to root
  period : Nat                -- The time period this signature is for

/-- KES verification key (32 bytes - hash of left||right child VKs at root) -/
structure KESVerificationKey where
  bytes : List UInt8          -- 32 bytes
  deriving BEq

-- ====================
-- = KES Verification =
-- ====================

/-- Hash two verification keys to produce their parent VK.
    parent_vk = SHA-512(left_vk || right_vk) truncated to 32 bytes.
    Note: Cardano actually uses Blake2b-256 for this, but we use SHA-512
    truncated for the pure Lean implementation. The FFI Blake2b can be
    used when available. -/
private def hashVKPair (left right : List UInt8) : List UInt8 :=
  let hashOutput := Internal.hashMessage (left ++ right)
  let hashBytes := hashOutput.flatMap Dion.Crypto.Integer.UInt64.toUInt8BE
  hashBytes.take 32

/-- Verify a KES signature at a given period.
    verificationKey: the root KES verification key (32 bytes)
    period: the time period
    message: the message that was signed
    signature: the KES signature chain -/
def verify (verificationKey : KESVerificationKey) (period : Nat)
    (message : List UInt8) (sig : KESSignature) : Bool :=
  if sig.levels.isEmpty then false
  else if sig.period != period then false
  else
    let depth := sig.levels.length
    -- Walk from leaf to root, verifying each level
    -- At the leaf level, verify the Ed25519 signature
    match sig.levels[0]? with
    | none => false
    | some leafLevel =>
      -- The leaf's signing VK is derived from the path through the tree
      -- For verification, we reconstruct the root VK from the signature chain
      -- and check it matches the expected one

      -- Determine the path through the tree based on the period
      -- Each bit of the period (from LSB) determines left (0) or right (1)
      let rec buildVK (idx : Nat) (currentVK : List UInt8) : List UInt8 :=
        if idx >= depth then currentVK
        else
          match sig.levels[idx]? with
          | none => currentVK  -- shouldn't happen
          | some level =>
            let bit := (period >>> idx) &&& 1
            let parentVK := if bit == 0 then
              hashVKPair currentVK level.companionVK
            else
              hashVKPair level.companionVK currentVK
            buildVK (idx + 1) parentVK
      termination_by depth - idx

      -- First, verify the Ed25519 signature at the leaf
      -- The leaf VK is the companion of level 0, or reconstructed
      -- For simplicity, we verify the leaf signature against the message
      let leafSigValid := Dion.Crypto.Sign.Ed25519.Signature.verify
        leafLevel.companionVK message leafLevel.signature

      if !leafSigValid then false
      else
        -- Build root VK from leaf upward
        let computedRoot := buildVK 0 leafLevel.companionVK
        computedRoot == verificationKey.bytes

-- ====================
-- = Serialization    =
-- ====================

/-- Parse a KES signature from raw bytes (Cardano wire format).
    The format is: for each level (leaf to root):
      - 64 bytes: Ed25519 signature
      - 32 bytes: companion verification key -/
def parseSignature (bytes : List UInt8) (depth : Nat) (period : Nat) : Option KESSignature :=
  let levelSize := 64 + 32  -- signature + companion VK
  if bytes.length < depth * levelSize then none
  else
    let levels := (List.range depth).map fun i =>
      let offset := i * levelSize
      let sig := (bytes.drop offset).take 64
      let vk := (bytes.drop (offset + 64)).take 32
      { signature := sig, companionVK := vk : KESLevel }
    some { levels := levels, period := period }

/-- Get the maximum number of periods for a given depth -/
def maxPeriods (depth : Nat := defaultDepth) : Nat :=
  2 ^ depth

/-- Check if a KES period is valid for the given depth -/
def isValidPeriod (period : Nat) (depth : Nat := defaultDepth) : Bool :=
  period < maxPeriods depth

end Dion.Crypto.Sign.KES
