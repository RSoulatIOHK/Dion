/-!
# KES Signing FFI

C FFI bindings for Sum-KES signing operations:
- Sign a message with a KES key at a given period
- Evolve a key to the next period (forward security)
- Derive the verification key from a signing key

Uses the same Ed25519 + Blake2b primitives as verification.
-/

namespace Dion.Crypto.Sign.KESSign

/-- Sign a message with a KES signing key at the given period.
    Returns a 576-byte Sum-KES-6 signature. -/
@[extern "dion_kes_sign"]
opaque kesSign (signingKey : @& ByteArray) (period : UInt32) (message : @& ByteArray) : IO (Except String ByteArray)

/-- Evolve a KES signing key to the next period.
    Erases previous period key material (forward security). -/
@[extern "dion_kes_evolve"]
opaque kesEvolve (signingKey : @& ByteArray) (currentPeriod : UInt32) : IO (Except String ByteArray)

/-- Derive the 32-byte root verification key from a KES signing key. -/
@[extern "dion_kes_derive_vk"]
opaque kesDeriveVK (signingKey : @& ByteArray) : IO (Except String ByteArray)

/-- Generate a fresh Sum-KES-6 key pair at period 0.
    Returns (signingKey: 256 bytes, verificationKey: 32 bytes). -/
@[extern "dion_kes_keygen"]
opaque kesKeygen : IO (Except String (ByteArray × ByteArray))

/-- Maximum KES evolutions for depth 6 -/
def maxEvolutions : Nat := 64

/-- Check if a KES period is valid -/
def isValidPeriod (period : Nat) : Bool := period < maxEvolutions

end Dion.Crypto.Sign.KESSign
