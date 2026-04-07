import Dion.Network.Cbor
import Dion.Test.TestHarness

/-!
# CBOR Roundtrip & Golden Tests

Validates CBOR encoding/decoding correctness:
1. Encode→Decode roundtrip for all major types
2. Golden tests against known Cardano CBOR fragments
3. Edge cases: empty arrays, large integers, nested structures

## References
- RFC 8949: Concise Binary Object Representation (CBOR)
- Cardano node CBOR encoding conventions
-/

namespace Dion.Test.CborTest

open Dion.Network.Cbor
open Dion.Test.TestHarness

-- ====================
-- = Roundtrip Tests  =
-- ====================

/-- Test uint encoding roundtrip -/
private def testUIntRoundtrip (n : Nat) : IO Bool := do
  let encoded := encodeHead .UnsignedInt n
  match decodeHead encoded with
  | some ⟨(0, v), _⟩ => return v == n
  | _ => return false

/-- Test bytes encoding roundtrip -/
private def testBytesRoundtrip (bs : ByteArray) : IO Bool := do
  let encoded := encodeBytes bs
  match decodeBytes encoded with
  | some ⟨decoded, _⟩ => return decoded == bs
  | _ => return false

/-- Test array header roundtrip -/
private def testArrayHeaderRoundtrip (n : Nat) : IO Bool := do
  let encoded := encodeArrayHeader n
  match decodeArrayHeader encoded with
  | some ⟨decoded, _⟩ => return decoded == n
  | _ => return false

/-- Test map header roundtrip -/
private def testMapHeaderRoundtrip (n : Nat) : IO Bool := do
  let encoded := encodeMapHeader n
  match decodeMapHeader encoded with
  | some ⟨decoded, _⟩ => return decoded == n
  | _ => return false

def runCborTests : IO (Array TestResult) := do
  let mut results : Array TestResult := #[]

  -- UInt roundtrips
  for n in [0, 1, 23, 24, 255, 256, 65535, 65536, 1000000] do
    results := results.push (← runTest "cbor" s!"uint_roundtrip_{n}" (testUIntRoundtrip n))

  -- Bytes roundtrips
  results := results.push (← runTest "cbor" "bytes_empty" (testBytesRoundtrip ByteArray.empty))
  results := results.push (← runTest "cbor" "bytes_1" (testBytesRoundtrip (ByteArray.mk #[0xAB])))
  results := results.push (← runTest "cbor" "bytes_32" (testBytesRoundtrip (ByteArray.mk (Array.replicate 32 0xFF))))
  results := results.push (← runTest "cbor" "bytes_256" (testBytesRoundtrip (ByteArray.mk (Array.replicate 256 0x42))))

  -- Array header roundtrips
  for n in [0, 1, 5, 23, 24, 100, 1000] do
    results := results.push (← runTest "cbor" s!"array_header_{n}" (testArrayHeaderRoundtrip n))

  -- Map header roundtrips
  for n in [0, 1, 5, 23, 24, 100] do
    results := results.push (← runTest "cbor" s!"map_header_{n}" (testMapHeaderRoundtrip n))

  -- Bool roundtrips
  results := results.push (← runTest "cbor" "bool_true" do
    let encoded := encodeBool true
    match decodeBool encoded with
    | some ⟨v, _⟩ => return v == true
    | _ => return false)

  results := results.push (← runTest "cbor" "bool_false" do
    let encoded := encodeBool false
    match decodeBool encoded with
    | some ⟨v, _⟩ => return v == false
    | _ => return false)

  -- Tag roundtrip
  results := results.push (← runTest "cbor" "tag_roundtrip" do
    let inner := encodeUInt 42
    let encoded := encodeTagged 258 inner
    match decodeTag encoded with
    | some ⟨tag, rest⟩ => return tag == 258 && rest.size > 0
    | _ => return false)

  -- Golden: CBOR encoding of uint 0 = 0x00
  results := results.push (← runTest "cbor" "golden_uint_0" do
    let encoded := encodeHead .UnsignedInt 0
    return encoded == ByteArray.mk #[0x00])

  -- Golden: CBOR encoding of uint 23 = 0x17
  results := results.push (← runTest "cbor" "golden_uint_23" do
    let encoded := encodeHead .UnsignedInt 23
    return encoded == ByteArray.mk #[0x17])

  -- Golden: CBOR encoding of uint 24 = 0x18 0x18
  results := results.push (← runTest "cbor" "golden_uint_24" do
    let encoded := encodeHead .UnsignedInt 24
    return encoded == ByteArray.mk #[0x18, 0x18])

  -- Golden: CBOR encoding of uint 256 = 0x19 0x01 0x00
  results := results.push (← runTest "cbor" "golden_uint_256" do
    let encoded := encodeHead .UnsignedInt 256
    return encoded == ByteArray.mk #[0x19, 0x01, 0x00])

  -- Golden: Empty bytes = 0x40
  results := results.push (← runTest "cbor" "golden_bytes_empty" do
    let encoded := encodeBytes ByteArray.empty
    return encoded == ByteArray.mk #[0x40])

  -- Golden: Empty array = 0x80
  results := results.push (← runTest "cbor" "golden_array_empty" do
    let encoded := encodeArrayHeader 0
    return encoded == ByteArray.mk #[0x80])

  -- Golden: Empty map = 0xa0
  results := results.push (← runTest "cbor" "golden_map_empty" do
    let encoded := encodeMapHeader 0
    return encoded == ByteArray.mk #[0xa0])

  -- Golden: true = 0xf5, false = 0xf4
  results := results.push (← runTest "cbor" "golden_true" do
    return encodeBool true == ByteArray.mk #[0xf5])
  results := results.push (← runTest "cbor" "golden_false" do
    return encodeBool false == ByteArray.mk #[0xf4])

  -- Golden: break = 0xff
  results := results.push (← runTest "cbor" "golden_break" do
    return encodeBreak == ByteArray.mk #[0xff])

  -- Golden: indefinite array header = 0x9f
  results := results.push (← runTest "cbor" "golden_indef_array" do
    return encodeIndefiniteArrayHeader == ByteArray.mk #[0x9f])

  -- Golden: indefinite map header = 0xbf
  results := results.push (← runTest "cbor" "golden_indef_map" do
    return encodeIndefiniteMapHeader == ByteArray.mk #[0xbf])

  return results

end Dion.Test.CborTest
