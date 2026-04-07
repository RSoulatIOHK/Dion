import Dion.Test.TestHarness
import Dion.Consensus.Praos.BlockForge
import Dion.Consensus.Praos.TxSelection
import Dion.Network.ConwayBlock
import Dion.Network.Cbor
import Dion.Network.CborCursor
import Dion.Network.Crypto
import Dion.Crypto.Sign.Ed25519.Point
import Dion.Crypto.VRF.ECVRF

/-!
# Block CBOR Round-Trip Tests

Verifies that forged blocks can be parsed back correctly.
-/

namespace Dion.Test.BlockRoundTripTest

open Dion.Test.TestHarness
open Dion.Consensus.Praos.BlockForge
open Dion.Consensus.Praos.TxSelection
open Dion.Consensus.Praos.ConsensusState
open Dion.Network.Cbor
open Dion.Network.CborCursor
open Dion.Network.ConwayBlock
open Dion.Network.Crypto
open Dion.Crypto.Sign.Ed25519.Point
open Dion.Crypto.VRF.ECVRF

/-- Dummy VRF proof for testing -/
private def dummyVrfProof : VRFProof :=
  { gamma := EdPoint.zero, challenge := 0, response := 0 }

private def dummyVrfOutput : List UInt8 := List.replicate 64 0

private def dummyOpCert : OperationalCert :=
  { hotVKey := ByteArray.mk (Array.replicate 32 0xAA)
    sequenceNumber := 1
    kesPeriod := 100
    coldKeySignature := ByteArray.mk (Array.replicate 64 0xBB) }

private def zeroHash32 : ByteArray := ByteArray.mk (Array.replicate 32 0)
private def zeroHash28 : ByteArray := ByteArray.mk (Array.replicate 28 0)

/-- Build a full 5-element block from body components -/
private def buildFullBlock (bodyComponents : BlockBodyComponents) : IO ByteArray := do
  let bodySerialized := bodyComponents.serialize
  let bodyHash ← blake2b_256 bodySerialized
  let headerBody := encodeHeaderBody
    42 1000 zeroHash32 zeroHash28 zeroHash32
    dummyVrfProof dummyVrfOutput
    bodySerialized.size bodyHash dummyOpCert 10 0
  let kesSig := ByteArray.mk (Array.replicate 576 0)
  let headerBytes := encodeBlockHeader headerBody kesSig
  return encodeArrayHeader 5
    ++ headerBytes
    ++ bodyComponents.txBodies
    ++ bodyComponents.witnessSets
    ++ bodyComponents.auxData
    ++ bodyComponents.invalidTxs

/-- Check CBOR major type of first byte -/
private def majorTypeAt (c : Cursor) : UInt8 := c.peek >>> 5

/-- Test: Empty block is a valid 5-element CBOR array with correct element types -/
def testEmptyBlockStructure : IO TestResult :=
  runTest "BlockRoundTrip" "empty block has valid 5-element CBOR structure" do
    let bodyComponents := encodeBlockBody ([] : List SelectedTx)
    let fullBlock ← buildFullBlock bodyComponents
    let c := Dion.Network.CborCursor.Cursor.mk' fullBlock
    -- Must be a 5-element array
    let some r := Dion.Network.CborCursor.decodeArrayHeader c | return false
    if r.value != 5 then
      IO.eprintln s!"  expected 5-element array, got {r.value}"
      return false
    -- Skip header (element 0)
    let some afterHeader := Dion.Network.CborCursor.skipValue r.cursor | return false
    -- Element 1: tx_bodies array
    if majorTypeAt afterHeader != 4 then
      IO.eprintln "  tx_bodies is not a CBOR array"
      return false
    let some afterBodies := Dion.Network.CborCursor.skipValue afterHeader | return false
    -- Element 2: witness_sets array
    if majorTypeAt afterBodies != 4 then
      IO.eprintln "  witness_sets is not a CBOR array"
      return false
    let some afterWit := Dion.Network.CborCursor.skipValue afterBodies | return false
    -- Element 3: aux_data map
    if majorTypeAt afterWit != 5 then
      IO.eprintln "  aux_data is not a CBOR map"
      return false
    let some afterAux := Dion.Network.CborCursor.skipValue afterWit | return false
    -- Element 4: invalid_txs array
    if majorTypeAt afterAux != 4 then
      IO.eprintln "  invalid_txs is not a CBOR array"
      return false
    return true

/-- Test: Empty block has 0 tx bodies -/
def testEmptyBlockTxCount : IO TestResult :=
  runTest "BlockRoundTrip" "empty block has 0 tx bodies" do
    let bodyComponents := encodeBlockBody ([] : List SelectedTx)
    let fullBlock ← buildFullBlock bodyComponents
    let c := Dion.Network.CborCursor.Cursor.mk' fullBlock
    let some r := Dion.Network.CborCursor.decodeArrayHeader c | return false
    let some afterHeader := Dion.Network.CborCursor.skipValue r.cursor | return false
    let some bodiesR := Dion.Network.CborCursor.decodeArrayHeader afterHeader | return false
    if bodiesR.value != 0 then
      IO.eprintln s!"  expected 0 tx bodies, got {bodiesR.value}"
      return false
    return true

/-- Test: Body components each have correct CBOR major type -/
def testBodyComponentTypes : IO TestResult :=
  runTest "BlockRoundTrip" "body components are valid CBOR types" do
    let bc := encodeBlockBody ([] : List SelectedTx)
    let txOk := bc.txBodies.size > 0 && bc.txBodies[0]! >>> 5 == 4     -- array
    let witOk := bc.witnessSets.size > 0 && bc.witnessSets[0]! >>> 5 == 4 -- array
    let auxOk := bc.auxData.size > 0 && bc.auxData[0]! >>> 5 == 5       -- map
    let invOk := bc.invalidTxs.size > 0 && bc.invalidTxs[0]! >>> 5 == 4 -- array
    return txOk && witOk && auxOk && invOk

/-- Test: Body hash is deterministic -/
def testBodyHashDeterministic : IO TestResult :=
  runTest "BlockRoundTrip" "body hash = blake2b_256(concatenated components)" do
    let bc := encodeBlockBody ([] : List SelectedTx)
    let h1 ← blake2b_256 bc.serialize
    let h2 ← blake2b_256 (bc.txBodies ++ bc.witnessSets ++ bc.auxData ++ bc.invalidTxs)
    return h1 == h2

/-- Test: Block with 1 tx has correct tx count -/
def testBlockWithOneTx : IO TestResult :=
  runTest "BlockRoundTrip" "block with 1 tx has correct structure and count" do
    let fakeTxBody := encodeMapHeader 3
      ++ encodeUInt 0 ++ encodeArrayHeader 0
      ++ encodeUInt 1 ++ encodeArrayHeader 0
      ++ encodeUInt 2 ++ encodeUInt 200000
    let fakeWitness := encodeMapHeader 0
    let tx : SelectedTx := {
      txHash := ByteArray.mk (Array.replicate 32 0xCC)
      rawBytes := ByteArray.empty
      bodyRawBytes := fakeTxBody
      witnessRawBytes := fakeWitness
      auxDataRawBytes := none
      fee := 200000
      size := 100
    }
    let bc := encodeBlockBody [tx]
    let fullBlock ← buildFullBlock bc
    let c := Dion.Network.CborCursor.Cursor.mk' fullBlock
    let some r := Dion.Network.CborCursor.decodeArrayHeader c | return false
    if r.value != 5 then return false
    let some afterHeader := Dion.Network.CborCursor.skipValue r.cursor | return false
    let some bodiesR := Dion.Network.CborCursor.decodeArrayHeader afterHeader | return false
    if bodiesR.value != 1 then
      IO.eprintln s!"  expected 1 tx body, got {bodiesR.value}"
      return false
    -- Verify witness_sets also has count 1
    let some afterBodies := Dion.Network.CborCursor.skipValue afterHeader | return false
    let some witR := Dion.Network.CborCursor.decodeArrayHeader afterBodies | return false
    if witR.value != 1 then
      IO.eprintln s!"  expected 1 witness set, got {witR.value}"
      return false
    return true

/-- Test: Header body is 10-element array -/
def testHeaderBody10Elements : IO TestResult :=
  runTest "BlockRoundTrip" "header body is 10-element array" do
    let headerBody := encodeHeaderBody
      42 1000 zeroHash32 zeroHash28 zeroHash32
      dummyVrfProof dummyVrfOutput
      100 zeroHash32 dummyOpCert 10 0
    let c := Dion.Network.CborCursor.Cursor.mk' headerBody
    let some r := Dion.Network.CborCursor.decodeArrayHeader c | return false
    if r.value != 10 then
      IO.eprintln s!"  expected 10 elements, got {r.value}"
      return false
    return true

/-- Test: Block with 2 txs, one with aux data -/
def testBlockWithAuxData : IO TestResult :=
  runTest "BlockRoundTrip" "block with aux data has non-empty aux_data map" do
    let fakeTxBody := encodeMapHeader 3
      ++ encodeUInt 0 ++ encodeArrayHeader 0
      ++ encodeUInt 1 ++ encodeArrayHeader 0
      ++ encodeUInt 2 ++ encodeUInt 100000
    let fakeWitness := encodeMapHeader 0
    let fakeAux := encodeMapHeader 1 ++ encodeUInt 0 ++ encodeBytes (ByteArray.mk #[0xDE, 0xAD])
    let tx1 : SelectedTx := {
      txHash := ByteArray.mk (Array.replicate 32 0x01)
      rawBytes := ByteArray.empty
      bodyRawBytes := fakeTxBody
      witnessRawBytes := fakeWitness
      auxDataRawBytes := some fakeAux
      fee := 100000
      size := 80
    }
    let tx2 : SelectedTx := {
      txHash := ByteArray.mk (Array.replicate 32 0x02)
      rawBytes := ByteArray.empty
      bodyRawBytes := fakeTxBody
      witnessRawBytes := fakeWitness
      auxDataRawBytes := none
      fee := 100000
      size := 80
    }
    let bc := encodeBlockBody [tx1, tx2]
    let fullBlock ← buildFullBlock bc
    let c := Dion.Network.CborCursor.Cursor.mk' fullBlock
    let some r := Dion.Network.CborCursor.decodeArrayHeader c | return false
    let some afterHeader := Dion.Network.CborCursor.skipValue r.cursor | return false
    let some afterBodies := Dion.Network.CborCursor.skipValue afterHeader | return false
    let some afterWit := Dion.Network.CborCursor.skipValue afterBodies | return false
    -- aux_data map should have 1 entry (only tx1 has aux data)
    let some auxR := Dion.Network.CborCursor.decodeMapHeader afterWit | return false
    if auxR.value != 1 then
      IO.eprintln s!"  expected 1 aux_data entry, got {auxR.value}"
      return false
    return true

-- ====================
-- = Test Runner      =
-- ====================

def runBlockRoundTripTests : IO (List TestResult) := do
  let mut results : List TestResult := []
  results := results ++ [← testEmptyBlockStructure]
  results := results ++ [← testEmptyBlockTxCount]
  results := results ++ [← testBodyComponentTypes]
  results := results ++ [← testBodyHashDeterministic]
  results := results ++ [← testBlockWithOneTx]
  results := results ++ [← testHeaderBody10Elements]
  results := results ++ [← testBlockWithAuxData]
  return results

end Dion.Test.BlockRoundTripTest
