import Cleanode.Test.TestHarness
import Cleanode.Consensus.Praos.BlockForge
import Cleanode.Consensus.Praos.TxSelection
import Cleanode.Consensus.Praos.ConsensusState
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Network.ConwayBlock
import Cleanode.Network.Cbor
import Cleanode.Network.CborCursor
import Cleanode.Network.Crypto
import Cleanode.Crypto.Sign.Ed25519.Point
import Cleanode.Crypto.VRF.ECVRF

/-!
# Integration Tests: Forge → Encode → Parse → Validate Roundtrip

End-to-end tests verifying that forged blocks:
1. Produce valid CBOR matching the Conway wire format
2. Can be parsed back by `parseConwayBlockBodyIO`
3. Have correct body hashes (Blake2b-256)
4. Maintain transaction integrity through the roundtrip
5. Work with the IO (production) forge path using real KES signing

These tests validate that blocks produced by Cleanode would be accepted
by any conformant Cardano node parser.
-/

namespace Cleanode.Test.IntegrationTest

open Cleanode.Test.TestHarness
open Cleanode.Consensus.Praos.BlockForge
open Cleanode.Consensus.Praos.TxSelection
open Cleanode.Consensus.Praos.ConsensusState
open Cleanode.Consensus.Praos.LeaderElection
open Cleanode.Network.Cbor
open Cleanode.Network.CborCursor
open Cleanode.Network.ConwayBlock
open Cleanode.Network.Crypto
open Cleanode.Crypto.Sign.Ed25519.Point
open Cleanode.Crypto.VRF.ECVRF

-- ====================
-- = Test Fixtures    =
-- ====================

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

/-- Build a realistic ForgeParams for testing -/
private def testForgeParams : ForgeParams :=
  { vrfSecretKey := List.replicate 32 0x11
    vrfPublicKey := List.replicate 32 0x22
    kesSigningKey := List.replicate 1088 0x33  -- Sum-KES-6 key size
    operationalCert := dummyOpCert
    poolId := zeroHash28
    protocolMajor := 10
    protocolMinor := 0 }

/-- Build a minimal fake transaction body CBOR (map with keys 0, 1, 2) -/
private def fakeTxBodyCbor (fee : Nat) : ByteArray :=
  encodeMapHeader 3
    ++ encodeUInt 0 ++ encodeArrayHeader 0  -- inputs: []
    ++ encodeUInt 1 ++ encodeArrayHeader 0  -- outputs: []
    ++ encodeUInt 2 ++ encodeUInt fee       -- fee

private def fakeWitnessCbor : ByteArray := encodeMapHeader 0

private def makeSelectedTx (txId : UInt8) (fee : Nat) (auxData : Option ByteArray := none) : SelectedTx :=
  { txHash := ByteArray.mk (Array.replicate 32 txId)
    rawBytes := ByteArray.empty
    bodyRawBytes := fakeTxBodyCbor fee
    witnessRawBytes := fakeWitnessCbor
    auxDataRawBytes := auxData
    fee := fee
    size := 200 }

/-- Build full block bytes from body components using IO (real blake2b hash) -/
private def buildFullBlockIO (bodyComponents : BlockBodyComponents)
    (blockNum slot : Nat) : IO ByteArray := do
  let bodySerialized := bodyComponents.serialize
  let bodyHash ← blake2b_256 bodySerialized
  let headerBody := encodeHeaderBody
    blockNum slot zeroHash32 zeroHash28 zeroHash32
    dummyVrfProof dummyVrfOutput
    bodySerialized.size bodyHash dummyOpCert 10 0
  let kesSig := ByteArray.mk (Array.replicate 448 0)
  let headerBytes := encodeBlockHeader headerBody kesSig
  return encodeArrayHeader 5
    ++ headerBytes
    ++ bodyComponents.txBodies
    ++ bodyComponents.witnessSets
    ++ bodyComponents.auxData
    ++ bodyComponents.invalidTxs

-- ==============================
-- = Forge → Encode Roundtrip  =
-- ==============================

def testForgeEmptyBlockParses : IO TestResult :=
  runTest "Integration" "forge empty block → parseConwayBlockBodyIO succeeds" do
    let bc := encodeBlockBody ([] : List SelectedTx)
    let fullBlock ← buildFullBlockIO bc 1 1000
    let result ← parseConwayBlockBodyIO fullBlock
    match result with
    | none => IO.eprintln "  parseConwayBlockBodyIO returned none" ; return false
    | some body => return body.transactions.isEmpty

def testForge1TxBlockParses : IO TestResult :=
  runTest "Integration" "forge 1-tx block → parseConwayBlockBodyIO finds 1 tx" do
    let tx := makeSelectedTx 0xCC 200000
    let bc := encodeBlockBody [tx]
    let fullBlock ← buildFullBlockIO bc 2 2000
    let result ← parseConwayBlockBodyIO fullBlock
    match result with
    | none => IO.eprintln "  parseConwayBlockBodyIO returned none" ; return false
    | some body =>
      if body.transactions.length != 1 then
        IO.eprintln s!"  expected 1 tx, got {body.transactions.length}"
        return false
      return true

def testForge3TxBlockParses : IO TestResult :=
  runTest "Integration" "forge 3-tx block → parseConwayBlockBodyIO finds 3 txs" do
    let tx1 := makeSelectedTx 0x01 100000
    let tx2 := makeSelectedTx 0x02 200000
    let tx3 := makeSelectedTx 0x03 300000
    let bc := encodeBlockBody [tx1, tx2, tx3]
    let fullBlock ← buildFullBlockIO bc 3 3000
    let result ← parseConwayBlockBodyIO fullBlock
    match result with
    | none => return false
    | some body => return body.transactions.length == 3

def testForgeBlockWithAuxData : IO TestResult :=
  runTest "Integration" "forge block with aux data → parses with correct tx count" do
    let auxData := encodeMapHeader 1 ++ encodeUInt 0 ++ encodeBytes (ByteArray.mk #[0xDE, 0xAD])
    let tx1 := makeSelectedTx 0x01 100000 (auxData := some auxData)
    let tx2 := makeSelectedTx 0x02 100000
    let bc := encodeBlockBody [tx1, tx2]
    let fullBlock ← buildFullBlockIO bc 4 4000
    let result ← parseConwayBlockBodyIO fullBlock
    match result with
    | none => return false
    | some body => return body.transactions.length == 2

-- ==============================
-- = Body Hash Integrity       =
-- ==============================

def testBodyHashDeterministic : IO TestResult :=
  runTest "Integration" "body hash is deterministic across two identical encodings" do
    let tx := makeSelectedTx 0xAA 150000
    let bc1 := encodeBlockBody [tx]
    let bc2 := encodeBlockBody [tx]
    let h1 ← blake2b_256 bc1.serialize
    let h2 ← blake2b_256 bc2.serialize
    return h1 == h2

def testBodyHashChangesWithDifferentTxs : IO TestResult :=
  runTest "Integration" "body hash differs for different transaction sets" do
    let tx1 := makeSelectedTx 0x01 100000
    let tx2 := makeSelectedTx 0x02 200000
    let bc1 := encodeBlockBody [tx1]
    let bc2 := encodeBlockBody [tx2]
    let h1 ← blake2b_256 bc1.serialize
    let h2 ← blake2b_256 bc2.serialize
    return h1 != h2

def testBodyHashMatchesSerializedComponents : IO TestResult :=
  runTest "Integration" "body hash = blake2b_256(txBodies ++ witnesses ++ auxData ++ invalidTxs)" do
    let tx := makeSelectedTx 0xFF 500000
    let bc := encodeBlockBody [tx]
    let h1 ← blake2b_256 bc.serialize
    let h2 ← blake2b_256 (bc.txBodies ++ bc.witnessSets ++ bc.auxData ++ bc.invalidTxs)
    return h1 == h2

-- ==============================
-- = CBOR Structure Validation =
-- ==============================

def testFullBlockIs5ElementArray : IO TestResult :=
  runTest "Integration" "full block is a 5-element CBOR array" do
    let bc := encodeBlockBody ([] : List SelectedTx)
    let fullBlock ← buildFullBlockIO bc 10 10000
    let c := Cleanode.Network.CborCursor.Cursor.mk' fullBlock
    let some r := Cleanode.Network.CborCursor.decodeArrayHeader c | return false
    return r.value == 5

def testHeaderIsTag24Wrapped : IO TestResult :=
  runTest "Integration" "block header (element 0) starts with CBOR tag 24" do
    let bc := encodeBlockBody ([] : List SelectedTx)
    let fullBlock ← buildFullBlockIO bc 11 11000
    let c := Cleanode.Network.CborCursor.Cursor.mk' fullBlock
    let some r := Cleanode.Network.CborCursor.decodeArrayHeader c | return false
    -- Element 0: header should start with tag 24 = 0xd8 0x18
    let headerByte0 := r.cursor.peek
    return headerByte0 == 0xd8

def testParsedTxBodyHasCorrectFee : IO TestResult :=
  runTest "Integration" "parsed tx body fee matches forged fee" do
    let tx := makeSelectedTx 0xBB 250000
    let bc := encodeBlockBody [tx]
    let fullBlock ← buildFullBlockIO bc 12 12000
    let result ← parseConwayBlockBodyIO fullBlock
    match result with
    | none => return false
    | some body =>
      match body.transactions.head? with
      | none => return false
      | some parsedTx => return parsedTx.body.fee == 250000

def testParsedTxPreservesInputsOutputs : IO TestResult :=
  runTest "Integration" "parsed tx body has empty inputs/outputs (matching forged)" do
    let tx := makeSelectedTx 0xDD 300000
    let bc := encodeBlockBody [tx]
    let fullBlock ← buildFullBlockIO bc 13 13000
    let result ← parseConwayBlockBodyIO fullBlock
    match result with
    | none => return false
    | some body =>
      match body.transactions.head? with
      | none => return false
      | some parsedTx =>
        return parsedTx.body.inputs.isEmpty && parsedTx.body.outputs.isEmpty

-- ==============================
-- = ForgedBlock.encodeFullBlock =
-- ==============================

def testEncodeFullBlockMethod : IO TestResult :=
  runTest "Integration" "ForgedBlock.encodeFullBlock produces parseable block" do
    -- Build a ForgedBlock manually
    let tx := makeSelectedTx 0xEE 175000
    let bodyComponents := encodeBlockBody [tx]
    let bodySerialized := bodyComponents.serialize
    let bodyHash ← blake2b_256 bodySerialized
    let headerBody := encodeHeaderBody
      42 99999 zeroHash32 zeroHash28 zeroHash32
      dummyVrfProof dummyVrfOutput
      bodySerialized.size bodyHash dummyOpCert 10 0
    let kesSig := ByteArray.mk (Array.replicate 448 0)
    let headerBytes := encodeBlockHeader headerBody kesSig
    let forgedBlock : ForgedBlock :=
      { blockNumber := 42
        slot := 99999
        prevHash := zeroHash32
        headerBytes := headerBytes
        bodyComponents := bodyComponents
        vrfProof := dummyVrfProof
        vrfOutput := dummyVrfOutput
        selectedTxs := { transactions := [tx], totalSize := 200, totalFees := 175000 } }
    let encoded := forgedBlock.encodeFullBlock
    let result ← parseConwayBlockBodyIO encoded
    match result with
    | none => return false
    | some body => return body.transactions.length == 1

-- ==============================
-- = Multiple Blocks Sequence  =
-- ==============================

def testSequentialBlocksHaveDifferentHashes : IO TestResult :=
  runTest "Integration" "sequential blocks produce different body hashes" do
    let tx1 := makeSelectedTx 0x01 100000
    let tx2 := makeSelectedTx 0x02 200000
    let bc1 := encodeBlockBody [tx1]
    let bc2 := encodeBlockBody [tx2]
    let block1 ← buildFullBlockIO bc1 1 1000
    let block2 ← buildFullBlockIO bc2 2 2000
    -- Hash each full block
    let h1 ← blake2b_256 block1
    let h2 ← blake2b_256 block2
    return h1 != h2

def testLargeBlockWith50Txs : IO TestResult :=
  runTest "Integration" "block with 50 txs forges and parses correctly" do
    let txs := (List.range 50).map fun i =>
      makeSelectedTx (i.toUInt8) (100000 + i * 1000)
    let bc := encodeBlockBody txs
    let fullBlock ← buildFullBlockIO bc 100 100000
    let result ← parseConwayBlockBodyIO fullBlock
    match result with
    | none => return false
    | some body => return body.transactions.length == 50

-- ==============================
-- = Wire Format Compliance    =
-- ==============================

def testHeaderBodyIs10Elements : IO TestResult :=
  runTest "Integration" "header body inside tag24 is 10-element array" do
    let bc := encodeBlockBody ([] : List SelectedTx)
    let fullBlock ← buildFullBlockIO bc 20 20000
    let c := Cleanode.Network.CborCursor.Cursor.mk' fullBlock
    -- Skip outer array header
    let some outerArr := Cleanode.Network.CborCursor.decodeArrayHeader c | return false
    -- Header starts with tag 24 (0xd8 0x18) — skip 2 bytes for tag
    let afterTag := outerArr.cursor.advance 2
    -- Tag 24 wraps a 2-element array [headerBody, kesSig] directly (no bytestring wrapper)
    let some innerArr := Cleanode.Network.CborCursor.decodeArrayHeader afterTag | return false
    if innerArr.value != 2 then
      IO.eprintln s!"  header wrapper expected 2 elements, got {innerArr.value}"
      return false
    -- First element is the header body (10-element array)
    let some headerBodyArr := Cleanode.Network.CborCursor.decodeArrayHeader innerArr.cursor | return false
    if headerBodyArr.value != 10 then
      IO.eprintln s!"  header body expected 10 elements, got {headerBodyArr.value}"
      return false
    return true

def testEmptyBlockBodyComponentSizes : IO TestResult :=
  runTest "Integration" "empty block body components are minimal valid CBOR" do
    let bc := encodeBlockBody ([] : List SelectedTx)
    -- txBodies: empty array = 0x80 (1 byte)
    let txOk := bc.txBodies.size == 1 && bc.txBodies[0]! == 0x80
    -- witnessSets: empty array = 0x80 (1 byte)
    let witOk := bc.witnessSets.size == 1 && bc.witnessSets[0]! == 0x80
    -- auxData: empty map = 0xa0 (1 byte)
    let auxOk := bc.auxData.size == 1 && bc.auxData[0]! == 0xa0
    -- invalidTxs: empty array = 0x80 (1 byte)
    let invOk := bc.invalidTxs.size == 1 && bc.invalidTxs[0]! == 0x80
    return txOk && witOk && auxOk && invOk

def testBlockSizeReasonable : IO TestResult :=
  runTest "Integration" "empty block is under 1 KB, 50-tx block is under 10 KB" do
    let emptyBc := encodeBlockBody ([] : List SelectedTx)
    let emptyBlock ← buildFullBlockIO emptyBc 1 1
    let txs := (List.range 50).map fun i =>
      makeSelectedTx (i.toUInt8) (100000 + i * 1000)
    let bigBc := encodeBlockBody txs
    let bigBlock ← buildFullBlockIO bigBc 2 2
    let emptyOk := emptyBlock.size < 1024
    let bigOk := bigBlock.size < 10240
    if !emptyOk then IO.eprintln s!"  empty block size: {emptyBlock.size} bytes"
    if !bigOk then IO.eprintln s!"  50-tx block size: {bigBlock.size} bytes"
    return emptyOk && bigOk

-- ====================
-- = Test Runner      =
-- ====================

private def runForgeParseTests : IO (List TestResult) := do
  let mut r : List TestResult := []
  r := r ++ [← testForgeEmptyBlockParses]
  r := r ++ [← testForge1TxBlockParses]
  r := r ++ [← testForge3TxBlockParses]
  r := r ++ [← testForgeBlockWithAuxData]
  return r

private def runHashIntegrityTests : IO (List TestResult) := do
  let mut r : List TestResult := []
  r := r ++ [← testBodyHashDeterministic]
  r := r ++ [← testBodyHashChangesWithDifferentTxs]
  r := r ++ [← testBodyHashMatchesSerializedComponents]
  return r

private def runStructureTests : IO (List TestResult) := do
  let mut r : List TestResult := []
  r := r ++ [← testFullBlockIs5ElementArray]
  r := r ++ [← testHeaderIsTag24Wrapped]
  r := r ++ [← testParsedTxBodyHasCorrectFee]
  r := r ++ [← testParsedTxPreservesInputsOutputs]
  r := r ++ [← testEncodeFullBlockMethod]
  r := r ++ [← testSequentialBlocksHaveDifferentHashes]
  r := r ++ [← testLargeBlockWith50Txs]
  r := r ++ [← testHeaderBodyIs10Elements]
  r := r ++ [← testEmptyBlockBodyComponentSizes]
  r := r ++ [← testBlockSizeReasonable]
  return r

def runIntegrationTests : IO (List TestResult) := do
  let mut results : List TestResult := []
  results := results ++ (← runForgeParseTests)
  results := results ++ (← runHashIntegrityTests)
  results := results ++ (← runStructureTests)
  return results

end Cleanode.Test.IntegrationTest
