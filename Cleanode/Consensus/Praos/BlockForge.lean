import Dion.Consensus.Praos.ConsensusState
import Dion.Consensus.Praos.LeaderElection
import Dion.Consensus.Praos.TxSelection
import Dion.Crypto.Sign.KES
import Dion.Crypto.Sign.KESSign
import Dion.Crypto.VRF.ECVRF
import Dion.Network.Cbor
import Dion.Network.Crypto

/-!
# Block Forging

Creates new blocks when a stake pool is elected as slot leader.

## Block Header Structure (Shelley+)
The header body is a 10-element CBOR array:
0. blockNumber : Nat
1. slot : Nat
2. prevHash : ByteArray (32 bytes, or empty for genesis)
3. issuerVKeyHash : ByteArray (28 bytes)
4. vrfVKey : ByteArray (32 bytes)
5. vrfResult : [vrfOutput, vrfProof]
6. bodySize : Nat
7. bodyHash : ByteArray (32 bytes)
8. opCert : [hotVKey, seqNum, kesPeriod, coldSig]
9. protocolVersion : [major, minor]

## References
- Cardano Ledger Spec: Block structure
- Shelley CDDL specification
-/

namespace Dion.Consensus.Praos.BlockForge

open Dion.Consensus.Praos.ConsensusState
open Dion.Consensus.Praos.LeaderElection
open Dion.Consensus.Praos.TxSelection
open Dion.Crypto.Sign.KES
open Dion.Crypto.VRF.ECVRF

-- ====================
-- = Forge Parameters =
-- ====================

/-- Parameters needed to forge a block -/
structure ForgeParams where
  vrfSecretKey : List UInt8     -- VRF signing key (32 bytes)
  vrfPublicKey : List UInt8     -- VRF verification key (32 bytes)
  kesSigningKey : List UInt8    -- KES signing key bytes
  operationalCert : OperationalCert
  poolId : ByteArray            -- Pool operator's key hash (28 bytes)
  protocolMajor : Nat           -- Protocol version major
  protocolMinor : Nat           -- Protocol version minor

-- ====================
-- = Forged Block     =
-- ====================

/-- The 4 independently-encoded CBOR components of a block body.
    bodyHash = blake2b_256(txBodies ++ witnessSets ++ auxData ++ invalidTxs)
    The full block is a 5-element CBOR array: [header, txBodies, witnessSets, auxData, invalidTxs] -/
structure BlockBodyComponents where
  txBodies    : ByteArray  -- CBOR-encoded array of tx bodies
  witnessSets : ByteArray  -- CBOR-encoded array of witness sets
  auxData     : ByteArray  -- CBOR-encoded map of auxiliary data
  invalidTxs  : ByteArray  -- CBOR-encoded array of invalid tx indices

/-- Concatenation of body components (used for hashing and size) -/
def BlockBodyComponents.serialize (c : BlockBodyComponents) : ByteArray :=
  c.txBodies ++ c.witnessSets ++ c.auxData ++ c.invalidTxs

/-- A forged block ready for network propagation -/
structure ForgedBlock where
  blockNumber : Nat
  slot : Nat
  prevHash : ByteArray
  headerBytes : ByteArray       -- CBOR-encoded header (for hashing and sending)
  bodyComponents : BlockBodyComponents  -- 4 body components (separately encoded)
  vrfProof : VRFProof
  vrfOutput : List UInt8
  selectedTxs : BlockBody

/-- Encode the full block as a 5-element CBOR array for BlockFetch serving:
    [header, tx_bodies, witness_sets, aux_data, invalid_txs] -/
def ForgedBlock.encodeFullBlock (block : ForgedBlock) : ByteArray :=
  open Dion.Network.Cbor in
  encodeArrayHeader 5
    ++ block.headerBytes
    ++ block.bodyComponents.txBodies
    ++ block.bodyComponents.witnessSets
    ++ block.bodyComponents.auxData
    ++ block.bodyComponents.invalidTxs

-- ==========================
-- = Header CBOR Encoding   =
-- ==========================

open Dion.Network.Cbor in
/-- Encode VRF result as CBOR array [output, proof].
    output: 64-byte VRF output as byte string
    proof: 80-byte VRF proof as byte string -/
private def encodeVrfResult (proof : VRFProof) (output : List UInt8) : ByteArray :=
  let outputBytes := ByteArray.mk output.toArray
  let proofGamma := Dion.Crypto.Sign.Ed25519.Point.EdPoint.compress proof.gamma
  -- VRF proof encoding: [gamma (32B) || challenge (16B) || response (32B)] = 80 bytes
  let challengeBytes := (List.range 16).map fun i =>
    ((proof.challenge >>> (i * 8)) % 256).toUInt8
  let responseBytes := (List.range 32).map fun i =>
    ((proof.response >>> (i * 8)) % 256).toUInt8
  let proofBytes := ByteArray.mk (proofGamma ++ challengeBytes ++ responseBytes).toArray
  encodeArrayHeader 2 ++ encodeBytes outputBytes ++ encodeBytes proofBytes

open Dion.Network.Cbor in
/-- Encode operational certificate as CBOR array [hotVKey, seqNum, kesPeriod, coldSig] -/
private def encodeOpCert (cert : OperationalCert) : ByteArray :=
  encodeArrayHeader 4
    ++ encodeBytes cert.hotVKey
    ++ encodeUInt cert.sequenceNumber
    ++ encodeUInt cert.kesPeriod
    ++ encodeBytes cert.coldKeySignature

open Dion.Network.Cbor in
/-- Encode protocol version as CBOR array [major, minor] -/
private def encodeProtocolVersion (major minor : Nat) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt major ++ encodeUInt minor

open Dion.Network.Cbor in
/-- Encode the 10-element header body as CBOR.
    Fields:
    0. blockNumber
    1. slot
    2. prevHash (32 bytes, or empty for genesis)
    3. issuerVKeyHash (28 bytes)
    4. vrfVKey (32 bytes)
    5. vrfResult [output, proof]
    6. bodySize
    7. bodyHash (32 bytes)
    8. opCert [hotVKey, seqNum, kesPeriod, coldSig]
    9. protocolVersion [major, minor] -/
def encodeHeaderBody (blockNumber slot : Nat) (prevHash issuerVKeyHash vrfVKey : ByteArray)
    (vrfProof : VRFProof) (vrfOutput : List UInt8)
    (bodySize : Nat) (bodyHash : ByteArray)
    (opCert : OperationalCert) (protocolMajor protocolMinor : Nat) : ByteArray :=
  encodeArrayHeader 10
    ++ encodeUInt blockNumber
    ++ encodeUInt slot
    ++ encodeBytes prevHash
    ++ encodeBytes issuerVKeyHash
    ++ encodeBytes vrfVKey
    ++ encodeVrfResult vrfProof vrfOutput
    ++ encodeUInt bodySize
    ++ encodeBytes bodyHash
    ++ encodeOpCert opCert
    ++ encodeProtocolVersion protocolMajor protocolMinor

open Dion.Network.Cbor in
/-- Wrap header body in a full block header: [headerBody, kesSig]
    Wrapped in CBOR tag 24 (encoded CBOR) for on-wire format. -/
def encodeBlockHeader (headerBodyBytes kesSig : ByteArray) : ByteArray :=
  let innerArray := encodeArrayHeader 2 ++ headerBodyBytes ++ encodeBytes kesSig
  encodeTagged 24 innerArray

open Dion.Network.Cbor in
/-- Encode the block body as 4 independently-encoded CBOR components:
    tx_bodies[], witness_sets[], {idx: aux_data}, invalid_txs[]

    These are stored separately because:
    - bodyHash = blake2b_256(txBodies ++ witnessSets ++ auxData ++ invalidTxs)
    - The full block is a flat 5-element array: [header, txBodies, witnessSets, auxData, invalidTxs] -/
def encodeBlockBody (txs : List SelectedTx) : BlockBodyComponents :=
  -- tx_bodies array
  let txBodies := txs.foldl (fun acc tx => acc ++ tx.bodyRawBytes)
    (encodeArrayHeader txs.length)
  -- witness_sets array
  let witnessSets := txs.foldl (fun acc tx => acc ++ tx.witnessRawBytes)
    (encodeArrayHeader txs.length)
  -- auxiliary_data map {index => auxData}
  let auxPairs := (List.range txs.length).zip txs |>.filterMap fun (idx, tx) =>
    tx.auxDataRawBytes.map (idx, ·)
  let auxDataMap := auxPairs.foldl (fun acc (idx, auxBytes) => acc ++ encodeUInt idx ++ auxBytes)
    (encodeMapHeader auxPairs.length)
  -- invalid_txs (always empty — we only include valid txs)
  let invalidTxs := encodeArrayHeader 0
  { txBodies, witnessSets, auxData := auxDataMap, invalidTxs }

-- ====================
-- = Forge Logic      =
-- ====================

/-- Attempt to forge a block for a given slot.
    Returns none if the pool is not elected leader for this slot. -/
def tryForgeBlock (params : ForgeParams) (consensusState : ConsensusState)
    (blockNumber : Nat) (slot : Nat) (prevHash : ByteArray)
    (blockBody : BlockBody) : Option ForgedBlock :=
  -- Check leader election
  let poolStake := getPoolStake consensusState params.poolId
  let totalStake := consensusState.stakeSnapshot.totalStake
  match checkLeader params.vrfSecretKey consensusState.epochNonce
    slot consensusState.activeSlotsCoeff poolStake totalStake with
  | .notLeader => none
  | .invalidPool => none
  | .isLeader vrfProof vrfOutput =>
    -- Encode the block body components
    let bodyComponents := encodeBlockBody blockBody.transactions
    let bodySerialized := bodyComponents.serialize
    -- Compute body hash (placeholder — in production use blake2b_256 via IO)
    let bodyHash := ByteArray.mk #[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    -- Encode header body (VRF key from params)
    let vrfVKey := ByteArray.mk params.vrfPublicKey.toArray
    let headerBodyBytes := encodeHeaderBody blockNumber slot prevHash
      params.poolId vrfVKey vrfProof vrfOutput
      bodySerialized.size bodyHash
      params.operationalCert params.protocolMajor params.protocolMinor
    -- KES signature placeholder (would be signed via IO in production)
    let kesSig := ByteArray.mk (Array.replicate 448 0)  -- Sum-KES depth 6: 448 bytes
    let headerBytes := encodeBlockHeader headerBodyBytes kesSig
    some {
      blockNumber := blockNumber
      slot := slot
      prevHash := prevHash
      headerBytes := headerBytes
      bodyComponents := bodyComponents
      vrfProof := vrfProof
      vrfOutput := vrfOutput
      selectedTxs := blockBody
    }

/-- Full block forging pipeline:
    1. Check leader election
    2. Select transactions from mempool
    3. Build and sign the block -/
def forgeBlock (params : ForgeParams) (consensusState : ConsensusState)
    (blockNumber : Nat) (slot : Nat) (prevHash : ByteArray)
    (mempool : Dion.Network.Mempool.Mempool) (maxBlockSize : Nat)
    : Option ForgedBlock :=
  -- Select transactions
  let blockBody := selectTransactions mempool maxBlockSize
  -- Attempt to forge
  tryForgeBlock params consensusState blockNumber slot prevHash blockBody

-- ==========================
-- = IO Forge (Production)  =
-- ==========================

open Dion.Network.Crypto in
open Dion.Crypto.Sign.KESSign in
/-- Production block forging with real Blake2b body hash and KES signing.
    Returns none if not elected leader, or an error string on failure. -/
def tryForgeBlockIO (params : ForgeParams) (consensusState : ConsensusState)
    (blockNumber : Nat) (slot : Nat) (prevHash : ByteArray)
    (blockBody : BlockBody) (kesPeriod : Nat)
    : IO (Except String (Option ForgedBlock)) := do
  -- Check leader election (pure)
  let poolStake := getPoolStake consensusState params.poolId
  let totalStake := consensusState.stakeSnapshot.totalStake
  match checkLeader params.vrfSecretKey consensusState.epochNonce
    slot consensusState.activeSlotsCoeff poolStake totalStake with
  | .notLeader => return .ok none
  | .invalidPool => return .ok none
  | .isLeader vrfProof vrfOutput =>
    -- KES period range check: opcert.kesPeriod <= currentKesPeriod < opcert.kesPeriod + 62
    let opcertKesPeriod := params.operationalCert.kesPeriod
    if kesPeriod < opcertKesPeriod then
      return .error s!"KES period {kesPeriod} is before opcert start period {opcertKesPeriod}"
    if kesPeriod >= opcertKesPeriod + 62 then
      return .error s!"KES period {kesPeriod} is past opcert max period {opcertKesPeriod + 62}"
    -- Encode the block body components
    let bodyComponents := encodeBlockBody blockBody.transactions
    let bodySerialized := bodyComponents.serialize
    -- Compute body hash via Blake2b-256 FFI
    let bodyHash ← blake2b_256 bodySerialized
    -- Encode header body
    let vrfVKey := ByteArray.mk params.vrfPublicKey.toArray
    let headerBodyBytes := encodeHeaderBody blockNumber slot prevHash
      params.poolId vrfVKey vrfProof vrfOutput
      bodySerialized.size bodyHash
      params.operationalCert params.protocolMajor params.protocolMinor
    -- Sign header body with KES using RELATIVE period (evolutions since opcert start)
    -- The KES key is freshly generated at evolution 0; it must be evolved
    -- (kesPeriod - opcert.kesPeriod) times from its starting state.
    let kesKey := ByteArray.mk params.kesSigningKey.toArray
    let relativeKesPeriod := kesPeriod - opcertKesPeriod
    let kesSigResult ← kesSign kesKey (UInt32.ofNat relativeKesPeriod) headerBodyBytes
    match kesSigResult with
    | .error e => return .error s!"KES signing failed: {e}"
    | .ok kesSig =>
      let headerBytes := encodeBlockHeader headerBodyBytes kesSig
      return .ok (some {
        blockNumber := blockNumber
        slot := slot
        prevHash := prevHash
        headerBytes := headerBytes
        bodyComponents := bodyComponents
        vrfProof := vrfProof
        vrfOutput := vrfOutput
        selectedTxs := blockBody
      })

open Dion.Network.Crypto in
open Dion.Crypto.Sign.KESSign in
/-- Production forge pipeline with IO: leader check → tx selection → sign -/
def forgeBlockIO (params : ForgeParams) (consensusState : ConsensusState)
    (blockNumber : Nat) (slot : Nat) (prevHash : ByteArray)
    (mempool : Dion.Network.Mempool.Mempool) (maxBlockSize : Nat)
    (kesPeriod : Nat)
    : IO (Except String (Option ForgedBlock)) := do
  let blockBody := selectTransactions mempool maxBlockSize
  tryForgeBlockIO params consensusState blockNumber slot prevHash blockBody kesPeriod

end Dion.Consensus.Praos.BlockForge
