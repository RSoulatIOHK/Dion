import Cleanode.Consensus.Praos.ConsensusState
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Consensus.Praos.TxSelection
import Cleanode.Crypto.Sign.KES
import Cleanode.Crypto.VRF.ECVRF
import Cleanode.Network.Cbor

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

namespace Cleanode.Consensus.Praos.BlockForge

open Cleanode.Consensus.Praos.ConsensusState
open Cleanode.Consensus.Praos.LeaderElection
open Cleanode.Consensus.Praos.TxSelection
open Cleanode.Crypto.Sign.KES
open Cleanode.Crypto.VRF.ECVRF

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

/-- A forged block ready for network propagation -/
structure ForgedBlock where
  blockNumber : Nat
  slot : Nat
  prevHash : ByteArray
  headerBytes : ByteArray       -- CBOR-encoded header (for hashing and sending)
  bodyBytes : ByteArray         -- CBOR-encoded block body
  vrfProof : VRFProof
  vrfOutput : List UInt8
  selectedTxs : BlockBody

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
    -- We're the leader! Build the block.
    some {
      blockNumber := blockNumber
      slot := slot
      prevHash := prevHash
      headerBytes := ByteArray.mk #[]  -- TODO: CBOR encode header
      bodyBytes := ByteArray.mk #[]    -- TODO: CBOR encode body
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
    (mempool : Cleanode.Network.Mempool.Mempool) (maxBlockSize : Nat)
    : Option ForgedBlock :=
  -- Select transactions
  let blockBody := selectTransactions mempool maxBlockSize
  -- Attempt to forge
  tryForgeBlock params consensusState blockNumber slot prevHash blockBody

end Cleanode.Consensus.Praos.BlockForge
