import Cleanode.Config.Genesis
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Crypto.Hash.Sha512

/-!
# Consensus State

Tracks the evolving consensus state needed for Ouroboros Praos:
- Epoch nonce (accumulated from VRF outputs for randomness)
- Stake distribution snapshots (from 2 epochs ago, per Praos)
- Current KES period
- Operational certificate tracking

## References
- Ouroboros Praos specification, Section 4
-/

namespace Cleanode.Consensus.Praos.ConsensusState

open Cleanode.Config.Genesis
open Cleanode.Consensus.Praos.LeaderElection
open Cleanode.Crypto.Hash.Sha512

-- ====================
-- = Operational Cert =
-- ====================

/-- Operational certificate: links a cold key to a hot (KES) key -/
structure OperationalCert where
  hotVKey : ByteArray           -- KES verification key (32 bytes)
  sequenceNumber : Nat          -- Monotonically increasing counter
  kesPeriod : Nat               -- KES period when this cert was issued
  coldKeySignature : ByteArray  -- Ed25519 signature by the cold key

instance : Repr OperationalCert where
  reprPrec c _ := s!"OperationalCert(seq={c.sequenceNumber}, kesPeriod={c.kesPeriod})"

-- ====================
-- = Consensus State  =
-- ====================

/-- Full consensus state -/
structure ConsensusState where
  /-- Current epoch nonce (32 bytes), used as VRF input -/
  epochNonce : ByteArray
  /-- Nonce from the previous epoch (for epoch transition) -/
  prevEpochNonce : ByteArray
  /-- Evolving nonce: accumulated from block VRF outputs within current epoch -/
  evolvingNonce : ByteArray
  /-- Stake snapshot from 2 epochs ago (used for current leader election) -/
  stakeSnapshot : StakeSnapshot
  /-- Current epoch number -/
  currentEpoch : Nat
  /-- First slot of the current epoch -/
  epochFirstSlot : Nat
  /-- Active slots coefficient -/
  activeSlotsCoeff : Rational
  /-- Security parameter k -/
  securityParam : Nat
  /-- Slots per epoch -/
  epochLength : Nat

instance : Repr ConsensusState where
  reprPrec s _ := s!"ConsensusState(epoch={s.currentEpoch}, pools={s.stakeSnapshot.poolStakes.length})"

/-- Create initial consensus state -/
def ConsensusState.initial (genesis : ShelleyGenesis) : ConsensusState :=
  { epochNonce := ByteArray.mk #[0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 0]
    prevEpochNonce := ByteArray.mk #[]
    evolvingNonce := ByteArray.mk #[]
    stakeSnapshot := { poolStakes := [], totalStake := 0 }
    currentEpoch := 0
    epochFirstSlot := 0
    activeSlotsCoeff := genesis.activeSlotsCoeff
    securityParam := genesis.securityParam
    epochLength := genesis.epochLength }

/-- Compute the epoch for a given slot -/
def slotToEpoch (state : ConsensusState) (slot : Nat) : Nat :=
  slot / state.epochLength

/-- Compute the KES period for a given slot.
    KES period = slot / slotsPerKESPeriod (typically 129600 slots = 36 hours) -/
def slotToKESPeriod (slot : Nat) (slotsPerKESPeriod : Nat := 129600) : Nat :=
  slot / slotsPerKESPeriod

/-- Check if we've crossed an epoch boundary and need to update state -/
def needsEpochTransition (state : ConsensusState) (slot : Nat) : Bool :=
  slotToEpoch state slot > state.currentEpoch

/-- Pure SHA-512-based hash for nonce evolution.
    evolvingNonce = SHA-512(evolvingNonce || vrfOutput) truncated to 32 bytes.
    In production this should use Blake2b-256 via FFI; this pure version
    ensures determinism without IO. -/
private def hashNonce (a b : ByteArray) : ByteArray :=
  let input := a.toList ++ b.toList
  let hashOutput := Internal.hashMessage input
  let hashBytes := hashOutput.flatMap Cleanode.Crypto.Integer.UInt64.toUInt8BE
  ByteArray.mk (hashBytes.take 32).toArray

/-- Process epoch transition: rotate nonces and snapshots.
    New epoch nonce = hash(prevEpochNonce || evolvingNonce) per Praos spec. -/
def processEpochTransition (state : ConsensusState) (newEpoch : Nat)
    (newSnapshot : StakeSnapshot) : ConsensusState :=
  let newEpochNonce :=
    if state.evolvingNonce.size > 0 then
      hashNonce state.epochNonce state.evolvingNonce
    else state.epochNonce
  { state with
    prevEpochNonce := state.epochNonce
    epochNonce := newEpochNonce
    evolvingNonce := ByteArray.mk #[]
    stakeSnapshot := newSnapshot
    currentEpoch := newEpoch
    epochFirstSlot := newEpoch * state.epochLength }

/-- Update the evolving nonce with a new block's VRF output.
    evolvingNonce' = hash(evolvingNonce || vrfOutput) -/
def updateEvolvingNonce (state : ConsensusState) (vrfOutput : ByteArray) : ConsensusState :=
  let newNonce := if state.evolvingNonce.size == 0 then
    hashNonce (ByteArray.mk #[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) vrfOutput
  else
    hashNonce state.evolvingNonce vrfOutput
  { state with evolvingNonce := newNonce }

/-- Look up a pool's stake in the current snapshot -/
def getPoolStake (state : ConsensusState) (poolId : ByteArray) : Nat :=
  match state.stakeSnapshot.poolStakes.find? (fun (pid, _) => pid == poolId) with
  | some (_, stake) => stake
  | none => 0

end Cleanode.Consensus.Praos.ConsensusState
