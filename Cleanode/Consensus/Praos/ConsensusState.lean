import Cleanode.Config.Genesis
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Crypto.Hash.Sha512
import Cleanode.Network.Crypto

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

/-- Hash two byte arrays using Blake2b-256 for nonce evolution.
    Per Praos spec, all nonce hashing uses Blake2b-256. -/
private def hashNonceIO (a b : ByteArray) : IO ByteArray :=
  Cleanode.Network.Crypto.blake2b_256 (a ++ b)

/-- Pure SHA-512 fallback for nonce hashing (used only when IO is unavailable). -/
private def hashNoncePure (a b : ByteArray) : ByteArray :=
  let input := a.toList ++ b.toList
  let hashOutput := Internal.hashMessage input
  let hashBytes := hashOutput.flatMap Cleanode.Crypto.Integer.UInt64.toUInt8BE
  ByteArray.mk (hashBytes.take 32).toArray

/-- Process epoch transition: rotate nonces and snapshots.
    New epoch nonce = Blake2b-256(prevEpochNonce || evolvingNonce) per Praos spec. -/
def processEpochTransitionIO (state : ConsensusState) (newEpoch : Nat)
    (newSnapshot : StakeSnapshot) : IO ConsensusState := do
  let newEpochNonce ←
    if state.evolvingNonce.size > 0 then
      hashNonceIO state.epochNonce state.evolvingNonce
    else pure state.epochNonce
  return { state with
    prevEpochNonce := state.epochNonce
    epochNonce := newEpochNonce
    evolvingNonce := ByteArray.mk #[]
    stakeSnapshot := newSnapshot
    currentEpoch := newEpoch
    epochFirstSlot := newEpoch * state.epochLength }

/-- Pure version for backward compatibility (uses SHA-512 fallback) -/
def processEpochTransition (state : ConsensusState) (newEpoch : Nat)
    (newSnapshot : StakeSnapshot) : ConsensusState :=
  let newEpochNonce :=
    if state.evolvingNonce.size > 0 then
      hashNoncePure state.epochNonce state.evolvingNonce
    else state.epochNonce
  { state with
    prevEpochNonce := state.epochNonce
    epochNonce := newEpochNonce
    evolvingNonce := ByteArray.mk #[]
    stakeSnapshot := newSnapshot
    currentEpoch := newEpoch
    epochFirstSlot := newEpoch * state.epochLength }

/-- Update the evolving nonce with a new block's VRF output using Blake2b-256.
    evolvingNonce' = Blake2b-256(evolvingNonce || vrfOutput) -/
def updateEvolvingNonceIO (state : ConsensusState) (vrfOutput : ByteArray) : IO ConsensusState := do
  let zeroNonce := ByteArray.mk #[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  let base := if state.evolvingNonce.size == 0 then zeroNonce else state.evolvingNonce
  let newNonce ← hashNonceIO base vrfOutput
  return { state with evolvingNonce := newNonce }

/-- Pure version for backward compatibility -/
def updateEvolvingNonce (state : ConsensusState) (vrfOutput : ByteArray) : ConsensusState :=
  let newNonce := if state.evolvingNonce.size == 0 then
    hashNoncePure (ByteArray.mk #[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) vrfOutput
  else
    hashNoncePure state.evolvingNonce vrfOutput
  { state with evolvingNonce := newNonce }

/-- Look up a pool's stake in the current snapshot -/
def getPoolStake (state : ConsensusState) (poolId : ByteArray) : Nat :=
  match state.stakeSnapshot.poolStakes.find? (fun (pid, _) => pid == poolId) with
  | some (_, stake) => stake
  | none => 0

-- ====================
-- = OpCert Counters  =
-- ====================

/-- #402: Track last seen OpCert counter per pool for monotonicity enforcement -/
structure OpCertCounterMap where
  /-- poolId (28 bytes) → last seen sequence number -/
  counters : List (ByteArray × Nat) := []

/-- Check and update OpCert counter for a pool.
    Pre-Vasil: counter must be >= last seen.
    Post-Vasil: counter must be exactly last seen + 1.
    Returns (isValid, updatedMap). -/
def OpCertCounterMap.checkAndUpdate (m : OpCertCounterMap) (poolId : ByteArray)
    (counter : Nat) (strictMode : Bool := true) : (Bool × OpCertCounterMap) :=
  match m.counters.find? (fun (pid, _) => pid == poolId) with
  | some (_, lastSeen) =>
    let valid := if strictMode then
      counter == lastSeen + 1 || counter == lastSeen  -- same block re-validation
    else
      counter >= lastSeen
    let updated := m.counters.map fun (pid, c) =>
      if pid == poolId then (pid, max c counter) else (pid, c)
    (valid, { counters := updated })
  | none =>
    -- First time seeing this pool's OpCert
    (true, { counters := (poolId, counter) :: m.counters })

-- ====================
-- = Dual VRF (#403)  =
-- ====================

/-- #403: Verify both VRF proofs from a block header.
    Each block must contain two VRF proofs:
    1. certVRF (leader eligibility): input = epochNonce || 0x4C || slot
    2. nonceVRF (nonce contribution): input = epochNonce || 0x4E || slot -/
structure DualVRFResult where
  certVRFValid : Bool       -- Leader eligibility proof verified
  nonceVRFValid : Bool      -- Nonce contribution proof verified
  nonceVRFOutput : ByteArray -- Output used for evolving nonce

/-- Verify both VRF proofs from a block header -/
def verifyDualVRF (vrfPublicKey : List UInt8) (epochNonce : ByteArray)
    (slot : Nat) (certProof nonceProof : Cleanode.Crypto.VRF.ECVRF.VRFProof)
    (activeSlotsCoeff : Rational) (poolStake totalStake : Nat) : DualVRFResult :=
  -- Verify certVRF (leader election, domain sep 0x4C)
  let certInput := vrfInput epochNonce slot 0x4C
  let certValid := Cleanode.Crypto.VRF.ECVRF.verify vrfPublicKey certInput certProof
  let certOutput := Cleanode.Crypto.VRF.ECVRF.proofToHash certProof
  let certThresholdOk := if certValid then
    let y := vrfOutputToNat certOutput
    let threshold := computeThresholdAccurate activeSlotsCoeff poolStake totalStake
    y < threshold
  else false
  -- Verify nonceVRF (nonce contribution, domain sep 0x4E)
  let nonceInput := vrfInputNonce epochNonce slot
  let nonceValid := Cleanode.Crypto.VRF.ECVRF.verify vrfPublicKey nonceInput nonceProof
  let nonceOutput := Cleanode.Crypto.VRF.ECVRF.proofToHash nonceProof
  { certVRFValid := certValid && certThresholdOk
    nonceVRFValid := nonceValid
    nonceVRFOutput := ByteArray.mk nonceOutput.toArray }

/-- #404: Update evolving nonce from a validated block's nonceVRF output.
    Only blocks in the first 4/5 of the epoch (stability window) contribute. -/
def updateEvolvingNonceFromBlock (state : ConsensusState) (slot : Nat)
    (nonceVRFOutput : ByteArray) : IO ConsensusState := do
  -- Only accumulate nonces from the stability window (first 4k/f slots)
  let stabilityWindow := state.epochLength * 4 / 5
  let slotInEpoch := slot - state.epochFirstSlot
  if slotInEpoch < stabilityWindow then
    updateEvolvingNonceIO state nonceVRFOutput
  else
    pure state  -- Past stability window, don't contribute to nonce

-- ====================
-- = Persistence      =
-- ====================

/-- Hex encoding helper -/
private def bytesToHex (bs : ByteArray) : String :=
  let hexChars := "0123456789abcdef"
  bs.foldl (init := "") fun acc b =>
    acc ++ ⟨[hexChars.get ⟨b.toNat / 16⟩, hexChars.get ⟨b.toNat % 16⟩]⟩

/-- Hex decoding helper -/
private def hexToBytes (s : String) : Option ByteArray := do
  let chars := s.toList
  if chars.length % 2 != 0 then none
  let mut result := ByteArray.emptyWithCapacity (chars.length / 2)
  let mut i := 0
  while h : i + 1 < chars.length do
    let hi ← hexNibble chars[i]
    let lo ← hexNibble chars[i + 1]
    result := result.push (hi * 16 + lo)
    i := i + 2
  some result
where
  hexNibble (c : Char) : Option UInt8 :=
    if c >= '0' && c <= '9' then some (c.toNat - '0'.toNat).toUInt8
    else if c >= 'a' && c <= 'f' then some (c.toNat - 'a'.toNat + 10).toUInt8
    else if c >= 'A' && c <= 'F' then some (c.toNat - 'A'.toNat + 10).toUInt8
    else none

/-- Save consensus nonce state to a file for persistence across restarts.
    Format: epoch epochNonce(hex) evolvingNonce(hex) -/
def saveConsensusState (state : ConsensusState) (path : String := "data/consensus.state") : IO Unit := do
  let content := s!"{state.currentEpoch} {bytesToHex state.epochNonce} {bytesToHex state.evolvingNonce}"
  IO.FS.writeFile path content

/-- Load consensus nonce state from file. Returns (epoch, epochNonce, evolvingNonce). -/
def loadConsensusState (path : String := "data/consensus.state") : IO (Option (Nat × ByteArray × ByteArray)) := do
  try
    let content ← IO.FS.readFile path
    let parts := content.trim.splitOn " "
    match parts with
    | [epochStr, nonceHex, evolvingHex] => do
      let epoch ← match epochStr.toNat? with
        | some n => pure n
        | none => return none
      let nonce ← match hexToBytes nonceHex with
        | some bs => pure bs
        | none => return none
      let evolving ← match hexToBytes evolvingHex with
        | some bs => pure bs
        | none => return none
      return some (epoch, nonce, evolving)
    | _ => return none
  catch _ => return none

end Cleanode.Consensus.Praos.ConsensusState
