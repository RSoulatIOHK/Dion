import Cleanode.Ledger.State
import Cleanode.Ledger.Governance

/-!
# Cardano Certificates

Certificates are embedded in transactions to manage stake delegation,
pool registration/retirement, and governance participation.

## Certificate Types (Shelley+)
- StakeKeyRegistration: Register a stake credential
- StakeKeyDeregistration: Deregister a stake credential
- StakeDelegation: Delegate stake to a pool
- PoolRegistration: Register or update a stake pool
- PoolRetirement: Schedule a pool for retirement

## CBOR Format
Certificates appear in transaction body field key 4 as a list of
[certType, ...params] arrays.

## References
- Shelley CDDL: certificate definition
- Cardano Ledger Spec: DELEG, POOL, GOVCERT STS rules
-/

namespace Cleanode.Ledger.Certificate

open Cleanode.Ledger.State
open Cleanode.Ledger.Governance

-- ====================
-- = Certificate Types =
-- ====================

/-- Pool relay information -/
inductive PoolRelay where
  | singleHostAddr (port : Option Nat) (ipv4 : Option ByteArray) (ipv6 : Option ByteArray)
  | singleHostName (port : Option Nat) (hostname : String)
  | multiHostName (hostname : String)

instance : Repr PoolRelay where
  reprPrec
    | .singleHostAddr p _ _, _ => s!"SingleHostAddr(port={repr p})"
    | .singleHostName p h, _ => s!"SingleHostName({h}, port={repr p})"
    | .multiHostName h, _ => s!"MultiHostName({h})"

/-- Pool metadata reference -/
structure PoolMetadataRef where
  url : String                  -- Metadata URL (max 64 bytes)
  metadataHash : ByteArray      -- Blake2b-256 hash of metadata JSON

instance : Repr PoolMetadataRef where
  reprPrec m _ := s!"PoolMetadata(url={m.url})"

/-- Extended pool parameters (superset of PoolParams from State.lean) -/
structure FullPoolParams where
  poolId : ByteArray            -- Pool operator verification key hash (28 bytes)
  vrfKeyHash : ByteArray        -- VRF verification key hash (32 bytes)
  pledge : Nat                  -- Pledged ADA in lovelace
  cost : Nat                    -- Fixed cost per epoch in lovelace
  margin : Nat                  -- Margin numerator (denominator = 1000000)
  rewardAccount : ByteArray     -- Pool reward account address
  owners : List ByteArray       -- Pool owner key hashes
  relays : List PoolRelay       -- Pool relays
  metadata : Option PoolMetadataRef -- Optional metadata

instance : Repr FullPoolParams where
  reprPrec p _ := s!"FullPoolParams(poolId={p.poolId.size}B, pledge={p.pledge}, cost={p.cost})"

/-- Cardano certificate (Shelley + Conway) -/
inductive Certificate where
  -- Shelley (types 0-4)
  | stakeKeyRegistration (keyHash : ByteArray)        -- 0
  | stakeKeyDeregistration (keyHash : ByteArray)      -- 1
  | stakeDelegation (keyHash : ByteArray) (poolId : ByteArray) -- 2
  | poolRegistration (params : FullPoolParams)         -- 3
  | poolRetirement (poolId : ByteArray) (epoch : Nat) -- 4
  -- Conway (types 7-15)
  | conwayRegistration (keyHash : ByteArray) (deposit : Nat)         -- 7
  | conwayDeregistration (keyHash : ByteArray) (refund : Nat)        -- 8
  | voteDelegation (keyHash : ByteArray) (drep : DRepCredential)     -- 9
  | stakeVoteDelegation (keyHash : ByteArray) (poolId : ByteArray)
      (drep : DRepCredential)                                         -- 10
  | stakeRegDelegation (keyHash : ByteArray) (poolId : ByteArray)
      (deposit : Nat)                                                 -- 11
  | voteRegDelegation (keyHash : ByteArray) (drep : DRepCredential)
      (deposit : Nat)                                                 -- 12
  | stakeVoteRegDelegation (keyHash : ByteArray) (poolId : ByteArray)
      (drep : DRepCredential) (deposit : Nat)                         -- 13
  | authCommitteeHot (coldCredHash : ByteArray)
      (hotCredHash : ByteArray)                                       -- 14
  | resignCommitteeCold (coldCredHash : ByteArray)                    -- 15

instance : Repr Certificate where
  reprPrec
    | .stakeKeyRegistration _, _ => "StakeKeyRegistration"
    | .stakeKeyDeregistration _, _ => "StakeKeyDeregistration"
    | .stakeDelegation _ pid, _ => s!"StakeDelegation(pool={pid.size}B)"
    | .poolRegistration p, _ => s!"PoolRegistration({repr p})"
    | .poolRetirement _ e, _ => s!"PoolRetirement(epoch={e})"
    | .conwayRegistration _ d, _ => s!"ConwayRegistration(deposit={d})"
    | .conwayDeregistration _ r, _ => s!"ConwayDeregistration(refund={r})"
    | .voteDelegation _ d, _ => s!"VoteDelegation({repr d})"
    | .stakeVoteDelegation _ _ d, _ => s!"StakeVoteDelegation({repr d})"
    | .stakeRegDelegation _ _ d, _ => s!"StakeRegDelegation(deposit={d})"
    | .voteRegDelegation _ d _, _ => s!"VoteRegDelegation({repr d})"
    | .stakeVoteRegDelegation _ _ d _, _ => s!"StakeVoteRegDelegation({repr d})"
    | .authCommitteeHot _ _, _ => "AuthCommitteeHot"
    | .resignCommitteeCold _, _ => "ResignCommitteeCold"

-- ====================
-- = Certificate      =
-- = Processing       =
-- ====================

/-- Convert FullPoolParams to the simpler PoolParams used in LedgerState -/
def toPoolParams (p : FullPoolParams) : PoolParams :=
  { poolId := p.poolId
    vrfKeyHash := p.vrfKeyHash
    pledge := p.pledge
    cost := p.cost
    margin := p.margin
    rewardAccount := p.rewardAccount
    owners := p.owners
    metadata := p.metadata.map (·.metadataHash) }

/-- Apply a single certificate to the ledger state -/
def applyCertificate (state : LedgerState) (cert : Certificate) : LedgerState :=
  match cert with
  | .stakeKeyRegistration keyHash =>
    { state with delegation := state.delegation.registerStakeKey keyHash }
  | .stakeKeyDeregistration keyHash =>
    { state with delegation := state.delegation.deregisterStakeKey keyHash }
  | .stakeDelegation keyHash poolId =>
    { state with delegation := state.delegation.delegate keyHash poolId }
  | .poolRegistration params =>
    { state with pools := state.pools.register (toPoolParams params) }
  | .poolRetirement poolId epoch =>
    { state with pools := state.pools.retire poolId epoch }
  -- Conway certificates
  | .conwayRegistration keyHash _ =>
    { state with delegation := state.delegation.registerStakeKey keyHash }
  | .conwayDeregistration keyHash _ =>
    { state with delegation := state.delegation.deregisterStakeKey keyHash }
  | .voteDelegation _ _ => state  -- DRep delegation tracked in governance state
  | .stakeVoteDelegation keyHash poolId _ =>
    { state with delegation := state.delegation.delegate keyHash poolId }
  | .stakeRegDelegation keyHash poolId _ =>
    let s := { state with delegation := state.delegation.registerStakeKey keyHash }
    { s with delegation := s.delegation.delegate keyHash poolId }
  | .voteRegDelegation keyHash _ _ =>
    { state with delegation := state.delegation.registerStakeKey keyHash }
  | .stakeVoteRegDelegation keyHash poolId _ _ =>
    let s := { state with delegation := state.delegation.registerStakeKey keyHash }
    { s with delegation := s.delegation.delegate keyHash poolId }
  | .authCommitteeHot _ _ => state  -- Tracked in governance state
  | .resignCommitteeCold _ => state  -- Tracked in governance state

/-- Apply all certificates from a transaction to the ledger state -/
def applyCertificates (state : LedgerState) (certs : List Certificate) : LedgerState :=
  certs.foldl applyCertificate state

-- ====================
-- = Validation       =
-- ====================

/-- Certificate validation errors -/
inductive CertError where
  | stakeKeyAlreadyRegistered
  | stakeKeyNotRegistered
  | poolNotRegistered
  | retirementTooFar (epoch maxEpoch : Nat)
  | ownerNotSigned

instance : Repr CertError where
  reprPrec
    | .stakeKeyAlreadyRegistered, _ => "StakeKeyAlreadyRegistered"
    | .stakeKeyNotRegistered, _ => "StakeKeyNotRegistered"
    | .poolNotRegistered, _ => "PoolNotRegistered"
    | .retirementTooFar e m, _ => s!"RetirementTooFar(epoch={e}, max={m})"
    | .ownerNotSigned, _ => "OwnerNotSigned"

/-- Validate a certificate against current ledger state -/
def validateCertificate (state : LedgerState) (cert : Certificate)
    : Except CertError Unit := do
  match cert with
  | .stakeKeyRegistration keyHash =>
    if state.delegation.registeredStakeKeys.any (· == keyHash) then
      throw .stakeKeyAlreadyRegistered
  | .stakeKeyDeregistration keyHash =>
    if !state.delegation.registeredStakeKeys.any (· == keyHash) then
      throw .stakeKeyNotRegistered
  | .stakeDelegation keyHash _ =>
    if !state.delegation.registeredStakeKeys.any (· == keyHash) then
      throw .stakeKeyNotRegistered
  | .poolRegistration _ => pure ()  -- Pool registration is always valid (creates or updates)
  | .poolRetirement poolId epoch =>
    if !state.pools.registeredPools.any (fun p => p.poolId == poolId) then
      throw .poolNotRegistered
    let maxEpoch := state.protocolParams.epoch + 18  -- eMax from protocol params
    if epoch > maxEpoch then
      throw (.retirementTooFar epoch maxEpoch)
  -- Conway certificates: validate stake key preconditions
  | .conwayRegistration keyHash _ =>
    if state.delegation.registeredStakeKeys.any (· == keyHash) then
      throw .stakeKeyAlreadyRegistered
  | .conwayDeregistration keyHash _ =>
    if !state.delegation.registeredStakeKeys.any (· == keyHash) then
      throw .stakeKeyNotRegistered
  | .voteDelegation keyHash _ =>
    if !state.delegation.registeredStakeKeys.any (· == keyHash) then
      throw .stakeKeyNotRegistered
  | .stakeVoteDelegation keyHash _ _ =>
    if !state.delegation.registeredStakeKeys.any (· == keyHash) then
      throw .stakeKeyNotRegistered
  | .stakeRegDelegation _ _ _ => pure ()  -- Registration + delegation in one
  | .voteRegDelegation _ _ _ => pure ()
  | .stakeVoteRegDelegation _ _ _ _ => pure ()
  | .authCommitteeHot _ _ => pure ()
  | .resignCommitteeCold _ => pure ()

end Cleanode.Ledger.Certificate
