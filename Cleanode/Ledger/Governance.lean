import Std.Data.HashMap

/-!
# Conway Governance State

Tracks the governance state introduced in the Conway era:
- DRep (Delegated Representative) registrations and delegations
- Governance proposals and their lifecycle
- Constitutional Committee membership
- Treasury and donation tracking

## Governance Actions
Conway introduces on-chain governance with these action types:
1. ParameterChange — modify protocol parameters
2. HardForkInitiation — trigger a hard fork
3. TreasuryWithdrawals — withdraw from treasury
4. NoConfidence — motion of no confidence in committee
5. UpdateCommittee — add/remove committee members
6. NewConstitution — propose new constitution
7. InfoAction — informational (no on-chain effect)

## References
- CIP-1694: Cardano Governance
- Conway Ledger Spec: GOV, GOVCERT rules
-/

namespace Cleanode.Ledger.Governance

-- ====================
-- = DRep Credential  =
-- ====================

/-- DRep credential for Conway governance -/
inductive DRepCredential where
  | keyHash (hash : ByteArray)
  | scriptHash (hash : ByteArray)
  | alwaysAbstain
  | alwaysNoConfidence

instance : Repr DRepCredential where
  reprPrec
    | .keyHash _, _ => "DRep(keyHash)"
    | .scriptHash _, _ => "DRep(scriptHash)"
    | .alwaysAbstain, _ => "DRep(alwaysAbstain)"
    | .alwaysNoConfidence, _ => "DRep(alwaysNoConfidence)"

-- ====================
-- = DRep State       =
-- ====================

/-- DRep registration entry -/
structure DRepEntry where
  credential : DRepCredential
  deposit : Nat
  anchor : Option ByteArray  -- Optional metadata anchor hash

instance : Repr DRepEntry where
  reprPrec e _ := s!"DRepEntry({repr e.credential}, deposit={e.deposit})"

/-- DRep state: all registered DReps and vote delegations -/
structure DRepState where
  /-- Registered DReps -/
  registeredDReps : List DRepEntry
  /-- Vote delegations: stakeKeyHash → DRepCredential -/
  voteDelegations : List (ByteArray × DRepCredential)

instance : Repr DRepState where
  reprPrec s _ := s!"DRepState(dreps={s.registeredDReps.length}, delegations={s.voteDelegations.length})"

def DRepState.empty : DRepState :=
  { registeredDReps := [], voteDelegations := [] }

/-- Delegate voting power to a DRep -/
def DRepState.delegateVote (s : DRepState) (stakeKeyHash : ByteArray)
    (drep : DRepCredential) : DRepState :=
  let filtered := s.voteDelegations.filter (fun (h, _) => h != stakeKeyHash)
  { s with voteDelegations := (stakeKeyHash, drep) :: filtered }

-- ====================
-- = Governance Actions =
-- ====================

/-- Type of governance action -/
inductive GovActionType where
  | ParameterChange
  | HardForkInitiation
  | TreasuryWithdrawals
  | NoConfidence
  | UpdateCommittee
  | NewConstitution
  | InfoAction
  deriving BEq, Repr

/-- A governance proposal -/
structure GovProposal where
  actionType : GovActionType
  deposit : Nat
  returnAddr : ByteArray        -- Deposit return address
  anchor : Option ByteArray     -- Metadata anchor hash
  rawCbor : ByteArray           -- Raw CBOR of the proposal action

instance : Repr GovProposal where
  reprPrec p _ := s!"GovProposal({repr p.actionType}, deposit={p.deposit})"

/-- A governance action ID (tx hash + index within tx) -/
structure GovActionId where
  txHash : ByteArray
  index : Nat

instance : BEq GovActionId where
  beq a b := a.txHash == b.txHash && a.index == b.index

instance : Repr GovActionId where
  reprPrec g _ := s!"GovActionId(idx={g.index})"

instance : Hashable GovActionId where
  hash g :=
    let h := if g.txHash.size >= 8 then
      g.txHash[0]!.toUInt64 <<< 56 ||| g.txHash[1]!.toUInt64 <<< 48 |||
      g.txHash[2]!.toUInt64 <<< 40 ||| g.txHash[3]!.toUInt64 <<< 32
    else 0
    h ^^^ UInt64.ofNat g.index

-- ====================
-- = Committee        =
-- ====================

/-- Constitutional Committee state -/
structure CommitteeState where
  /-- Active committee members: coldCredHash → hotCredHash mapping -/
  members : List (ByteArray × ByteArray)
  /-- Quorum threshold (numerator; denominator = 100) -/
  quorum : Nat

instance : Repr CommitteeState where
  reprPrec s _ := s!"CommitteeState(members={s.members.length}, quorum={s.quorum})"

def CommitteeState.empty : CommitteeState :=
  { members := [], quorum := 67 }  -- Default 2/3 quorum

/-- Authorize a hot credential for a committee member -/
def CommitteeState.authorize (s : CommitteeState) (coldCred hotCred : ByteArray) : CommitteeState :=
  let filtered := s.members.filter (fun (c, _) => c != coldCred)
  { s with members := (coldCred, hotCred) :: filtered }

/-- Resign a committee member -/
def CommitteeState.resign (s : CommitteeState) (coldCred : ByteArray) : CommitteeState :=
  { s with members := s.members.filter (fun (c, _) => c != coldCred) }

-- ====================
-- = Voting           =
-- ====================

/-- Vote choice -/
inductive Vote where
  | Yes | No | Abstain
  deriving BEq, Repr

/-- Who is voting -/
inductive VoterRole where
  | ConstitutionalCommittee (credHash : ByteArray)
  | DRep (cred : DRepCredential)
  | StakePoolOperator (poolId : ByteArray)

instance : Repr VoterRole where
  reprPrec
    | .ConstitutionalCommittee _, _ => "CommitteeMember"
    | .DRep d, _ => s!"DRep({repr d})"
    | .StakePoolOperator _, _ => "SPO"

/-- A single vote on a governance action -/
structure VoteEntry where
  voter : VoterRole
  actionId : GovActionId
  vote : Vote
  anchor : Option ByteArray

instance : Repr VoteEntry where
  reprPrec v _ := s!"VoteEntry({repr v.voter}, {repr v.vote})"

-- ====================
-- = Full Gov State   =
-- ====================

/-- Complete governance state -/
structure GovernanceState where
  dreps : DRepState
  committee : CommitteeState
  /-- Active proposals awaiting votes -/
  proposals : List (GovActionId × GovProposal)
  /-- Accumulated votes on active proposals -/
  votes : List VoteEntry
  /-- Constitution hash (Blake2b-256 of the constitution document) -/
  constitutionHash : ByteArray
  /-- Constitution script hash (optional guardrail script) -/
  constitutionScriptHash : Option ByteArray

instance : Repr GovernanceState where
  reprPrec s _ := s!"GovernanceState(dreps={s.dreps.registeredDReps.length}, proposals={s.proposals.length}, committee={s.committee.members.length})"

def GovernanceState.empty : GovernanceState :=
  { dreps := DRepState.empty
    committee := CommitteeState.empty
    proposals := []
    votes := []
    constitutionHash := ByteArray.mk (Array.replicate 32 0)
    constitutionScriptHash := none }

-- ====================
-- = State Updates    =
-- ====================

/-- Delegate voting power to a DRep (called from certificate processing) -/
def GovernanceState.delegateVote (gs : GovernanceState) (keyHash : ByteArray)
    (drep : DRepCredential) : GovernanceState :=
  { gs with dreps := gs.dreps.delegateVote keyHash drep }

/-- Authorize committee hot key (called from certificate processing) -/
def GovernanceState.authorizeCommittee (gs : GovernanceState)
    (coldCred hotCred : ByteArray) : GovernanceState :=
  { gs with committee := gs.committee.authorize coldCred hotCred }

/-- Resign committee member (called from certificate processing) -/
def GovernanceState.resignCommittee (gs : GovernanceState)
    (coldCred : ByteArray) : GovernanceState :=
  { gs with committee := gs.committee.resign coldCred }

/-- Record a vote -/
def GovernanceState.addVote (gs : GovernanceState) (entry : VoteEntry) : GovernanceState :=
  { gs with votes := entry :: gs.votes }

/-- Submit a new governance proposal -/
def GovernanceState.addProposal (gs : GovernanceState) (id : GovActionId)
    (proposal : GovProposal) : GovernanceState :=
  { gs with proposals := (id, proposal) :: gs.proposals }

/-- Remove enacted or expired proposals at epoch boundary -/
def GovernanceState.pruneProposals (gs : GovernanceState)
    (toRemove : List GovActionId) : GovernanceState :=
  { gs with
    proposals := gs.proposals.filter (fun (id, _) => !toRemove.any (· == id))
    votes := gs.votes.filter (fun v => !toRemove.any (· == v.actionId)) }

end Cleanode.Ledger.Governance
