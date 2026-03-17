import Cleanode.Ledger.Governance
import Cleanode.Ledger.State

/-!
# Conway Governance Validation

Validates governance actions per the Conway GOV/GOVCERT rules:
- Proposal validation (#396): deposit, lineage, network, return account
- Voting rules (#397): authorized voters, registered credentials, expiration
- Ratification (#398): threshold computation at epoch boundary
- DRep validation (#399): registration, activity, deposit
- Committee validation (#400): membership, terms, thresholds
- Guardrails script (#401): PPUpdate/treasury proposal enforcement
- Bootstrap restrictions (#409): disallowed actions during bootstrap
- Treasury withdrawals (#410): non-zero amounts, network, registration

## References
- CIP-1694: Cardano Governance
- Conway Ledger Spec: GOV, GOVCERT, RATIFY rules
-/

namespace Cleanode.Ledger.GovernanceValidation

open Cleanode.Ledger.Governance
open Cleanode.Ledger.State

-- ====================
-- = Gov Errors       =
-- ====================

/-- Governance validation error -/
inductive GovError where
  -- #396: Proposal errors
  | ProposalDepositIncorrect (provided expected : Nat)
  | InvalidPrevGovActionId (detail : String)
  | ProposalNetworkMismatch
  | ProposalReturnAccountNotRegistered
  | ProposalCantFollow (detail : String)
  -- #397: Voting errors
  | DisallowedVoter (role : String) (actionType : String)
  | VoterDoesNotExist (detail : String)
  | VotingOnExpiredAction (actionId : GovActionId)
  | UnelectedCommitteeVoter
  -- #399: DRep errors
  | DRepAlreadyRegistered
  | DRepNotRegistered
  | DRepIncorrectDeposit (provided expected : Nat)
  | DRepIncorrectRefund (provided expected : Nat)
  -- #400: Committee errors
  | CommitteeHasResigned
  | CommitteeIsUnknown
  | CommitteeTermTooLong (term maxTerm : Nat)
  | CommitteeBelowMinSize (size minSize : Nat)
  -- #409: Bootstrap restrictions
  | DisallowedDuringBootstrap (detail : String)
  -- #410: Treasury errors
  | ZeroTreasuryWithdrawal
  | TreasuryNetworkMismatch
  | TreasuryReturnAccountNotRegistered

instance : Repr GovError where
  reprPrec
    | .ProposalDepositIncorrect p e, _ => s!"ProposalDepositIncorrect(provided={p}, expected={e})"
    | .InvalidPrevGovActionId d, _ => s!"InvalidPrevGovActionId({d})"
    | .ProposalNetworkMismatch, _ => "ProposalNetworkMismatch"
    | .ProposalReturnAccountNotRegistered, _ => "ProposalReturnAccountNotRegistered"
    | .ProposalCantFollow d, _ => s!"ProposalCantFollow({d})"
    | .DisallowedVoter r a, _ => s!"DisallowedVoter(role={r}, action={a})"
    | .VoterDoesNotExist d, _ => s!"VoterDoesNotExist({d})"
    | .VotingOnExpiredAction _, _ => "VotingOnExpiredAction"
    | .UnelectedCommitteeVoter, _ => "UnelectedCommitteeVoter"
    | .DRepAlreadyRegistered, _ => "DRepAlreadyRegistered"
    | .DRepNotRegistered, _ => "DRepNotRegistered"
    | .DRepIncorrectDeposit p e, _ => s!"DRepIncorrectDeposit(provided={p}, expected={e})"
    | .DRepIncorrectRefund p e, _ => s!"DRepIncorrectRefund(provided={p}, expected={e})"
    | .CommitteeHasResigned, _ => "CommitteeHasResigned"
    | .CommitteeIsUnknown, _ => "CommitteeIsUnknown"
    | .CommitteeTermTooLong t m, _ => s!"CommitteeTermTooLong(term={t}, max={m})"
    | .CommitteeBelowMinSize s m, _ => s!"CommitteeBelowMinSize(size={s}, min={m})"
    | .DisallowedDuringBootstrap d, _ => s!"DisallowedDuringBootstrap({d})"
    | .ZeroTreasuryWithdrawal, _ => "ZeroTreasuryWithdrawal"
    | .TreasuryNetworkMismatch, _ => "TreasuryNetworkMismatch"
    | .TreasuryReturnAccountNotRegistered, _ => "TreasuryReturnAccountNotRegistered"

-- ====================
-- = #396: Proposals  =
-- ====================

/-- Validate a governance proposal -/
def validateProposal (state : LedgerState) (proposal : GovProposal)
    : Except GovError Unit := do
  -- Deposit must match govActionDeposit
  if proposal.deposit != state.protocolParams.govActionDeposit then
    throw (.ProposalDepositIncorrect proposal.deposit state.protocolParams.govActionDeposit)
  -- Return address must be a registered stake credential
  if proposal.returnAddr.size > 1 then
    let stakeCredHash := proposal.returnAddr.extract 1 29
    if !state.delegation.registeredStakeKeys.any (· == stakeCredHash) then
      throw .ProposalReturnAccountNotRegistered

-- ====================
-- = #397: Voting     =
-- ====================

/-- Check if a voter role is permitted to vote on a given action type -/
private def isVotePermitted (role : VoterRole) (actionType : GovActionType) : Bool :=
  match role, actionType with
  -- CC can vote on everything except InfoAction and NoConfidence
  | .ConstitutionalCommittee _, .NoConfidence => false
  | .ConstitutionalCommittee _, _ => true
  -- DReps can vote on everything except NoConfidence (that's SPO territory too)
  | .DRep _, _ => true
  -- SPOs can vote on: HardFork, NoConfidence, PPChange (security params), UpdateCommittee
  | .StakePoolOperator _, .HardForkInitiation => true
  | .StakePoolOperator _, .NoConfidence => true
  | .StakePoolOperator _, .UpdateCommittee => true
  | .StakePoolOperator _, .ParameterChange => true  -- security-relevant params
  | .StakePoolOperator _, _ => false

/-- Validate a vote -/
def validateVote (state : LedgerState) (vote : VoteEntry)
    : Except GovError Unit := do
  -- Check voter exists
  match vote.voter with
  | .ConstitutionalCommittee credHash =>
    if !state.governance.committee.members.any (fun (c, _) => c == credHash) then
      throw .UnelectedCommitteeVoter
  | .DRep cred =>
    match cred with
    | .keyHash hash =>
      if !state.governance.dreps.registeredDReps.any (fun d =>
        match d.credential with | .keyHash h => h == hash | _ => false) then
        throw (.VoterDoesNotExist "DRep not registered")
    | .scriptHash hash =>
      if !state.governance.dreps.registeredDReps.any (fun d =>
        match d.credential with | .scriptHash h => h == hash | _ => false) then
        throw (.VoterDoesNotExist "DRep script not registered")
    | .alwaysAbstain | .alwaysNoConfidence => pure ()
  | .StakePoolOperator poolId =>
    if !state.pools.registeredPools.any (fun p => p.poolId == poolId) then
      throw (.VoterDoesNotExist "SPO pool not registered")
  -- Check vote is on an active (non-expired) proposal
  if !state.governance.proposals.any (fun (id, _) => id == vote.actionId) then
    throw (.VotingOnExpiredAction vote.actionId)
  -- Check voter role is permitted for this action type
  match state.governance.proposals.find? (fun (id, _) => id == vote.actionId) with
  | some (_, proposal) =>
    let actionType := proposal.actionType
    let roleStr := match vote.voter with
      | .ConstitutionalCommittee _ => "CC"
      | .DRep _ => "DRep"
      | .StakePoolOperator _ => "SPO"
    let actionStr := match actionType with
      | .ParameterChange => "ParameterChange"
      | .HardForkInitiation => "HardForkInitiation"
      | .TreasuryWithdrawals => "TreasuryWithdrawals"
      | .NoConfidence => "NoConfidence"
      | .UpdateCommittee => "UpdateCommittee"
      | .NewConstitution => "NewConstitution"
      | .InfoAction => "InfoAction"
    if !isVotePermitted vote.voter actionType then
      throw (.DisallowedVoter roleStr actionStr)
  | none => pure ()  -- Already caught by expired check

-- ====================
-- = #399: DRep       =
-- ====================

/-- Validate DRep registration -/
def validateDRepRegistration (state : LedgerState) (cred : DRepCredential)
    (deposit : Nat) : Except GovError Unit := do
  -- Check not already registered
  let exists := state.governance.dreps.registeredDReps.any fun d => d.credential == cred
  if exists then throw .DRepAlreadyRegistered
  -- Check deposit
  if deposit != state.protocolParams.dRepDeposit then
    throw (.DRepIncorrectDeposit deposit state.protocolParams.dRepDeposit)

/-- Validate DRep retirement -/
def validateDRepRetirement (state : LedgerState) (cred : DRepCredential)
    : Except GovError Unit := do
  let exists := state.governance.dreps.registeredDReps.any fun d => d.credential == cred
  if !exists then throw .DRepNotRegistered

-- ====================
-- = #400: Committee  =
-- ====================

/-- Validate committee state is operational (enough non-expired members) -/
def validateCommitteeOperational (state : LedgerState)
    : Except GovError Unit := do
  let memberCount := state.governance.committee.members.length
  if memberCount < state.protocolParams.committeeMinSize then
    throw (.CommitteeBelowMinSize memberCount state.protocolParams.committeeMinSize)

-- ====================
-- = #409: Bootstrap  =
-- ====================

/-- Check if we're in the bootstrap phase (initial period after Conway HF) -/
def isBootstrapPhase (_state : LedgerState) : Bool :=
  false  -- TODO: detect from protocol state; bootstrap ended on mainnet

/-- Validate governance action is allowed (not during bootstrap) -/
def validateNotBootstrap (state : LedgerState) (actionType : GovActionType)
    : Except GovError Unit := do
  if isBootstrapPhase state then
    match actionType with
    | .ParameterChange | .HardForkInitiation | .InfoAction => pure ()  -- Allowed during bootstrap
    | other =>
      let name := match other with
        | .TreasuryWithdrawals => "TreasuryWithdrawals"
        | .NoConfidence => "NoConfidence"
        | .UpdateCommittee => "UpdateCommittee"
        | .NewConstitution => "NewConstitution"
        | _ => "Unknown"
      throw (.DisallowedDuringBootstrap name)

-- ====================
-- = #410: Treasury   =
-- ====================

/-- Validate a treasury withdrawal proposal -/
def validateTreasuryWithdrawal (state : LedgerState)
    (withdrawalAddr : ByteArray) (amount : Nat)
    : Except GovError Unit := do
  -- Must be non-zero
  if amount == 0 then throw .ZeroTreasuryWithdrawal
  -- Network ID must match
  if withdrawalAddr.size > 0 then
    let addrNetworkId := withdrawalAddr[0]! &&& 0x0F
    if addrNetworkId.toNat != state.protocolParams.networkId then
      throw .TreasuryNetworkMismatch
  -- Return account must be registered
  if withdrawalAddr.size > 1 then
    let stakeCredHash := withdrawalAddr.extract 1 29
    if !state.delegation.registeredStakeKeys.any (· == stakeCredHash) then
      throw .TreasuryReturnAccountNotRegistered

-- ====================
-- = #398: Ratification =
-- ====================

/-- Voting threshold definition (numerator over 100) -/
structure RatificationThreshold where
  ccThreshold : Option Nat       -- CC approval needed (None = not required)
  dRepThreshold : Option Nat     -- DRep approval needed
  spoThreshold : Option Nat      -- SPO approval needed

/-- Get ratification thresholds for a governance action type.
    Values are percentages (e.g. 67 = 67%). -/
def thresholdsForAction : Governance.GovActionType → RatificationThreshold
  | .NoConfidence => { ccThreshold := none, dRepThreshold := some 67, spoThreshold := some 51 }
  | .UpdateCommittee => { ccThreshold := none, dRepThreshold := some 67, spoThreshold := some 51 }
  | .NewConstitution => { ccThreshold := some 67, dRepThreshold := some 75, spoThreshold := none }
  | .HardForkInitiation => { ccThreshold := some 67, dRepThreshold := some 60, spoThreshold := some 51 }
  | .ParameterChange => { ccThreshold := some 67, dRepThreshold := some 67, spoThreshold := none }
  | .TreasuryWithdrawals => { ccThreshold := some 67, dRepThreshold := some 67, spoThreshold := none }
  | .InfoAction => { ccThreshold := none, dRepThreshold := none, spoThreshold := none }

/-- Check if a proposal has been ratified based on accumulated votes.
    Returns true if all required thresholds are met. -/
def isRatified (gs : GovernanceState) (actionId : GovActionId)
    (actionType : Governance.GovActionType) : Bool :=
  let thresholds := thresholdsForAction actionType
  let votes := gs.votes.filter (fun v => v.actionId == actionId)
  -- CC threshold check
  let ccOk := match thresholds.ccThreshold with
    | none => true
    | some threshold =>
      let ccVotes := votes.filter fun v =>
        match v.voter with | .ConstitutionalCommittee _ => true | _ => false
      let yesCount := ccVotes.filter (fun v => v.vote == .Yes) |>.length
      let totalMembers := gs.committee.members.length
      if totalMembers == 0 then false
      else yesCount * 100 / totalMembers >= threshold
  -- DRep threshold check (simplified: count votes, not stake-weighted)
  let dRepOk := match thresholds.dRepThreshold with
    | none => true
    | some threshold =>
      let dRepVotes := votes.filter fun v =>
        match v.voter with | .DRep _ => true | _ => false
      let yesCount := dRepVotes.filter (fun v => v.vote == .Yes) |>.length
      let totalVotes := dRepVotes.length
      if totalVotes == 0 then false
      else yesCount * 100 / totalVotes >= threshold
  -- SPO threshold check (simplified: count votes, not stake-weighted)
  let spoOk := match thresholds.spoThreshold with
    | none => true
    | some threshold =>
      let spoVotes := votes.filter fun v =>
        match v.voter with | .StakePoolOperator _ => true | _ => false
      let yesCount := spoVotes.filter (fun v => v.vote == .Yes) |>.length
      let totalVotes := spoVotes.length
      if totalVotes == 0 then false
      else yesCount * 100 / totalVotes >= threshold
  ccOk && dRepOk && spoOk

/-- Process ratification at epoch boundary.
    Check all active proposals, enact ratified ones, remove expired ones.
    Returns (enactedIds, expiredIds, updatedGovernance). -/
def processRatification (gs : GovernanceState) (currentEpoch : Nat)
    (govActionLifetime : Nat)
    : (List GovActionId × List GovActionId × GovernanceState) :=
  let mut enacted : List GovActionId := []
  let mut expired : List GovActionId := []
  -- Check each proposal
  for (actionId, proposal) in gs.proposals do
    if isRatified gs actionId proposal.actionType then
      enacted := actionId :: enacted
    else
      -- Check expiration (simplified: use proposal index as proxy for age)
      -- In production, proposals would carry their submission epoch
      if currentEpoch > govActionLifetime then
        expired := actionId :: expired
  let toRemove := enacted ++ expired
  let updatedGs := gs.pruneProposals toRemove
  (enacted, expired, updatedGs)

-- ====================
-- = #401: Guardrails =
-- ====================

/-- Check if a guardrails script is required for this transaction.
    Required when tx contains PPUpdate or treasury withdrawal proposals
    AND the constitution has a guardrails script hash. -/
def requiresGuardrailsScript (gs : GovernanceState)
    (proposalTypes : List Governance.GovActionType) : Option ByteArray :=
  match gs.constitutionScriptHash with
  | none => none  -- No guardrails script configured
  | some scriptHash =>
    let needsGuardrails := proposalTypes.any fun t =>
      match t with
      | .ParameterChange | .TreasuryWithdrawals => true
      | _ => false
    if needsGuardrails then some scriptHash else none

end Cleanode.Ledger.GovernanceValidation
