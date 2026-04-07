import Std.Data.HashMap
import Dion.Network.CborCursor

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

namespace Dion.Ledger.Governance

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
-- = Param Updates    =
-- ====================

/-- Sparse protocol parameters update (all fields optional).
    Only the fields present in the on-chain governance action are populated.
    Conway CDDL: pparams_update = {* uint => any} -/
structure ProtocolParamsUpdate where
  maxTxExUnits    : Option (Nat × Nat) := none  -- key 17
  maxBlockExUnits : Option (Nat × Nat) := none  -- key 18
  maxTxSize       : Option Nat         := none  -- key 2
  maxBlockSize    : Option Nat         := none  -- key 13 (maxBBSize)
  minFeeA         : Option Nat         := none  -- key 0
  minFeeB         : Option Nat         := none  -- key 1
  stakeKeyDeposit : Option Nat         := none  -- key 5
  poolDeposit     : Option Nat         := none  -- key 6
  maxValueSize    : Option Nat         := none  -- key 22 (maxValSize)
  collateralPct   : Option Nat         := none  -- key 25 (collateralPercentage)
  maxCollateral   : Option Nat         := none  -- key 26 (maxCollateralInputs)
  govActionDeposit: Option Nat         := none  -- key 37
  dRepDeposit     : Option Nat         := none  -- key 38
  deriving Repr

/-- A pending parameter change: the update itself + the epoch it was proposed.
    Enacted at the epoch boundary AFTER ratification (tracked via votes). -/
structure PendingParamChange where
  proposedEpoch : Nat
  update : ProtocolParamsUpdate
  actionId : GovActionId
  /-- True once we've seen enough votes (simplified: set when first SPO votes Yes,
      or after govActionLifetime epochs — full DRep/CC ratification is #TODO). -/
  ratified : Bool := false
  deriving Repr

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
  /-- Pending protocol parameter changes from ParameterChangeAction proposals -/
  pendingParamChanges : List PendingParamChange
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
    pendingParamChanges := []
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

-- ====================
-- = Param Update CBOR=
-- ====================

open Dion.Network.CborCursor in
/-- Parse one key-value entry from pparams_update map, return updated acc and next cursor. -/
private def parsePParamsEntry (kR : CResult Nat) (acc : ProtocolParamsUpdate)
    : Option (ProtocolParamsUpdate × Cursor) :=
  match kR.value with
  | 0  => (decodeUInt kR.cursor).map fun vR => ({ acc with minFeeA := some vR.value }, vR.cursor)
  | 1  => (decodeUInt kR.cursor).map fun vR => ({ acc with minFeeB := some vR.value }, vR.cursor)
  | 2  => (decodeUInt kR.cursor).map fun vR => ({ acc with maxTxSize := some vR.value }, vR.cursor)
  | 5  => (decodeUInt kR.cursor).map fun vR => ({ acc with stakeKeyDeposit := some vR.value }, vR.cursor)
  | 6  => (decodeUInt kR.cursor).map fun vR => ({ acc with poolDeposit := some vR.value }, vR.cursor)
  | 13 => (decodeUInt kR.cursor).map fun vR => ({ acc with maxBlockSize := some vR.value }, vR.cursor)
  | 17 => do  -- maxTxExUnits = [mem, steps]
    let aR ← decodeArrayHeader kR.cursor
    let memR ← decodeUInt aR.cursor
    let stepsR ← decodeUInt memR.cursor
    some ({ acc with maxTxExUnits := some (memR.value, stepsR.value) }, stepsR.cursor)
  | 18 => do  -- maxBlockExUnits = [mem, steps]
    let aR ← decodeArrayHeader kR.cursor
    let memR ← decodeUInt aR.cursor
    let stepsR ← decodeUInt memR.cursor
    some ({ acc with maxBlockExUnits := some (memR.value, stepsR.value) }, stepsR.cursor)
  | 22 => (decodeUInt kR.cursor).map fun vR => ({ acc with maxValueSize := some vR.value }, vR.cursor)
  | 25 => (decodeUInt kR.cursor).map fun vR => ({ acc with collateralPct := some vR.value }, vR.cursor)
  | 26 => (decodeUInt kR.cursor).map fun vR => ({ acc with maxCollateral := some vR.value }, vR.cursor)
  | 37 => (decodeUInt kR.cursor).map fun vR => ({ acc with govActionDeposit := some vR.value }, vR.cursor)
  | 38 => (decodeUInt kR.cursor).map fun vR => ({ acc with dRepDeposit := some vR.value }, vR.cursor)
  | _  => (skipValue kR.cursor).map fun after => (acc, after)

open Dion.Network.CborCursor in
/-- Parse a Conway pparams_update CBOR map {* uint => any} into a ProtocolParamsUpdate.
    Only extracts the fields we care about; unknown keys are skipped. -/
private partial def parsePParamsUpdateMap (cur : Cursor) : ProtocolParamsUpdate :=
  match decodeMapHeader cur with
  | none => {}
  | some mR =>
    (List.range mR.value).foldl (fun (state : ProtocolParamsUpdate × Cursor) _ =>
      let (acc, c) := state
      match decodeUInt c with
      | none => (acc, c)
      | some kR => match parsePParamsEntry kR acc with
        | some (newAcc, next) => (newAcc, next)
        | none => (acc, c)
    ) ({}, mR.cursor) |>.1

open Dion.Network.CborCursor in
/-- Try to parse one proposal_procedure and return (parsed update or none, next cursor). -/
private def parseOneProposal (c : Cursor)
    : Option (Option ProtocolParamsUpdate × Cursor) :=
  -- Each proposal_procedure = [deposit, reward_account, gov_action, anchor]
  match decodeArrayHeader c with
  | none => skipValue c |>.map (none, ·)
  | some arrR =>
    if arrR.value < 4 then skipValue c |>.map (none, ·)
    else
      match decodeUInt arrR.cursor with   -- deposit
      | none => none
      | some depR =>
      match decodeBytes depR.cursor with  -- reward_account
      | none => none
      | some raR =>
      match decodeArrayHeader raR.cursor with  -- gov_action array header
      | none => none
      | some gaR =>
      if gaR.value == 0 then skipValue c |>.map (none, ·)
      else
        match decodeUInt gaR.cursor with  -- action type tag
        | none => none
        | some typeR =>
        if typeR.value != 0 then
          -- Not ParameterChange — skip whole proposal_procedure
          skipValue c |>.map (none, ·)
        else
          -- ParameterChange = [0, prev_action_id/null, pparams_update, policy/null]
          match skipValue typeR.cursor with  -- skip prev_action_id
          | none => none
          | some afterPrev =>
          let update := parsePParamsUpdateMap afterPrev
          match skipValue afterPrev with    -- skip pparams_update map
          | none => none
          | some afterMap =>
          match skipValue afterMap with     -- skip policy_hash
          | none => none
          | some afterGovA =>
          match skipValue afterGovA with    -- skip anchor
          | none => none
          | some afterAnchor => some (some update, afterAnchor)

open Dion.Network.CborCursor in
/-- Parse proposal_procedures raw CBOR, extract all ParameterChangeAction param updates.
    Conway CDDL: proposal_procedures = #{+ proposal_procedure}
    proposal_procedure = [deposit, reward_account, gov_action, anchor]
    gov_action (ParameterChange) = [0, prev_action_id / null, pparams_update, policy_hash / null] -/
def parseProposalParamUpdates (raw : ByteArray) : List ProtocolParamsUpdate :=
  let cur := Cursor.mk' raw
  -- CBOR tag 258 (set) has header 0xd9 0x01 0x02 — skip 3 bytes if present
  let cur := if cur.data.size > 2 && cur.data[cur.pos]! == 0xd9 then
    { cur with pos := cur.pos + 3 } else cur
  match decodeArrayHeader cur with
  | none => []
  | some aR =>
    (List.range aR.value).foldl (fun (state : List ProtocolParamsUpdate × Cursor) _ =>
      let (acc, c) := state
      match parseOneProposal c with
      | some (some u, next) => (u :: acc, next)
      | some (none,   next) => (acc, next)
      | none                => (acc, c)
    ) ([], aR.cursor) |>.1

/-- Add a pending param change from a ParameterChangeAction proposal. -/
def GovernanceState.addPendingParamChange (gs : GovernanceState)
    (actionId : GovActionId) (epoch : Nat) (update : ProtocolParamsUpdate)
    : GovernanceState :=
  let entry : PendingParamChange := { proposedEpoch := epoch, update, actionId }
  { gs with pendingParamChanges := entry :: gs.pendingParamChanges }

/-- At epoch boundary: mark proposals as ratified if they received any SPO Yes vote.
    TODO: implement full DRep + CC ratification per CIP-1694. -/
def GovernanceState.ratifyPendingChanges (gs : GovernanceState) : GovernanceState :=
  let spoYesIds := gs.votes.filterMap fun v =>
    match v.voter, v.vote with
    | .StakePoolOperator _, .Yes => some v.actionId
    | _, _ => none
  let updated := gs.pendingParamChanges.map fun p =>
    if spoYesIds.any (· == p.actionId) then { p with ratified := true } else p
  { gs with pendingParamChanges := updated }

/-- Extract all ratified param updates and remove them from pending list. -/
def GovernanceState.drainRatifiedChanges (gs : GovernanceState)
    : List ProtocolParamsUpdate × GovernanceState :=
  let (ready, waiting) := gs.pendingParamChanges.partition (·.ratified)
  (ready.map (·.update), { gs with pendingParamChanges := waiting })

end Dion.Ledger.Governance
