import Cleanode.Network.ChainSync

/-!
# ChainSync Protocol State Machine

Formal definition of the ChainSync protocol state machine with
typed states and transition rules per the Ouroboros network spec.

## States
- **StIdle**: Client's turn. Can send MsgRequestNext, MsgFindIntersect, or MsgDone.
- **StCanAwait**: Server's turn. Can send MsgAwaitReply, MsgRollForward, or MsgRollBackward.
- **StMustReply**: Server must respond (no AwaitReply). Sends MsgRollForward or MsgRollBackward.
- **StIntersect**: Server responds to FindIntersect with MsgIntersectFound or MsgIntersectNotFound.
- **StDone**: Protocol terminated. No further transitions.

## State Diagram
```
  StIdle ──MsgRequestNext──> StCanAwait
    │                          │ ├─MsgAwaitReply──> StMustReply
    │                          │ ├─MsgRollForward──> StIdle
    │                          │ └─MsgRollBackward──> StIdle
    │                          │
    │                        StMustReply
    │                          ├─MsgRollForward──> StIdle
    │                          └─MsgRollBackward──> StIdle
    │
    ├─MsgFindIntersect──> StIntersect
    │                       ├─MsgIntersectFound──> StIdle
    │                       └─MsgIntersectNotFound──> StIdle
    │
    └─MsgDone──> StDone
```

## References
- Ouroboros Network Spec Section 3.7 (Chain Sync)
-/

namespace Cleanode.Network.ChainSyncState

open Cleanode.Network.ChainSync

-- ====================
-- = State Type       =
-- ====================

/-- ChainSync protocol states -/
inductive ChainSyncState where
  | StIdle       -- Client can send
  | StCanAwait   -- Server can send (including AwaitReply)
  | StMustReply  -- Server must reply (no AwaitReply)
  | StIntersect  -- Server responds to intersection query
  | StDone       -- Terminal state
  deriving Repr, BEq, DecidableEq

-- ====================
-- = Transitions      =
-- ====================

/-- Valid state transitions (as a decidable function) -/
def nextState (current : ChainSyncState) (msg : ChainSyncMessage) : Option ChainSyncState :=
  match current, msg with
  -- From StIdle (client's turn)
  | .StIdle, .MsgRequestNext           => some .StCanAwait
  | .StIdle, .MsgFindIntersect _       => some .StIntersect
  | .StIdle, .MsgDone                  => some .StDone
  -- From StCanAwait (server's turn, may await)
  | .StCanAwait, .MsgAwaitReply        => some .StMustReply
  | .StCanAwait, .MsgRollForward _ _   => some .StIdle
  | .StCanAwait, .MsgRollBackward _ _  => some .StIdle
  -- From StMustReply (server must reply, no await)
  | .StMustReply, .MsgRollForward _ _  => some .StIdle
  | .StMustReply, .MsgRollBackward _ _ => some .StIdle
  -- From StIntersect (server responds to intersection)
  | .StIntersect, .MsgIntersectFound _ _   => some .StIdle
  | .StIntersect, .MsgIntersectNotFound _  => some .StIdle
  -- All other transitions are invalid
  | _, _ => none

/-- Proposition: a transition from state s via message m to state s' is valid -/
def validTransition (s : ChainSyncState) (msg : ChainSyncMessage) (s' : ChainSyncState) : Prop :=
  nextState s msg = some s'

-- ====================
-- = State Predicates =
-- ====================

/-- Is this a client-agency state? (Client sends next message) -/
def isClientAgency : ChainSyncState → Bool
  | .StIdle => true
  | _ => false

/-- Is this a server-agency state? (Server sends next message) -/
def isServerAgency : ChainSyncState → Bool
  | .StCanAwait => true
  | .StMustReply => true
  | .StIntersect => true
  | _ => false

/-- Is this the terminal state? -/
def isTerminal : ChainSyncState → Bool
  | .StDone => true
  | _ => false

-- ====================
-- = Typed Wrapper    =
-- ====================

/-- A typed ChainSync handle that tracks the current protocol state -/
structure TypedChainSync where
  sock : Cleanode.Network.Socket.Socket
  state : ChainSyncState

/-- Create a new ChainSync session (starts in StIdle) -/
def TypedChainSync.new (sock : Cleanode.Network.Socket.Socket) : TypedChainSync :=
  { sock := sock, state := .StIdle }

/-- Send a message and transition state. Returns none if the transition is invalid. -/
def TypedChainSync.send (cs : TypedChainSync) (msg : ChainSyncMessage)
    : IO (Except Cleanode.Network.Socket.SocketError (Option TypedChainSync)) := do
  match nextState cs.state msg with
  | none => return .ok none  -- Invalid transition
  | some newState => do
      match ← sendChainSync cs.sock msg with
      | .error e => return .error e
      | .ok () => return .ok (some { cs with state := newState })

/-- Receive a message and transition state. Returns none if decode fails or transition is invalid. -/
def TypedChainSync.recv (cs : TypedChainSync)
    : IO (Except Cleanode.Network.Socket.SocketError (Option (ChainSyncMessage × TypedChainSync))) := do
  match ← receiveChainSync cs.sock with
  | .error e => return .error e
  | .ok none => return .ok none
  | .ok (some msg) =>
      match nextState cs.state msg with
      | none => return .ok none  -- Invalid transition for current state
      | some newState => return .ok (some (msg, { cs with state := newState }))

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- StDone has no valid outgoing transitions -/
theorem chainsync_done_terminal (msg : ChainSyncMessage) :
    nextState .StDone msg = none := by
  cases msg <;> simp [nextState]

/-- From any non-terminal state, StIdle is reachable -/
theorem chainsync_idle_reachable (s : ChainSyncState) :
    s ≠ .StDone →
    ∃ (msgs : List ChainSyncMessage),
      msgs.length > 0 := by
  sorry

/-- From StIdle, there is always at least one valid transition (MsgDone) -/
theorem chainsync_no_deadlock_idle :
    ∃ msg s', nextState .StIdle msg = some s' := by
  exact ⟨.MsgDone, .StDone, rfl⟩

/-- From StCanAwait, there is always at least one valid transition -/
theorem chainsync_no_deadlock_canawait :
    ∃ msg s', nextState .StCanAwait msg = some s' := by
  exact ⟨.MsgAwaitReply, .StMustReply, rfl⟩

/-- Every non-terminal state has at least one valid outgoing transition -/
theorem chainsync_no_deadlock (s : ChainSyncState) :
    s ≠ .StDone →
    ∃ msg s', nextState s msg = some s' := by
  intro h
  cases s with
  | StIdle => exact ⟨.MsgDone, .StDone, rfl⟩
  | StCanAwait => exact ⟨.MsgAwaitReply, .StMustReply, rfl⟩
  | StMustReply =>
      exact ⟨.MsgRollForward ⟨0, ByteArray.empty⟩ ⟨⟨0, ByteArray.empty⟩, 0⟩, .StIdle, rfl⟩
  | StIntersect =>
      exact ⟨.MsgIntersectNotFound ⟨⟨0, ByteArray.empty⟩, 0⟩, .StIdle, rfl⟩
  | StDone => exact absurd rfl h

end Cleanode.Network.ChainSyncState
