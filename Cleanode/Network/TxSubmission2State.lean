import Cleanode.Network.TxSubmission2

/-!
# TxSubmission2 Protocol State Machine

Formal definition of the TxSubmission2 protocol state machine with
typed states and transition rules per the Ouroboros network spec.

## States
- **StInit**: Client must send MsgInit to begin the protocol.
- **StIdle**: Server's turn. Can request tx IDs, request txs, or terminate.
- **StTxIds**: Client must reply with transaction IDs.
- **StTxs**: Client must reply with full transactions.
- **StDone**: Protocol terminated. No further transitions.

## State Diagram
```
  StInit ──MsgInit──> StIdle
                        │
                        ├─MsgRequestTxIds──> StTxIds
                        │                      └─MsgReplyTxIds──> StIdle
                        │
                        ├─MsgRequestTxs──> StTxs
                        │                    └─MsgReplyTxs──> StIdle
                        │
                        └─MsgDone──> StDone
```

## References
- Ouroboros Network Spec Section 3.9 (TxSubmission)
-/

namespace Cleanode.Network.TxSubmission2State

open Cleanode.Network.TxSubmission2

-- ====================
-- = State Type       =
-- ====================

/-- TxSubmission2 protocol states -/
inductive TxSubmission2State where
  | StInit     -- Client must send MsgInit
  | StIdle     -- Server can request or terminate
  | StTxIds    -- Client must reply with tx IDs
  | StTxs      -- Client must reply with full txs
  | StDone     -- Terminal state
  deriving Repr, BEq, DecidableEq

-- ====================
-- = Transitions      =
-- ====================

/-- Valid state transitions (as a decidable function) -/
def nextState (current : TxSubmission2State) (msg : TxSubmission2Message) : Option TxSubmission2State :=
  match current, msg with
  -- From StInit (client sends MsgInit)
  | .StInit, .MsgInit                      => some .StIdle
  -- From StIdle (server's turn)
  | .StIdle, .MsgRequestTxIds _ _ _        => some .StTxIds
  | .StIdle, .MsgRequestTxs _             => some .StTxs
  | .StIdle, .MsgDone                      => some .StDone
  -- From StTxIds (client replies)
  | .StTxIds, .MsgReplyTxIds _             => some .StIdle
  -- From StTxs (client replies)
  | .StTxs, .MsgReplyTxs _                => some .StIdle
  -- All other transitions are invalid
  | _, _ => none

/-- Proposition: a transition from state s via message m to state s' is valid -/
def validTransition (s : TxSubmission2State) (msg : TxSubmission2Message) (s' : TxSubmission2State) : Prop :=
  nextState s msg = some s'

-- ====================
-- = State Predicates =
-- ====================

/-- Is this a client-agency state? (Client sends next message) -/
def isClientAgency : TxSubmission2State → Bool
  | .StInit => true
  | .StTxIds => true
  | .StTxs => true
  | _ => false

/-- Is this a server-agency state? (Server sends next message) -/
def isServerAgency : TxSubmission2State → Bool
  | .StIdle => true
  | _ => false

/-- Is this the terminal state? -/
def isTerminal : TxSubmission2State → Bool
  | .StDone => true
  | _ => false

-- ====================
-- = Typed Wrapper    =
-- ====================

/-- A typed TxSubmission2 handle that tracks the current protocol state -/
structure TypedTxSubmission2 where
  sock : Cleanode.Network.Socket.Socket
  state : TxSubmission2State

/-- Create a new TxSubmission2 session (starts in StInit) -/
def TypedTxSubmission2.new (sock : Cleanode.Network.Socket.Socket) : TypedTxSubmission2 :=
  { sock := sock, state := .StInit }

/-- Send a message and transition state. Returns none if the transition is invalid. -/
def TypedTxSubmission2.send (ts : TypedTxSubmission2) (msg : TxSubmission2Message)
    : IO (Except Cleanode.Network.Socket.SocketError (Option TypedTxSubmission2)) := do
  match nextState ts.state msg with
  | none => return .ok none  -- Invalid transition
  | some newState => do
      match ← sendTxSubmission2 ts.sock msg with
      | .error e => return .error e
      | .ok () => return .ok (some { ts with state := newState })

/-- Receive a message and transition state. Returns none if decode fails or transition is invalid. -/
def TypedTxSubmission2.recv (ts : TypedTxSubmission2)
    : IO (Except Cleanode.Network.Socket.SocketError (Option (TxSubmission2Message × TypedTxSubmission2))) := do
  match ← receiveTxSubmission2 ts.sock with
  | .error e => return .error e
  | .ok none => return .ok none
  | .ok (some msg) =>
      match nextState ts.state msg with
      | none => return .ok none  -- Invalid transition for current state
      | some newState => return .ok (some (msg, { ts with state := newState }))

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- StDone has no valid outgoing transitions -/
theorem txsubmission_done_terminal (msg : TxSubmission2Message) :
    nextState .StDone msg = none := by
  cases msg <;> simp [nextState]

/-- From StInit, only MsgInit is valid -/
theorem txsubmission_init_only_init (msg : TxSubmission2Message) (s' : TxSubmission2State) :
    nextState .StInit msg = some s' → msg = .MsgInit := by
  cases msg <;> simp [nextState]

/-- Every non-terminal state has at least one valid outgoing transition -/
theorem txsubmission_no_deadlock (s : TxSubmission2State) :
    s ≠ .StDone →
    ∃ msg s', nextState s msg = some s' := by
  intro h
  cases s with
  | StInit => exact ⟨.MsgInit, .StIdle, rfl⟩
  | StIdle => exact ⟨.MsgDone, .StDone, rfl⟩
  | StTxIds => exact ⟨.MsgReplyTxIds [], .StIdle, rfl⟩
  | StTxs => exact ⟨.MsgReplyTxs [], .StIdle, rfl⟩
  | StDone => exact absurd rfl h

/-- State machine is deterministic: same input always gives same output -/
theorem txsubmission_deterministic (s : TxSubmission2State) (msg : TxSubmission2Message)
    (s1 s2 : TxSubmission2State) :
    nextState s msg = some s1 → nextState s msg = some s2 → s1 = s2 := by
  intros h1 h2; rw [h1] at h2; exact Option.some.inj h2

end Cleanode.Network.TxSubmission2State
