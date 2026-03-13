import Cleanode.Network.PeerSharing

/-!
# PeerSharing Protocol State Machine

Formal definition of the PeerSharing protocol state machine with
typed states and transition rules.

## States
- **StIdle**: Client's turn. Can send MsgShareRequest or MsgDone.
- **StBusy**: Server's turn. Must respond with MsgSharePeers.
- **StDone**: Protocol terminated. No further transitions.

## State Diagram
```
  StIdle ──MsgShareRequest──> StBusy
    │                           └─MsgSharePeers──> StIdle
    │
    └─MsgDone──> StDone
```

## References
- Ouroboros Network Spec (Peer Sharing)
-/

namespace Cleanode.Network.PeerSharingState

open Cleanode.Network.PeerSharing

-- ====================
-- = State Type       =
-- ====================

/-- PeerSharing protocol states -/
inductive PeerSharingState where
  | StIdle  -- Client can send request or done
  | StBusy  -- Server must respond with peers
  | StDone  -- Terminal state
  deriving Repr, BEq, DecidableEq

-- ====================
-- = Transitions      =
-- ====================

/-- Valid state transitions -/
def nextState (current : PeerSharingState) (msg : PeerSharingMessage) : Option PeerSharingState :=
  match current, msg with
  | .StIdle, .MsgShareRequest _ => some .StBusy
  | .StIdle, .MsgDone           => some .StDone
  | .StBusy, .MsgSharePeers _   => some .StIdle
  | _, _                         => none

/-- Proposition: a transition is valid -/
def validTransition (s : PeerSharingState) (msg : PeerSharingMessage) (s' : PeerSharingState) : Prop :=
  nextState s msg = some s'

-- ====================
-- = State Predicates =
-- ====================

/-- Is this a client-agency state? -/
def isClientAgency : PeerSharingState → Bool
  | .StIdle => true
  | _ => false

/-- Is this a server-agency state? -/
def isServerAgency : PeerSharingState → Bool
  | .StBusy => true
  | _ => false

/-- Is this the terminal state? -/
def isTerminal : PeerSharingState → Bool
  | .StDone => true
  | _ => false

-- ====================
-- = Typed Wrapper    =
-- ====================

/-- A typed PeerSharing handle that tracks protocol state -/
structure TypedPeerSharing where
  sock : Cleanode.Network.Socket.Socket
  state : PeerSharingState

/-- Create a new PeerSharing session (starts in StIdle) -/
def TypedPeerSharing.new (sock : Cleanode.Network.Socket.Socket) : TypedPeerSharing :=
  { sock := sock, state := .StIdle }

/-- Send a message and transition state -/
def TypedPeerSharing.send (ps : TypedPeerSharing) (msg : PeerSharingMessage)
    : IO (Except Cleanode.Network.Socket.SocketError (Option TypedPeerSharing)) := do
  match nextState ps.state msg with
  | none => return .ok none
  | some newState => do
      match ← sendPeerSharing ps.sock msg with
      | .error e => return .error e
      | .ok () => return .ok (some { ps with state := newState })

/-- Receive a message and transition state -/
def TypedPeerSharing.recv (ps : TypedPeerSharing)
    : IO (Except Cleanode.Network.Socket.SocketError (Option (PeerSharingMessage × TypedPeerSharing))) := do
  match ← receivePeerSharing ps.sock with
  | .error e => return .error e
  | .ok none => return .ok none
  | .ok (some msg) =>
      match nextState ps.state msg with
      | none => return .ok none
      | some newState => return .ok (some (msg, { ps with state := newState }))

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- StDone has no valid outgoing transitions -/
theorem peersharing_done_terminal (msg : PeerSharingMessage) :
    nextState .StDone msg = none := by
  cases msg <;> simp [nextState]

/-- Every non-terminal state has at least one valid outgoing transition -/
theorem peersharing_no_deadlock (s : PeerSharingState) :
    s ≠ .StDone →
    ∃ msg s', nextState s msg = some s' := by
  intro h
  cases s with
  | StIdle => exact ⟨.MsgDone, .StDone, rfl⟩
  | StBusy => exact ⟨.MsgSharePeers [], .StIdle, rfl⟩
  | StDone => exact absurd rfl h

end Cleanode.Network.PeerSharingState
