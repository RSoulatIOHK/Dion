import Dion.Network.BlockFetch
import Dion.Network.ChainSync

/-!
# BlockFetch Client State Machine

Formal state machine for the BlockFetch mini-protocol as specified in the
Ouroboros Network Specification Section 3.8.

## State Machine
- StIdle: Client can send MsgRequestRange or MsgClientDone
- StBusy: Waiting for server response (MsgStartBatch or MsgNoBlocks)
- StStreaming: Receiving blocks (MsgBlock repeated, then MsgBatchDone)
- StDone: Protocol terminated

## References
- Ouroboros Network Spec Section 3.8 (Block Fetch)
-/

namespace Dion.Network.BlockFetchState

open Dion.Network.BlockFetch
open Dion.Network.ChainSync

-- ====================
-- = State Machine    =
-- ====================

/-- BlockFetch protocol states -/
inductive BlockFetchState where
  | StIdle       -- Client can request or terminate
  | StBusy       -- Waiting for server response
  | StStreaming   -- Receiving block bodies
  | StDone       -- Protocol terminated
  deriving Repr, BEq

/-- Valid state transitions -/
inductive BlockFetchTransition where
  | RequestRange   : Point → Point → BlockFetchTransition  -- StIdle → StBusy
  | ClientDone     : BlockFetchTransition                   -- StIdle → StDone
  | StartBatch     : BlockFetchTransition                   -- StBusy → StStreaming
  | NoBlocks       : BlockFetchTransition                   -- StBusy → StIdle
  | Block          : ByteArray → BlockFetchTransition       -- StStreaming → StStreaming
  | BatchDone      : BlockFetchTransition                   -- StStreaming → StIdle

/-- Apply a transition to the current state -/
def applyTransition : BlockFetchState → BlockFetchTransition → Option BlockFetchState
  | .StIdle,      .RequestRange _ _ => some .StBusy
  | .StIdle,      .ClientDone       => some .StDone
  | .StBusy,      .StartBatch       => some .StStreaming
  | .StBusy,      .NoBlocks         => some .StIdle
  | .StStreaming,  .Block _          => some .StStreaming
  | .StStreaming,  .BatchDone        => some .StIdle
  | _,             _                 => none

/-- Convert a received message to a transition -/
def messageToTransition (msg : BlockFetchMessage) : Option BlockFetchTransition :=
  match msg with
  | .MsgRequestRange fp tp => some (.RequestRange fp tp)
  | .MsgClientDone         => some .ClientDone
  | .MsgStartBatch         => some .StartBatch
  | .MsgNoBlocks           => some .NoBlocks
  | .MsgBlock bytes        => some (.Block bytes)
  | .MsgBatchDone          => some .BatchDone

-- ====================
-- = Client Manager   =
-- ====================

/-- Block fetch client state with request queue -/
structure BlockFetchClient where
  state : BlockFetchState
  pendingRanges : List (Point × Point)  -- Queued ranges to fetch
  receivedBlocks : List ByteArray        -- Blocks received in current batch
  totalFetched : Nat                     -- Total blocks fetched

instance : Repr BlockFetchClient where
  reprPrec c _ := s!"BlockFetchClient(state={repr c.state}, pending={c.pendingRanges.length}, fetched={c.totalFetched})"

/-- Create initial client -/
def BlockFetchClient.initial : BlockFetchClient :=
  { state := .StIdle,
    pendingRanges := [],
    receivedBlocks := [],
    totalFetched := 0 }

/-- Add a range to the fetch queue -/
def BlockFetchClient.enqueueRange (client : BlockFetchClient) (fp tp : Point) : BlockFetchClient :=
  { client with pendingRanges := client.pendingRanges ++ [(fp, tp)] }

/-- Process a received message, updating client state -/
def BlockFetchClient.processMessage (client : BlockFetchClient) (msg : BlockFetchMessage) : Option BlockFetchClient := do
  let transition ← messageToTransition msg
  let newState ← applyTransition client.state transition
  match msg with
  | .MsgStartBatch =>
      some { client with state := newState, receivedBlocks := [] }
  | .MsgBlock blockData =>
      let newBlocks := client.receivedBlocks ++ [blockData]
      some { client with state := newState, receivedBlocks := newBlocks }
  | .MsgBatchDone =>
      let newTotal := client.totalFetched + client.receivedBlocks.length
      some { client with state := newState, totalFetched := newTotal, receivedBlocks := [] }
  | .MsgNoBlocks =>
      some { client with state := newState }
  | _ =>
      some { client with state := newState }

/-- Get the next request to send (if in idle state with pending ranges) -/
def BlockFetchClient.nextRequest (client : BlockFetchClient) : Option (BlockFetchMessage × BlockFetchClient) :=
  match client.state, client.pendingRanges with
  | .StIdle, (fp, tp) :: rest =>
      some (.MsgRequestRange fp tp, { client with state := .StBusy, pendingRanges := rest })
  | .StIdle, [] =>
      some (.MsgClientDone, { client with state := .StDone })
  | _, _ => none

/-- Check if client is done (terminal state) -/
def BlockFetchClient.isDone (client : BlockFetchClient) : Bool :=
  client.state == .StDone

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- State machine is deterministic: each (state, transition) pair has at most one result -/
theorem blockfetch_state_deterministic :
    ∀ (s : BlockFetchState) (t : BlockFetchTransition) (s1 s2 : BlockFetchState),
      applyTransition s t = some s1 → applyTransition s t = some s2 → s1 = s2 := by
  intros s t s1 s2 h1 h2
  rw [h1] at h2
  exact Option.some.inj h2

/-- Terminal state has no valid transitions -/
theorem blockfetch_done_terminal :
    ∀ (t : BlockFetchTransition),
      applyTransition .StDone t = none := by
  intro t
  cases t <;> rfl

end Dion.Network.BlockFetchState
