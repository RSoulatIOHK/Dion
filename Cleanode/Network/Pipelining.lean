import Dion.Network.BlockFetch
import Dion.Network.BlockFetchState
import Dion.Network.ChainSync

/-!
# Block Fetch Pipelining

Pipelining allows sending multiple BlockFetch requests before receiving
responses, increasing throughput during initial sync.

## Strategy
- Maintain a window of in-flight requests
- Send new requests as responses arrive (sliding window)
- Limit in-flight to avoid memory pressure

## References
- Ouroboros Network: BlockFetch Pipelining
-/

namespace Dion.Network.Pipelining

open Dion.Network.BlockFetch
open Dion.Network.BlockFetchState
open Dion.Network.ChainSync
open Dion.Network.Socket

-- ====================
-- = Configuration    =
-- ====================

/-- Pipelining configuration -/
structure PipelineConfig where
  maxInFlight : Nat := 10          -- Maximum concurrent requests
  batchSize : Nat := 5             -- Blocks per request range
  deriving Repr

-- ====================
-- = Pipeline State   =
-- ====================

/-- A pending block fetch request -/
structure PendingRequest where
  fromPoint : Point
  toPoint : Point
  sentAt : Nat                      -- Timestamp when sent

instance : Repr PendingRequest where
  reprPrec r _ := s!"PendingRequest(sentAt={r.sentAt})"

/-- Pipeline state -/
structure PipelineState where
  config : PipelineConfig
  inFlight : List PendingRequest    -- Requests waiting for response
  queued : List (Point × Point)     -- Ranges waiting to be sent
  completed : Nat                   -- Total completed requests
  receivedBlocks : Nat              -- Total received blocks

instance : Repr PipelineState where
  reprPrec s _ := s!"PipelineState(inFlight={s.inFlight.length}, queued={s.queued.length}, completed={s.completed})"

-- ====================
-- = Operations       =
-- ====================

/-- Create initial pipeline state -/
def PipelineState.initial (config : PipelineConfig := {}) : PipelineState :=
  { config := config,
    inFlight := [],
    queued := [],
    completed := 0,
    receivedBlocks := 0 }

/-- Number of in-flight requests -/
def PipelineState.inFlightCount (ps : PipelineState) : Nat :=
  ps.inFlight.length

/-- Can we send more requests? -/
def PipelineState.canSendMore (ps : PipelineState) : Bool :=
  ps.inFlight.length < ps.config.maxInFlight && !ps.queued.isEmpty

/-- Enqueue a range for fetching -/
def PipelineState.enqueue (ps : PipelineState) (fp tp : Point) : PipelineState :=
  { ps with queued := ps.queued ++ [(fp, tp)] }

/-- Enqueue multiple ranges -/
def PipelineState.enqueueAll (ps : PipelineState) (ranges : List (Point × Point)) : PipelineState :=
  { ps with queued := ps.queued ++ ranges }

/-- Get the next request to send (if pipeline has capacity) -/
def PipelineState.nextRequest (ps : PipelineState) (now : Nat := 0) : Option (BlockFetchMessage × PipelineState) :=
  if ps.inFlight.length >= ps.config.maxInFlight then none
  else match ps.queued with
    | [] => none
    | (fp, tp) :: rest =>
        let pending : PendingRequest := { fromPoint := fp, toPoint := tp, sentAt := now }
        some (.MsgRequestRange fp tp,
              { ps with inFlight := ps.inFlight ++ [pending], queued := rest })

/-- Mark the oldest in-flight request as complete -/
def PipelineState.completeRequest (ps : PipelineState) (blocksReceived : Nat) : PipelineState :=
  { ps with
    inFlight := ps.inFlight.drop 1,
    completed := ps.completed + 1,
    receivedBlocks := ps.receivedBlocks + blocksReceived }

/-- Handle NoBlocks response (request failed, may re-queue) -/
def PipelineState.onNoBlocks (ps : PipelineState) : PipelineState :=
  match ps.inFlight.head? with
  | none => ps
  | some pending =>
      { ps with
        inFlight := ps.inFlight.drop 1,
        queued := (pending.fromPoint, pending.toPoint) :: ps.queued }

/-- Check if pipeline is done (no in-flight and no queued) -/
def PipelineState.isDone (ps : PipelineState) : Bool :=
  ps.inFlight.isEmpty && ps.queued.isEmpty

/-- Send all available pipelined requests -/
def sendPipelinedRequests (sock : Socket) (ps : PipelineState) : IO (Except SocketError PipelineState) := do
  let mut state := ps
  while state.canSendMore do
    match state.nextRequest with
    | none => break
    | some (msg, newState) =>
        match ← sendBlockFetch sock msg with
        | .error e => return .error e
        | .ok () => state := newState
  return .ok state

-- ====================
-- = Batch Helpers    =
-- ====================

/-- Create ranges for a list of points (each point becomes a single-block range) -/
def pointsToRanges (points : List Point) : List (Point × Point) :=
  points.map fun p => (p, p)

/-- Split a large range into smaller batches -/
def splitRange (fp tp : Point) (_batchSize : Nat) : List (Point × Point) :=
  -- Simplified: for single-point ranges, just return as-is
  -- In production, would split by slot range
  [(fp, tp)]

end Dion.Network.Pipelining
