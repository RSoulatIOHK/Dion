import Cleanode.Consensus.Praos.BlockForge
import Cleanode.Network.ChainSync
import Cleanode.Network.BlockFetch
import Cleanode.Network.Multiplexer
import Cleanode.Network.Socket
import Cleanode.Network.Crypto

/-!
# Block Announcement

Broadcasts newly forged blocks to connected peers via ChainSync (server mode).

## Design

When this node forges a block, we need to push it to all peers that have
an active ChainSync subscription (i.e., peers in the `StCanAwait` state
waiting for new headers). This module:

1. Maintains a registry of peer sockets subscribed to our chain
2. Encodes forged blocks as `MsgRollForward` messages with the proper
   era-wrapped header format
3. Sends the announcement to all registered peers
4. Handles BlockFetch `MsgRequestRange` for blocks we've forged

## Peer Lifecycle

Inbound peers go through:
```
Handshake → MsgFindIntersect → MsgRequestNext → (waiting for us)
                                    ↓
                             MsgRollForward ← (we push when block forged)
```

## References
- Ouroboros Network Spec: ChainSync mini-protocol (server role)
- Ouroboros Network Spec: BlockFetch mini-protocol (server role)
-/

namespace Cleanode.Consensus.Praos.BlockAnnounce

open Cleanode.Consensus.Praos.BlockForge
open Cleanode.Network.ChainSync
open Cleanode.Network.BlockFetch
open Cleanode.Network.Multiplexer
open Cleanode.Network.Socket

-- ====================
-- = Peer Registry    =
-- ====================

/-- A peer that has subscribed to our chain via ChainSync -/
structure ChainSyncSubscriber where
  /-- Peer's socket for sending updates -/
  socket : Socket
  /-- Peer identifier (host:port) -/
  peerId : String
  /-- Whether the peer is waiting for the next block (StCanAwait) -/
  isWaiting : Bool
  /-- Last point we sent to this peer -/
  lastSentSlot : Nat

/-- Registry of peers subscribed to our ChainSync feed -/
structure PeerRegistry where
  /-- Active ChainSync subscribers -/
  subscribers : Array ChainSyncSubscriber
  /-- Forged blocks we can serve via BlockFetch (slot → block) -/
  forgedBlocks : Array ForgedBlock

/-- Empty registry -/
def PeerRegistry.empty : PeerRegistry :=
  { subscribers := #[], forgedBlocks := #[] }

/-- Add a subscriber -/
def PeerRegistry.addSubscriber (reg : PeerRegistry) (sub : ChainSyncSubscriber)
    : PeerRegistry :=
  { reg with subscribers := reg.subscribers.push sub }

/-- Remove a subscriber by peer ID -/
def PeerRegistry.removeSubscriber (reg : PeerRegistry) (peerId : String)
    : PeerRegistry :=
  { reg with subscribers := reg.subscribers.filter (fun s => s.peerId != peerId) }

/-- Store a forged block for BlockFetch serving -/
def PeerRegistry.storeBlock (reg : PeerRegistry) (block : ForgedBlock)
    : PeerRegistry :=
  -- Keep last 100 forged blocks
  let blocks := if reg.forgedBlocks.size >= 100 then
    reg.forgedBlocks.extract 1 reg.forgedBlocks.size |>.push block
  else
    reg.forgedBlocks.push block
  { reg with forgedBlocks := blocks }

/-- Look up a forged block by slot -/
def PeerRegistry.findBlock (reg : PeerRegistry) (slot : Nat)
    : Option ForgedBlock :=
  reg.forgedBlocks.find? (fun b => b.slot == slot)

-- ====================
-- = Block Encoding   =
-- ====================

/-- Convert a ForgedBlock to a ChainSync Header (era 6 = Conway) -/
def forgedBlockToHeader (block : ForgedBlock) (era : Nat := 6) : Header :=
  { era := era, headerBytes := block.headerBytes }

/-- Build a Tip from a ForgedBlock -/
def forgedBlockToTip (block : ForgedBlock) : IO Tip := do
  -- Compute block hash from header bytes
  let blockHash ← Cleanode.Network.Crypto.blake2b_256 block.headerBytes
  return {
    point := { slot := UInt64.ofNat block.slot, hash := blockHash },
    blockNo := UInt64.ofNat block.blockNumber
  }

-- ====================
-- = Announcement     =
-- ====================

/-- Send a ChainSync message to a peer as responder (server mode).
    Uses mode=Responder (bit 15 set) for the mux frame. -/
def sendChainSyncResponder (sock : Socket) (msg : ChainSyncMessage)
    : IO (Except SocketError Unit) := do
  let payload := encodeChainSyncMessage msg
  let frame ← createFrame .ChainSync .Responder payload
  let frameBytes := encodeMuxFrame frame
  socket_send sock frameBytes

/-- Announce a forged block to a single subscriber.
    Sends MsgRollForward with the block header and tip. -/
def announceToSubscriber (sub : ChainSyncSubscriber) (block : ForgedBlock)
    (tip : Tip) : IO Bool := do
  if !sub.isWaiting then return true  -- Peer not waiting, skip
  let header := forgedBlockToHeader block
  let msg := ChainSyncMessage.MsgRollForward header tip
  match ← sendChainSyncResponder sub.socket msg with
  | .ok () => return true
  | .error e =>
    IO.eprintln s!"[announce] Failed to send to {sub.peerId}: {e}"
    return false

/-- Announce a forged block to all subscribed peers.
    Returns the number of peers successfully notified. -/
def announceBlock (registryRef : IO.Ref PeerRegistry) (block : ForgedBlock)
    : IO Nat := do
  let tip ← forgedBlockToTip block
  let registry ← registryRef.get

  -- Store the block for future BlockFetch requests
  registryRef.modify (·.storeBlock block)

  let mut successCount := 0
  let mut failedPeers : Array String := #[]

  for sub in registry.subscribers do
    let ok ← announceToSubscriber sub block tip
    if ok then
      successCount := successCount + 1
    else
      failedPeers := failedPeers.push sub.peerId

  -- Remove failed peers
  for peerId in failedPeers do
    registryRef.modify (·.removeSubscriber peerId)

  if successCount > 0 then
    IO.println s!"[announce] Block {block.blockNumber} (slot {block.slot}) sent to {successCount} peers"
  return successCount

-- ====================
-- = ChainSync Server =
-- ====================

/-- Handle a ChainSync MsgFindIntersect from an inbound peer.
    For now, always respond with MsgIntersectFound at genesis or
    the peer's requested point if we have it. -/
def handleFindIntersect (sock : Socket) (points : List Point)
    (currentTip : Tip) : IO Unit := do
  -- Simple strategy: accept the first point the peer offers
  match points.head? with
  | some pt =>
    let _ ← sendChainSyncResponder sock (.MsgIntersectFound pt currentTip)
  | none =>
    let _ ← sendChainSyncResponder sock (.MsgIntersectNotFound currentTip)

/-- Handle a ChainSync MsgRequestNext from an inbound peer.
    If we have a new block, send MsgRollForward. Otherwise, send MsgAwaitReply
    and register the peer as waiting. -/
def handleRequestNext (registryRef : IO.Ref PeerRegistry)
    (sock : Socket) (peerId : String)
    (pendingBlocks : IO.Ref (Array ForgedBlock))
    : IO Unit := do
  let pending ← pendingBlocks.get
  if h : 0 < pending.size then
    -- Send the next pending block
    let block := pending[0]
    let remaining := pending.extract 1 pending.size
    pendingBlocks.set remaining
    let tip ← forgedBlockToTip block
    let header := forgedBlockToHeader block
    let _ ← sendChainSyncResponder sock (.MsgRollForward header tip)
  else
    -- No blocks available — tell peer to wait
    let _ ← sendChainSyncResponder sock .MsgAwaitReply
    -- Mark this peer as waiting in the registry
    registryRef.modify fun reg =>
      { reg with subscribers := reg.subscribers.map fun sub =>
        if sub.peerId == peerId then { sub with isWaiting := true }
        else sub }

-- ====================
-- = Announcement Loop =
-- ====================

/-- Background loop that watches the forged blocks queue and announces
    new blocks to all subscribed peers.
    Runs alongside the forge loop — the forge loop pushes to `forgedBlocksRef`,
    this loop drains it and broadcasts. -/
partial def announcementLoop (registryRef : IO.Ref PeerRegistry)
    (forgedBlocksRef : IO.Ref (Array ForgedBlock))
    : IO Unit := do
  while true do
    IO.sleep 100  -- Check every 100ms
    let blocks ← forgedBlocksRef.get
    if blocks.size > 0 then
      -- Drain the queue
      forgedBlocksRef.set #[]
      for block in blocks do
        let _ ← announceBlock registryRef block

/-- Start the announcement loop as a background task. -/
def startAnnouncementLoop (registryRef : IO.Ref PeerRegistry)
    (forgedBlocksRef : IO.Ref (Array ForgedBlock))
    : IO (Task (Except IO.Error Unit)) := do
  IO.asTask (prio := .dedicated) do
    announcementLoop registryRef forgedBlocksRef

-- ==========================
-- = BlockFetch Server      =
-- ==========================

/-- Look up a forged block by point (slot + hash match) -/
def PeerRegistry.findBlockByPoint (reg : PeerRegistry) (point : Point)
    : IO (Option ForgedBlock) := do
  for block in reg.forgedBlocks do
    if block.slot == point.slot.toNat then
      -- Verify hash matches
      let blockHash ← Cleanode.Network.Crypto.blake2b_256 block.headerBytes
      if blockHash == point.hash then
        return some block
  return none

/-- Collect all forged blocks in a slot range [fromSlot, toSlot] -/
def PeerRegistry.findBlocksInRange (reg : PeerRegistry) (fromSlot toSlot : Nat)
    : Array ForgedBlock :=
  reg.forgedBlocks.filter (fun b => b.slot >= fromSlot && b.slot <= toSlot)

/-- Handle a BlockFetch MsgRequestRange from an inbound peer.
    Looks up the requested blocks and serves them. -/
def handleBlockFetchRequest (registryRef : IO.Ref PeerRegistry)
    (sock : Socket) (fromPoint toPoint : Point) : IO Unit := do
  let reg ← registryRef.get
  let blocks := reg.findBlocksInRange fromPoint.slot.toNat toPoint.slot.toNat
  if blocks.isEmpty then
    let _ ← sendBlockFetchResponder sock .MsgNoBlocks
  else
    let _ ← sendBlockFetchResponder sock .MsgStartBatch
    for block in blocks do
      -- Send the full block as a proper 5-element CBOR array
      let fullBlock := block.encodeFullBlock
      let _ ← sendBlockFetchResponder sock (.MsgBlock fullBlock)
    let _ ← sendBlockFetchResponder sock .MsgBatchDone

-- ====================
-- = Diagnostics      =
-- ====================

/-- Print announcement system status -/
def printAnnouncementStatus (registryRef : IO.Ref PeerRegistry) : IO Unit := do
  let reg ← registryRef.get
  IO.println s!"[announce] Status:"
  IO.println s!"  Subscribers: {reg.subscribers.size}"
  IO.println s!"  Stored blocks: {reg.forgedBlocks.size}"
  let waiting := reg.subscribers.filter (·.isWaiting) |>.size
  IO.println s!"  Waiting peers: {waiting}"

end Cleanode.Consensus.Praos.BlockAnnounce
