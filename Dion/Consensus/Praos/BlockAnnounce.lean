import Dion.Consensus.Praos.BlockForge
import Dion.Network.ChainSync
import Dion.Network.BlockFetch
import Dion.Network.Multiplexer
import Dion.Network.Socket
import Dion.Network.Crypto
import Dion.Network.Handshake
import Dion.Network.TxSubmission2

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

namespace Dion.Consensus.Praos.BlockAnnounce

open Dion.Consensus.Praos.BlockForge
open Dion.Network.ChainSync
open Dion.Network.BlockFetch
open Dion.Network.Multiplexer
open Dion.Network.Socket
open Dion.Network.Handshake
open Dion.Network.TxSubmission2

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
  let blockHash ← Dion.Network.Crypto.blake2b_256 block.headerBytes
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

-- ==========================
-- = Outbound Block Push    =
-- ==========================

/-- Read mux messages from the peer, acting as ChainSync producer, until we
    successfully push `block` or exhaust the attempt budget.
    We are the TCP initiator, so we send with mode=Initiator (sendChainSync).
    This works when the remote peer operates in InitiatorAndResponder diffusion
    mode and issues a ChainSync consumer session from their side. -/
partial def servePushSession (sock : Socket) (block : ForgedBlock) (tip : Tip)
    (timeoutMs : UInt32 := 5000) (attemptsLeft : Nat := 40) : IO Bool := do
  if attemptsLeft == 0 then return false
  -- Read mux header with timeout so we don't block forever
  match ← socket_receive_exact_timeout sock 8 timeoutMs with
  | .error _ => return false
  | .ok none  => return false  -- timeout: peer is Initiator-only, give up
  | .ok (some hdrBytes) =>
    match decodeMuxHeader hdrBytes with
    | none => return false
    | some hdr =>
      match ← socket_receive_exact sock hdr.payloadLength.toNat.toUInt32 with
      | .error _ => return false
      | .ok payload =>
        if hdr.protocolId == .ChainSync then
          match decodeChainSyncMessage payload with
          | some (.MsgFindIntersect points) =>
            -- Peer wants to subscribe to our chain.
            -- Reply with IntersectFound at the peer's requested point (or genesis).
            let point := points.head?.getD Point.genesis
            let _ ← sendChainSync sock (.MsgIntersectFound point tip)
            servePushSession sock block tip timeoutMs (attemptsLeft - 1)
          | some .MsgRequestNext =>
            -- Peer is asking for the next block — deliver ours!
            let header := forgedBlockToHeader block
            let _ ← sendChainSync sock (.MsgRollForward header tip)
            IO.println s!"[push] Block {block.blockNumber} (slot {block.slot}) pushed via outbound connection"
            return true
          | _ =>
            servePushSession sock block tip timeoutMs (attemptsLeft - 1)
        else
          -- Non-ChainSync message (TxSubmission, KeepAlive, etc.) — skip it
          servePushSession sock block tip timeoutMs (attemptsLeft - 1)

/-- Push a forged block to an outbound peer by opening a fresh TCP connection.
    After the handshake, waits for the peer's ChainSync consumer session
    (InitiatorAndResponder diffusion mode) and delivers the block.
    Returns true if the block was successfully pushed. -/
def pushBlockToPeer (host : String) (port : UInt16)
    (proposal : HandshakeMessage) (block : ForgedBlock) : IO Bool := do
  match ← socket_connect host port with
  | .error e =>
    IO.eprintln s!"[push] {host}:{port}: connect failed: {e}"
    return false
  | .ok sock => do
    -- Send handshake (Initiator mode — we opened the connection)
    match ← sendHandshake sock proposal with
    | .error _ => socket_close sock; return false
    | .ok _ => do
      -- Read handshake response header
      match ← socket_receive_exact sock 8 with
      | .error _ => socket_close sock; return false
      | .ok hdrBytes =>
        match decodeMuxHeader hdrBytes with
        | none => socket_close sock; return false
        | some hdr =>
          match ← socket_receive_exact sock hdr.payloadLength.toNat.toUInt32 with
          | .error _ => socket_close sock; return false
          | .ok _ => do
            -- Send TxSubmission2 MsgInit so the peer's TxSubmission consumer
            -- doesn't stall waiting for us (we're the TxSubmission producer on
            -- outbound connections).
            let _ ← sendTxSubmission2 sock .MsgInit
            -- Now wait for the peer's ChainSync consumer FindIntersect (I&R mode)
            let tip ← forgedBlockToTip block
            let ok ← servePushSession sock block tip 5000 40
            socket_close sock
            return ok

/-- Push a forged block to all known outbound peers (fire-and-forget per peer). -/
def pushBlockToOutboundPeers (peerAddrs : List (String × UInt16))
    (proposal : HandshakeMessage) (block : ForgedBlock) : IO Unit := do
  for (host, port) in peerAddrs do
    let _ ← IO.asTask (do
      try
        let ok ← pushBlockToPeer host port proposal block
        if !ok then
          IO.eprintln s!"[push] {host}:{port}: peer did not subscribe (Initiator-only?)"
      catch e =>
        IO.eprintln s!"[push] {host}:{port}: {e}")
  pure ()

-- ====================
-- = Announcement Loop =
-- ====================

/-- Background loop that watches the forged blocks queue and announces
    new blocks to all subscribed peers (inbound) and pushes to outbound peers. -/
partial def announcementLoop (registryRef : IO.Ref PeerRegistry)
    (forgedBlocksRef : IO.Ref (Array ForgedBlock))
    (outboundPeers : List (String × UInt16) := [])
    (proposal : Option HandshakeMessage := none)
    : IO Unit := do
  while true do
    IO.sleep 100  -- Check every 100ms
    let blocks ← forgedBlocksRef.get
    if blocks.size > 0 then
      -- Drain the queue
      forgedBlocksRef.set #[]
      for block in blocks do
        -- Announce to inbound subscribers (existing mechanism)
        let _ ← announceBlock registryRef block
        -- Also push to outbound peers (InitiatorAndResponder mode)
        if let some prop := proposal then
          if !outboundPeers.isEmpty then
            pushBlockToOutboundPeers outboundPeers prop block

/-- Start the announcement loop as a background task. -/
def startAnnouncementLoop (registryRef : IO.Ref PeerRegistry)
    (forgedBlocksRef : IO.Ref (Array ForgedBlock))
    (outboundPeers : List (String × UInt16) := [])
    (proposal : Option HandshakeMessage := none)
    : IO (Task (Except IO.Error Unit)) := do
  IO.asTask (prio := .dedicated) do
    announcementLoop registryRef forgedBlocksRef outboundPeers proposal

-- ==========================
-- = BlockFetch Server      =
-- ==========================

/-- Look up a forged block by point (slot + hash match) -/
def PeerRegistry.findBlockByPoint (reg : PeerRegistry) (point : Point)
    : IO (Option ForgedBlock) := do
  for block in reg.forgedBlocks do
    if block.slot == point.slot.toNat then
      -- Verify hash matches
      let blockHash ← Dion.Network.Crypto.blake2b_256 block.headerBytes
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

end Dion.Consensus.Praos.BlockAnnounce
