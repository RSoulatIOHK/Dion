import Dion.Network.Cbor
import Dion.Network.Multiplexer
import Dion.Network.Socket
import Dion.Network.ChainSync
import Dion.Network.TxSubmission2
import Dion.Network.Mempool

/-!
# BlockFetch Mini-Protocol

The BlockFetch protocol is used to download full block bodies from peers.
While ChainSync only provides block headers, BlockFetch provides the complete
block including all transactions.

## Protocol Flow
1. Client sends MsgRequestRange with range of blocks to fetch
2. Server responds with:
   - MsgStartBatch → MsgBlock (repeated) → MsgBatchDone, or
   - MsgNoBlocks (if range not available)
3. Client can request more ranges or send MsgClientDone to terminate

## State Machine
- StIdle: Client can send MsgRequestRange or MsgClientDone
- StBusy: Server sends MsgStartBatch/MsgNoBlocks, then blocks, then MsgBatchDone
- StStreaming: Server sending block bodies
- StDone: Protocol terminated

## References
- Ouroboros Network Spec Section 3.8 (Block Fetch)
- Protocol number: 3 (node-to-node)
-/

namespace Dion.Network.BlockFetch

open Dion.Network.Cbor
open Dion.Network.Multiplexer
open Dion.Network.Socket
open Dion.Network.ChainSync

-- ==============
-- = Core Types =
-- ==============

/-- Full block (header + body) -/
structure Block where
  header : Header       -- Block header (from ChainSync)
  bodyBytes : ByteArray -- Full block body (CBOR encoded)

instance : Repr Block where
  reprPrec b _ := s!"Block(era={b.header.era}, headerSize={b.header.headerBytes.size}B, bodySize={b.bodyBytes.size}B)"

-- ===================
-- = Protocol Messages =
-- ===================

/-- BlockFetch protocol messages -/
inductive BlockFetchMessage where
  | MsgRequestRange (fromPoint : Point) (toPoint : Point)  -- [0] Request block range
  | MsgClientDone                                          -- [1] Client terminates
  | MsgStartBatch                                          -- [2] Server starts batch
  | MsgNoBlocks                                            -- [3] No blocks in range
  | MsgBlock (block : ByteArray)                           -- [4] Block body (raw CBOR)
  | MsgBatchDone                                           -- [5] Batch complete

instance : Repr BlockFetchMessage where
  reprPrec
    | .MsgRequestRange fp tp, _ => s!"MsgRequestRange({repr fp}, {repr tp})"
    | .MsgClientDone, _ => "MsgClientDone"
    | .MsgStartBatch, _ => "MsgStartBatch"
    | .MsgNoBlocks, _ => "MsgNoBlocks"
    | .MsgBlock block, _ => s!"MsgBlock({block.size}B)"
    | .MsgBatchDone, _ => "MsgBatchDone"

-- ==============
-- = Encoding   =
-- ==============

/-- Encode BlockFetch message -/
def encodeBlockFetchMessage : BlockFetchMessage → ByteArray
  | .MsgRequestRange fromPoint toPoint =>
      let arr := encodeArrayHeader 3
      let msgId := encodeUInt 0
      let fromEnc := encodePoint fromPoint
      let toEnc := encodePoint toPoint
      arr ++ msgId ++ fromEnc ++ toEnc
  | .MsgClientDone =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 1
      arr ++ msgId
  | .MsgStartBatch =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 2
      arr ++ msgId
  | .MsgNoBlocks =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 3
      arr ++ msgId
  | .MsgBlock blockBytes =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 4
      arr ++ msgId ++ blockBytes
  | .MsgBatchDone =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 5
      arr ++ msgId

-- ==============
-- = Decoding   =
-- ==============

/-- Decode BlockFetch message -/
def decodeBlockFetchMessage (bs : ByteArray) : Option (DecodeResult BlockFetchMessage) := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining

  match r2.value with
  | 0 => do  -- MsgRequestRange
      if r1.value != 3 then none
      let r3 ← decodePoint r2.remaining
      let r4 ← decodePoint r3.remaining
      some { value := .MsgRequestRange r3.value r4.value, remaining := r4.remaining }
  | 1 => if r1.value == 1 then some { value := .MsgClientDone, remaining := r2.remaining } else none
  | 2 => if r1.value == 1 then some { value := .MsgStartBatch, remaining := r2.remaining } else none
  | 3 => if r1.value == 1 then some { value := .MsgNoBlocks, remaining := r2.remaining } else none
  | 4 => do  -- MsgBlock
      if r1.value != 2 then none
      -- The block data is wrapped in tag24, then a CBOR bytestring
      -- Skip tag24 if present (0xD8 0x18)
      let afterTag :=
        if r2.remaining.size >= 2 && r2.remaining[0]! == 0xD8 && r2.remaining[1]! == 0x18 then
          r2.remaining.extract 2 r2.remaining.size
        else
          r2.remaining
      -- Now decode the bytestring
      let r3 ← decodeBytes afterTag
      some { value := .MsgBlock r3.value, remaining := r3.remaining }
  | 5 => if r1.value == 1 then some { value := .MsgBatchDone, remaining := r2.remaining } else none
  | _ => none

-- ==============
-- = Client API =
-- ==============

/-- Send BlockFetch message over socket (client/initiator mode) -/
def sendBlockFetch (sock : Socket) (msg : BlockFetchMessage) : IO (Except SocketError Unit) := do
  let payload := encodeBlockFetchMessage msg
  let frame ← createFrame .BlockFetch .Initiator payload
  let frameBytes := encodeMuxFrame frame
  socket_send sock frameBytes

/-- Send BlockFetch message as server/responder (bit 15 set in mux frame) -/
def sendBlockFetchResponder (sock : Socket) (msg : BlockFetchMessage) : IO (Except SocketError Unit) := do
  let payload := encodeBlockFetchMessage msg
  let frame ← createFrame .BlockFetch .Responder payload
  socket_send sock (encodeMuxFrame frame)

/-- Result of receiving a BlockFetch message, including any leftover bytes -/
structure BlockFetchReceiveResult where
  message : BlockFetchMessage
  leftoverBytes : ByteArray

/-- Encode a KeepAlive response and send it -/
private def handleKeepAlive (sock : Socket) (payload : ByteArray) : IO Unit := do
  -- MsgKeepAlive = [0, cookie], respond with MsgKeepAliveResponse = [1, cookie]
  if payload.size >= 3 && payload[0]! == 0x82 && payload[1]! == 0x00 then
    -- Replace the 0x00 (MsgKeepAlive tag) with 0x01 (MsgKeepAliveResponse tag)
    let responsePayload := payload.set! 1 0x01
    let frame ← createFrame .KeepAlive .Initiator responsePayload
    let _ ← socket_send sock (encodeMuxFrame frame)
    pure ()
  else
    pure ()

/-- Handle TxSubmission2 MsgRequestTxs inline during BlockFetch.
    When a peer requests tx bodies, we reply immediately from the mempool. -/
private def handleTxSubmission2Inline (sock : Socket) (payload : ByteArray)
    (mempoolRef : Option (IO.Ref Dion.Network.Mempool.Mempool)) : IO Unit := do
  match Dion.Network.TxSubmission2.decodeTxSubmission2Message payload with
  | some (.MsgRequestTxs hashes) => do
      IO.eprintln s!"[TxSub] MsgRequestTxs received during BlockFetch ({hashes.length} hashes)"
      match mempoolRef with
      | some mpRef => do
          let pool ← mpRef.get
          let txBodies := pool.getTxsByHash hashes
          IO.eprintln s!"[TxSub] → Replying with {txBodies.length} tx bodies"
          let _ ← Dion.Network.TxSubmission2.sendTxSubmission2 sock (.MsgReplyTxs txBodies)
          pure ()
      | none =>
          let _ ← Dion.Network.TxSubmission2.sendTxSubmission2 sock (.MsgReplyTxs [])
          pure ()
  | some (.MsgRequestTxIds blocking ack req) => do
      IO.eprintln s!"[TxSub] MsgRequestTxIds during BlockFetch (blk={blocking},ack={ack},req={req})"
      match mempoolRef with
      | some mpRef => do
          let pool ← mpRef.get
          let txIds := pool.getTxIds req.toNat
          IO.eprintln s!"[TxSub] → Replying with {txIds.length} tx IDs from mempool (inline)"
          let _ ← Dion.Network.TxSubmission2.sendTxSubmission2 sock (.MsgReplyTxIds txIds)
          pure ()
      | none =>
          -- No mempool ref — reply empty (non-blocking only; blocking + empty is a protocol violation)
          if !blocking then
            let _ ← Dion.Network.TxSubmission2.sendTxSubmission2 sock (.MsgReplyTxIds [])
            pure ()
          else
            -- For blocking requests with no mempool, we have no choice but to reply empty
            -- This is technically a protocol violation but avoids hanging
            let _ ← Dion.Network.TxSubmission2.sendTxSubmission2 sock (.MsgReplyTxIds [])
            pure ()
  | _ => pure ()  -- Ignore other TxSubmission2 messages during BlockFetch

/-- Read a single MUX frame from the socket, responding to KeepAlive and
    TxSubmission2 transparently. Returns only BlockFetch frames. -/
private partial def receiveMuxFrame (sock : Socket)
    (mempoolRef : Option (IO.Ref Dion.Network.Mempool.Mempool) := none)
    : IO (Except SocketError (MuxHeader × ByteArray)) := do
  match ← socket_receive_exact sock 8 with
  | .error e => return .error e
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none => return .error (SocketError.ReceiveFailed "Bad MUX header")
      | some header => do
          let payloadSize := header.payloadLength.toNat.toUInt32
          match ← socket_receive_exact sock payloadSize with
          | .error e => return .error e
          | .ok payload =>
              if header.protocolId == .KeepAlive then
                handleKeepAlive sock payload
                receiveMuxFrame sock mempoolRef
              else if header.protocolId == .TxSubmission2 then
                handleTxSubmission2Inline sock payload mempoolRef
                receiveMuxFrame sock mempoolRef
              else if header.protocolId == .BlockFetch then
                return .ok (header, payload)
              else
                -- Skip other frames (ChainSync, PeerSharing)
                receiveMuxFrame sock mempoolRef

/-- Receive BlockFetch message from socket, handling multi-frame messages, KeepAlive,
    and TxSubmission2 transparently. If leftoverBytes is provided, starts decoding from those. -/
def receiveBlockFetch (sock : Socket) (leftoverBytes : ByteArray := ⟨#[]⟩) (maxSize : UInt32 := 65535)
    (mempoolRef : Option (IO.Ref Dion.Network.Mempool.Mempool) := none)
    : IO (Except SocketError (Option BlockFetchReceiveResult)) := do
  -- Start with leftover bytes from previous decode (if any)
  let mut completePayload := leftoverBytes

  -- If we have leftover bytes, try to decode them first
  if leftoverBytes.size > 0 then do
    match decodeBlockFetchMessage leftoverBytes with
    | some result => return .ok (some { message := result.value, leftoverBytes := result.remaining })
    | none => pure ()  -- Fall through to read more frames

  -- Read first frame (KeepAlive and TxSubmission2 handled transparently)
  match ← receiveMuxFrame sock mempoolRef with
  | .error e => return .error e
  | .ok (firstHeader, firstPayload) => do
      let payloadSize := firstHeader.payloadLength.toNat.toUInt32
      if payloadSize > maxSize then return .ok none

      completePayload := completePayload ++ firstPayload

      -- Try to decode - if it succeeds, return it with leftover bytes
      match decodeBlockFetchMessage completePayload with
      | some result => return .ok (some { message := result.value, leftoverBytes := result.remaining })
      | none => do
          -- Message incomplete - likely a multi-frame MsgBlock
          -- Keep reading frames until we can decode or hit limit
          for _ in [0:100] do  -- Max 100 additional frames (blocks can be large)
            match ← receiveMuxFrame sock mempoolRef with
            | .error _ => break
            | .ok (nextHeader, nextPayload) => do
                if nextHeader.protocolId != .BlockFetch then break

                completePayload := completePayload ++ nextPayload
                match decodeBlockFetchMessage completePayload with
                | some result =>
                    return .ok (some { message := result.value, leftoverBytes := result.remaining })
                | none => continue

          -- Final attempt to decode
          match decodeBlockFetchMessage completePayload with
          | some result => return .ok (some { message := result.value, leftoverBytes := result.remaining })
          | none => return .ok none

/-- Request a range of blocks (typically just one block at a time) -/
def requestBlockRange (fromPoint : Point) (toPoint : Point) : BlockFetchMessage :=
  .MsgRequestRange fromPoint toPoint

/-- Request a single block by point -/
def requestSingleBlock (point : Point) : BlockFetchMessage :=
  .MsgRequestRange point point

end Dion.Network.BlockFetch
