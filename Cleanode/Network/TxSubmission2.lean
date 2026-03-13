import Cleanode.Network.Cbor
import Cleanode.Network.Multiplexer
import Cleanode.Network.Socket

/-!
# TxSubmission2 Mini-Protocol

The TxSubmission2 protocol allows nodes to submit transactions to each other.
In node-to-node mode, the initiator (client) announces transaction IDs and
provides full transactions on request from the responder (server).

## Protocol Flow
1. Client sends MsgInit to signal TxSubmission v2
2. Server sends MsgRequestTxIds (blocking or non-blocking) to request tx IDs
3. Client replies with MsgReplyTxIds containing available transaction IDs
4. Server sends MsgRequestTxs for specific transactions it wants
5. Client replies with MsgReplyTxs containing full transaction bytes
6. Server sends MsgDone to terminate

## State Machine
- StInit: Client must send MsgInit
- StIdle: Server can request tx IDs, request txs, or terminate
- StTxIds: Client must reply with tx IDs
- StTxs: Client must reply with full transactions
- StDone: Protocol terminated

## References
- Ouroboros Network Spec Section 3.9 (TxSubmission)
- Protocol number: 4 (node-to-node)
-/

namespace Cleanode.Network.TxSubmission2

open Cleanode.Network.Cbor
open Cleanode.Network.Multiplexer
open Cleanode.Network.Socket

-- ==============
-- = Core Types =
-- ==============

/-- Transaction identifier for the submission protocol -/
structure TxId where
  hash : ByteArray   -- Blake2b-256 of the transaction
  size : UInt32      -- Size of the serialized transaction in bytes
  deriving BEq

instance : Repr TxId where
  reprPrec t _ := s!"TxId(hash={t.hash.size}B, size={t.size})"

-- ===================
-- = Protocol Messages =
-- ===================

/-- TxSubmission2 protocol messages -/
inductive TxSubmission2Message where
  | MsgInit                                                            -- [6] Client init (v2 signal)
  | MsgRequestTxIds (blocking : Bool) (ack : UInt16) (req : UInt16)    -- [0] Server requests tx IDs
  | MsgReplyTxIds (txIds : List TxId)                                  -- [1] Client replies with tx IDs
  | MsgRequestTxs (txIds : List ByteArray)                             -- [2] Server requests full txs
  | MsgReplyTxs (txs : List ByteArray)                                 -- [3] Client replies with full txs
  | MsgDone                                                            -- [4] Terminate protocol

instance : Repr TxSubmission2Message where
  reprPrec msg _ := match msg with
    | .MsgInit => "MsgInit"
    | .MsgRequestTxIds b a r => s!"MsgRequestTxIds(blocking={b}, ack={a}, req={r})"
    | .MsgReplyTxIds ids => s!"MsgReplyTxIds({ids.length} txs)"
    | .MsgRequestTxs ids => s!"MsgRequestTxs({ids.length} hashes)"
    | .MsgReplyTxs txs => s!"MsgReplyTxs({txs.length} txs)"
    | .MsgDone => "MsgDone"

-- ==============
-- = Encoding   =
-- ==============

/-- Encode a TxId as CBOR array [hash, size] -/
def encodeTxId (txId : TxId) : ByteArray :=
  let header := encodeArrayHeader 2
  let hashEnc := encodeBytes txId.hash
  let sizeEnc := encodeUInt txId.size.toNat
  header ++ hashEnc ++ sizeEnc

/-- Encode TxSubmission2 message -/
def encodeTxSubmission2Message : TxSubmission2Message → ByteArray
  | .MsgInit =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 6
      arr ++ msgId
  | .MsgRequestTxIds blocking ack req =>
      let arr := encodeArrayHeader 4
      let msgId := encodeUInt 0
      let blockingEnc := encodeBool blocking
      let ackEnc := encodeUInt ack.toNat
      let reqEnc := encodeUInt req.toNat
      arr ++ msgId ++ blockingEnc ++ ackEnc ++ reqEnc
  | .MsgReplyTxIds txIds =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 1
      let idsArr := encodeArrayHeader txIds.length
      let idsEnc := txIds.foldl (fun acc id => acc ++ encodeTxId id) ⟨#[]⟩
      arr ++ msgId ++ idsArr ++ idsEnc
  | .MsgRequestTxs txIds =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 2
      let idsArr := encodeArrayHeader txIds.length
      let idsEnc := txIds.foldl (fun acc id => acc ++ encodeBytes id) ⟨#[]⟩
      arr ++ msgId ++ idsArr ++ idsEnc
  | .MsgReplyTxs txs =>
      let arr := encodeArrayHeader 2
      let msgId := encodeUInt 3
      let txsArr := encodeArrayHeader txs.length
      let txsEnc := txs.foldl (fun acc tx => acc ++ encodeBytes tx) ⟨#[]⟩
      arr ++ msgId ++ txsArr ++ txsEnc
  | .MsgDone =>
      let arr := encodeArrayHeader 1
      let msgId := encodeUInt 4
      arr ++ msgId

-- ==============
-- = Decoding   =
-- ==============

/-- Decode a TxId from CBOR -/
def decodeTxId (bs : ByteArray) : Option (DecodeResult TxId) := do
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none
  let r2 ← decodeBytes r1.remaining
  let hash := r2.value
  let r3 ← decodeUInt r2.remaining
  let size := UInt32.ofNat r3.value
  some { value := { hash := hash, size := size }, remaining := r3.remaining }

/-- Decode a list of TxIds -/
def decodeTxIdList (bs : ByteArray) : Option (DecodeResult (List TxId)) := do
  let r1 ← decodeArrayHeader bs
  let count := r1.value
  let mut remaining := r1.remaining
  let mut txIds : List TxId := []
  for _ in List.range count do
    let r ← decodeTxId remaining
    txIds := txIds ++ [r.value]
    remaining := r.remaining
  some { value := txIds, remaining := remaining }

/-- Decode a list of ByteArrays (tx hashes or full txs) -/
def decodeByteArrayList (bs : ByteArray) : Option (DecodeResult (List ByteArray)) := do
  let r1 ← decodeArrayHeader bs
  let count := r1.value
  let mut remaining := r1.remaining
  let mut items : List ByteArray := []
  for _ in List.range count do
    let r ← decodeBytes remaining
    items := items ++ [r.value]
    remaining := r.remaining
  some { value := items, remaining := remaining }

/-- Decode TxSubmission2 message -/
def decodeTxSubmission2Message (bs : ByteArray) : Option TxSubmission2Message := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining

  match r2.value with
  | 6 => if r1.value == 1 then some .MsgInit else none
  | 0 => do  -- MsgRequestTxIds
      if r1.value != 4 then none
      let r3 ← decodeBool r2.remaining
      let blocking := r3.value
      let r4 ← decodeUInt r3.remaining
      let ack := UInt16.ofNat r4.value
      let r5 ← decodeUInt r4.remaining
      let req := UInt16.ofNat r5.value
      some (.MsgRequestTxIds blocking ack req)
  | 1 => do  -- MsgReplyTxIds
      if r1.value != 2 then none
      let r3 ← decodeTxIdList r2.remaining
      some (.MsgReplyTxIds r3.value)
  | 2 => do  -- MsgRequestTxs
      if r1.value != 2 then none
      let r3 ← decodeByteArrayList r2.remaining
      some (.MsgRequestTxs r3.value)
  | 3 => do  -- MsgReplyTxs
      if r1.value != 2 then none
      let r3 ← decodeByteArrayList r2.remaining
      some (.MsgReplyTxs r3.value)
  | 4 => if r1.value == 1 then some .MsgDone else none
  | _ => none

-- ==============
-- = Client API =
-- ==============

/-- Send TxSubmission2 message over socket -/
def sendTxSubmission2 (sock : Socket) (msg : TxSubmission2Message) : IO (Except SocketError Unit) := do
  let payload := encodeTxSubmission2Message msg
  let frame ← createFrame .TxSubmission2 .Initiator payload
  let frameBytes := encodeMuxFrame frame
  socket_send sock frameBytes

/-- Receive TxSubmission2 message from socket -/
def receiveTxSubmission2 (sock : Socket) : IO (Except SocketError (Option TxSubmission2Message)) := do
  match ← socket_receive sock 8 with
  | .error e => return .error e
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none => return .ok none
      | some header => do
          match ← socket_receive sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error e
          | .ok payload => return .ok (decodeTxSubmission2Message payload)

/-- Create the initial MsgInit message (sent immediately after handshake) -/
def msgInit : TxSubmission2Message := .MsgInit

/-- Create a reply with no transaction IDs (empty mempool) -/
def emptyTxIds : TxSubmission2Message := .MsgReplyTxIds []

/-- Create a reply with no transactions -/
def emptyTxs : TxSubmission2Message := .MsgReplyTxs []

end Cleanode.Network.TxSubmission2
