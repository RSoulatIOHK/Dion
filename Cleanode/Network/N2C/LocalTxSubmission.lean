import Dion.Network.Cbor
import Dion.Network.CborCursor
import Dion.Network.N2C.Mux
import Dion.Network.Mempool
import Dion.Network.ConwayBlock
import Dion.Network.Crypto

/-!
# LocalTxSubmission Mini-Protocol (N2C Protocol 6)

Allows `cardano-cli transaction submit` to submit transactions directly
to Dion's mempool via the local Unix socket.

## Message Flow
1. Client sends MsgSubmitTx(eraId, txBytes)
2. Server responds MsgAcceptTx or MsgRejectTx(reason)
3. Repeat until client sends MsgDone

## CBOR Encoding
- MsgSubmitTx: [0, [eraId, txBytes]]
- MsgAcceptTx: [1]
- MsgRejectTx: [2, [eraId, [reason]]]
- MsgDone: [3]

## References
- Ouroboros Network Spec Section 3.12
-/

namespace Dion.Network.N2C.LocalTxSubmission

open Dion.Network.Cbor
open Dion.Network.N2C.Mux
open Dion.Network.N2C.MiniProtocolId
open Dion.Network.Multiplexer (Mode)
open Dion.Network.Socket

-- ====================
-- = Types            =
-- ====================

/-- LocalTxSubmission messages -/
inductive LocalTxSubmissionMessage where
  | MsgSubmitTx (eraId : Nat) (txBytes : ByteArray)
  | MsgAcceptTx
  | MsgRejectTx (reason : String)
  | MsgDone

-- ====================
-- = Encoding         =
-- ====================

/-- Encode a server response -/
def encodeLocalTxSubmissionMessage : LocalTxSubmissionMessage → ByteArray
  | .MsgAcceptTx =>
    -- [1]
    encodeArrayHeader 1 ++ encodeUInt 1
  | .MsgRejectTx reason =>
    -- [2, [eraId, [reasonTag, reasonText]]]
    -- Simplified: we send a Conway-era (6) rejection with text reason
    let rejectReason := encodeArrayHeader 2 ++ encodeUInt 0 ++ encodeBytes reason.toUTF8
    let eraWrapped := encodeArrayHeader 2 ++ encodeUInt 6 ++ rejectReason
    encodeArrayHeader 2 ++ encodeUInt 2 ++ eraWrapped
  | .MsgDone =>
    encodeArrayHeader 1 ++ encodeUInt 3
  | .MsgSubmitTx _ _ =>
    -- Client-only message, server shouldn't encode this
    ByteArray.empty

-- ====================
-- = Decoding         =
-- ====================

/-- Decode a client message -/
partial def decodeLocalTxSubmissionMessage (bs : ByteArray)
    : Option LocalTxSubmissionMessage := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining
  match r2.value with
  | 0 => do  -- MsgSubmitTx: [0, [eraId, txCBOR]]
    let r3 ← decodeArrayHeader r2.remaining
    if r3.value != 2 then none
    let r4 ← decodeUInt r3.remaining  -- eraId
    -- Remaining bytes are the raw CBOR-encoded transaction
    some (.MsgSubmitTx r4.value r4.remaining)
  | 3 =>    -- MsgDone
    some .MsgDone
  | _ => none

-- ====================
-- = Server Loop      =
-- ====================

/-- Handle one LocalTxSubmission frame. Returns false on MsgDone. -/
def handleTxSubmissionFrame (sock : Socket) (payload : ByteArray)
    (mempoolRef : IO.Ref Dion.Network.Mempool.Mempool)
    : IO Bool := do
  match decodeLocalTxSubmissionMessage payload with
  | none =>
    IO.eprintln "N2C: Failed to decode LocalTxSubmission message"
    return false
  | some .MsgDone =>
    return false
  | some (.MsgSubmitTx _eraId txBytes) => do
    -- Extract tx body bytes for proper hashing.
    -- A Cardano tx is CBOR: [body, witnesses, is_valid, aux_data?]
    -- The tx ID = blake2b_256(body_bytes)
    -- Debug: dump first 32 bytes
    let hexDump := txBytes.toList.take 32 |>.map fun b =>
      let hi := b.toNat / 16
      let lo := b.toNat % 16
      let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
      String.mk [toHex hi, toHex lo]
    IO.eprintln s!"N2C: txBytes ({txBytes.size}B): {String.intercalate " " hexDump}"
    -- Unwrap CBOR tag 24 + byte string wrapper if present
    -- Tag 24 = "encoded CBOR data item": d8 18 <bytestring>
    let innerTxBytes ← do
      let c := Dion.Network.CborCursor.Cursor.mk' txBytes
      -- Check for tag (major type 6)
      let firstByte := txBytes[0]!.toNat
      if firstByte / 32 == 6 then
        -- It's a CBOR tag — skip it, then decode the byte string inside
        match Dion.Network.CborCursor.skipTag c with
        | some r =>
          match Dion.Network.CborCursor.decodeBytes r.cursor with
          | some r2 => pure r2.value
          | none => pure txBytes  -- fallback
        | none => pure txBytes  -- fallback
      else
        pure txBytes
    let cursor := Dion.Network.CborCursor.Cursor.mk' innerTxBytes
    match Dion.Network.CborCursor.decodeArrayHeader cursor with
    | none =>
      let fb := innerTxBytes[0]!.toNat
      IO.eprintln s!"N2C: Failed to decode transaction array header (first byte: 0x{fb})"
      let response := encodeLocalTxSubmissionMessage (.MsgRejectTx "invalid CBOR")
      let _ ← sendN2CPayload sock .LocalTxSubmission .Responder response
      return true
    | some r => do
      let bodyStart := r.cursor.pos
      match Dion.Network.CborCursor.skipValue r.cursor with
      | none =>
        IO.eprintln "N2C: Failed to skip tx body"
        let response := encodeLocalTxSubmissionMessage (.MsgRejectTx "invalid tx body")
        let _ ← sendN2CPayload sock .LocalTxSubmission .Responder response
        return true
      | some afterBody => do
        let bodyBytes := innerTxBytes.extract bodyStart afterBody.pos
        let txHash ← Dion.Network.Crypto.blake2b_256 bodyBytes
        let mempool ← mempoolRef.get
        -- Check for duplicates
        if mempool.contains txHash then
          IO.println s!"N2C: Transaction already in mempool"
          let response := encodeLocalTxSubmissionMessage .MsgAcceptTx
          let _ ← sendN2CPayload sock .LocalTxSubmission .Responder response
          return true
        -- Parse the tx body to extract real inputs (needed for mempool eviction on block apply)
        let parsedBody := Dion.Network.ConwayBlock.parseTransactionBodyC
          (Dion.Network.CborCursor.Cursor.mk' bodyBytes) |>.map (·.value)
        let txBody : Dion.Network.ConwayBlock.TransactionBody := match parsedBody with
          | some b => b
          | none   => { inputs := [], outputs := [], fee := 0, certificates := [], rawBytes := bodyBytes }
        let dummyTx : Dion.Network.ConwayBlock.Transaction := {
          body := txBody
          witnesses := { redeemers := [] }
        }
        let entry : Dion.Network.Mempool.MempoolEntry := {
          txHash := txHash
          transaction := dummyTx
          rawBytes := innerTxBytes  -- unwrapped tx for N2N relay
          addedAt := 0
          size := innerTxBytes.size
        }
        mempoolRef.set { mempool with
          entries := entry :: mempool.entries
          totalBytes := mempool.totalBytes + innerTxBytes.size
        }
        let response := encodeLocalTxSubmissionMessage .MsgAcceptTx
        match ← sendN2CPayload sock .LocalTxSubmission .Responder response with
        | .error _ => return false
        | .ok () =>
          -- Log the tx hash in hex
          let hexHash := txHash.toList.map fun b =>
            let hi := b.toNat / 16
            let lo := b.toNat % 16
            let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
            String.mk [toHex hi, toHex lo]
          let hashStr := String.join hexHash
          IO.println s!"N2C: Accepted tx {hashStr} ({txBytes.size} bytes, body={bodyBytes.size}B)"
          return true
  | some (.MsgRejectTx _) => return true  -- Server shouldn't receive this
  | some .MsgAcceptTx => return true      -- Server shouldn't receive this

end Dion.Network.N2C.LocalTxSubmission
