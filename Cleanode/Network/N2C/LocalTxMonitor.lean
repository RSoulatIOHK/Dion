import Cleanode.Network.Cbor
import Cleanode.Network.N2C.Mux
import Cleanode.Network.Mempool
import Cleanode.Ledger.State

/-!
# LocalTxMonitor Mini-Protocol (N2C Protocol 9)

Allows clients to monitor the mempool: iterate transactions, check membership,
and query sizes.

## Messages
- MsgAcquire: [0]                  — client acquires mempool snapshot
- MsgAcquired(slot): [1, slot]     — server confirms with current slot
- MsgNextTx: [3]                   — client requests next tx
- MsgReplyNextTx(tx): [4, maybeTx] — server responds ([] or [eraId, tx])
- MsgHasTx(txId): [5, txId]       — client checks tx presence
- MsgReplyHasTx(bool): [6, bool]  — server responds
- MsgGetSizes: [7]                 — client queries mempool sizes
- MsgReplyGetSizes: [8, [n,b,s]]  — server responds with counts
- MsgRelease: [9]                  — client releases snapshot
- MsgDone: [10]                    — client is done

## References
- Ouroboros Network Spec Section 3.14
-/

namespace Cleanode.Network.N2C.LocalTxMonitor

open Cleanode.Network.Cbor
open Cleanode.Network.N2C.Mux
open Cleanode.Network.N2C.MiniProtocolId
open Cleanode.Network.Multiplexer (Mode)
open Cleanode.Network.Socket
open Cleanode.Network.Mempool

-- ====================
-- = Types            =
-- ====================

/-- Snapshot of mempool at time of acquire -/
structure MempoolSnapshot where
  entries : List MempoolEntry
  slot : Nat
  remaining : List MempoolEntry  -- entries not yet iterated

-- ====================
-- = Decoding         =
-- ====================

/-- Decode a client message -/
partial def decodeTxMonitorMessage (bs : ByteArray) : Option Nat := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining
  some r2.value

-- ====================
-- = Response Encoding =
-- ====================

/-- MsgAcquired(slot): [1, slot] -/
def encodeMsgAcquired (slot : Nat) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt 1 ++ encodeUInt slot

/-- MsgReplyNextTx with no transaction: [4, []] -/
def encodeMsgReplyNextTxNone : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt 4 ++ encodeArrayHeader 0

/-- MsgReplyNextTx with transaction: [4, [eraId, txBytes]] -/
def encodeMsgReplyNextTxSome (eraId : Nat) (txBytes : ByteArray) : ByteArray :=
  let inner := encodeArrayHeader 2 ++ encodeUInt eraId ++ encodeBytes txBytes
  encodeArrayHeader 2 ++ encodeUInt 4 ++ inner

/-- MsgReplyHasTx: [6, bool] -/
def encodeMsgReplyHasTx (has : Bool) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt 6 ++ encodeBool has

/-- MsgReplyGetSizes: [8, [numTxs, totalBytes, numBytes]] -/
def encodeMsgReplyGetSizes (numTxs totalBytes : Nat) : ByteArray :=
  let sizes := encodeArrayHeader 3 ++ encodeUInt numTxs ++ encodeUInt totalBytes ++ encodeUInt totalBytes
  encodeArrayHeader 2 ++ encodeUInt 8 ++ sizes

-- ====================
-- = Frame Handler    =
-- ====================

/-- Handle one LocalTxMonitor frame. Returns (continue, updatedSnapshot). -/
def handleTxMonitorFrame (sock : Socket) (payload : ByteArray)
    (mempoolRef : IO.Ref Mempool) (snapshot : Option MempoolSnapshot)
    (ledgerStateRef : IO.Ref Cleanode.Ledger.State.LedgerState)
    : IO (Bool × Option MempoolSnapshot) := do
  match decodeTxMonitorMessage payload with
  | none =>
    IO.eprintln "N2C: Failed to decode LocalTxMonitor message"
    return (false, snapshot)
  | some msgId =>
    match msgId with
    | 0 => do  -- MsgAcquire
      let mempool ← mempoolRef.get
      let snap : MempoolSnapshot := {
        entries := mempool.entries
        slot := (← ledgerStateRef.get).lastSlot
        remaining := mempool.entries
      }
      let response := encodeMsgAcquired snap.slot
      match ← sendN2CPayload sock .LocalTxMonitor .Responder response with
      | .error _ => return (false, none)
      | .ok () => return (true, some snap)
    | 3 => do  -- MsgNextTx
      match snapshot with
      | none => do
        let response := encodeMsgReplyNextTxNone
        match ← sendN2CPayload sock .LocalTxMonitor .Responder response with
        | .error _ => return (false, snapshot)
        | .ok () => return (true, snapshot)
      | some snap =>
        match snap.remaining with
        | [] => do
          let response := encodeMsgReplyNextTxNone
          match ← sendN2CPayload sock .LocalTxMonitor .Responder response with
          | .error _ => return (false, some snap)
          | .ok () => return (true, some snap)
        | entry :: rest => do
          let response := encodeMsgReplyNextTxSome 6 entry.rawBytes  -- era 6 = Conway
          match ← sendN2CPayload sock .LocalTxMonitor .Responder response with
          | .error _ => return (false, some snap)
          | .ok () => return (true, some { snap with remaining := rest })
    | 5 => do  -- MsgHasTx (followed by txId bytes)
      -- Simplified: always say false for now
      let response := encodeMsgReplyHasTx false
      match ← sendN2CPayload sock .LocalTxMonitor .Responder response with
      | .error _ => return (false, snapshot)
      | .ok () => return (true, snapshot)
    | 7 => do  -- MsgGetSizes
      let mempool ← mempoolRef.get
      let response := encodeMsgReplyGetSizes mempool.entries.length mempool.totalBytes
      match ← sendN2CPayload sock .LocalTxMonitor .Responder response with
      | .error _ => return (false, snapshot)
      | .ok () => return (true, snapshot)
    | 9 => do  -- MsgRelease
      return (true, none)
    | 10 =>    -- MsgDone
      return (false, none)
    | _ => do
      IO.eprintln s!"N2C: Unknown LocalTxMonitor message ID: {msgId}"
      return (true, snapshot)

end Cleanode.Network.N2C.LocalTxMonitor
