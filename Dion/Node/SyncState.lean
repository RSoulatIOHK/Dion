import Dion.Network.Basic
import Dion.Network.Socket
import Dion.Network.Handshake
import Dion.Network.Multiplexer
import Dion.Network.ChainSync
import Dion.Network.Byron
import Dion.Network.Shelley
import Dion.Network.BlockFetch
import Dion.Network.BlockFetchClient
import Dion.Network.ConwayBlock
import Dion.Network.Crypto
import Dion.Network.Bech32
import Dion.Network.TxSubmission2
import Dion.Network.Mempool
import Dion.Network.PeerSharing
import Dion.Network.PeerDb
import Dion.Network.PeerConnection
import Dion.Network.ConnectionManager
import Dion.Network.MuxDispatcher
import Dion.Network.HandshakeServer
import Dion.Config.Topology
import Dion.Config.Genesis
import Dion.Storage.BlockStore
import Dion.Storage.ChainDB
import Dion.Storage.Database
import Dion.Consensus.Praos.LeaderElection
import Dion.Consensus.Praos.ConsensusState
import Dion.Crypto.VRF.ECVRF
import Dion.Crypto.Sign.Ed25519.Signature
import Std.Sync
import Pigment
import Dion.TUI.State
import Dion.TUI.Render
import Dion.Mithril.Types
import Dion.Mithril.Client
import Dion.Mithril.Replay
import Dion.CLI.Args
import Dion.CLI.Query
import Dion.Monitoring.Server
import Dion.Consensus.Praos.SPOKeys
import Dion.Consensus.Praos.ForgeLoop
import Dion.Consensus.Praos.BlockAnnounce
import Dion.Network.N2C.Server
import Dion.Ledger.State
import Dion.Ledger.Certificate
import Dion.Ledger.Snapshot
import Dion.Consensus.Praos.StakeDistribution

open Dion.Network
open Dion.Network.Socket
open Dion.Network.Handshake
open Dion.Network.Multiplexer
open Dion.Network.ChainSync
open Dion.Network.Byron
open Dion.Network.Shelley
open Dion.Network.BlockFetch
open Dion.Network.Crypto
open Dion.Network.Bech32
open Dion.Network.BlockFetchClient
open Dion.Network.ConwayBlock
open Dion.Network.TxSubmission2
open Dion.Network.Mempool
open Dion.Network.PeerDb
open Dion.Network.PeerConnection
open Dion.Network.ConnectionManager
open Dion.Network.MuxDispatcher
open Dion.Network.PeerSharing
open Dion.Network.HandshakeServer
open Dion.Config.Topology
open Dion.Config.Genesis
open Dion.Storage.BlockStore
open Dion.Storage.ChainDB
open Dion.Storage.Database
open Dion.Consensus.Praos.LeaderElection
open Dion.Consensus.Praos.ConsensusState
open Pigment
open Dion.TUI.State
open Dion.TUI.Render

namespace Dion.Node

/-- Atomically check if a key is in the set; if not, add it.
    Returns true if the key was already present (= duplicate).
    Uses swap-based spinlock: `none` means locked, `some list` means unlocked.
    Thread takes ownership by swapping in `none`, then puts the list back. -/
partial def atomicCheckAndMark (ref : IO.Ref (Option (List Nat))) (key : Nat) : IO Bool := do
  -- Acquire: spin until we swap out a `some`
  let rec acquire : IO (List Nat) := do
    match ← ref.swap none with
    | some list => return list
    | none => acquire  -- Another thread holds it, retry
  let list ← acquire
  let alreadySeen := list.contains key
  let newList := if alreadySeen then list else key :: list
  -- Release: put the list back
  ref.set (some newList)
  return alreadySeen

/-- Shared peer discovery state (plain data only — safe across threads) -/
structure DiscoveryState where
  discovered : List (String × UInt16)  -- Queue of newly found peers
  known      : List (String × UInt16)  -- All peers we've seen

/-- Encode a KeepAlive response (MsgKeepAliveResponse) -/
def encodeKeepAliveResponse (cookie : UInt16) : ByteArray :=
  -- KeepAlive response: CBOR array [1, cookie] where 1 = MsgKeepAliveResponse
  -- [1, cookie] in CBOR: 82 01 19 XXXX (for 2-byte cookie)
  let cookieHi := UInt8.ofNat ((cookie >>> 8).toNat % 256)
  let cookieLo := UInt8.ofNat (cookie.toNat % 256)
  ⟨#[0x82, 0x01, 0x19, cookieHi, cookieLo]⟩

/-- Send a KeepAlive response back over the multiplexer -/
def sendKeepAliveResponse (sock : Socket) (cookie : UInt16) : IO Unit := do
  let payload := encodeKeepAliveResponse cookie
  let frame ← createFrame .KeepAlive .Initiator payload
  let _ ← socket_send sock (encodeMuxFrame frame)
  pure ()

/-- Extract cookie from KeepAlive payload: CBOR [0, cookie] -/
def extractKeepAliveCookie (payload : ByteArray) : Option UInt16 :=
  -- MsgKeepAlive = [0, cookie] in CBOR
  -- 82 00 19 HH LL  (cookie as 2-byte uint)
  -- 82 00 0X        (cookie as small uint)
  if payload.size >= 3 && payload[0]! == 0x82 && payload[1]! == 0x00 then
    if payload.size >= 5 && payload[2]! == 0x19 then
      some (UInt16.ofNat (payload[3]!.toNat * 256 + payload[4]!.toNat))
    else
      some (UInt16.ofNat payload[2]!.toNat)
  else
    none

/-- Send KeepAlive response as responder (for inbound connections) -/
def sendKeepAliveResponseInbound (sock : Socket) (cookie : UInt16) : IO Unit := do
  let payload := encodeKeepAliveResponse cookie
  let frame ← createFrame .KeepAlive .Responder payload
  let _ ← socket_send sock (encodeMuxFrame frame)
  pure ()

/-- Poll a responder queue until a frame payload arrives -/
partial def pollResponderQueue (q : IO.Ref (List ByteArray)) : IO ByteArray := do
  let item ← q.modifyGet fun items =>
    match items with
    | [] => (none, [])
    | x :: rest => (some x, rest)
  match item with
  | some payload => return payload
  | none => IO.sleep 100; pollResponderQueue q

/-- TxSubmission2 responder loop: request txs from a peer's mempool.
    Runs as a background task per peer. We act as the server, requesting
    transaction IDs and bodies from the peer's mempool.
    Starts by waiting for peer's MsgInit, then sends our MsgInit + first request. -/
partial def txSubmResponderLoop (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (responderQueue : IO.Ref (List ByteArray))
    (tuiRef : Option (IO.Ref TUIState) := none)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (ackedSoFar : Nat := 0) (initialized : Bool := false) : IO Unit := do
  if !initialized then
    -- Wait for peer's MsgInit on the responder instance
    let initPayload ← pollResponderQueue responderQueue
    match decodeTxSubmission2Message initPayload with
    | some .MsgInit =>
        -- Respond with our MsgInit, then start requesting txs
        let _ ← sendTxSubmission2Responder sock .MsgInit
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0 true
    | _ =>
        -- Not MsgInit yet, retry
        IO.sleep 500
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0 false
    return
  -- Request tx IDs from peer (blocking on first call, non-blocking on subsequent)
  let blocking := ackedSoFar == 0
  let ack := UInt16.ofNat ackedSoFar
  let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds blocking ack 10)
  -- Wait for MsgReplyTxIds routed from receiveChainSyncFrame
  let replyPayload ← pollResponderQueue responderQueue
  match decodeTxSubmission2Message replyPayload with
  | some (.MsgReplyTxIds txIds) =>
      if txIds.isEmpty then
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0
      else
        let pool ← mempoolRef.get
        let wanted := txIds.filter fun tid => !pool.contains tid.hash
        if wanted.isEmpty then
          txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef txIds.length
        else
          let _ ← sendTxSubmission2Responder sock (.MsgRequestTxs (wanted.map (·.hash)))
          let txReplyPayload ← pollResponderQueue responderQueue
          match decodeTxSubmission2Message txReplyPayload with
          | some (.MsgReplyTxs txBodies) => do
              let now ← Dion.TUI.Render.nowMs
              let slot ← match ledgerStateRef with
                | some r => do let ls ← r.atomically (fun ref => ref.get); pure ls.lastSlot
                | none => pure 0
              for txBytes in txBodies do
                let pool ← mempoolRef.get
                let result ← match ledgerStateRef with
                  | some lsRef => pool.addTxValidated txBytes now (← lsRef.atomically (fun ref => ref.get)) slot
                  | none => pool.addTxRaw txBytes now
                match result with
                | .ok newPool => mempoolRef.set newPool
                | .error _ => pure ()
              if let some tRef := tuiRef then
                let pool ← mempoolRef.get
                tRef.modify (·.updateMempool pool.entries.length pool.totalBytes)
              txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef txIds.length
          | _ =>
              IO.sleep 1000
              txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0
  | _ =>
      IO.sleep 1000
      txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0

/-- Receive a ChainSync MUX frame, handling KeepAlive transparently.
    Uses exact reads: 8 bytes for header, then payloadLength bytes for payload.
    Skips TxSubmission2 frames and handles PeerSharing responses inline.
    Loops until a ChainSync frame arrives or an error occurs. -/
partial def receiveChainSyncFrame (sock : Socket)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (txSubmPeerRef : Option (IO.Ref TxSubmPeerState) := none)
    (txSubmResponderQueue : Option (IO.Ref (List ByteArray)) := none)
    (selfAddr : Option Dion.Network.PeerSharing.PeerAddress := none)
    : IO (Except String MuxFrame) := do
  -- Check if there's a pending blocking MsgRequestTxIds to flush
  let hasPending ← match mempoolRef, txSubmPeerRef with
    | some mpRef, some peerRef => do
      let peerSt ← peerRef.get
      match peerSt.pendingBlockingReq with
      | some reqCount => do
        let pool ← mpRef.get
        let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
        if !txIds.isEmpty then
          IO.eprintln s!"[TxSub] → Flushing deferred blocking reply ({txIds.length} txs)"
          let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
          let hashes := txIds.map (·.hash)
          peerRef.modify fun s =>
            { s with announcedTxIds := s.announcedTxIds ++ hashes, pendingBlockingReq := none }
          pure false
        else pure true  -- still pending
      | none => pure false
    | _, _ => pure false
  -- Read MUX header: use 1s timeout when there's a pending tx request so we
  -- can re-check the mempool periodically, otherwise block indefinitely
  let headerResult ← if hasPending then
    match ← socket_receive_exact_timeout sock 8 1000 with
    | .error e => pure (Except.error s!"Failed to receive MUX header: {e}")
    | .ok none => do
      -- Timeout: loop back to re-check mempool
      return ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr
    | .ok (some bytes) => pure (Except.ok bytes)
  else
    -- 60s timeout: if a peer goes silent (TCP dead), detect it and reconnect
    match ← socket_receive_exact_timeout sock 8 60000 with
    | .error e => pure (Except.error s!"Failed to receive MUX header: {e}")
    | .ok none => pure (Except.error "Peer timeout (60s no data) — reconnecting")
    | .ok (some bytes) => pure (Except.ok bytes)
  match headerResult with
  | .error e => return .error e
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none =>
          -- Unknown protocol: skip the payload bytes and continue
          let payloadLen := (headerBytes.get! 6).toNat * 256 + (headerBytes.get! 7).toNat
          if payloadLen > 0 then
            let _ ← socket_receive_exact sock payloadLen.toUInt32
          receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr
      | some header => do
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error s!"Failed to receive payload: {e}"
          | .ok payload => do
              if header.protocolId == .KeepAlive then
                -- Auto-respond to KeepAlive
                match extractKeepAliveCookie payload with
                | some cookie => sendKeepAliveResponse sock cookie
                | none => pure ()
                receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr
              else if header.protocolId == .TxSubmission2 then
                let modeStr := if header.mode == .Initiator then "Init" else "Resp"
                let msgDesc := match decodeTxSubmission2Message payload with
                  | some .MsgInit => "MsgInit"
                  | some (.MsgRequestTxIds b a r) => s!"MsgRequestTxIds(blk={b},ack={a},req={r})"
                  | some (.MsgRequestTxs h) => s!"MsgRequestTxs(n={h.length})"
                  | some (.MsgReplyTxIds t) => s!"MsgReplyTxIds(n={t.length})"
                  | some (.MsgReplyTxs t) => s!"MsgReplyTxs(n={t.length})"
                  | some .MsgDone => "MsgDone"
                  | none => s!"DECODE_FAILED(size={payload.size})"
                IO.eprintln s!"[TxSub] mode={modeStr} {msgDesc}"
                match decodeTxSubmission2Message payload with
                  | some .MsgInit => do
                      -- Peer acknowledged our MsgInit — our MsgRequestTxIds was already sent
                      IO.eprintln s!"[TxSub] → Peer MsgInit ack (mode={modeStr}), pull already in flight"
                  | some (.MsgRequestTxIds blocking ack req) => do
                      IO.eprintln s!"[TxSub] → Peer requesting our txs (mode={modeStr}, mpRef={mempoolRef.isSome}, peerRef={txSubmPeerRef.isSome})"
                      match mempoolRef, txSubmPeerRef with
                      | some mpRef, some peerRef => do
                          let acked := ack.toNat
                          peerRef.modify fun s =>
                            { s with announcedTxIds := s.announcedTxIds.drop acked }
                          let reqCount := req.toNat
                          let pool ← mpRef.get
                          IO.eprintln s!"[TxSub] → Mempool has {pool.entries.length} txs, blocking={blocking}, req={reqCount}"
                          if blocking then
                            let peerSt ← peerRef.get
                            let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
                            if !txIds.isEmpty then
                              -- Debug: dump the encoded payload
                              let payload := encodeTxSubmission2Message (.MsgReplyTxIds txIds)
                              let hexPayload := payload.toList.take 48 |>.map fun b =>
                                let hi := b.toNat / 16
                                let lo := b.toNat % 16
                                let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
                                String.mk [toHex hi, toHex lo]
                              IO.eprintln s!"[TxSub] → Replying with {txIds.length} tx IDs immediately"
                              for tid in txIds do
                                let hh := tid.hash.toList.take 8 |>.map fun b =>
                                  let hi := b.toNat / 16
                                  let lo := b.toNat % 16
                                  let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
                                  String.mk [toHex hi, toHex lo]
                                IO.eprintln s!"[TxSub]   txId hash={String.join hh}... size={tid.size}"
                              IO.eprintln s!"[TxSub]   CBOR({payload.size}B): {String.intercalate " " hexPayload}"
                              let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
                              let hashes := txIds.map (·.hash)
                              peerRef.modify fun s =>
                                { s with announcedTxIds := s.announcedTxIds ++ hashes }
                            else
                              -- Mempool empty: store pending request, will be flushed by receive loop
                              IO.eprintln s!"[TxSub] → Blocking request deferred (mempool empty, need {reqCount})"
                              peerRef.modify fun s =>
                                { s with pendingBlockingReq := some reqCount }
                          else
                            let peerSt ← peerRef.get
                            let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
                            IO.eprintln s!"[TxSub] → Non-blocking reply with {txIds.length} tx IDs"
                            let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
                            let hashes := txIds.map (·.hash)
                            peerRef.modify fun s =>
                              { s with announcedTxIds := s.announcedTxIds ++ hashes }
                      | _, _ =>
                          IO.eprintln s!"[TxSub] → WARNING: no mempool/peer refs! blocking={blocking}"
                          if !blocking then
                            let _ ← sendTxSubmission2 sock (.MsgReplyTxIds [])
                            pure ()
                  | some (.MsgRequestTxs hashes) => do
                      -- Mode Initiator: peer requesting full tx bodies from us (instance 0)
                      IO.eprintln s!"[TxSub] → Peer requesting {hashes.length} tx bodies"
                      for h in hashes do
                        let hexH := h.toList.take 8 |>.map fun b =>
                          let hi := b.toNat / 16; let lo := b.toNat % 16
                          let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
                          String.mk [toHex hi, toHex lo]
                        IO.eprintln s!"[TxSub]   requested hash: {String.intercalate "" hexH}... ({h.size} bytes)"
                      match mempoolRef with
                      | some mpRef => do
                          let pool ← mpRef.get
                          IO.eprintln s!"[TxSub]   mempool has {pool.entries.length} entries"
                          let txBodies := pool.getTxsByHash hashes
                          IO.eprintln s!"[TxSub]   → Sending {txBodies.length} tx bodies in MsgReplyTxs"
                          for txB in txBodies do
                            IO.eprintln s!"[TxSub]     tx size={txB.size}"
                          let _ ← sendTxSubmission2 sock (.MsgReplyTxs txBodies)
                      | none =>
                          IO.eprintln "[TxSub]   → No mempool ref!"
                          let _ ← sendTxSubmission2 sock (.MsgReplyTxs [])
                          pure ()
                  | some (.MsgReplyTxIds txIds) => do
                      -- Peer replied to OUR MsgRequestTxIds (we're pulling from them)
                      IO.eprintln s!"[TxSub] → Peer announced {txIds.length} tx IDs (mode={modeStr})"
                      if txIds.isEmpty then
                        IO.eprintln "[TxSub] → Empty, re-requesting (blocking)"
                        let _ ← sendTxSubmission2 sock (.MsgRequestTxIds true 0 10)
                      else
                        match mempoolRef with
                        | some mpRef => do
                            let pool ← mpRef.get
                            let wanted := txIds.filter fun tid => !pool.contains tid.hash
                            IO.eprintln s!"[TxSub] → Want {wanted.length}/{txIds.length}"
                            if wanted.isEmpty then
                              let _ ← sendTxSubmission2 sock
                                (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                            else
                              let _ ← sendTxSubmission2 sock
                                (.MsgRequestTxs (wanted.map (·.hash)))
                        | none =>
                            IO.eprintln "[TxSub] → No mempoolRef for MsgReplyTxIds!"
                            let _ ← sendTxSubmission2 sock
                              (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                  | some (.MsgReplyTxs txBodies) => do
                      IO.eprintln s!"[TxSub] → Received {txBodies.length} tx bodies! (mode={modeStr})"
                      match mempoolRef with
                      | some mpRef => do
                          let now ← Dion.TUI.Render.nowMs
                          for txBytes in txBodies do
                            let pool ← mpRef.get
                            match ← pool.addTxRaw txBytes now with
                            | .ok newPool =>
                                mpRef.set newPool
                                IO.eprintln s!"[TxSub] → addTxRaw OK (size={txBytes.size})"
                            | .error e =>
                                IO.eprintln s!"[TxSub] → addTxRaw FAILED: {e}"
                          let _ ← sendTxSubmission2 sock
                            (.MsgRequestTxIds false (UInt16.ofNat txBodies.length) 10)
                      | none =>
                          IO.eprintln "[TxSub] → No mempoolRef for MsgReplyTxs!"
                  | some .MsgDone =>
                      IO.eprintln "[TxSub] → MsgDone"
                  | none =>
                      IO.eprintln s!"[TxSub] → DECODE FAILED (size={payload.size})"
                receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr
              else if header.protocolId == .PeerSharing then
                -- Handle PeerSharing inline: respond to requests AND process responses
                match decodePeerSharingMessage payload with
                | some (.MsgShareRequest amount) => do
                    -- Peer wants our known peers. Respond with selfAddr + some discovered peers.
                    let knownPeers ← match discoveryRef with
                      | some dRef => do
                          let ds ← dRef.get
                          pure (ds.discovered.take amount.toNat |>.map fun (h, p) =>
                            (Dion.Network.PeerSharing.PeerAddress.mk h p))
                      | none => pure []
                    let replyPeers := match selfAddr with
                      | some sa => (sa :: knownPeers).take amount.toNat
                      | none    => knownPeers.take amount.toNat
                    let _ ← sendPeerSharing sock (.MsgSharePeers replyPeers)
                    IO.eprintln s!"[PeerSharing] Responded to MsgShareRequest({amount}) with {replyPeers.length} peers"
                | some (.MsgSharePeers peers) => do
                    match discoveryRef with
                    | some dRef =>
                        let peerAddrs := peers.map fun p => (p.host, p.port)
                        if peerAddrs.length > 0 then
                          dRef.modify fun ds =>
                            -- Filter out bogus addresses: multicast (224-239.x.x.x),
                            -- loopback (127.x), unspecified (0.x), IPv6 literals
                            let isUsable := fun (host : String) =>
                              let firstOctet := (host.splitOn ".").head?.bind (·.toNat?) |>.getD 0
                              !host.contains ':' &&  -- no IPv6
                              firstOctet != 0 &&
                              firstOctet != 127 &&
                              firstOctet < 224  -- no multicast/reserved
                            let newPeers := peerAddrs.filter fun (h, p) =>
                              isUsable h && !ds.known.any (· == (h, p))
                            { ds with discovered := ds.discovered ++ newPeers }
                        -- Send MsgDone to cleanly close PeerSharing
                        let _ ← sendPeerSharing sock .MsgDone
                    | none => pure ()
                | _ => pure ()  -- MsgDone or decode failure: ignore
                receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr
              else
                return .ok { header := header, payload := payload }

/-- Result of the sync loop: why did it exit? -/
inductive SyncExit where
  | connectionLost (reason : String)  -- Recoverable: relay dropped us
  | protocolError (reason : String)   -- Not recoverable without reconnect
  | done                              -- Clean exit

end Dion.Node
