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
import Dion.Node.PeerSync

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
open Dion.Node

namespace Dion.Node

open Dion.Consensus.Praos.BlockAnnounce in
/-- Inbound peer loop: handle a single inbound connection.
    Serves ChainSync (server mode), BlockFetch (server mode),
    TxSubmission2 (server mode), and KeepAlive. -/
partial def inboundPeerLoop (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (peerId : String)
    (pendingBlocks : IO.Ref (Array Dion.Consensus.Praos.BlockForge.ForgedBlock))
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (ackedSoFar : Nat := 0) : IO Unit := do
  -- Read mux header (8 bytes)
  match ← socket_receive_exact sock 8 with
  | .error _ =>
      registryRef.modify (·.removeSubscriber peerId)
      let _ ← socket_close sock; return
  | .ok headerBytes =>
      match decodeMuxHeader headerBytes with
      | none =>
          registryRef.modify (·.removeSubscriber peerId)
          let _ ← socket_close sock; return
      | some header => do
          -- Read payload
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error _ =>
              registryRef.modify (·.removeSubscriber peerId)
              let _ ← socket_close sock; return
          | .ok payload => do
              -- Route by protocol
              if header.protocolId == .KeepAlive then
                match extractKeepAliveCookie payload with
                | some cookie => sendKeepAliveResponseInbound sock cookie
                | none => pure ()
                inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else if header.protocolId == .ChainSync then
                match decodeChainSyncMessage payload with
                | some (.MsgFindIntersect points) =>
                    -- Peer wants to find intersection — register as subscriber
                    let reg ← registryRef.get
                    let tip ← match reg.forgedBlocks.back? with
                      | some block => forgedBlockToTip block
                      | none => pure { point := Point.genesis, blockNo := 0 }
                    handleFindIntersect sock points tip
                    -- Register peer as ChainSync subscriber
                    let sub : ChainSyncSubscriber := {
                      socket := sock
                      peerId := peerId
                      isWaiting := false
                      lastSentSlot := 0
                    }
                    registryRef.modify (·.addSubscriber sub)
                    tuiLog tuiRef s!"Inbound peer {peerId} subscribed to ChainSync"
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | some .MsgRequestNext =>
                    handleRequestNext registryRef sock peerId pendingBlocks
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | some .MsgDone =>
                    registryRef.modify (·.removeSubscriber peerId)
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | _ =>
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else if header.protocolId == .BlockFetch then
                match decodeBlockFetchMessage payload with
                | some result => match result.value with
                  | .MsgRequestRange fromPt toPt =>
                      handleBlockFetchRequest registryRef sock fromPt toPt
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                  | .MsgClientDone =>
                      -- Peer is done fetching, continue loop
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                  | _ =>
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | none =>
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else if header.protocolId == .TxSubmission2 then
                match decodeTxSubmission2Message payload with
                | some .MsgInit =>
                    let _ ← sendTxSubmission2Responder sock .MsgInit
                    let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef 0
                | some (.MsgReplyTxIds txIds) =>
                    if txIds.isEmpty then
                      let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef 0
                    else
                      let pool ← mempoolRef.get
                      let wanted := txIds.filter fun tid => !pool.contains tid.hash
                      if wanted.isEmpty then
                        -- All already known: ack all and request more
                        let _ ← sendTxSubmission2Responder sock
                          (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                        inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef 0
                      else do
                        let _ ← sendTxSubmission2Responder sock (.MsgRequestTxs (wanted.map (·.hash)))
                        -- Store total txIds count in ackedSoFar so MsgReplyTxs can ack correctly
                        inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef txIds.length
                | some (.MsgReplyTxs txBodies) =>
                    let now ← Dion.TUI.Render.nowMs
                    let slot ← match ledgerStateRef with
                      | some r => do let ls ← r.atomically (fun ref => ref.get); pure ls.lastSlot
                      | none => pure 0
                    let mut added := 0
                    for txBytes in txBodies do
                      let pool ← mempoolRef.get
                      let result ← match ledgerStateRef with
                        | some lsRef => pool.addTxValidated txBytes now (← lsRef.atomically (fun ref => ref.get)) slot
                        | none => pool.addTxRaw txBytes now
                      match result with
                      | .ok newPool => mempoolRef.set newPool; added := added + 1
                      | .error _ => pure ()
                    if added > 0 then
                      tuiLog tuiRef s!"[mempool] Received {added} tx(s) from peer {peerId}"
                    if let some tRef := tuiRef then
                      let pool ← mempoolRef.get
                      tRef.modify (·.updateMempool pool.entries.length pool.totalBytes)
                    -- Ack the full previous MsgReplyTxIds count (stored in ackedSoFar)
                    let ackCount := if ackedSoFar > 0 then ackedSoFar else txBodies.length
                    let _ ← sendTxSubmission2Responder sock
                      (.MsgRequestTxIds false (UInt16.ofNat ackCount) 10)
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef 0
                | _ =>
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else
                -- Unknown protocol, continue
                inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

open Dion.Consensus.Praos.BlockAnnounce in
/-- Handle a single inbound peer: handshake then enter mux loop -/
partial def handleInboundPeer (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (network : NetworkMagic := .Mainnet)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none) : IO Unit := do
  IO.eprintln "[Inbound] New connection, starting handshake..."
  match ← receiveAndRespondHandshake sock (network := network) with
  | .error e =>
      IO.eprintln s!"[Inbound] Handshake failed: {e}"
      let _ ← socket_close sock; return
  | .ok none =>
      IO.eprintln "[Inbound] Handshake returned none"
      let _ ← socket_close sock; return
  | .ok (some _version) =>
      IO.eprintln "[Inbound] Handshake OK, entering mux loop"
      tuiLog tuiRef "Inbound peer connected (handshake OK)"
      if let some ref := tuiRef then ref.modify (·.addInbound)
      -- Generate a peer ID from the connection
      let reg ← registryRef.get
      let peerId := s!"inbound-{reg.subscribers.size}"
      let pendingBlocks ← IO.mkRef (#[] : Array Dion.Consensus.Praos.BlockForge.ForgedBlock)
      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef
      if let some ref := tuiRef then ref.modify (·.removeInbound)

open Dion.Consensus.Praos.BlockAnnounce in
/-- Accept loop: listen for inbound connections, spawn handler per peer -/
partial def acceptLoop (listenSock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (network : NetworkMagic := .Mainnet)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none) : IO Unit := do
  match ← socket_accept listenSock with
  | .error _ =>
      IO.sleep 1000
      acceptLoop listenSock mempoolRef tuiRef registryRef network ledgerStateRef
  | .ok clientSock => do
      let _ ← IO.asTask (do
        try handleInboundPeer clientSock mempoolRef tuiRef registryRef network ledgerStateRef
        catch _ => let _ ← socket_close clientSock; pure ())
      acceptLoop listenSock mempoolRef tuiRef registryRef network ledgerStateRef

end Dion.Node
