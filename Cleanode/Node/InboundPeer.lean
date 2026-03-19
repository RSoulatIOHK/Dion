import Cleanode.Network.Basic
import Cleanode.Network.Socket
import Cleanode.Network.Handshake
import Cleanode.Network.Multiplexer
import Cleanode.Network.ChainSync
import Cleanode.Network.Byron
import Cleanode.Network.Shelley
import Cleanode.Network.BlockFetch
import Cleanode.Network.BlockFetchClient
import Cleanode.Network.ConwayBlock
import Cleanode.Network.Crypto
import Cleanode.Network.Bech32
import Cleanode.Network.TxSubmission2
import Cleanode.Network.Mempool
import Cleanode.Network.PeerSharing
import Cleanode.Network.PeerDb
import Cleanode.Network.PeerConnection
import Cleanode.Network.ConnectionManager
import Cleanode.Network.MuxDispatcher
import Cleanode.Network.HandshakeServer
import Cleanode.Config.Topology
import Cleanode.Config.Genesis
import Cleanode.Storage.BlockStore
import Cleanode.Storage.ChainDB
import Cleanode.Storage.Database
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Consensus.Praos.ConsensusState
import Cleanode.Crypto.VRF.ECVRF
import Cleanode.Crypto.Sign.Ed25519.Signature
import Std.Sync
import Pigment
import Cleanode.TUI.State
import Cleanode.TUI.Render
import Cleanode.Mithril.Types
import Cleanode.Mithril.Client
import Cleanode.Mithril.Replay
import Cleanode.CLI.Args
import Cleanode.CLI.Query
import Cleanode.Monitoring.Server
import Cleanode.Consensus.Praos.SPOKeys
import Cleanode.Consensus.Praos.ForgeLoop
import Cleanode.Consensus.Praos.BlockAnnounce
import Cleanode.Network.N2C.Server
import Cleanode.Ledger.State
import Cleanode.Ledger.Certificate
import Cleanode.Ledger.Snapshot
import Cleanode.Consensus.Praos.StakeDistribution
import Cleanode.Node.PeerSync

open Cleanode.Network
open Cleanode.Network.Socket
open Cleanode.Network.Handshake
open Cleanode.Network.Multiplexer
open Cleanode.Network.ChainSync
open Cleanode.Network.Byron
open Cleanode.Network.Shelley
open Cleanode.Network.BlockFetch
open Cleanode.Network.Crypto
open Cleanode.Network.Bech32
open Cleanode.Network.BlockFetchClient
open Cleanode.Network.ConwayBlock
open Cleanode.Network.TxSubmission2
open Cleanode.Network.Mempool
open Cleanode.Network.PeerDb
open Cleanode.Network.PeerConnection
open Cleanode.Network.ConnectionManager
open Cleanode.Network.MuxDispatcher
open Cleanode.Network.PeerSharing
open Cleanode.Network.HandshakeServer
open Cleanode.Config.Topology
open Cleanode.Config.Genesis
open Cleanode.Storage.BlockStore
open Cleanode.Storage.ChainDB
open Cleanode.Storage.Database
open Cleanode.Consensus.Praos.LeaderElection
open Cleanode.Consensus.Praos.ConsensusState
open Pigment
open Cleanode.TUI.State
open Cleanode.TUI.Render
open Cleanode.Node

namespace Cleanode.Node

open Cleanode.Consensus.Praos.BlockAnnounce in
/-- Inbound peer loop: handle a single inbound connection.
    Serves ChainSync (server mode), BlockFetch (server mode),
    TxSubmission2 (server mode), and KeepAlive. -/
partial def inboundPeerLoop (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (peerId : String)
    (pendingBlocks : IO.Ref (Array Cleanode.Consensus.Praos.BlockForge.ForgedBlock))
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
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
                        let _ ← sendTxSubmission2Responder sock
                          (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                        inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef txIds.length
                      else
                        let _ ← sendTxSubmission2Responder sock (.MsgRequestTxs (wanted.map (·.hash)))
                        inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | some (.MsgReplyTxs txBodies) =>
                    let now ← Cleanode.TUI.Render.nowMs
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
                    let _ ← sendTxSubmission2Responder sock
                      (.MsgRequestTxIds false (UInt16.ofNat txBodies.length) 10)
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef txBodies.length
                | _ =>
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else
                -- Unknown protocol, continue
                inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

open Cleanode.Consensus.Praos.BlockAnnounce in
/-- Handle a single inbound peer: handshake then enter mux loop -/
partial def handleInboundPeer (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (network : NetworkMagic := .Mainnet)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none) : IO Unit := do
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
      -- Generate a peer ID from the connection
      let reg ← registryRef.get
      let peerId := s!"inbound-{reg.subscribers.size}"
      let pendingBlocks ← IO.mkRef (#[] : Array Cleanode.Consensus.Praos.BlockForge.ForgedBlock)
      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef

open Cleanode.Consensus.Praos.BlockAnnounce in
/-- Accept loop: listen for inbound connections, spawn handler per peer -/
partial def acceptLoop (listenSock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (network : NetworkMagic := .Mainnet)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none) : IO Unit := do
  match ← socket_accept listenSock with
  | .error _ =>
      IO.sleep 1000
      acceptLoop listenSock mempoolRef tuiRef registryRef network ledgerStateRef
  | .ok clientSock => do
      let _ ← IO.asTask (do
        try handleInboundPeer clientSock mempoolRef tuiRef registryRef network ledgerStateRef
        catch _ => let _ ← socket_close clientSock; pure ())
      acceptLoop listenSock mempoolRef tuiRef registryRef network ledgerStateRef

end Cleanode.Node
