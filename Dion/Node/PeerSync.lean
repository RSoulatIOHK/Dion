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
import Dion.Node.SyncState
import Dion.Node.BlockApply

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

/-- Continuous sync loop: requests blocks via ChainSync + BlockFetch.
    Returns a SyncExit indicating why it stopped. -/
partial def syncLoop (sock : Socket) (blockCount : Nat) (chainDb : Option ChainDB := none)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (peerAddr : String := "")
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (txSubmPeerRef : Option (IO.Ref TxSubmPeerState) := none)
    (txSubmResponderQueue : Option (IO.Ref (List ByteArray)) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (checkpointRef : Option (IO.Ref Dion.Ledger.State.CheckpointRing) := none)
    (selfAddr : Option Dion.Network.PeerSharing.PeerAddress := none) : IO SyncExit := do
  -- Request next block header
  match ← sendChainSync sock requestNext with
  | .error e =>
      return .connectionLost s!"Failed to send RequestNext: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr with
      | .error e =>
          return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | none => do
              return .protocolError "Failed to decode ChainSync message"
          | some (.MsgRollForward header tip) => do
              let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef
              if ok then
                syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr
              else
                return .protocolError "Failed to fetch block"
          | some (.MsgRollBackward rollbackPoint _) => do
              -- Update forge tip to rollback point
              if let some phRef := prevHashRef then
                phRef.set rollbackPoint.hash
              -- Restore ledger state from nearest checkpoint at or before rollback slot
              if let some cpRef := checkpointRef then
                if let some lsMutex := ledgerStateRef then
                  let ring ← cpRef.get
                  match ring.findRollbackTarget rollbackPoint.slot.toNat with
                  | some cp =>
                    lsMutex.atomically fun lsRef => lsRef.set cp.ledger
                    if let some bnRef := blockNoRef then bnRef.set cp.blockNo
                  | none => pure ()  -- No checkpoint found; UTxO state may be stale
              -- Dedup: only print once across peers (encode as slot + 1B offset)
              let shouldPrint ← do
                if let some ref := seenBlocks then
                  let dup ← atomicCheckAndMark ref (rollbackPoint.slot.toNat + 1_000_000_000)
                  pure (!dup)
                else pure true
              if shouldPrint then
                match tuiRef with
                | some ref => ref.modify (·.addRollback)
                | none =>
                  run do
                    concat [
                      ("↩ Rollback to slot ".style |> yellow),
                      (s!"{rollbackPoint.slot}".style |> yellow |> bold)
                    ]
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr
          | some (.MsgAwaitReply) => do
              -- Dedup: only print once across peers (encode as blockCount + 2B offset)
              let shouldPrint ← do
                if let some ref := seenBlocks then
                  let dup ← atomicCheckAndMark ref (blockCount + 2_000_000_000)
                  pure (!dup)
                else pure true
              if shouldPrint then
                match tuiRef with
                | some ref => ref.modify (·.addLog "At tip -- waiting for new block...")
                | none =>
                  run do
                    concat [
                      ("At tip -- waiting for new block...".style |> cyan |> dim)
                    ]
              -- Server will push MsgRollForward when a new block arrives
              -- Keep receiving, handling KeepAlive frames transparently
              match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr with
              | .error e =>
                  return .connectionLost e
              | .ok frame => do
                  match decodeChainSyncMessage frame.payload with
                  | some (.MsgRollForward header tip) => do
                      let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef
                      if ok then
                        syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr
                      else
                        return .protocolError "Failed to fetch block"
                  | some (.MsgRollBackward rollbackPoint2 _) => do
                      if let some phRef := prevHashRef then
                        phRef.set rollbackPoint2.hash
                      if let some cpRef := checkpointRef then
                        if let some lsMutex := ledgerStateRef then
                          let ring ← cpRef.get
                          match ring.findRollbackTarget rollbackPoint2.slot.toNat with
                          | some cp =>
                            lsMutex.atomically fun lsRef => lsRef.set cp.ledger
                            if let some bnRef := blockNoRef then bnRef.set cp.blockNo
                          | none => pure ()
                      let shouldPrint2 ← do
                        if let some ref := seenBlocks then
                          let dup ← atomicCheckAndMark ref (rollbackPoint2.slot.toNat + 1_000_000_000)
                          pure (!dup)
                        else pure true
                      if shouldPrint2 then
                        match tuiRef with
                        | some ref => ref.modify (·.addRollback)
                        | none =>
                          run do
                            concat [
                              ("↩ Rollback to slot ".style |> yellow),
                              (s!"{rollbackPoint2.slot}".style |> yellow |> bold)
                            ]
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr
                  | some other => do
                      IO.println s!"Unexpected message: {repr other}"
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr
                  | none =>
                      return .protocolError "Failed to decode ChainSync message"
          | some other => do
              IO.println s!"Unexpected message: {repr other}"
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr

/-- Connect, handshake, find tip, intersect, and enter sync loop.
    Returns a SyncExit indicating why the session ended. -/
def connectAndSync (host : String) (port : UInt16) (proposal : HandshakeMessage)
    (chainDb : ChainDB) : IO SyncExit := do
  match ← socket_connect host port with
  | .error e =>
      return .connectionLost s!"Connection failed: {e}"
  | .ok sock => do
      -- Handshake
      match ← sendHandshake sock proposal with
      | .error e =>
          socket_close sock
          return .connectionLost s!"Failed to send handshake: {e}"
      | .ok _ => do
          match ← socket_receive sock 1024 with
          | .error e =>
              socket_close sock
              return .connectionLost s!"Failed to receive handshake: {e}"
          | .ok rawData => do
              match decodeMuxFrame rawData >>= fun f => decodeHandshakeMessage f.payload with
              | none =>
                  socket_close sock
                  return .protocolError "Failed to decode handshake response"
              | some msg => do
                  IO.println s!"  ✓ Handshake: {repr msg}"

                  -- Find chain tip
                  match ← sendChainSync sock findIntersectTip with
                  | .error e =>
                      socket_close sock
                      return .connectionLost s!"Failed to send FindIntersect: {e}"
                  | .ok _ => do
                      match ← socket_receive sock 8192 with
                      | .error e =>
                          socket_close sock
                          return .connectionLost s!"Failed to receive tip: {e}"
                      | .ok rawData => do
                          match decodeMuxFrame rawData >>= fun f => decodeChainSyncMessage f.payload with
                          | some (.MsgIntersectNotFound tip) => do
                              IO.println s!"  ✓ Chain tip at slot {tip.point.slot}"

                              -- Intersect at tip
                              match ← sendChainSync sock (findIntersectAt tip.point) with
                              | .error e =>
                                  socket_close sock
                                  return .connectionLost s!"Failed to intersect: {e}"
                              | .ok _ => do
                                  match ← socket_receive sock 8192 with
                                  | .error e =>
                                      socket_close sock
                                      return .connectionLost s!"Failed to receive intersection: {e}"
                                  | .ok rawData => do
                                      match decodeMuxFrame rawData >>= fun f => decodeChainSyncMessage f.payload with
                                      | some (.MsgIntersectFound point _) => do
                                          IO.println s!"  ✓ Intersected at slot {point.slot}"
                                          let result ← syncLoop sock 0 (some chainDb)
                                          socket_close sock
                                          return result
                                      | _ =>
                                          socket_close sock
                                          return .protocolError "Failed to intersect at tip"
                          | _ =>
                              socket_close sock
                              return .protocolError "Failed to find chain tip"

/-- Reconnection loop: keeps reconnecting on connection loss with backoff -/
partial def reconnectLoop (host : String) (port : UInt16) (proposal : HandshakeMessage)
    (networkName : String) (chainDb : ChainDB) (attempt : Nat := 0) : IO Unit := do
  if attempt > 0 then
    let delaySec := min (attempt * 5) 30  -- 5s, 10s, 15s, ... capped at 30s
    run do
      concat [
        ("⟳ Reconnecting in ".style |> yellow),
        (s!"{delaySec}s".style |> yellow |> bold),
        (s!" (attempt {attempt + 1})...".style |> yellow |> dim)
      ]
    IO.sleep (UInt32.ofNat (delaySec * 1000))
  else
    run do
      println (("=== Following Chain Tip (Ctrl+C to stop) ===".style |> cyan |> bold))

  match ← connectAndSync host port proposal chainDb with
  | .connectionLost reason => do
      run do
        concat [
          ("⚡ Connection lost: ".style |> yellow),
          (reason.style |> yellow |> dim)
        ]
      reconnectLoop host port proposal networkName chainDb (attempt + 1)
  | .protocolError reason => do
      run do
        concat [
          ("✗ Protocol error: ".style |> red |> bold),
          (reason.style |> red)
        ]
      -- Protocol errors are also worth retrying — the relay might behave differently
      reconnectLoop host port proposal networkName chainDb (attempt + 1)
  | .done => pure ()

/-- Receive a MUX frame using exact reads (8-byte header + payload) -/
def receiveMuxFrameExact (sock : Socket) : IO (Except String MuxFrame) := do
  match ← socket_receive_exact sock 8 with
  | .error e => return .error s!"Failed to receive MUX header: {e}"
  | .ok headerBytes =>
      match decodeMuxHeader headerBytes with
      | none => return .error "Failed to decode MUX header"
      | some header => do
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error s!"Failed to receive payload: {e}"
          | .ok payload =>
              return .ok { header := header, payload := payload }


/-- After handshake succeeds: find tip, intersect, and sync.
    Separated to reduce nesting depth (avoids LLVM optimizer crash). -/
def peerFindTipAndSync (sock : Socket) (addrStr : String) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState))
    (seenBlocks : Option (IO.Ref (Option (List Nat))))
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (txSubmPeerRef : Option (IO.Ref TxSubmPeerState) := none)
    (txSubmResponderQueue : Option (IO.Ref (List ByteArray)) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (checkpointRef : Option (IO.Ref Dion.Ledger.State.CheckpointRing) := none)
    (skipToTip : Bool := false)
    (selfAddr : Option Dion.Network.PeerSharing.PeerAddress := none) : IO SyncExit := do
  -- Check for saved sync state (e.g., from Mithril fast-sync or previous run)
  -- skipToTip: bypass saved state and intersect at chain tip immediately
  let savedPoint ← if skipToTip then pure none else chainDb.getLastSyncedPoint

  -- If we have a saved point, try to intersect there first (with tip as fallback)
  let intersectPoints : List Point := match savedPoint with
    | some pt => [pt, Point.genesis]
    | none => []
  if let some pt := savedPoint then
    tuiLog tuiRef s!"Peer {addrStr}: resuming from saved sync state at slot {pt.slot}"

  if intersectPoints.length > 0 then
    -- Try to intersect at saved point
    match ← sendChainSync sock (.MsgFindIntersect intersectPoints) with
    | .error e => return .connectionLost s!"Failed to find intersection: {e}"
    | .ok _ => do
        match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr with
        | .error e => return .connectionLost e
        | .ok frame => do
            match decodeChainSyncMessage frame.payload with
            | some (.MsgIntersectFound point _) => do
                tuiLog tuiRef s!"Peer {addrStr}: intersected at saved point slot {point.slot}"
                return ← syncLoop sock 0 (some chainDb) discoveryRef seenBlocks tuiRef addrStr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr
            | _ =>
                tuiLog tuiRef s!"Peer {addrStr}: saved point not found, falling back to tip"

  -- Default: find tip, intersect there, sync forward
  match ← sendChainSync sock findIntersectTip with
  | .error e => return .connectionLost s!"Failed to query tip: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr with
      | .error e => return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | some (.MsgIntersectNotFound tip) => do
              tuiLog tuiRef s!"Peer {addrStr}: tip at slot {tip.point.slot}"
              -- Intersect at tip
              match ← sendChainSync sock (findIntersectAt tip.point) with
              | .error e => return .connectionLost s!"Failed to intersect: {e}"
              | .ok _ => do
                  match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue selfAddr with
                  | .error e => return .connectionLost e
                  | .ok frame2 => do
                      match decodeChainSyncMessage frame2.payload with
                      | some (.MsgIntersectFound point _) => do
                          tuiLog tuiRef s!"Peer {addrStr}: intersected at slot {point.slot}"
                          syncLoop sock 0 (some chainDb) discoveryRef seenBlocks tuiRef addrStr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef selfAddr
                      | _ => return .protocolError "Failed to intersect"
          | _ => return .protocolError "Failed to find tip"

/-- Connect to a peer, handshake, find tip, intersect, and sync.
    Returns a SyncExit indicating why it stopped. -/
def peerConnectAndSync (host : String) (port : UInt16) (addrStr : String)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (checkpointRef : Option (IO.Ref Dion.Ledger.State.CheckpointRing) := none)
    (skipToTip : Bool := false)
    (selfAddr : Option Dion.Network.PeerSharing.PeerAddress := none) : IO SyncExit := do
  match ← socket_connect host port with
  | .error e => return .connectionLost s!"Connection failed: {e}"
  | .ok sock => do
      -- Update TUI peer status
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "connecting")
      -- Handshake
      match ← sendHandshake sock proposal with
      | .error e =>
          socket_close sock
          return .connectionLost s!"Handshake send failed: {e}"
      | .ok _ => do
          -- Read handshake response using proper mux framing (8-byte header + payload)
          -- to avoid consuming extra bytes (e.g. TxSubmission2 MsgInit) from the TCP buffer
          match ← socket_receive_exact sock 8 with
          | .error e =>
              socket_close sock
              return .connectionLost s!"Handshake recv failed: {e}"
          | .ok hdrBytes => do
              match decodeMuxHeader hdrBytes with
              | none =>
                  socket_close sock
                  return .protocolError "Handshake header decode failed"
              | some hdr => do
                  match ← socket_receive_exact sock hdr.payloadLength.toNat.toUInt32 with
                  | .error e =>
                      socket_close sock
                      return .connectionLost s!"Handshake payload recv failed: {e}"
                  | .ok payloadBytes => do
                      match decodeHandshakeMessage payloadBytes with
                      | none =>
                          socket_close sock
                          return .protocolError "Handshake decode failed"
                      | some hsMsg => do
                  let peerSharingEnabled := match hsMsg with
                    | .AcceptVersion _ vd => vd.peerSharing == 1
                    | _ => false
                  match hsMsg with
                  | .AcceptVersion vn vd =>
                      tuiLog tuiRef s!"Peer {addrStr}: version {vn.value}, peerSharing={vd.peerSharing}, diffusion={vd.initiatorAndResponderDiffusionMode}"
                  | _ =>
                      tuiLog tuiRef s!"Peer {addrStr}: handshake ok"
                  -- Update TUI peer status to syncing
                  if let some ref := tuiRef then
                    ref.modify (·.updatePeer addrStr "syncing")
                  -- TxSubmission2 on outbound connection: we are the TCP initiator,
                  -- so we are the producer (server). We send MsgInit to start the protocol.
                  -- The peer (TCP acceptor) is the consumer and will send MsgRequestTxIds
                  -- on mode=Responder to pull txs from us. We reply on mode=Initiator.
                  -- To receive txs FROM peers, we need inbound connections (peers connect to us).
                  let _ ← sendTxSubmission2 sock .MsgInit
                  -- Create per-peer TxSubmission2 state
                  let txSubmPeerRef ← IO.mkRef TxSubmPeerState.empty
                  -- Responder queue: no longer used (handled inline in mux loop)
                  let responderQueue ← IO.mkRef ([] : List ByteArray)
                  if peerSharingEnabled && discoveryRef.isSome then
                    tuiLog tuiRef s!"Peer {addrStr}: PeerSharing enabled, requesting peers..."
                    let _ ← sendPeerSharing sock (.MsgShareRequest 10)
                  let result ← peerFindTipAndSync sock addrStr chainDb discoveryRef seenBlocks tuiRef mempoolRef (some txSubmPeerRef) (some responderQueue) consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef skipToTip selfAddr
                  socket_close sock
                  return result

/-- Per-peer reconnection loop: connects, syncs, and reconnects on failure with backoff. -/
partial def peerReconnectLoop (host : String) (port : UInt16)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (checkpointRef : Option (IO.Ref Dion.Ledger.State.CheckpointRing) := none)
    (skipToTip : Bool := false)
    (selfAddr : Option Dion.Network.PeerSharing.PeerAddress := none)
    (attempt : Nat := 0) : IO Unit := do
  let addrStr := s!"{host}:{port}"
  if attempt > 0 then
    let delaySec := min (attempt * 5) 30
    if let some ref := tuiRef then
      ref.modify (·.updatePeer addrStr "reconnecting")
    else
      IO.println s!"  ⟳ Peer {addrStr}: reconnecting in {delaySec}s (attempt {attempt + 1})..."
    IO.sleep (UInt32.ofNat (delaySec * 1000))

  match ← peerConnectAndSync host port addrStr proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef skipToTip selfAddr with
  | .connectionLost reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "disconnected")
      else
        IO.println s!"  ⚡ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef skipToTip selfAddr (attempt + 1)
  | .protocolError reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "error")
      else
        IO.println s!"  ✗ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef skipToTip selfAddr (attempt + 1)
  | .done => pure ()

/-- Peer spawner loop: drains discovered peers and spawns new connection tasks -/
partial def peerSpawnerLoop (discoveryRef : IO.Ref DiscoveryState)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Dion.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (checkpointRef : Option (IO.Ref Dion.Ledger.State.CheckpointRing) := none)
    (skipToTip : Bool := false)
    (selfAddr : Option Dion.Network.PeerSharing.PeerAddress := none)
    (maxPeers : Nat := 20) : IO Unit := do
  IO.sleep 30000  -- Check every 30 seconds
  let newPeers ← discoveryRef.modifyGet fun ds =>
    (ds.discovered, { ds with discovered := [], known := ds.known ++ ds.discovered })
  if newPeers.isEmpty then
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef skipToTip selfAddr maxPeers
  else do
    let knownCount := (← discoveryRef.get).known.length
    let budget := if knownCount >= maxPeers then 0 else maxPeers - knownCount
    let toSpawn := newPeers.take budget
    for (host, port) in toSpawn do
      tuiLog tuiRef s!"Connecting to discovered peer {host}:{port}"
      let _ ← IO.asTask (do
        try
          peerReconnectLoop host port proposal chainDb (some discoveryRef) seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef skipToTip selfAddr
        catch e =>
          tuiLog tuiRef s!"Discovered peer {host}:{port}: {e}")
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef checkpointRef skipToTip selfAddr maxPeers

end Dion.Node
