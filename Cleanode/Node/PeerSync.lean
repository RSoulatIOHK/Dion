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
import Cleanode.Node.SyncState
import Cleanode.Node.BlockApply

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
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none) : IO SyncExit := do
  -- Request next block header
  match ← sendChainSync sock requestNext with
  | .error e =>
      return .connectionLost s!"Failed to send RequestNext: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
      | .error e =>
          return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | none => do
              return .protocolError "Failed to decode ChainSync message"
          | some (.MsgRollForward header tip) => do
              let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef
              if ok then
                syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
              else
                return .protocolError "Failed to fetch block"
          | some (.MsgRollBackward rollbackPoint _) => do
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
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
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
              match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
              | .error e =>
                  return .connectionLost e
              | .ok frame => do
                  match decodeChainSyncMessage frame.payload with
                  | some (.MsgRollForward header tip) => do
                      let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef
                      if ok then
                        syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
                      else
                        return .protocolError "Failed to fetch block"
                  | some (.MsgRollBackward rollbackPoint2 _) => do
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
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
                  | some other => do
                      IO.println s!"Unexpected message: {repr other}"
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
                  | none =>
                      return .protocolError "Failed to decode ChainSync message"
          | some other => do
              IO.println s!"Unexpected message: {repr other}"
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef

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
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none) : IO SyncExit := do
  -- Check for saved sync state (e.g., from Mithril fast-sync or previous run)
  let savedPoint ← chainDb.getLastSyncedPoint

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
        match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
        | .error e => return .connectionLost e
        | .ok frame => do
            match decodeChainSyncMessage frame.payload with
            | some (.MsgIntersectFound point _) => do
                tuiLog tuiRef s!"Peer {addrStr}: intersected at saved point slot {point.slot}"
                return ← syncLoop sock 0 (some chainDb) discoveryRef seenBlocks tuiRef addrStr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
            | _ =>
                tuiLog tuiRef s!"Peer {addrStr}: saved point not found, falling back to tip"

  -- Default: find tip, intersect there, sync forward
  match ← sendChainSync sock findIntersectTip with
  | .error e => return .connectionLost s!"Failed to query tip: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
      | .error e => return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | some (.MsgIntersectNotFound tip) => do
              tuiLog tuiRef s!"Peer {addrStr}: tip at slot {tip.point.slot}"
              -- Intersect at tip
              match ← sendChainSync sock (findIntersectAt tip.point) with
              | .error e => return .connectionLost s!"Failed to intersect: {e}"
              | .ok _ => do
                  match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
                  | .error e => return .connectionLost e
                  | .ok frame2 => do
                      match decodeChainSyncMessage frame2.payload with
                      | some (.MsgIntersectFound point _) => do
                          tuiLog tuiRef s!"Peer {addrStr}: intersected at slot {point.slot}"
                          syncLoop sock 0 (some chainDb) discoveryRef seenBlocks tuiRef addrStr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
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
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none) : IO SyncExit := do
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
                  -- Send MsgInit as TxSubmission2 Initiator (we are client on outbound)
                  -- NOTE: On outbound connections, we can only be the TxSubmission2 client.
                  -- The peer acts as server (sends MsgRequestTxIds). To receive tx announcements
                  -- FROM peers, we need inbound connections (where we act as server).
                  let _ ← sendTxSubmission2 sock .MsgInit
                  -- Create per-peer TxSubmission2 state
                  let txSubmPeerRef ← IO.mkRef TxSubmPeerState.empty
                  -- Responder queue: no longer used (handled inline in mux loop)
                  let responderQueue ← IO.mkRef ([] : List ByteArray)
                  if peerSharingEnabled && discoveryRef.isSome then
                    tuiLog tuiRef s!"Peer {addrStr}: PeerSharing enabled, requesting peers..."
                    let _ ← sendPeerSharing sock (.MsgShareRequest 10)
                  let result ← peerFindTipAndSync sock addrStr chainDb discoveryRef seenBlocks tuiRef mempoolRef (some txSubmPeerRef) (some responderQueue) consensusRef ledgerStateRef prevHashRef blockNoRef
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
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (attempt : Nat := 0) : IO Unit := do
  let addrStr := s!"{host}:{port}"
  if attempt > 0 then
    let delaySec := min (attempt * 5) 30
    if let some ref := tuiRef then
      ref.modify (·.updatePeer addrStr "reconnecting")
    else
      IO.println s!"  ⟳ Peer {addrStr}: reconnecting in {delaySec}s (attempt {attempt + 1})..."
    IO.sleep (UInt32.ofNat (delaySec * 1000))

  match ← peerConnectAndSync host port addrStr proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef with
  | .connectionLost reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "disconnected")
      else
        IO.println s!"  ⚡ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef (attempt + 1)
  | .protocolError reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "error")
      else
        IO.println s!"  ✗ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef (attempt + 1)
  | .done => pure ()

/-- Peer spawner loop: drains discovered peers and spawns new connection tasks -/
partial def peerSpawnerLoop (discoveryRef : IO.Ref DiscoveryState)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (maxPeers : Nat := 20) : IO Unit := do
  IO.sleep 30000  -- Check every 30 seconds
  let newPeers ← discoveryRef.modifyGet fun ds =>
    (ds.discovered, { ds with discovered := [], known := ds.known ++ ds.discovered })
  if newPeers.isEmpty then
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef maxPeers
  else do
    let knownCount := (← discoveryRef.get).known.length
    let budget := if knownCount >= maxPeers then 0 else maxPeers - knownCount
    let toSpawn := newPeers.take budget
    for (host, port) in toSpawn do
      tuiLog tuiRef s!"Connecting to discovered peer {host}:{port}"
      let _ ← IO.asTask (do
        try
          peerReconnectLoop host port proposal chainDb (some discoveryRef) seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef
        catch e =>
          tuiLog tuiRef s!"Discovered peer {host}:{port}: {e}")
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef maxPeers

end Cleanode.Node
