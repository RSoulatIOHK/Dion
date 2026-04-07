import Dion.Network.Socket
import Dion.Network.Multiplexer
import Dion.Network.Handshake
import Dion.Network.ChainSync
import Dion.Network.BlockFetch
import Dion.Network.BlockFetchClient
import Dion.Network.TxSubmission2
import Dion.Network.KeepAlive
import Dion.Network.PeerSharing
import Dion.Network.Mempool
import Dion.Network.PeerDb
import Dion.Network.PeerConnection
import Dion.Network.MuxDispatcher
import Dion.Storage.ChainDB
import Dion.Config.Topology

/-!
# Connection Manager

Manages multiple concurrent peer connections for the relay node.
Handles peer selection, connection lifecycle, and the governor loop
that maintains target connection counts.

## Design
- Outbound-only for Phase 3 (no server/listener)
- Governor loop runs periodically to maintain target peer count
- Each peer gets its own protocol loop via IO.asTask
- Shared state (mempool, ChainDB, PeerDb) via IO.Ref
- Peer scoring based on sync performance and connection stability
-/

namespace Dion.Network.ConnectionManager

open Dion.Network.Socket
open Dion.Network.Multiplexer
open Dion.Network.Handshake
open Dion.Network.ChainSync
open Dion.Network.BlockFetch
open Dion.Network.BlockFetchClient
open Dion.Network.TxSubmission2
open Dion.Network.KeepAlive
open Dion.Network.PeerSharing
open Dion.Network.Mempool
open Dion.Network.PeerDb
open Dion.Network.PeerConnection
open Dion.Network.MuxDispatcher
open Dion.Storage.ChainDB
open Dion.Config.Topology

-- ====================
-- = Configuration    =
-- ====================

/-- Connection limits and targets -/
structure ConnectionLimits where
  maxOutbound    : Nat := 20    -- Hard limit on outbound connections
  targetOutbound : Nat := 5     -- How many peers we want connected
  warmValency    : Nat := 3     -- Peers we actively ChainSync with
  governorIntervalMs : Nat := 10000  -- Governor check interval (10s)
  deriving Repr

-- ====================
-- = Manager State    =
-- ====================

/-- Connection manager state (held in IO.Ref for shared access) -/
structure ConnectionManagerState where
  connections : List PeerConnection
  peerDb      : PeerDb
  limits      : ConnectionLimits
  mempool     : Mempool
  nextPeerId  : Nat
  deriving Repr

-- ====================
-- = Construction     =
-- ====================

/-- Create initial connection manager state from topology -/
def ConnectionManagerState.new (topo : Topology)
    (limits : ConnectionLimits := {}) : ConnectionManagerState :=
  { connections := []
    peerDb := PeerDb.fromTopology topo
    limits := limits
    mempool := Mempool.empty {}
    nextPeerId := 1 }

-- ====================
-- = Queries          =
-- ====================

/-- Number of active connections -/
def ConnectionManagerState.activeCount (mgr : ConnectionManagerState) : Nat :=
  mgr.connections.length

/-- Check if we need more connections -/
def ConnectionManagerState.needsMorePeers (mgr : ConnectionManagerState) : Bool :=
  mgr.connections.length < mgr.limits.targetOutbound

/-- Find a connection by peer ID -/
def ConnectionManagerState.findPeer (mgr : ConnectionManagerState) (peerId : Nat)
    : Option PeerConnection :=
  mgr.connections.find? (fun pc => pc.peerId == peerId)

-- ====================
-- = Peer Connection  =
-- ====================

/-- Connect to a peer: socket_connect + handshake + TxSubmission2 MsgInit.
    Returns the new PeerConnection or an error. -/
def connectToPeer (addr : PeerAddress) (peerId : Nat)
    (proposal : HandshakeMessage) : IO (Except String PeerConnection) := do
  -- Connect socket
  match ← socket_connect addr.host addr.port with
  | .error e => return .error s!"Connection failed to {addr}: {repr e}"
  | .ok sock => do
      -- Handshake
      match ← sendHandshake sock proposal with
      | .error e =>
          socket_close sock
          return .error s!"Handshake send failed: {repr e}"
      | .ok _ => do
          match ← socket_receive sock 1024 with
          | .error e =>
              socket_close sock
              return .error s!"Handshake recv failed: {repr e}"
          | .ok rawData => do
              match decodeMuxFrame rawData >>= fun f => decodeHandshakeMessage f.payload with
              | none =>
                  socket_close sock
                  return .error "Failed to decode handshake response"
              | some _ => do
                  let pc := PeerConnection.new peerId addr sock
                  return .ok pc

-- ====================
-- = Peer Removal     =
-- ====================

/-- Disconnect a peer by ID -/
def disconnectPeer (mgr : ConnectionManagerState) (peerId : Nat)
    : IO ConnectionManagerState := do
  match mgr.findPeer peerId with
  | none => return mgr
  | some pc => do
      socket_close pc.sock
      let remaining := mgr.connections.filter (fun c => c.peerId != peerId)
      let peerDb := mgr.peerDb.onDisconnected pc.address
      return { mgr with connections := remaining, peerDb := peerDb }

-- ====================
-- = Peer Selection   =
-- ====================

/-- Select peers to connect to (highest score, not already connected) -/
def selectPeers (mgr : ConnectionManagerState) : List PeerRecord :=
  let needed := mgr.limits.targetOutbound - mgr.connections.length
  if needed == 0 then []
  else mgr.peerDb.getAvailable needed

-- ====================
-- = Governor Logic   =
-- ====================

/-- One iteration of the governor: connect to new peers if below target,
    disconnect unhealthy peers. Returns updated state + list of new connections. -/
def governorTick (mgr : ConnectionManagerState) (proposal : HandshakeMessage)
    : IO (ConnectionManagerState × List PeerConnection) := do
  -- Thaw cooled peers
  let now ← IO.monoMsNow
  let mgr := { mgr with peerDb := mgr.peerDb.thaw now }

  -- Check if we need more peers
  if !mgr.needsMorePeers then
    return (mgr, [])
  else do
    let candidates := selectPeers mgr
    let mut currentMgr := mgr
    let mut newConns : List PeerConnection := []

    for peer in candidates do
      let peerId := currentMgr.nextPeerId
      match ← connectToPeer peer.address peerId proposal with
      | .error reason => do
          IO.println s!"  ✗ Failed to connect to {peer.address}: {reason}"
          currentMgr := { currentMgr with
            peerDb := currentMgr.peerDb.onError peer.address now
            nextPeerId := peerId + 1 }
      | .ok pc => do
          IO.println s!"  ✓ Connected to peer #{peerId} ({pc.address})"
          currentMgr := { currentMgr with
            connections := currentMgr.connections ++ [pc]
            peerDb := currentMgr.peerDb.onConnected peer.address now
            nextPeerId := peerId + 1 }
          newConns := newConns ++ [pc]

    return (currentMgr, newConns)

/-- Governor loop: periodically check peer targets and manage connections -/
partial def governorLoop (mgrRef : IO.Ref ConnectionManagerState)
    (proposal : HandshakeMessage)
    (onNewPeer : PeerConnection → IO Unit) : IO Unit := do
  IO.sleep (UInt32.ofNat 10000)  -- Wait 10 seconds between checks
  let mgr ← mgrRef.get
  let (newMgr, newConns) ← governorTick mgr proposal
  mgrRef.set newMgr

  -- Launch peer loops for new connections
  for pc in newConns do
    let _ ← IO.asTask (onNewPeer pc)

  governorLoop mgrRef proposal onNewPeer

-- ====================
-- = Stats            =
-- ====================

/-- Connection manager statistics -/
structure ConnectionStats where
  activePeers   : Nat
  totalBlocks   : Nat
  mempoolSize   : Nat
  peerDbStats   : PeerDbStats
  deriving Repr

/-- Get connection manager stats -/
def ConnectionManagerState.stats (mgr : ConnectionManagerState) : ConnectionStats :=
  let totalBlocks := mgr.connections.foldl (fun acc pc => acc + pc.blocksSynced) 0
  { activePeers := mgr.connections.length
    totalBlocks := totalBlocks
    mempoolSize := mgr.mempool.size
    peerDbStats := mgr.peerDb.stats }

end Dion.Network.ConnectionManager
