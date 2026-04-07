import Dion.Network.Socket
import Dion.Network.PeerSharing
import Dion.Network.ChainSyncState
import Dion.Network.TxSubmission2State
import Dion.Network.PeerSharingState
import Dion.Network.KeepAlive

/-!
# Peer Connection

Per-peer connection state bundling a socket with all mini-protocol states.
Each connected peer gets one PeerConnection tracking where each protocol
is in its state machine.
-/

namespace Dion.Network.PeerConnection

open Dion.Network.Socket
open Dion.Network.PeerSharing
open Dion.Network.ChainSyncState
open Dion.Network.TxSubmission2State
open Dion.Network.PeerSharingState
open Dion.Network.KeepAlive

-- ====================
-- = Connection Dir   =
-- ====================

/-- Direction of the connection -/
inductive ConnectionDirection where
  | Outbound  -- We initiated the connection
  | Inbound   -- Peer connected to us (future: server mode)
  deriving Repr, BEq

-- ====================
-- = Peer Connection  =
-- ====================

/-- Per-peer connection state -/
structure PeerConnection where
  peerId         : Nat
  address        : PeerAddress
  sock           : Socket
  chainSyncState : ChainSyncState
  txSubmState     : TxSubmission2State
  peerSharingState : PeerSharingState
  keepAliveState : KeepAliveLatencyState
  direction      : ConnectionDirection
  connectedAt    : Nat             -- Timestamp in ms
  blocksSynced   : Nat             -- Blocks received from this peer

instance : Repr PeerConnection where
  reprPrec pc _ := s!"Peer#{pc.peerId}({pc.address}, blocks={pc.blocksSynced})"

/-- Create a new outbound peer connection (all protocols in initial states) -/
def PeerConnection.new (peerId : Nat) (addr : PeerAddress) (sock : Socket)
    (timestampMs : Nat := 0) : PeerConnection :=
  { peerId := peerId
    address := addr
    sock := sock
    chainSyncState := .StIdle
    txSubmState := .StInit
    peerSharingState := .StIdle
    keepAliveState := KeepAliveLatencyState.initial
    direction := .Outbound
    connectedAt := timestampMs
    blocksSynced := 0 }

/-- Update the ChainSync state -/
def PeerConnection.withChainSyncState (pc : PeerConnection) (s : ChainSyncState) : PeerConnection :=
  { pc with chainSyncState := s }

/-- Update the TxSubmission2 state -/
def PeerConnection.withTxSubmState (pc : PeerConnection) (s : TxSubmission2State) : PeerConnection :=
  { pc with txSubmState := s }

/-- Increment blocks synced counter -/
def PeerConnection.onBlockSynced (pc : PeerConnection) : PeerConnection :=
  { pc with blocksSynced := pc.blocksSynced + 1 }

end Dion.Network.PeerConnection
