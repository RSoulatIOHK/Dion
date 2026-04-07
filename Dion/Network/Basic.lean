/-!
# Network Module

This module implements the Ouroboros network layer for Dion.

## Responsibilities
- TCP socket management
- Multiplexer/demultiplexer for mini-protocols
- Implementation of node-to-node mini-protocols:
  - Handshake (version negotiation)
  - ChainSync (block header synchronization)
  - BlockFetch (block body retrieval)
  - TxSubmission2 (transaction propagation)
  - KeepAlive (connection health)
  - PeerSharing (P2P discovery)

## Architecture
The network layer sits between raw TCP sockets and the consensus/ledger layers.
It provides a typed interface for communicating with Cardano peers.

## References
- [Ouroboros Network Spec](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf)
- [Cardano Blueprint Network](https://cardano-scaling.github.io/cardano-blueprint/network/)
-/

namespace Dion.Network

/-- Known Cardano mainnet bootstrap peers (backbone infrastructure) -/
def mainnetBootstrapPeers : List (String × UInt16) := [
  ("backbone.cardano.iog.io", 3001),
  ("backbone.mainnet.cardanofoundation.org", 3001),
  ("backbone.mainnet.emurgornd.com", 3001)
]

/-- Curated mainnet relay peers from Cardano peer snapshot.
    These are real SPO relays sorted by stake, more likely to support PeerSharing.
    Source: https://book.world.dev.cardano.org/environments/mainnet/peer-snapshot.json -/
def mainnetRelayPeers : List (String × UInt16) := [
  -- Top stake pool relays (from official Cardano peer snapshot)
  ("relay-trustwallet-5-0.cardano.mainnet.kiln.fi", 3001),
  ("relay-kiln-7-0.cardano.mainnet.kiln.fi", 3001),
  ("relays.bladepool.com", 3001),
  ("47.cardano.staked.cloud", 3001),
  ("44.cardano.staked.cloud", 3001),
  ("cf1r1.mainnet.pool.cardanofoundation.org", 3001),
  ("cf1r2.mainnet.pool.cardanofoundation.org", 3001),
  ("cf4r1.mainnet.pool.cardanofoundation.org", 3001),
  ("gateway.adavault.com", 4021),
  ("cardanosuisse.com", 170)
]

/-- Known Cardano preprod bootstrap peers -/
def preprodBootstrapPeers : List (String × UInt16) := [
  ("preprod-node.world.dev.cardano.org", 30000),
  ("preprod-node.play.dev.cardano.org", 3001),
  ("backbone.preprod.cardanofoundation.org", 3001)
]

/-- Known Cardano preview bootstrap peers -/
def previewBootstrapPeers : List (String × UInt16) := [
  ("preview-node.world.dev.cardano.org", 30002),
  ("preview-node.play.dev.cardano.org", 3001)
]

/-- Known Cardano SanchoNet bootstrap peers -/
def sanchonetBootstrapPeers : List (String × UInt16) := [
  ("sanchonet-node.world.dev.cardano.org", 30004),
  ("sanchonet-node.play.dev.cardano.org", 3001)
]

/-- Default to mainnet peers for backward compatibility -/
def bootstrapPeers : List (String × UInt16) := mainnetBootstrapPeers

end Dion.Network
