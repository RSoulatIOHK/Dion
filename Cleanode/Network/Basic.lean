/-!
# Network Module

This module implements the Ouroboros network layer for Cleanode.

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

namespace Cleanode.Network

/-- Known Cardano mainnet bootstrap peers -/
def mainnetBootstrapPeers : List (String × UInt16) := [
  ("backbone.cardano.iog.io", 3001),
  ("backbone.mainnet.cardanofoundation.org", 3001),
  ("backbone.mainnet.emurgornd.com", 3001)
]

/-- Known Cardano preprod bootstrap peers -/
def preprodBootstrapPeers : List (String × UInt16) := [
  ("preprod-node.world.dev.cardano.org", 30000),
  ("preprod-node.play.dev.cardano.org", 3001)
]

/-- Known Cardano preview bootstrap peers -/
def previewBootstrapPeers : List (String × UInt16) := [
  ("preview-node.world.dev.cardano.org", 30002),
  ("preview-node.play.dev.cardano.org", 3001)
]

/-- Default to mainnet peers for backward compatibility -/
def bootstrapPeers : List (String × UInt16) := mainnetBootstrapPeers

end Cleanode.Network
