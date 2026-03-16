/-!
# Node-to-Client Mini-Protocol Identifiers

The Node-to-Client (N2C) protocol uses different protocol IDs than Node-to-Node.
These are used for local communication between `cardano-cli` and the node via
a Unix domain socket.

## Protocol IDs
- 0x0000: Handshake (same as N2N)
- 0x0005: ChainSync (N2N uses 0x0002)
- 0x0006: LocalTxSubmission
- 0x0007: LocalStateQuery
- 0x0009: LocalTxMonitor

## References
- Ouroboros Network Spec Section 3.17
-/

namespace Cleanode.Network.N2C.MiniProtocolId

/-- Mini-protocol identifiers for node-to-client communication -/
inductive N2CMiniProtocolId where
  | Handshake         -- 0x0000: Version negotiation
  | ChainSync         -- 0x0005: Local chain sync (full blocks)
  | LocalTxSubmission -- 0x0006: Local transaction submission
  | LocalStateQuery   -- 0x0007: Local ledger state queries
  | LocalTxMonitor    -- 0x0009: Local mempool monitoring
  deriving Repr, BEq

def N2CMiniProtocolId.toUInt16 : N2CMiniProtocolId → UInt16
  | .Handshake         => 0x0000
  | .ChainSync         => 0x0005
  | .LocalTxSubmission => 0x0006
  | .LocalStateQuery   => 0x0007
  | .LocalTxMonitor    => 0x0009

def N2CMiniProtocolId.fromUInt16 (n : UInt16) : Option N2CMiniProtocolId :=
  match n with
  | 0x0000 => some .Handshake
  | 0x0005 => some .ChainSync
  | 0x0006 => some .LocalTxSubmission
  | 0x0007 => some .LocalStateQuery
  | 0x0009 => some .LocalTxMonitor
  | _      => none

instance : ToString N2CMiniProtocolId where
  toString
    | .Handshake         => "Handshake"
    | .ChainSync         => "ChainSync"
    | .LocalTxSubmission => "LocalTxSubmission"
    | .LocalStateQuery   => "LocalStateQuery"
    | .LocalTxMonitor    => "LocalTxMonitor"

end Cleanode.Network.N2C.MiniProtocolId
