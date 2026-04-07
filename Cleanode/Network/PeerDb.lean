import Dion.Network.PeerSharing
import Dion.Config.Topology

/-!
# Peer Database

Tracks known peers with status, scoring, and connection history.
Seeded from topology configuration and enriched via PeerSharing discovery.

## Design
- In-memory peer registry backed by topology config
- Peer scoring: +1 for successful sync, -5 for connection failure, -10 for protocol error
- Cooling period after failures before retry
- Sources: "topology" (config file), "peer-sharing" (discovered), "ledger" (future)

## References
- Cardano node peer selection: warm/hot/cold peer classification
-/

namespace Dion.Network.PeerDb

open Dion.Network.PeerSharing
open Dion.Config.Topology

-- ====================
-- = Peer Status      =
-- ====================

/-- Current status of a known peer -/
inductive PeerStatus where
  | Available                    -- Known, not connected
  | Connected                    -- Currently connected
  | Cooling (untilMs : Nat)      -- Temporarily demoted after failure
  | Banned (reason : String)     -- Permanently excluded
  deriving Repr

-- ====================
-- = Peer Record      =
-- ====================

/-- A peer record with status and scoring metadata -/
structure PeerRecord where
  address      : PeerAddress
  status       : PeerStatus
  score        : Int             -- Quality score (higher = better)
  lastConnected : Option Nat     -- Timestamp of last successful connection
  lastFailed   : Option Nat      -- Timestamp of last failure
  failCount    : Nat             -- Consecutive failures
  successCount : Nat             -- Total successful connections
  source       : String          -- How we learned about this peer
  deriving Repr

/-- Create a peer record from a PeerAddress -/
def PeerRecord.fromAddress (addr : PeerAddress) (source : String := "topology") : PeerRecord :=
  { address := addr
    status := .Available
    score := 0
    lastConnected := none
    lastFailed := none
    failCount := 0
    successCount := 0
    source := source }

-- ====================
-- = Peer Database    =
-- ====================

/-- The peer database -/
structure PeerDb where
  peers    : List PeerRecord
  maxPeers : Nat := 200
  deriving Repr

-- ====================
-- = Construction     =
-- ====================

/-- Create an empty peer database -/
def PeerDb.empty : PeerDb :=
  { peers := [], maxPeers := 200 }

/-- Seed peer database from topology configuration -/
def PeerDb.fromTopology (topo : Topology) : PeerDb :=
  let bootstrapPeers := topo.bootstrapPeers.map fun bp =>
    PeerRecord.fromAddress { host := bp.address, port := bp.port } "topology-bootstrap"
  let localPeers := (topo.localRoots.map fun lr =>
    lr.accessPoints.map fun ap =>
      PeerRecord.fromAddress { host := ap.address, port := ap.port } "topology-local").flatten
  let publicPeers := (topo.publicRoots.map fun pr =>
    pr.accessPoints.map fun ap =>
      PeerRecord.fromAddress { host := ap.address, port := ap.port } "topology-public").flatten
  { peers := bootstrapPeers ++ localPeers ++ publicPeers, maxPeers := 200 }

-- ====================
-- = Queries          =
-- ====================

/-- Total number of known peers -/
def PeerDb.size (db : PeerDb) : Nat :=
  db.peers.length

/-- Get currently connected peers -/
def PeerDb.connected (db : PeerDb) : List PeerRecord :=
  db.peers.filter fun p =>
    match p.status with
    | .Connected => true
    | _ => false

/-- Get available (connectable) peers, sorted by score descending -/
def PeerDb.getAvailable (db : PeerDb) (count : Nat) : List PeerRecord :=
  let available := db.peers.filter fun p =>
    match p.status with
    | .Available => true
    | _ => false
  -- Sort by score (highest first)
  let sorted := available.toArray.qsort (fun a b => a.score > b.score)
  (sorted.toList).take count

/-- Find a peer by address -/
def PeerDb.find (db : PeerDb) (addr : PeerAddress) : Option PeerRecord :=
  db.peers.find? fun p => p.address == addr

-- ====================
-- = Mutations        =
-- ====================

/-- Helper: update a peer record by address -/
private def updatePeer (db : PeerDb) (addr : PeerAddress)
    (f : PeerRecord → PeerRecord) : PeerDb :=
  { db with peers := db.peers.map fun p =>
    if p.address == addr then f p else p }

/-- Add peers discovered via PeerSharing -/
def PeerDb.addDiscovered (db : PeerDb) (peers : List PeerAddress) : PeerDb :=
  let newPeers := peers.filter fun addr =>
    !db.peers.any (fun p => p.address == addr)
  let records := newPeers.map fun addr =>
    PeerRecord.fromAddress addr "peer-sharing"
  -- Enforce max peers limit
  let allPeers := db.peers ++ records
  let trimmed := if allPeers.length > db.maxPeers
    then allPeers.take db.maxPeers
    else allPeers
  { db with peers := trimmed }

/-- Mark a peer as connected -/
def PeerDb.onConnected (db : PeerDb) (addr : PeerAddress) (timestampMs : Nat := 0) : PeerDb :=
  updatePeer db addr fun p =>
    { p with
      status := .Connected
      lastConnected := some timestampMs
      failCount := 0
      successCount := p.successCount + 1
      score := p.score + 1 }

/-- Mark a peer as disconnected (goes back to available) -/
def PeerDb.onDisconnected (db : PeerDb) (addr : PeerAddress) : PeerDb :=
  updatePeer db addr fun p =>
    { p with status := .Available }

/-- Mark a peer as failed (cooling period) -/
def PeerDb.onError (db : PeerDb) (addr : PeerAddress) (timestampMs : Nat := 0) : PeerDb :=
  updatePeer db addr fun p =>
    let newFailCount := p.failCount + 1
    let cooldownMs := min (newFailCount * 30000) 300000  -- 30s per failure, max 5 min
    { p with
      status := .Cooling (untilMs := timestampMs + cooldownMs)
      lastFailed := some timestampMs
      failCount := newFailCount
      score := p.score - 5 }

/-- Unfreeze cooled peers whose cooling period has expired -/
def PeerDb.thaw (db : PeerDb) (currentMs : Nat) : PeerDb :=
  { db with peers := db.peers.map fun p =>
    match p.status with
    | .Cooling untilMs => if currentMs >= untilMs
        then { p with status := .Available }
        else p
    | _ => p }

/-- Adjust a peer's score directly -/
def PeerDb.adjustScore (db : PeerDb) (addr : PeerAddress) (delta : Int) : PeerDb :=
  updatePeer db addr fun p =>
    { p with score := p.score + delta }

/-- Ban a peer -/
def PeerDb.ban (db : PeerDb) (addr : PeerAddress) (reason : String) : PeerDb :=
  updatePeer db addr fun p =>
    { p with status := .Banned reason, score := p.score - 100 }

-- ====================
-- = Stats            =
-- ====================

/-- Peer database statistics -/
structure PeerDbStats where
  total      : Nat
  available  : Nat
  connected  : Nat
  cooling    : Nat
  banned     : Nat
  deriving Repr

/-- Get peer database statistics -/
def PeerDb.stats (db : PeerDb) : PeerDbStats :=
  let counts := db.peers.foldl (fun (a, c, co, b) p =>
    match p.status with
    | .Available => (a + 1, c, co, b)
    | .Connected => (a, c + 1, co, b)
    | .Cooling _ => (a, c, co + 1, b)
    | .Banned _ => (a, c, co, b + 1)
  ) (0, 0, 0, 0)
  { total := db.peers.length
    available := counts.1
    connected := counts.2.1
    cooling := counts.2.2.1
    banned := counts.2.2.2 }

end Dion.Network.PeerDb
