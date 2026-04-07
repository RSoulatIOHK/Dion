import Lean

/-!
# Topology File Parser

Parses Cardano node topology files that define peer connections.
The topology file specifies bootstrap peers, local roots, and public roots.

## Topology File Format (P2P)
```json
{
  "bootstrapPeers": [
    { "address": "backbone.cardano.iog.io", "port": 3001 }
  ],
  "localRoots": [
    { "accessPoints": [{ "address": "...", "port": 3001 }],
      "advertise": false, "valency": 1, "trustable": false }
  ],
  "publicRoots": [
    { "accessPoints": [{ "address": "...", "port": 3001 }],
      "advertise": false }
  ],
  "useLedgerAfterSlot": 128908821
}
```

## References
- cardano-node topology file documentation
- https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/understanding-config-files.md
-/

namespace Dion.Config.Topology

open Lean (Json)

-- ====================
-- = Types            =
-- ====================

/-- A single peer endpoint (address + port) -/
structure AccessPoint where
  address : String
  port : UInt16
  deriving Repr, BEq

/-- A bootstrap peer -/
structure BootstrapPeer where
  address : String
  port : UInt16
  deriving Repr, BEq

/-- A local root group (peers we always try to connect to) -/
structure LocalRoot where
  accessPoints : List AccessPoint
  advertise : Bool
  valency : Nat
  trustable : Bool
  deriving Repr

/-- A public root group (well-known peers for bootstrapping) -/
structure PublicRoot where
  accessPoints : List AccessPoint
  advertise : Bool
  deriving Repr

/-- Complete topology configuration -/
structure Topology where
  bootstrapPeers : List BootstrapPeer
  localRoots : List LocalRoot
  publicRoots : List PublicRoot
  useLedgerAfterSlot : Option Nat
  deriving Repr

-- ====================
-- = Parsing          =
-- ====================

/-- Parse an AccessPoint from JSON -/
def parseAccessPoint (j : Json) : Option AccessPoint := do
  let address ← (j.getObjValAs? String "address").toOption
  let port ← (j.getObjValAs? Nat "port").toOption
  some { address := address, port := UInt16.ofNat port }

/-- Parse a list of AccessPoints from a JSON array -/
def parseAccessPoints (j : Json) : Option (List AccessPoint) := do
  let arr ← j.getArr?.toOption
  let mut result : List AccessPoint := []
  for item in arr do
    match parseAccessPoint item with
    | some ap => result := result ++ [ap]
    | none => pure ()
  some result

/-- Parse a BootstrapPeer from JSON -/
def parseBootstrapPeer (j : Json) : Option BootstrapPeer := do
  let address ← (j.getObjValAs? String "address").toOption
  let port ← (j.getObjValAs? Nat "port").toOption
  some { address := address, port := UInt16.ofNat port }

/-- Parse a LocalRoot from JSON -/
def parseLocalRoot (j : Json) : Option LocalRoot := do
  let apJson ← (j.getObjVal? "accessPoints").toOption
  let accessPoints ← parseAccessPoints apJson
  let advertise := (j.getObjValAs? Bool "advertise").toOption |>.getD false
  let valency := (j.getObjValAs? Nat "valency").toOption |>.getD 1
  let trustable := (j.getObjValAs? Bool "trustable").toOption |>.getD false
  some { accessPoints := accessPoints, advertise := advertise,
         valency := valency, trustable := trustable }

/-- Parse a PublicRoot from JSON -/
def parsePublicRoot (j : Json) : Option PublicRoot := do
  let apJson ← (j.getObjVal? "accessPoints").toOption
  let accessPoints ← parseAccessPoints apJson
  let advertise := (j.getObjValAs? Bool "advertise").toOption |>.getD false
  some { accessPoints := accessPoints, advertise := advertise }

/-- Parse a Topology from JSON -/
def parseTopology (j : Json) : Option Topology := do
  -- Bootstrap peers (optional)
  let bootstrapPeers := match j.getObjVal? "bootstrapPeers" with
    | .ok arr => match arr.getArr?.toOption with
      | some items => items.toList.filterMap parseBootstrapPeer
      | none => []
    | .error _ => []

  -- Local roots (optional)
  let localRoots := match j.getObjVal? "localRoots" with
    | .ok arr => match arr.getArr?.toOption with
      | some items => items.toList.filterMap parseLocalRoot
      | none => []
    | .error _ => []

  -- Public roots (optional)
  let publicRoots := match j.getObjVal? "publicRoots" with
    | .ok arr => match arr.getArr?.toOption with
      | some items => items.toList.filterMap parsePublicRoot
      | none => []
    | .error _ => []

  -- useLedgerAfterSlot (optional)
  let useLedgerAfterSlot := (j.getObjValAs? Nat "useLedgerAfterSlot").toOption

  some {
    bootstrapPeers := bootstrapPeers,
    localRoots := localRoots,
    publicRoots := publicRoots,
    useLedgerAfterSlot := useLedgerAfterSlot
  }

/-- Load a Topology from a JSON file -/
def loadTopology (path : System.FilePath) : IO (Except String Topology) := do
  try
    let contents ← IO.FS.readFile path
    match Json.parse contents with
    | .error e => return .error s!"Failed to parse topology JSON: {e}"
    | .ok json =>
        match parseTopology json with
        | none => return .error "Failed to parse topology structure"
        | some topo => return .ok topo
  catch e =>
    return .error s!"Failed to read topology file: {e}"

/-- Get all peer addresses from a topology (for connection) -/
def Topology.allPeers (t : Topology) : List AccessPoint :=
  let bootstrap := t.bootstrapPeers.map fun bp => AccessPoint.mk bp.address bp.port
  let local_ := t.localRoots.foldl (fun acc lr => acc ++ lr.accessPoints) []
  let public_ := t.publicRoots.foldl (fun acc pr => acc ++ pr.accessPoints) []
  bootstrap ++ local_ ++ public_

end Dion.Config.Topology
