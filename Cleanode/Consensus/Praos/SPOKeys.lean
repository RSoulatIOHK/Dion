import Cleanode.Crypto.TextEnvelope
import Cleanode.Consensus.Praos.BlockForge
import Cleanode.Network.Crypto

/-!
# SPO Key Management

Loads all key files needed for block production:
- VRF signing key (.vrf.skey)
- VRF verification key (.vrf.vkey)
- KES signing key (.kes.skey)
- Operational certificate (.opcert)
- Pool cold verification key (.vkey) — for pool ID derivation

## Key File Locations
Typical Cardano SPO key layout:
```
keys/
  pool.vkey        # Cold verification key
  pool.skey        # Cold signing key (keep offline!)
  vrf.skey         # VRF signing key
  vrf.vkey         # VRF verification key
  kes.skey         # KES signing key
  kes.vkey         # KES verification key
  opcert           # Operational certificate
  opcert.counter   # OpCert counter file
```

## References
- https://www.coincashew.com/coins/overview-ada/guide-how-to-build-a-haskell-stakepool-node
-/

namespace Cleanode.Consensus.Praos.SPOKeys

open Cleanode.Crypto.TextEnvelope
open Cleanode.Consensus.Praos.BlockForge
open Cleanode.Consensus.Praos.ConsensusState
open Cleanode.Network.Crypto

-- ====================
-- = SPO Config       =
-- ====================

/-- Paths to all SPO key files -/
structure SPOKeyPaths where
  vrfSigningKey : String      -- Path to vrf.skey
  kesSigningKey : String      -- Path to kes.skey
  operationalCert : String    -- Path to node.cert (opcert)
  poolVerificationKey : String -- Path to pool.vkey (for pool ID)
  deriving Repr

/-- Default key paths relative to a directory -/
def SPOKeyPaths.default (keyDir : String) : SPOKeyPaths :=
  { vrfSigningKey := s!"{keyDir}/vrf.skey"
    kesSigningKey := s!"{keyDir}/kes.skey"
    operationalCert := s!"{keyDir}/node.cert"
    poolVerificationKey := s!"{keyDir}/pool.vkey" }

-- ====================
-- = Key Loading      =
-- ====================

/-- Load all SPO keys and build ForgeParams -/
def loadSPOKeys (paths : SPOKeyPaths) : IO (Except String ForgeParams) := do
  -- Load VRF signing key (returns seed + public key)
  let vrfResult ← loadVrfSigningKey paths.vrfSigningKey
  let (vrfSeed, _vrfPub) ← match vrfResult with
    | .ok r => pure r
    | .error e => return .error s!"VRF signing key: {e}"

  -- Load KES signing key
  let kesResult ← loadKesSigningKey paths.kesSigningKey
  let kesBytes ← match kesResult with
    | .ok r => pure r
    | .error e => return .error s!"KES signing key: {e}"

  -- Load operational certificate
  let opCertResult ← loadOperationalCert paths.operationalCert
  let (hotVKey, seqNum, kesPeriod, coldSig) ← match opCertResult with
    | .ok r => pure r
    | .error e => return .error s!"Operational certificate: {e}"

  -- Load pool verification key and derive pool ID (Blake2b-256 hash)
  let poolVKeyResult ← loadPoolVerificationKey paths.poolVerificationKey
  let poolVKey ← match poolVKeyResult with
    | .ok r => pure r
    | .error e => return .error s!"Pool verification key: {e}"

  let poolId ← blake2b_256 poolVKey

  -- Build the operational cert structure
  let opCert : OperationalCert := {
    hotVKey := hotVKey
    sequenceNumber := seqNum
    kesPeriod := kesPeriod
    coldKeySignature := coldSig
  }

  -- Build ForgeParams
  return .ok {
    vrfSecretKey := vrfSeed.toList.map (·)
    vrfPublicKey := []  -- Will be derived from secret key during VRF prove
    kesSigningKey := kesBytes.toList.map (·)
    operationalCert := opCert
    poolId := poolId
    protocolMajor := 10  -- Conway era
    protocolMinor := 0
  }

-- ====================
-- = Validation       =
-- ====================

/-- Validate that all key files exist before attempting to load -/
def validateKeyPaths (paths : SPOKeyPaths) : IO (List String) := do
  let mut errors : List String := []
  let files := [
    (paths.vrfSigningKey, "VRF signing key"),
    (paths.kesSigningKey, "KES signing key"),
    (paths.operationalCert, "Operational certificate"),
    (paths.poolVerificationKey, "Pool verification key")
  ]
  for pair in files do
    let fileExists ← System.FilePath.pathExists pair.1
    if !fileExists then
      errors := errors ++ [s!"{pair.2} not found: {pair.1}"]
  return errors

/-- Print key file info for diagnostics -/
def printKeyInfo (paths : SPOKeyPaths) : IO Unit := do
  IO.println "[spo] Key file paths:"
  IO.println s!"  VRF signing key:    {paths.vrfSigningKey}"
  IO.println s!"  KES signing key:    {paths.kesSigningKey}"
  IO.println s!"  Operational cert:   {paths.operationalCert}"
  IO.println s!"  Pool verification:  {paths.poolVerificationKey}"

  let errors ← validateKeyPaths paths
  if errors.isEmpty then
    IO.println "[spo] All key files found."
  else
    IO.println "[spo] Missing files:"
    for e in errors do
      IO.println s!"  - {e}"

end Cleanode.Consensus.Praos.SPOKeys
