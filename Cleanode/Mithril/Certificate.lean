import Cleanode.Mithril.Types
import Cleanode.Network.Http
import Lean

/-!
# Mithril Certificate Chain Verification

Fetches and verifies the certificate chain from a Mithril aggregator.
Each certificate's `previous_hash` must match the `hash` of its predecessor,
forming an unbroken chain back to the genesis certificate.

## Verification Steps
1. Fetch the certificate for a given snapshot
2. Walk the chain via `previous_hash` links
3. Verify hash linkage at each step
4. Stop at genesis certificate (previous_hash == "")

## References
- https://mithril.network/doc/mithril/mithril-protocol/certificates
-/

namespace Cleanode.Mithril.Certificate

open Cleanode.Mithril.Types
open Cleanode.Network.Http
open Lean (Json)

-- ====================
-- = Chain Errors     =
-- ====================

/-- Errors that can occur during certificate chain verification -/
inductive CertChainError where
  | httpError (msg : String)
  | parseError (msg : String)
  | chainBroken (certHash prevHash expectedHash : String)
  | chainTooLong (depth : Nat)
  | noCertificateFound
  deriving Repr

instance : ToString CertChainError where
  toString
    | .httpError msg => s!"HTTP error: {msg}"
    | .parseError msg => s!"Parse error: {msg}"
    | .chainBroken h p e => s!"Chain broken at {h}: previous_hash={p}, expected={e}"
    | .chainTooLong d => s!"Certificate chain too long (depth={d}), possible loop"
    | .noCertificateFound => "No certificate found"

-- ====================
-- = API Client       =
-- ====================

/-- Fetch a single certificate by hash from the aggregator -/
def fetchCertificate (baseUrl : String) (hash : String) : IO (Except CertChainError MithrilCertificate) := do
  let url := s!"{baseUrl}/certificate/{hash}"
  let result ← httpGetJson url
  match result with
  | .error msg => return .error (.httpError msg)
  | .ok body =>
    match Json.parse body with
    | .error e => return .error (.parseError s!"JSON parse failed: {e}")
    | .ok json =>
      match parseCertificate json with
      | some cert => return .ok cert
      | none => return .error (.parseError s!"Failed to parse certificate from {url}")

/-- Fetch the list of recent snapshots from the aggregator -/
def fetchSnapshotList (baseUrl : String) : IO (Except CertChainError (List MithrilSnapshot)) := do
  let url := s!"{baseUrl}/artifact/snapshots"
  let result ← httpGetJson url
  match result with
  | .error msg => return .error (.httpError msg)
  | .ok body =>
    match Json.parse body with
    | .error e => return .error (.parseError s!"JSON parse failed: {e}")
    | .ok json =>
      match parseSnapshots json with
      | some snaps => return .ok snaps
      | none => return .error (.parseError "Failed to parse snapshot list")

/-- Fetch the latest snapshot (first in the list) -/
def fetchLatestSnapshot (baseUrl : String) : IO (Except CertChainError MithrilSnapshot) := do
  let result ← fetchSnapshotList baseUrl
  match result with
  | .error e => return .error e
  | .ok snaps =>
    match snaps.head? with
    | some snap => return .ok snap
    | none => return .error .noCertificateFound

-- ====================
-- = Chain Verification =
-- ====================

/-- Maximum chain depth to prevent infinite loops -/
def maxChainDepth : Nat := 1000

/-- A verified certificate chain (newest first) -/
structure CertificateChain where
  certificates : List MithrilCertificate
  genesisHash : String   -- Hash of the genesis (last) certificate
  depth : Nat
  deriving Repr

/-- Verify the certificate chain starting from a certificate hash.
    Walks previous_hash links back to genesis, verifying linkage at each step.
    Returns the full chain (newest first) on success. -/
partial def verifyCertificateChain
    (baseUrl : String) (startHash : String)
    : IO (Except CertChainError CertificateChain) := do
  let mut chain : List MithrilCertificate := []
  let mut currentHash := startHash
  let mut depth : Nat := 0

  while depth < maxChainDepth do
    -- Fetch current certificate
    let result ← fetchCertificate baseUrl currentHash
    match result with
    | .error e => return .error e
    | .ok cert =>
      -- Verify hash matches what we expected
      if cert.hash != currentHash then
        return .error (.chainBroken cert.hash cert.hash currentHash)

      chain := cert :: chain

      -- Genesis certificate has empty previous_hash
      if cert.previousHash == "" then
        return .ok {
          certificates := chain.reverse
          genesisHash := cert.hash
          depth := depth + 1
        }

      -- Continue to previous certificate
      currentHash := cert.previousHash
      depth := depth + 1

  return .error (.chainTooLong maxChainDepth)

/-- Verify hash linkage of an already-fetched chain (no network calls).
    Checks that each cert's previous_hash matches the next cert's hash. -/
def verifyChainLinkage (chain : List MithrilCertificate) : Except CertChainError Unit := do
  let rec go : List MithrilCertificate → Except CertChainError Unit
    | [] => .ok ()
    | [_] => .ok ()  -- Last cert (genesis) — no more links to check
    | cert :: next :: rest =>
      if cert.previousHash != next.hash then
        .error (.chainBroken cert.hash cert.previousHash next.hash)
      else
        go (next :: rest)
  go chain

/-- Full verification: fetch chain and verify linkage -/
def fetchAndVerifyChain (baseUrl : String) (snapshotCertHash : String)
    : IO (Except CertChainError CertificateChain) := do
  let result ← verifyCertificateChain baseUrl snapshotCertHash
  match result with
  | .error e => return .error e
  | .ok chain =>
    -- Double-check linkage on the fetched chain
    match verifyChainLinkage chain.certificates with
    | .error e => return .error e
    | .ok () => return .ok chain

-- ====================
-- = Epoch Settings   =
-- ====================

/-- Fetch current epoch settings (STM parameters, signers) -/
def fetchEpochSettings (baseUrl : String) : IO (Except CertChainError StmParameters) := do
  let url := s!"{baseUrl}/epoch-settings"
  let result ← httpGetJson url
  match result with
  | .error msg => return .error (.httpError msg)
  | .ok body =>
    match Json.parse body with
    | .error e => return .error (.parseError s!"JSON parse failed: {e}")
    | .ok json =>
      let protoJson := (json.getObjVal? "signer_registration_protocol").toOption
      match protoJson >>= parseStmParameters with
      | some params => return .ok params
      | none => return .error (.parseError "Failed to parse STM parameters")

end Cleanode.Mithril.Certificate
