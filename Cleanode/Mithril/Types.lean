import Lean

/-!
# Mithril Types

Core data types for the Mithril protocol integration:
- Aggregator configuration per network
- Certificate structure (hash chain)
- Snapshot digest and download metadata

## References
- https://mithril.network/doc/manual/developer-docs/nodes/mithril-aggregator
- Mithril Aggregator REST API
-/

namespace Cleanode.Mithril.Types

open Lean (Json)

-- ====================
-- = Network Config   =
-- ====================

/-- Mithril network identifier -/
inductive MithrilNetwork where
  | mainnet
  | preprod
  | preview
  deriving Repr, BEq

instance : ToString MithrilNetwork where
  toString
    | .mainnet => "mainnet"
    | .preprod => "preprod"
    | .preview => "preview"

/-- Aggregator endpoint configuration -/
structure AggregatorConfig where
  network : MithrilNetwork
  aggregatorUrl : String
  genesisVerificationKey : String  -- Hex-encoded genesis verification key
  deriving Repr

/-- Default aggregator URLs per network -/
def defaultAggregatorUrl : MithrilNetwork → String
  | .mainnet => "https://aggregator.release-mainnet.api.mithril.network/aggregator"
  | .preprod => "https://aggregator.release-preprod.api.mithril.network/aggregator"
  | .preview => "https://aggregator.pre-release-preview.api.mithril.network/aggregator"

/-- Create default config for a network -/
def AggregatorConfig.default (network : MithrilNetwork) : AggregatorConfig :=
  { network
    aggregatorUrl := defaultAggregatorUrl network
    genesisVerificationKey := "" }

-- ====================
-- = STM Parameters   =
-- ====================

/-- STM (Stake-based Threshold Multi-signature) protocol parameters -/
structure StmParameters where
  k : Nat       -- Quorum parameter: minimum number of valid signatures
  m : Nat       -- Total number of lotteries
  phiF : Float  -- Participation rate (0.0 to 1.0)
  deriving Repr

-- ====================
-- = Certificate      =
-- ====================

/-- Beacon identifying a specific point in the chain -/
structure CardanoBeacon where
  epoch : Nat
  immutableFileNumber : Nat
  deriving Repr, BEq

/-- The type of entity that was signed -/
inductive SignedEntityType where
  | cardanoImmutableFilesFull (beacon : CardanoBeacon)
  | mithrilStakeDistribution (epoch : Nat)
  | cardanoTransactions (epoch : Nat) (blockNumber : Nat)
  | cardanoStakeDistribution (epoch : Nat)
  deriving Repr

/-- Certificate metadata -/
structure CertificateMetadata where
  network : String
  version : String
  parameters : StmParameters
  initiatedAt : String    -- ISO 8601 timestamp
  sealedAt : String       -- ISO 8601 timestamp
  totalSigners : Nat
  deriving Repr

/-- Protocol message parts (what was actually signed) -/
structure ProtocolMessageParts where
  snapshotDigest : Option String              -- For snapshot certificates
  cardanoTransactionsMerkleRoot : Option String
  nextAggregateVerificationKey : Option String
  latestBlockNumber : Option String
  currentEpoch : Option String
  deriving Repr

/-- A Mithril certificate in the chain -/
structure MithrilCertificate where
  hash : String                   -- Certificate hash (hex)
  previousHash : String           -- Previous certificate hash (hex), empty for genesis
  epoch : Nat
  signedEntityType : String       -- Raw JSON string of the entity type
  metadata : CertificateMetadata
  protocolMessage : ProtocolMessageParts
  signedMessage : String          -- Hex-encoded signed message
  aggregateVerificationKey : String  -- Hex-encoded AVK
  deriving Repr

-- ====================
-- = Snapshot Digest  =
-- ====================

/-- A Mithril snapshot entry from the aggregator -/
structure MithrilSnapshot where
  digest : String                 -- Snapshot digest (hex hash)
  network : String
  beacon : CardanoBeacon
  certificateHash : String        -- Hash of the certificate that signed this snapshot
  size : Nat                      -- Compressed size in bytes
  ancillarySize : Nat             -- Ancillary files size
  createdAt : String              -- ISO 8601 timestamp
  locations : List String         -- Download URLs
  ancillaryLocations : List String
  compressionAlgorithm : String   -- "zstandard"
  cardanoNodeVersion : String
  deriving Repr

-- ====================
-- = JSON Parsing     =
-- ====================

/-- Parse STM parameters from JSON -/
def parseStmParameters (j : Json) : Option StmParameters := do
  let k ← (j.getObjValAs? Nat "k").toOption
  let m ← (j.getObjValAs? Nat "m").toOption
  let phiF ← (j.getObjValAs? Float "phi_f").toOption
  some { k, m, phiF }

/-- Parse a CardanoBeacon from JSON -/
def parseBeacon (j : Json) : Option CardanoBeacon := do
  let epoch ← (j.getObjValAs? Nat "epoch").toOption
  let imm ← (j.getObjValAs? Nat "immutable_file_number").toOption
  some { epoch, immutableFileNumber := imm }

/-- Parse certificate metadata from JSON -/
def parseCertificateMetadata (j : Json) : Option CertificateMetadata := do
  let network ← (j.getObjValAs? String "network").toOption
  let version ← (j.getObjValAs? String "version").toOption
  let paramsJson ← (j.getObjVal? "parameters").toOption
  let parameters ← parseStmParameters paramsJson
  let initiatedAt ← (j.getObjValAs? String "initiated_at").toOption
  let sealedAt ← (j.getObjValAs? String "sealed_at").toOption
  let totalSigners ← (j.getObjValAs? Nat "total_signers").toOption
  some { network, version, parameters, initiatedAt, sealedAt, totalSigners }

/-- Parse protocol message parts from JSON -/
def parseProtocolMessage (j : Json) : Option ProtocolMessageParts := do
  let parts ← (j.getObjVal? "message_parts").toOption
  some {
    snapshotDigest := (parts.getObjValAs? String "snapshot_digest").toOption
    cardanoTransactionsMerkleRoot := (parts.getObjValAs? String "cardano_transactions_merkle_root").toOption
    nextAggregateVerificationKey := (parts.getObjValAs? String "next_aggregate_verification_key").toOption
    latestBlockNumber := (parts.getObjValAs? String "latest_block_number").toOption
    currentEpoch := (parts.getObjValAs? String "current_epoch").toOption
  }

/-- Parse a single MithrilCertificate from JSON -/
def parseCertificate (j : Json) : Option MithrilCertificate := do
  let hash ← (j.getObjValAs? String "hash").toOption
  let previousHash ← (j.getObjValAs? String "previous_hash").toOption
  let epoch ← (j.getObjValAs? Nat "epoch").toOption
  let metaJson ← (j.getObjVal? "metadata").toOption
  let metadata ← parseCertificateMetadata metaJson
  let protoJson ← (j.getObjVal? "protocol_message").toOption
  let protocolMessage ← parseProtocolMessage protoJson
  let signedMessage ← (j.getObjValAs? String "signed_message").toOption
  let avk ← (j.getObjValAs? String "aggregate_verification_key").toOption
  -- signed_entity_type is complex, store as raw string
  let entityType := match j.getObjVal? "signed_entity_type" with
    | .ok v => toString v
    | .error _ => ""
  some {
    hash, previousHash, epoch
    signedEntityType := entityType
    metadata, protocolMessage, signedMessage
    aggregateVerificationKey := avk
  }

/-- Parse a list of certificates from JSON array -/
def parseCertificates (j : Json) : Option (List MithrilCertificate) := do
  let arr ← j.getArr?.toOption
  let mut result : List MithrilCertificate := []
  for item in arr do
    match parseCertificate item with
    | some cert => result := result ++ [cert]
    | none => pure ()
  some result

/-- Helper to parse a string array from JSON -/
private def parseStringArray (j : Json) : List String :=
  match j.getArr?.toOption with
  | some arr => arr.foldl (fun acc item =>
      match item.getStr?.toOption with
      | some s => acc ++ [s]
      | none => acc) []
  | none => []

/-- Parse a MithrilSnapshot from JSON -/
def parseSnapshot (j : Json) : Option MithrilSnapshot := do
  let digest ← (j.getObjValAs? String "digest").toOption
  let network ← (j.getObjValAs? String "network").toOption
  let beaconJson ← (j.getObjVal? "beacon").toOption
  let beacon ← parseBeacon beaconJson
  let certificateHash ← (j.getObjValAs? String "certificate_hash").toOption
  let size ← (j.getObjValAs? Nat "size").toOption
  let ancillarySize := (j.getObjValAs? Nat "ancillary_size").toOption |>.getD 0
  let createdAt ← (j.getObjValAs? String "created_at").toOption
  let locations := match (j.getObjVal? "locations").toOption with
    | some v => parseStringArray v
    | none => []
  let ancillaryLocations := match (j.getObjVal? "ancillary_locations").toOption with
    | some v => parseStringArray v
    | none => []
  let compressionAlgorithm ← (j.getObjValAs? String "compression_algorithm").toOption
  let cardanoNodeVersion ← (j.getObjValAs? String "cardano_node_version").toOption
  some {
    digest, network, beacon, certificateHash, size, ancillarySize
    createdAt, locations, ancillaryLocations
    compressionAlgorithm, cardanoNodeVersion
  }

/-- Parse a list of snapshots from JSON array -/
def parseSnapshots (j : Json) : Option (List MithrilSnapshot) := do
  let arr ← j.getArr?.toOption
  let mut result : List MithrilSnapshot := []
  for item in arr do
    match parseSnapshot item with
    | some snap => result := result ++ [snap]
    | none => pure ()
  some result

end Cleanode.Mithril.Types
