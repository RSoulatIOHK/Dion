# Cleanode: Formally Verified Cardano Node in Lean 4
## Project Breakdown: PI Objectives → Epics → Stories → Tasks

---

## PI OBJECTIVE 1: Foundation & Network Layer
**Goal:** Establish core infrastructure for network connectivity and data parsing with formal verification

### Epic 1.1: Project Infrastructure & Cryptographic Primitives
**Description:** Set up project structure, build system, and integrate/implement cryptographic primitives needed for Cardano

#### Story 1.1.1: Project Structure Setup
**As a** developer
**I want** a well-organized Lean 4 project structure
**So that** we can organize code by functional domains

- [ ] **Task 1.1.1.1:** Create directory structure (Network/, Ledger/, Crypto/, Consensus/, Node/, Config/)
- [ ] **Task 1.1.1.2:** Set up lakefile.toml with proper dependencies
- [ ] **Task 1.1.1.3:** Configure lean-toolchain for appropriate Lean version
- [ ] **Task 1.1.1.4:** Create initial module files with documentation

#### Story 1.1.2: Cryptographic Primitives Interface
**As a** node implementer
**I want** formally verified cryptographic primitives
**So that** we can trust the security properties of our node

- [ ] **Task 1.1.2.1:** Define Blake2b hash interface with specifications
- [ ] **Task 1.1.2.2:** Define Ed25519 signature interface with specifications
- [ ] **Task 1.1.2.3:** Define VRF interface with specifications
- [ ] **Task 1.1.2.4:** Define KES interface with specifications
- [ ] **Task 1.1.2.5:** Implement or FFI Blake2b with proofs of interface adherence
- [ ] **Task 1.1.2.6:** Implement or FFI Ed25519 with proofs
- [ ] **Task 1.1.2.7:** Write property-based tests for crypto primitives

#### Story 1.1.3: Configuration Management
**As a** node operator
**I want** to load network configurations from standard Cardano config files
**So that** I can connect to mainnet, preprod, or preview networks

- [ ] **Task 1.1.3.1:** Define NetworkConfig structure matching cardano-node format
- [ ] **Task 1.1.3.2:** Implement JSON parser for config files
- [ ] **Task 1.1.3.3:** Download and vendor mainnet/preview/preprod configs
- [ ] **Task 1.1.3.4:** Implement topology file parser
- [ ] **Task 1.1.3.5:** Implement genesis file parsers (Byron, Shelley, Alonzo, Conway)
- [ ] **Task 1.1.3.6:** Create configuration validation with proofs

---

### Epic 1.2: CBOR Encoding/Decoding with Proofs
**Description:** Implement CBOR codec with formal verification of correctness

#### Story 1.2.1: CBOR Data Model
**As a** protocol implementer
**I want** a formally specified CBOR data model
**So that** we can prove encoding/decoding correctness

- [ ] **Task 1.2.1.1:** Define CBOR value type in Lean
- [ ] **Task 1.2.1.2:** Define CBOR encoding specification
- [ ] **Task 1.2.1.3:** Define CBOR decoding specification
- [ ] **Task 1.2.1.4:** Prove encoding/decoding round-trip property

#### Story 1.2.2: CBOR Decoder Implementation
**As a** protocol implementer
**I want** a verified CBOR decoder
**So that** we can safely parse network messages

- [x] **Task 1.2.2.1:** Implement CBOR decoder for primitive types
- [x] **Task 1.2.2.2:** Implement CBOR decoder for arrays and maps
- [x] **Task 1.2.2.3:** Implement CBOR decoder for tagged values
- [ ] **Task 1.2.2.4:** Implement CBOR decoder for indefinite-length items
- [ ] **Task 1.2.2.5:** Prove decoder correctness against specification
- [x] **Task 1.2.2.6:** Add error handling with proof of safety

#### Story 1.2.3: CBOR Encoder Implementation
**As a** protocol implementer
**I want** a verified CBOR encoder
**So that** we can safely create network messages

- [ ] **Task 1.2.3.1:** Implement CBOR encoder for primitive types
- [ ] **Task 1.2.3.2:** Implement CBOR encoder for arrays and maps
- [ ] **Task 1.2.3.3:** Implement CBOR encoder for tagged values
- [ ] **Task 1.2.3.4:** Prove encoder correctness against specification
- [ ] **Task 1.2.3.5:** Prove encoder/decoder form bijection

---

### Epic 1.3: Network Multiplexer & Mini-Protocols
**Description:** Implement Ouroboros network layer with multiplexing and mini-protocols

#### Story 1.3.1: TCP Socket Layer
**As a** network implementer
**I want** TCP socket connectivity
**So that** we can communicate with Cardano nodes

- [ ] **Task 1.3.1.1:** Define socket interface for FFI
- [ ] **Task 1.3.1.2:** Implement or FFI TCP connect/disconnect
- [ ] **Task 1.3.1.3:** Implement or FFI TCP send/receive
- [ ] **Task 1.3.1.4:** Add timeout and error handling
- [ ] **Task 1.3.1.5:** Create socket connection proofs (liveness, safety)

#### Story 1.3.2: Multiplexer/Demultiplexer
**As a** network implementer
**I want** to multiplex multiple mini-protocols over a single TCP connection
**So that** we can efficiently communicate using the Cardano protocol

- [ ] **Task 1.3.2.1:** Define MUX segment structure per spec
- [ ] **Task 1.3.2.2:** Implement MUX segmentation (max 12,288 bytes SDU)
- [ ] **Task 1.3.2.3:** Implement DEMUX reassembly
- [ ] **Task 1.3.2.4:** Implement mini-protocol routing by ID
- [ ] **Task 1.3.2.5:** Add flow control and buffering
- [ ] **Task 1.3.2.6:** Prove MUX/DEMUX correctness (no message loss/corruption)

#### Story 1.3.3: Handshake Mini-Protocol
**As a** network implementer
**I want** to negotiate protocol version with peers
**So that** we can establish compatible connections

- [ ] **Task 1.3.3.1:** Define handshake message types per spec
- [ ] **Task 1.3.3.2:** Implement handshake client side (propose versions)
- [ ] **Task 1.3.3.3:** Implement handshake server side (accept/refuse)
- [ ] **Task 1.3.3.4:** Implement version negotiation logic
- [ ] **Task 1.3.3.5:** Handle protocol parameter negotiation
- [ ] **Task 1.3.3.6:** Prove handshake protocol properties (termination, agreement)

#### Story 1.3.4: Keep-Alive Mini-Protocol
**As a** network implementer
**I want** to maintain connection health
**So that** we can detect and handle dead connections

- [ ] **Task 1.3.4.1:** Define keep-alive message types
- [ ] **Task 1.3.4.2:** Implement keep-alive client (send cookies)
- [ ] **Task 1.3.4.3:** Implement keep-alive server (respond to cookies)
- [ ] **Task 1.3.4.4:** Add timeout detection
- [ ] **Task 1.3.4.5:** Implement latency measurement

---

### Epic 1.4: Chain Sync Mini-Protocol (Read-Only)
**Description:** Implement chain sync protocol to receive blocks from peers

#### Story 1.4.1: Chain Sync Protocol Types
**As a** protocol implementer
**I want** formally specified chain sync message types
**So that** we can communicate with peers correctly

- [ ] **Task 1.4.1.1:** Define ChainSync message types per spec (v2)
- [ ] **Task 1.4.1.2:** Define Point type (slot, hash)
- [ ] **Task 1.4.1.3:** Define Tip type (epoch, slot, hash)
- [ ] **Task 1.4.1.4:** Implement CBOR encoding for chain sync messages
- [ ] **Task 1.4.1.5:** Implement CBOR decoding for chain sync messages

#### Story 1.4.2: Chain Sync Client State Machine
**As a** protocol implementer
**I want** a formally verified chain sync client
**So that** we can reliably follow the chain

- [ ] **Task 1.4.2.1:** Define state machine states (Idle, Intersect, CanAwait, MustReply, Done)
- [ ] **Task 1.4.2.2:** Implement FindIntersect logic
- [ ] **Task 1.4.2.3:** Implement RequestNext logic
- [ ] **Task 1.4.2.4:** Handle RollForward messages
- [ ] **Task 1.4.2.5:** Handle RollBackward messages
- [ ] **Task 1.4.2.6:** Prove state machine safety properties

#### Story 1.4.3: Block Header Data Structures
**As a** protocol implementer
**I want** formally specified block header structures
**So that** we can parse and validate headers

- [ ] **Task 1.4.3.1:** Define Byron block header structure (CDDL)
- [ ] **Task 1.4.3.2:** Define Shelley+ block header structure (CDDL)
- [ ] **Task 1.4.3.3:** Define era-agnostic block header wrapper
- [ ] **Task 1.4.3.4:** Implement CBOR decoder for Byron headers
- [ ] **Task 1.4.3.5:** Implement CBOR decoder for Shelley+ headers
- [ ] **Task 1.4.3.6:** Prove header parsing correctness

---

## PI OBJECTIVE 2: Chain Observer & Block Processing
**Goal:** Create a functional read-only node that can sync and display the blockchain

### Epic 2.1: Chain Observer Application
**Description:** Build a working application that connects to a node and displays blocks

#### Story 2.1.1: Basic Chain Observer
**As a** node operator
**I want** to connect to a Cardano node and see blocks
**So that** I can verify basic network connectivity

- [ ] **Task 2.1.1.1:** Implement peer connection management
- [ ] **Task 2.1.1.2:** Implement chain sync from genesis (or checkpoint)
- [ ] **Task 2.1.1.3:** Display block headers (number, slot, hash)
- [ ] **Task 2.1.1.4:** Add logging and metrics
- [ ] **Task 2.1.1.5:** Handle reconnection on failure

#### Story 2.1.2: Preview Testnet Integration
**As a** developer
**I want** to sync with Preview testnet
**So that** I can test quickly without mainnet sync time

- [ ] **Task 2.1.2.1:** Configure for Preview network (magic: 2)
- [ ] **Task 2.1.2.2:** Test connection to Preview bootstrap peers
- [ ] **Task 2.1.2.3:** Verify block sync from Preview genesis
- [ ] **Task 2.1.2.4:** Document Preview sync process

#### Story 2.1.3: Block Storage
**As a** node operator
**I want** to persist blocks to disk
**So that** I don't need to re-sync on restart

- [ ] **Task 2.1.3.1:** Define block storage interface
- [ ] **Task 2.1.3.2:** Implement immutable block database
- [ ] **Task 2.1.3.3:** Implement volatile block database (recent blocks)
- [ ] **Task 2.1.3.4:** Add block retrieval by hash/number
- [ ] **Task 2.1.3.5:** Implement checkpoint save/restore

---

### Epic 2.2: Block Fetch Mini-Protocol
**Description:** Implement block fetching to download full blocks (not just headers)

#### Story 2.2.1: Block Fetch Protocol Implementation
**As a** protocol implementer
**I want** to fetch ranges of blocks from peers
**So that** we can download full block bodies

- [ ] **Task 2.2.1.1:** Define BlockFetch message types per spec
- [ ] **Task 2.2.1.2:** Implement block fetch client state machine
- [ ] **Task 2.2.1.3:** Implement RequestRange logic
- [ ] **Task 2.2.1.4:** Handle BlockBody responses
- [ ] **Task 2.2.1.5:** Handle NoBlocks responses
- [ ] **Task 2.2.1.6:** Implement block fetch pipelining
- [ ] **Task 2.2.1.7:** Prove protocol correctness

#### Story 2.2.2: Full Block Data Structures
**As a** protocol implementer
**I want** formally specified full block structures
**So that** we can parse complete blocks

- [ ] **Task 2.2.2.1:** Define Byron block body structure
- [ ] **Task 2.2.2.2:** Define Shelley+ block body structure
- [ ] **Task 2.2.2.3:** Implement CBOR decoder for Byron blocks
- [ ] **Task 2.2.2.4:** Implement CBOR decoder for Shelley+ blocks
- [ ] **Task 2.2.2.5:** Prove block parsing correctness

---

### Epic 2.3: Transaction Parsing & Validation
**Description:** Parse and validate transactions with formal proofs

#### Story 2.3.1: Transaction Data Structures
**As a** ledger implementer
**I want** formally specified transaction structures for all eras
**So that** we can parse transactions correctly

- [ ] **Task 2.3.1.1:** Define Byron transaction structure (CDDL)
- [ ] **Task 2.3.1.2:** Define Shelley transaction structure (CDDL)
- [ ] **Task 2.3.1.3:** Define Mary transaction structure (multi-asset)
- [ ] **Task 2.3.1.4:** Define Alonzo transaction structure (Plutus)
- [ ] **Task 2.3.1.5:** Define Babbage transaction structure
- [ ] **Task 2.3.1.6:** Define Conway transaction structure (governance)
- [ ] **Task 2.3.1.7:** Implement CBOR decoders for all eras

#### Story 2.3.2: UTxO Model
**As a** ledger implementer
**I want** a formally verified UTxO model
**So that** we can prove transaction validity preserves invariants

- [ ] **Task 2.3.2.1:** Define UTxO set structure
- [ ] **Task 2.3.2.2:** Define TxIn (transaction input) type
- [ ] **Task 2.3.2.3:** Define TxOut (transaction output) type
- [ ] **Task 2.3.2.4:** Implement UTxO set operations (add, remove, lookup)
- [ ] **Task 2.3.2.5:** Prove UTxO invariants (no double-spend, balance preservation)

#### Story 2.3.3: Transaction Validation Rules
**As a** ledger implementer
**I want** formally verified transaction validation
**So that** we can prove only valid transactions are accepted

- [x] **Task 2.3.3.1:** Implement input validation (inputs exist in UTxO)
- [x] **Task 2.3.3.2:** Implement balance validation (inputs ≥ outputs + fees)
- [ ] **Task 2.3.3.3:** Implement signature validation → expanded in **Story 2.3.5**
- [ ] **Task 2.3.3.4:** Implement script validation (Plutus) → expanded in **Story 2.3.6**
- [ ] **Task 2.3.3.5:** Implement native script validation → expanded in **Story 2.3.6**
- [ ] **Task 2.3.3.6:** Prove validation correctness → expanded in **Story 2.3.8**

#### Story 2.3.4: Fee Calculation
**As a** ledger implementer
**I want** formally verified fee calculation
**So that** we can prove fee rules are correctly enforced

- [x] **Task 2.3.4.1:** Define fee calculation formula per era
- [x] **Task 2.3.4.2:** Implement minFee calculation
- [x] **Task 2.3.4.3:** Implement script execution cost calculation
- [x] **Task 2.3.4.4:** Prove fee calculation properties (monotonicity, non-negativity)

#### Story 2.3.5: Witness Set Extension & Signature Validation
**As a** ledger implementer
**I want** complete witness set parsing and Ed25519 signature verification
**So that** only properly signed transactions are accepted

- [ ] **Task 2.3.5.1:** Extend WitnessSet with vkey witnesses, scripts, datums
- [ ] **Task 2.3.5.2:** Implement CBOR decoder for extended WitnessSet
- [ ] **Task 2.3.5.3:** Implement transaction hash computation (Blake2b-256 of body CBOR)
- [ ] **Task 2.3.5.4:** Implement vkey witness verification (Ed25519 over tx hash)
- [ ] **Task 2.3.5.5:** Extract required signers from input addresses
- [ ] **Task 2.3.5.6:** Wire signature validation into validateTransaction (replace stub)
- [ ] **Task 2.3.5.7:** Implement bootstrap witness validation (Byron-era addresses)

#### Story 2.3.6: Script Validation Pipeline
**As a** ledger implementer
**I want** native script evaluation and Plutus script validation
**So that** script-locked UTxOs can only be spent when scripts succeed

- [ ] **Task 2.3.6.1:** Wire native script evaluation into validation pipeline
- [ ] **Task 2.3.6.2:** Validate script data hash matches witness content
- [ ] **Task 2.3.6.3:** Implement collateral input validation
- [ ] **Task 2.3.6.4:** Implement execution budget enforcement
- [ ] **Task 2.3.6.5:** Design UPLC interpreter integration strategy
- [ ] **Task 2.3.6.6:** Implement Plutus script context construction
- [ ] **Task 2.3.6.7:** Implement Plutus V1/V2/V3 script execution

#### Story 2.3.7: Extended Transaction Body & Validation Rules
**As a** ledger implementer
**I want** the full transaction body fields and remaining validation rules
**So that** all Cardano ledger rules are enforced

- [ ] **Task 2.3.7.1:** Extend TransactionBody with all Conway-era fields
- [ ] **Task 2.3.7.2:** Implement min-ADA per output validation
- [ ] **Task 2.3.7.3:** Implement multi-asset balance validation
- [ ] **Task 2.3.7.4:** Implement validity interval (TTL) validation
- [ ] **Task 2.3.7.5:** Implement required signers validation (Alonzo+)
- [ ] **Task 2.3.7.6:** Implement reference input validation (Babbage+)
- [ ] **Task 2.3.7.7:** Implement mint/burn policy validation
- [ ] **Task 2.3.7.8:** Implement certificate validation in state application

#### Story 2.3.8: Transaction Validation Formal Proofs
**As a** protocol implementer
**I want** formal proofs that validation preserves ledger invariants
**So that** we can guarantee correctness of the validation pipeline

- [ ] **Task 2.3.8.1:** Prove balance validation ensures value conservation
- [ ] **Task 2.3.8.2:** Prove UTxO set consistency after transaction application
- [ ] **Task 2.3.8.3:** Prove state transition correctness
- [ ] **Task 2.3.8.4:** Prove state invariants are preserved across transitions
- [ ] **Task 2.3.8.5:** Prove fee monotonicity and bounds

---

### Epic 2.4: Ledger State Management
**Description:** Maintain and update ledger state with proofs

#### Story 2.4.1: Ledger State Structure
**As a** ledger implementer
**I want** a formally specified ledger state
**So that** we can track blockchain state correctly

- [ ] **Task 2.4.1.1:** Define core ledger state structure
- [ ] **Task 2.4.1.2:** Define stake pool state
- [ ] **Task 2.4.1.3:** Define delegation state
- [ ] **Task 2.4.1.4:** Define protocol parameters state
- [ ] **Task 2.4.1.5:** Define epoch boundary state

#### Story 2.4.2: Block Application
**As a** ledger implementer
**I want** to apply blocks to ledger state with proofs
**So that** we can maintain correct state

- [ ] **Task 2.4.2.1:** Implement block application logic
- [ ] **Task 2.4.2.2:** Implement transaction application
- [ ] **Task 2.4.2.3:** Implement epoch boundary transitions
- [ ] **Task 2.4.2.4:** Prove state transition correctness
- [ ] **Task 2.4.2.5:** Prove state invariants are preserved

#### Story 2.4.3: Ledger State Persistence
**As a** node operator
**I want** to save and restore ledger state
**So that** restarts are fast

- [ ] **Task 2.4.3.1:** Implement ledger snapshot format
- [ ] **Task 2.4.3.2:** Implement snapshot creation
- [ ] **Task 2.4.3.3:** Implement snapshot restoration
- [ ] **Task 2.4.3.4:** Add snapshot validation

---

## PI OBJECTIVE 3: Relay Node Functionality
**Goal:** Enable transaction propagation and P2P networking

### Epic 3.1: Transaction Submission Mini-Protocol
**Description:** Implement transaction submission and propagation

#### Story 3.1.1: TxSubmission2 Protocol
**As a** relay node operator
**I want** to receive and forward transactions
**So that** I can participate in transaction diffusion

- [ ] **Task 3.1.1.1:** Define TxSubmission2 message types
- [ ] **Task 3.1.1.2:** Implement client side (announce transactions)
- [ ] **Task 3.1.1.3:** Implement server side (accept transactions)
- [ ] **Task 3.1.1.4:** Implement transaction acknowledgment
- [ ] **Task 3.1.1.5:** Prove protocol properties

#### Story 3.1.2: Mempool Management
**As a** relay node operator
**I want** a verified mempool
**So that** valid transactions are queued correctly

- [ ] **Task 3.1.2.1:** Define mempool structure
- [ ] **Task 3.1.2.2:** Implement transaction addition with validation
- [ ] **Task 3.1.2.3:** Implement transaction removal (after inclusion)
- [ ] **Task 3.1.2.4:** Implement mempool pruning (size limits)
- [ ] **Task 3.1.2.5:** Prove mempool invariants

#### Story 3.1.3: Mempool Validation Hardening
**As a** relay node operator
**I want** the mempool to properly validate and track transactions
**So that** only valid transactions are relayed to peers

- [ ] **Task 3.1.3.1:** Parse transactions from CBOR in addTxRaw (currently creates dummy tx)
- [ ] **Task 3.1.3.2:** Implement proper timestamp tracking (currently hardcoded to 0)
- [ ] **Task 3.1.3.3:** Track mempool-internal UTxO dependencies
- [ ] **Task 3.1.3.4:** Add proper mempool capacity error types (not reusing TxTooLarge)

---

### Epic 3.2: Peer-to-Peer Networking
**Description:** Implement P2P peer discovery and management

#### Story 3.2.1: Peer Sharing Mini-Protocol
**As a** relay node operator
**I want** to discover peers dynamically
**So that** I can participate in the P2P network

- [ ] **Task 3.2.1.1:** Define PeerSharing message types
- [ ] **Task 3.2.1.2:** Implement peer request logic
- [ ] **Task 3.2.1.3:** Implement peer response logic
- [ ] **Task 3.2.1.4:** Implement peer database

#### Story 3.2.2: Peer Selection
**As a** relay node operator
**I want** intelligent peer selection
**So that** I maintain healthy connections

- [ ] **Task 3.2.2.1:** Implement peer selection policy
- [ ] **Task 3.2.2.2:** Implement connection governor
- [ ] **Task 3.2.2.3:** Handle inbound/outbound connection limits
- [ ] **Task 3.2.2.4:** Implement peer scoring

---

## PI OBJECTIVE 4: Block Production
**Goal:** Enable stake pool operation and block forging

### Epic 4.1: Ouroboros Praos Consensus
**Description:** Implement consensus protocol with formal verification

#### Story 4.1.1: VRF Leader Election
**As a** stake pool operator
**I want** verified VRF-based leader election
**So that** I can determine when to produce blocks

- [ ] **Task 4.1.1.1:** Define VRF evaluation per Praos spec
- [ ] **Task 4.1.1.2:** Implement slot leader check
- [ ] **Task 4.1.1.3:** Implement chain density check
- [ ] **Task 4.1.1.4:** Prove leader election properties

#### Story 4.1.2: KES Key Management
**As a** stake pool operator
**I want** secure KES key handling
**So that** my block signatures are valid

- [ ] **Task 4.1.2.1:** Define KES key evolution
- [ ] **Task 4.1.2.2:** Implement key evolution logic
- [ ] **Task 4.1.2.3:** Implement secure key storage
- [ ] **Task 4.1.2.4:** Add key period validation

#### Story 4.1.3: Block Forging
**As a** stake pool operator
**I want** to forge valid blocks
**So that** I can participate in consensus

- [ ] **Task 4.1.3.1:** Implement block header creation
- [ ] **Task 4.1.3.2:** Implement transaction selection from mempool
- [ ] **Task 4.1.3.3:** Implement block signing with KES
- [ ] **Task 4.1.3.4:** Implement VRF proof generation
- [ ] **Task 4.1.3.5:** Prove forged blocks are valid

---

### Epic 4.2: Stake Pool Operations
**Description:** Support full stake pool functionality

#### Story 4.2.1: Stake Pool Registration
**As a** stake pool operator
**I want** to register my pool
**So that** I can accept delegation

- [ ] **Task 4.2.1.1:** Define pool registration certificate
- [ ] **Task 4.2.1.2:** Implement pool registration transaction
- [ ] **Task 4.2.1.3:** Implement pool metadata handling

#### Story 4.2.2: Rewards Calculation
**As a** stake pool operator
**I want** verified rewards calculation
**So that** rewards are distributed correctly

- [ ] **Task 4.2.2.1:** Define rewards calculation formula
- [ ] **Task 4.2.2.2:** Implement epoch rewards distribution
- [ ] **Task 4.2.2.3:** Prove rewards properties (sum preservation)

---

## PI OBJECTIVE 5: Advanced Features & Optimization
**Goal:** Production readiness and advanced capabilities

### Epic 5.1: Mithril Integration
**Description:** Fast bootstrap using Mithril snapshots

#### Story 5.1.1: Mithril Client
**As a** node operator
**I want** to bootstrap from Mithril snapshots
**So that** sync is fast

- [ ] **Task 5.1.1.1:** Define Mithril certificate structure
- [ ] **Task 5.1.1.2:** Implement STM signature verification
- [ ] **Task 5.1.1.3:** Implement snapshot download
- [ ] **Task 5.1.1.4:** Prove snapshot validity

---

### Epic 5.2: Performance Optimization
**Description:** Optimize for production workloads

#### Story 5.2.1: Code Generation Optimization
**As a** developer
**I want** optimized compiled code
**So that** the node performs well

- [ ] **Task 5.2.1.1:** Profile critical paths
- [ ] **Task 5.2.1.2:** Optimize CBOR parsing
- [ ] **Task 5.2.1.3:** Optimize cryptographic operations
- [ ] **Task 5.2.1.4:** Extract to C++ where needed

---

### Epic 5.3: Monitoring & Operations
**Description:** Production monitoring and operations tools

#### Story 5.3.1: Metrics & Monitoring
**As a** node operator
**I want** comprehensive metrics
**So that** I can monitor node health

- [ ] **Task 5.3.1.1:** Implement Prometheus metrics endpoint
- [ ] **Task 5.3.1.2:** Add key performance metrics
- [ ] **Task 5.3.1.3:** Add resource usage metrics

#### Story 5.3.2: CLI & Management
**As a** node operator
**I want** CLI tools for node management
**So that** I can operate the node effectively

- [ ] **Task 5.3.2.1:** Implement node CLI interface
- [ ] **Task 5.3.2.2:** Add node status commands
- [ ] **Task 5.3.2.3:** Add configuration commands

---

## Labels for GitHub Issues

- `pi-objective`: Program Increment level goal
- `epic`: Large body of work
- `story`: User-facing functionality
- `task`: Technical implementation work
- `phase-1`: Chain Observer
- `phase-2`: Block Processing
- `phase-3`: Relay Node
- `phase-4`: Block Production
- `phase-5`: Advanced Features
- `crypto`: Cryptographic primitives
- `network`: Network layer
- `ledger`: Ledger layer
- `consensus`: Consensus layer
- `formal-verification`: Involves formal proofs
- `documentation`: Documentation needed
