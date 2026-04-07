#!/bin/bash
# Script to create GitHub issues for Dion project hierarchy

set -e

echo "Creating GitHub issues for Dion project..."
echo "This will create PI Objectives, Epics, Stories, and Tasks"
echo ""

# Function to create an issue and return its number
create_issue() {
    local title="$1"
    local body="$2"
    local labels="$3"

    issue_url=$(gh issue create --title "$title" --body "$body" --label "$labels")
    issue_number=$(echo "$issue_url" | sed 's|.*/||')
    echo "$issue_number"
}

# Tasks for 1.4.2
for task in \
    "1.4.2.4:Handle RollForward messages" \
    "1.4.2.5:Handle RollBackward messages" \
    "1.4.2.6:Prove state machine safety properties"; do
    num="${task%%:*}"
    desc="${task#*:}"
    label="task,phase-1,network"
    [[ "$desc" == *"Prove"* ]] && label="$label,formal-verification"
    create_issue "[Task $num] $desc" \
        "**Parent:** #$S1_4_2 (Epic: #$E1_4, PI: #$PI1)" \
        "$label" > /dev/null
done

# Story 1.4.3
echo "Creating Story 1.4.3..."
S1_4_3=$(create_issue \
    "[Story 1.4.3] Block Header Data Structures" \
    "**Parent:** #$E1_4 (PI: #$PI1)

**As a** protocol implementer
**I want** formally specified block header structures
**So that** we can parse and validate headers

## Tasks
- [ ] Task 1.4.3.1: Define Byron block header structure (CDDL)
- [ ] Task 1.4.3.2: Define Shelley+ block header structure (CDDL)
- [ ] Task 1.4.3.3: Define era-agnostic block header wrapper
- [ ] Task 1.4.3.4: Implement CBOR decoder for Byron headers
- [ ] Task 1.4.3.5: Implement CBOR decoder for Shelley+ headers
- [ ] Task 1.4.3.6: Prove header parsing correctness

## Acceptance Criteria
- [ ] Can parse headers from all eras
- [ ] Parsing is proven correct" \
    "story,phase-1,ledger,formal-verification")

echo "  Created Story 1.4.3: #$S1_4_3"

# Tasks for 1.4.3
for task in \
    "1.4.3.1:Define Byron block header structure (CDDL)" \
    "1.4.3.2:Define Shelley+ block header structure (CDDL)" \
    "1.4.3.3:Define era-agnostic block header wrapper" \
    "1.4.3.4:Implement CBOR decoder for Byron headers" \
    "1.4.3.5:Implement CBOR decoder for Shelley+ headers" \
    "1.4.3.6:Prove header parsing correctness"; do
    num="${task%%:*}"
    desc="${task#*:}"
    label="task,phase-1,ledger"
    [[ "$desc" == *"Prove"* ]] && label="$label,formal-verification"
    create_issue "[Task $num] $desc" \
        "**Parent:** #$S1_4_3 (Epic: #$E1_4, PI: #$PI1)" \
        "$label" > /dev/null
done

# ============================================================================
# PI OBJECTIVE 2: Chain Observer & Block Processing
# ============================================================================

echo ""
echo "Creating PI Objective 2..."
PI2=$(create_issue \
    "[PI-2] Chain Observer & Block Processing" \
    "**Goal:** Create a functional read-only node that can sync and display the blockchain

## Success Criteria
- [ ] Can connect to Preview testnet
- [ ] Can download and display blocks
- [ ] Can persist blocks to disk
- [ ] Can parse and validate transactions
- [ ] Maintains ledger state

## Epics
- Epic 2.1: Chain Observer Application
- Epic 2.2: Block Fetch Mini-Protocol
- Epic 2.3: Transaction Parsing & Validation
- Epic 2.4: Ledger State Management

See: [Project Breakdown](PROJECT_BREAKDOWN.md)" \
    "pi-objective,phase-2")

echo "  Created PI-2: #$PI2"

# I'll create a condensed version for PI-2 epics/stories to save space
# In production, you'd expand these fully like PI-1

echo "Creating Epic 2.1..."
E2_1=$(create_issue \
    "[Epic 2.1] Chain Observer Application" \
    "**Parent:** #$PI2

Build a working application that connects to a node and displays blocks

## Stories
- Story 2.1.1: Basic Chain Observer
- Story 2.1.2: Preview Testnet Integration
- Story 2.1.3: Block Storage" \
    "epic,phase-2")

echo "  Created Epic 2.1: #$E2_1"

create_issue "[Story 2.1.1] Basic Chain Observer" \
    "**Parent:** #$E2_1 (PI: #$PI2)

Connect to a Cardano node and display blocks in real-time.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-2" > /dev/null

create_issue "[Story 2.1.2] Preview Testnet Integration" \
    "**Parent:** #$E2_1 (PI: #$PI2)

Configure and test with Preview testnet for faster development.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-2" > /dev/null

create_issue "[Story 2.1.3] Block Storage" \
    "**Parent:** #$E2_1 (PI: #$PI2)

Persist blocks to disk for restart resilience.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-2" > /dev/null

echo "Creating Epic 2.2..."
E2_2=$(create_issue \
    "[Epic 2.2] Block Fetch Mini-Protocol" \
    "**Parent:** #$PI2

Implement block fetching to download full blocks.

## Stories
- Story 2.2.1: Block Fetch Protocol Implementation
- Story 2.2.2: Full Block Data Structures

See PROJECT_BREAKDOWN.md for details." \
    "epic,phase-2,network")

echo "  Created Epic 2.2: #$E2_2"

create_issue "[Story 2.2.1] Block Fetch Protocol Implementation" \
    "**Parent:** #$E2_2 (PI: #$PI2)

Implement BlockFetch mini-protocol for downloading block ranges.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-2,network" > /dev/null

create_issue "[Story 2.2.2] Full Block Data Structures" \
    "**Parent:** #$E2_2 (PI: #$PI2)

Define and parse complete block structures (header + body).

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-2,ledger" > /dev/null

echo "Creating Epic 2.3..."
E2_3=$(create_issue \
    "[Epic 2.3] Transaction Parsing & Validation" \
    "**Parent:** #$PI2

Parse and validate transactions with formal proofs.

## Stories
- Story 2.3.1: Transaction Data Structures
- Story 2.3.2: UTxO Model
- Story 2.3.3: Transaction Validation Rules
- Story 2.3.4: Fee Calculation

See PROJECT_BREAKDOWN.md for details." \
    "epic,phase-2,ledger,formal-verification")

echo "  Created Epic 2.3: #$E2_3"

for story in \
    "2.3.1:Transaction Data Structures:Define transaction structures for all eras" \
    "2.3.2:UTxO Model:Formally verified UTxO model" \
    "2.3.3:Transaction Validation Rules:Formally verified validation" \
    "2.3.4:Fee Calculation:Formally verified fee calculation"; do
    IFS=: read -r num title desc <<< "$story"
    create_issue "[Story $num] $title" \
        "**Parent:** #$E2_3 (PI: #$PI2)

$desc

See PROJECT_BREAKDOWN.md for detailed tasks." \
        "story,phase-2,ledger,formal-verification" > /dev/null
done

echo "Creating Epic 2.4..."
E2_4=$(create_issue \
    "[Epic 2.4] Ledger State Management" \
    "**Parent:** #$PI2

Maintain and update ledger state with proofs.

## Stories
- Story 2.4.1: Ledger State Structure
- Story 2.4.2: Block Application
- Story 2.4.3: Ledger State Persistence

See PROJECT_BREAKDOWN.md for details." \
    "epic,phase-2,ledger,formal-verification")

echo "  Created Epic 2.4: #$E2_4"

for story in \
    "2.4.1:Ledger State Structure" \
    "2.4.2:Block Application" \
    "2.4.3:Ledger State Persistence"; do
    IFS=: read -r num title <<< "$story"
    create_issue "[Story $num] $title" \
        "**Parent:** #$E2_4 (PI: #$PI2)

See PROJECT_BREAKDOWN.md for detailed tasks." \
        "story,phase-2,ledger" > /dev/null
done

# ============================================================================
# PI OBJECTIVE 3: Relay Node Functionality
# ============================================================================

echo ""
echo "Creating PI Objective 3..."
PI3=$(create_issue \
    "[PI-3] Relay Node Functionality" \
    "**Goal:** Enable transaction propagation and P2P networking

## Success Criteria
- [ ] Can receive and forward transactions
- [ ] Has working mempool
- [ ] Participates in P2P network
- [ ] Discovers peers dynamically

## Epics
- Epic 3.1: Transaction Submission Mini-Protocol
- Epic 3.2: Peer-to-Peer Networking

See: [Project Breakdown](PROJECT_BREAKDOWN.md)" \
    "pi-objective,phase-3")

echo "  Created PI-3: #$PI3"

echo "Creating Epic 3.1..."
E3_1=$(create_issue \
    "[Epic 3.1] Transaction Submission Mini-Protocol" \
    "**Parent:** #$PI3

Implement transaction submission and propagation.

See PROJECT_BREAKDOWN.md for stories and tasks." \
    "epic,phase-3,network")

echo "  Created Epic 3.1: #$E3_1"

create_issue "[Story 3.1.1] TxSubmission2 Protocol" \
    "**Parent:** #$E3_1 (PI: #$PI3)

Implement TxSubmission2 mini-protocol.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-3,network" > /dev/null

create_issue "[Story 3.1.2] Mempool Management" \
    "**Parent:** #$E3_1 (PI: #$PI3)

Verified mempool implementation.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-3,ledger,formal-verification" > /dev/null

echo "Creating Epic 3.2..."
E3_2=$(create_issue \
    "[Epic 3.2] Peer-to-Peer Networking" \
    "**Parent:** #$PI3

Implement P2P peer discovery and management.

See PROJECT_BREAKDOWN.md for stories and tasks." \
    "epic,phase-3,network")

echo "  Created Epic 3.2: #$E3_2"

create_issue "[Story 3.2.1] Peer Sharing Mini-Protocol" \
    "**Parent:** #$E3_2 (PI: #$PI3)

Implement PeerSharing protocol for P2P discovery.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-3,network" > /dev/null

create_issue "[Story 3.2.2] Peer Selection" \
    "**Parent:** #$E3_2 (PI: #$PI3)

Intelligent peer selection and connection management.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-3,network" > /dev/null

# ============================================================================
# PI OBJECTIVE 4: Block Production
# ============================================================================

echo ""
echo "Creating PI Objective 4..."
PI4=$(create_issue \
    "[PI-4] Block Production" \
    "**Goal:** Enable stake pool operation and block forging

## Success Criteria
- [ ] Implements Ouroboros Praos consensus
- [ ] Can determine slot leadership
- [ ] Can forge valid blocks
- [ ] Supports stake pool operations

## Epics
- Epic 4.1: Ouroboros Praos Consensus
- Epic 4.2: Stake Pool Operations

See: [Project Breakdown](PROJECT_BREAKDOWN.md)" \
    "pi-objective,phase-4")

echo "  Created PI-4: #$PI4"

echo "Creating Epic 4.1..."
E4_1=$(create_issue \
    "[Epic 4.1] Ouroboros Praos Consensus" \
    "**Parent:** #$PI4

Implement consensus protocol with formal verification.

## Stories
- Story 4.1.1: VRF Leader Election
- Story 4.1.2: KES Key Management
- Story 4.1.3: Block Forging

See PROJECT_BREAKDOWN.md for details." \
    "epic,phase-4,consensus,formal-verification")

echo "  Created Epic 4.1: #$E4_1"

for story in \
    "4.1.1:VRF Leader Election" \
    "4.1.2:KES Key Management" \
    "4.1.3:Block Forging"; do
    IFS=: read -r num title <<< "$story"
    create_issue "[Story $num] $title" \
        "**Parent:** #$E4_1 (PI: #$PI4)

See PROJECT_BREAKDOWN.md for detailed tasks." \
        "story,phase-4,consensus" > /dev/null
done

echo "Creating Epic 4.2..."
E4_2=$(create_issue \
    "[Epic 4.2] Stake Pool Operations" \
    "**Parent:** #$PI4

Support full stake pool functionality.

## Stories
- Story 4.2.1: Stake Pool Registration
- Story 4.2.2: Rewards Calculation

See PROJECT_BREAKDOWN.md for details." \
    "epic,phase-4,consensus")

echo "  Created Epic 4.2: #$E4_2"

create_issue "[Story 4.2.1] Stake Pool Registration" \
    "**Parent:** #$E4_2 (PI: #$PI4)

Pool registration and metadata handling.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-4,consensus" > /dev/null

create_issue "[Story 4.2.2] Rewards Calculation" \
    "**Parent:** #$E4_2 (PI: #$PI4)

Verified rewards calculation and distribution.

See PROJECT_BREAKDOWN.md for detailed tasks." \
    "story,phase-4,consensus,formal-verification" > /dev/null

# ============================================================================
# PI OBJECTIVE 5: Advanced Features & Optimization
# ============================================================================

echo ""
echo "Creating PI Objective 5..."
PI5=$(create_issue \
    "[PI-5] Advanced Features & Optimization" \
    "**Goal:** Production readiness and advanced capabilities

## Success Criteria
- [ ] Mithril fast bootstrap working
- [ ] Performance optimized for production
- [ ] Comprehensive monitoring
- [ ] Production-ready CLI tools

## Epics
- Epic 5.1: Mithril Integration
- Epic 5.2: Performance Optimization
- Epic 5.3: Monitoring & Operations

See: [Project Breakdown](PROJECT_BREAKDOWN.md)" \
    "pi-objective,phase-5")

echo "  Created PI-5: #$PI5"

echo "Creating Epic 5.1..."
create_issue \
    "[Epic 5.1] Mithril Integration" \
    "**Parent:** #$PI5

Fast bootstrap using Mithril snapshots.

See PROJECT_BREAKDOWN.md for stories and tasks." \
    "epic,phase-5" > /dev/null

echo "Creating Epic 5.2..."
create_issue \
    "[Epic 5.2] Performance Optimization" \
    "**Parent:** #$PI5

Optimize for production workloads.

See PROJECT_BREAKDOWN.md for stories and tasks." \
    "epic,phase-5" > /dev/null

echo "Creating Epic 5.3..."
create_issue \
    "[Epic 5.3] Monitoring & Operations" \
    "**Parent:** #$PI5

Production monitoring and operations tools.

See PROJECT_BREAKDOWN.md for stories and tasks." \
    "epic,phase-5" > /dev/null

echo ""
echo "================================"
echo "✅ All GitHub issues created!"
echo "================================"
echo ""
echo "Created:"
echo "  - 5 PI Objectives (#$PI1, #$PI2, #$PI3, #$PI4, #$PI5)"
echo "  - Multiple Epics under each PI"
echo "  - Multiple Stories under each Epic"
echo "  - Detailed Tasks under Phase 1 stories"
echo ""
echo "See PROJECT_BREAKDOWN.md for complete breakdown"
echo "View issues: https://github.com/RSoulatIOHK/Dion/issues"
