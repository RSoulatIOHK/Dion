#!/bin/bash
# Continue creating GitHub issues from Task 1.4.2.4 onwards
# Uses GitHub's sub-issue feature for proper hierarchy

set -e

# Small delay to avoid rate limiting
rate_limit_delay() {
    sleep 0.5
}

# Function to create an issue and return its number
create_issue() {
    local title="$1"
    local body="$2"
    local labels="$3"

    rate_limit_delay
    issue_url=$(gh issue create --title "$title" --body "$body" --label "$labels")
    issue_number=$(echo "$issue_url" | sed 's|.*/||')
    echo "$issue_number"
}

# Function to add sub-issues to a parent issue
add_subissue() {
    local parent_issue="$1"
    local child_issue="$2"

    rate_limit_delay
    gh issue edit "$parent_issue" --add-sub-issue "$child_issue"
}

echo "Continuing GitHub issue creation from Task 1.4.2.4..."
echo ""

# We need the issue numbers from what was already created
echo "Looking up existing issue numbers..."
S1_4_2=$(gh issue list --search "[Story 1.4.2]" --json number -q ".[0].number")
echo "  Story 1.4.2: #$S1_4_2"

# ============================================================================
# Remaining tasks for Story 1.4.2
# ============================================================================

echo "Creating remaining tasks for Story 1.4.2..."

T1_4_2_4=$(create_issue \
    "[Task 1.4.2.4] Handle RollForward messages" \
    "**Parent:** Story #$S1_4_2

Implement handling of RollForward messages from chain sync protocol.

## Description
When the server sends RollForward, it provides the next block header in the chain. The client must:
- Accept the new block header
- Update its current chain tip
- Persist the header
- Transition to appropriate state

## Acceptance Criteria
- [ ] RollForward messages are parsed correctly
- [ ] Chain tip is updated
- [ ] State transitions correctly

## References
- Ouroboros Network Spec: Chain Sync protocol" \
    "task,phase-1,network")
echo "  Created Task 1.4.2.4: #$T1_4_2_4"
add_subissue "$S1_4_2" "$T1_4_2_4"

T1_4_2_5=$(create_issue \
    "[Task 1.4.2.5] Handle RollBackward messages" \
    "**Parent:** Story #$S1_4_2

Implement handling of RollBackward messages for chain reorganizations.

## Description
When a fork is detected, the server sends RollBackward to a previous point. The client must:
- Roll back its chain to the specified point
- Discard rolled-back headers
- Update chain tip
- Resume from the rollback point

## Acceptance Criteria
- [ ] RollBackward messages handled correctly
- [ ] Chain rolls back to correct point
- [ ] Discarded headers are removed
- [ ] Can resume sync after rollback

## References
- Ouroboros Network Spec: Chain Sync protocol
- Cardano docs on chain reorganization" \
    "task,phase-1,network")
echo "  Created Task 1.4.2.5: #$T1_4_2_5"
add_subissue "$S1_4_2" "$T1_4_2_5"

T1_4_2_6=$(create_issue \
    "[Task 1.4.2.6] Prove state machine safety properties" \
    "**Parent:** Story #$S1_4_2

Formally verify key safety properties of the chain sync client state machine.

## Description
Prove the following properties in Lean:
- **Termination**: State machine terminates or makes progress
- **Safety**: Never accepts invalid transitions
- **Liveness**: Eventually synchronizes with chain (under reasonable assumptions)
- **Invariant preservation**: Chain state remains consistent

## Acceptance Criteria
- [ ] Formal specification of state machine in Lean
- [ ] Key safety properties stated as theorems
- [ ] Proofs completed (or admitted with TODO)
- [ ] Properties documented

## References
- Formal methods for protocol verification
- Ouroboros Network Spec" \
    "task,phase-1,network,formal-verification")
echo "  Created Task 1.4.2.6: #$T1_4_2_6"
add_subissue "$S1_4_2" "$T1_4_2_6"

# ============================================================================
# Story 1.4.3 and its tasks
# ============================================================================

echo ""
echo "Looking up Story 1.4.3..."
S1_4_3=$(gh issue list --search "[Story 1.4.3]" --json number -q ".[0].number")
echo "  Story 1.4.3: #$S1_4_3"

echo "Creating tasks for Story 1.4.3..."

T1_4_3_1=$(create_issue \
    "[Task 1.4.3.1] Define Byron block header structure (CDDL)" \
    "**Parent:** Story #$S1_4_3

Define the Byron-era block header structure based on CDDL specification.

## Description
Byron headers have a different format than later eras. Define Lean types for:
- Protocol magic
- Previous block hash
- Proof (VRF and KES signatures)
- Body proof (Merkle root)
- Consensus data
- Extra data

## Acceptance Criteria
- [ ] Byron header structure defined in Lean
- [ ] Matches official CDDL spec
- [ ] All fields properly typed
- [ ] Documentation of field meanings

## References
- Byron CDDL: https://github.com/IntersectMBO/cardano-ledger/tree/master/eras/byron
- cardano-node Byron genesis" \
    "task,phase-1,ledger")
echo "  Created Task 1.4.3.1: #$T1_4_3_1"
add_subissue "$S1_4_3" "$T1_4_3_1"

T1_4_3_2=$(create_issue \
    "[Task 1.4.3.2] Define Shelley+ block header structure (CDDL)" \
    "**Parent:** Story #$S1_4_3

Define the Shelley-onwards block header structure.

## Description
From Shelley onwards (including Allegra, Mary, Alonzo, Babbage, Conway), headers follow the Shelley format:
- Block body hash
- Block number
- Slot number
- Previous block hash (prev_hash)
- Issuer VKey
- VRF VKey
- Block signature (KES)
- VRF proof
- Protocol version
- Operational certificate

## Acceptance Criteria
- [ ] Shelley+ header structure defined
- [ ] Matches CDDL specifications
- [ ] Compatible with all post-Byron eras
- [ ] Well-documented

## References
- Shelley CDDL specs
- Conway CDDL (latest)" \
    "task,phase-1,ledger")
echo "  Created Task 1.4.3.2: #$T1_4_3_2"
add_subissue "$S1_4_3" "$T1_4_3_2"

T1_4_3_3=$(create_issue \
    "[Task 1.4.3.3] Define era-agnostic block header wrapper" \
    "**Parent:** Story #$S1_4_3

Create a wrapper type that can represent any era's block header.

## Description
Define an algebraic data type:
\`\`\`lean
inductive BlockHeader where
  | Byron : ByronHeader → BlockHeader
  | Shelley : ShelleyHeader → BlockHeader
  -- Other eras use ShelleyHeader format
\`\`\`

Provide utility functions:
- getBlockNumber
- getSlotNumber
- getHash
- getPrevHash
- getEra

## Acceptance Criteria
- [ ] Wrapper type defined
- [ ] Accessor functions for common fields
- [ ] Pattern matching support
- [ ] Era detection logic

## References
- cardano-node multi-era handling" \
    "task,phase-1,ledger")
echo "  Created Task 1.4.3.3: #$T1_4_3_3"
add_subissue "$S1_4_3" "$T1_4_3_3"

T1_4_3_4=$(create_issue \
    "[Task 1.4.3.4] Implement CBOR decoder for Byron headers" \
    "**Parent:** Story #$S1_4_3

Implement CBOR decoder for Byron block headers using the CBOR library from Epic 1.2.

## Description
Byron headers are CBOR-encoded. Implement:
\`\`\`lean
def decodeByronHeader : Bytes → Except Error ByronHeader
\`\`\`

Handle all Byron header variants and validate structure.

## Acceptance Criteria
- [ ] Decoder implemented
- [ ] Parses real Byron headers from mainnet
- [ ] Proper error handling
- [ ] Test with genesis block and subsequent blocks

## References
- Byron CDDL
- Test vectors from cardano-node
- Depends on: Epic 1.2 (CBOR)" \
    "task,phase-1,ledger")
echo "  Created Task 1.4.3.4: #$T1_4_3_4"
add_subissue "$S1_4_3" "$T1_4_3_4"

T1_4_3_5=$(create_issue \
    "[Task 1.4.3.5] Implement CBOR decoder for Shelley+ headers" \
    "**Parent:** Story #$S1_4_3

Implement CBOR decoder for Shelley-onwards block headers.

## Description
Implement:
\`\`\`lean
def decodeShelleyHeader : Bytes → Except Error ShelleyHeader
\`\`\`

Must handle all Shelley-based eras (Shelley, Allegra, Mary, Alonzo, Babbage, Conway).

## Acceptance Criteria
- [ ] Decoder implemented
- [ ] Parses headers from all Shelley+ eras
- [ ] Proper validation
- [ ] Test with Preview/Preprod testnet blocks

## References
- Shelley/Conway CDDL
- Test vectors
- Depends on: Epic 1.2 (CBOR)" \
    "task,phase-1,ledger")
echo "  Created Task 1.4.3.5: #$T1_4_3_5"
add_subissue "$S1_4_3" "$T1_4_3_5"

T1_4_3_6=$(create_issue \
    "[Task 1.4.3.6] Prove header parsing correctness" \
    "**Parent:** Story #$S1_4_3

Formally verify that header parsing satisfies correctness properties.

## Description
Prove key properties:
- **Well-formedness**: Parsed headers satisfy structural invariants
- **Determinism**: Same bytes always parse to same header
- **Error safety**: Decoder never panics, returns proper errors
- **Round-trip** (if encoder exists): encode ∘ decode ≈ id

## Acceptance Criteria
- [ ] Properties formally stated
- [ ] Proofs completed (or admitted with plan)
- [ ] Integration with CBOR round-trip proof
- [ ] Documentation of assumptions

## References
- CBOR correctness proofs (Epic 1.2)
- Lean formal verification techniques" \
    "task,phase-1,ledger,formal-verification")
echo "  Created Task 1.4.3.6: #$T1_4_3_6"
add_subissue "$S1_4_3" "$T1_4_3_6"

echo ""
echo "================================"
echo "✅ Remaining Phase 1 issues created!"
echo "================================"
echo ""
echo "Next steps:"
echo "1. Review issues at: https://github.com/RSoulatIOHK/Cleanode/issues"
echo "2. The sub-issue hierarchy is now properly linked"
echo "3. Create similar detailed scripts for Phase 2-5 using PROJECT_BREAKDOWN.md"
