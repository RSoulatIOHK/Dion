#!/bin/bash
# Script to create GitHub issues for remaining Transaction Validation work
# These extend Epic 2.3 (Transaction Parsing & Validation) and related areas.

set -e

echo "Creating GitHub issues for transaction validation gaps..."
echo ""

create_issue() {
    local title="$1"
    local body="$2"
    local labels="$3"

    issue_url=$(gh issue create --title "$title" --body "$body" --label "$labels")
    issue_number=$(echo "$issue_url" | sed 's|.*/||')
    echo "$issue_number"
}

# ============================================================================
# STORY 2.3.5: Witness Set Extension & Signature Validation
# ============================================================================

echo "=== Story 2.3.5: Witness Set Extension & Signature Validation ==="

S2_3_5=$(create_issue \
    "[Story 2.3.5] Witness Set Extension & Signature Validation" \
    "**Parent:** Epic 2.3 — Transaction Parsing & Validation

**As a** ledger implementer
**I want** complete witness set parsing and Ed25519 signature verification
**So that** only properly signed transactions are accepted

## Context
The current \`WitnessSet\` structure only contains redeemers. We need to extend it
with vkey witnesses, native scripts, Plutus scripts, datums, and bootstrap witnesses.
The \`validateSignatures\` function is currently a stub (always returns \`.ok ()\`).

## Tasks
- [ ] Task 2.3.5.1: Extend WitnessSet with vkey witnesses, scripts, datums
- [ ] Task 2.3.5.2: Implement CBOR decoder for extended WitnessSet
- [ ] Task 2.3.5.3: Implement transaction hash computation (Blake2b-256 of body CBOR)
- [ ] Task 2.3.5.4: Implement vkey witness verification (Ed25519 over tx hash)
- [ ] Task 2.3.5.5: Extract required signers from input addresses
- [ ] Task 2.3.5.6: Wire signature validation into validateTransaction (replace stub)
- [ ] Task 2.3.5.7: Implement bootstrap witness validation (Byron-era addresses)

## Acceptance Criteria
- [ ] WitnessSet parses vkey witnesses, native scripts, Plutus scripts, datums
- [ ] Ed25519 signatures are verified against the transaction body hash
- [ ] Transactions with missing or invalid signatures are rejected
- [ ] Bootstrap (Byron) witnesses are handled correctly

## References
- Cardano Ledger Spec: UTXOW rule
- \`Cleanode/Ledger/Validation.lean\` lines 83–90 (stub)
- \`Cleanode/Network/ConwayBlock.lean\` lines 106–109 (incomplete WitnessSet)" \
    "story,phase-2,ledger,crypto")

echo "  Created Story 2.3.5: #$S2_3_5"

# Tasks for 2.3.5
echo "  Creating tasks for Story 2.3.5..."

create_issue "[Task 2.3.5.1] Extend WitnessSet with vkey witnesses, scripts, datums" \
    "**Parent:** #$S2_3_5

Extend the \`WitnessSet\` structure in \`ConwayBlock.lean\` to include:
- \`vkeyWitnesses : List VKeyWitness\` (public key + signature pairs)
- \`nativeScripts : List NativeScript\`
- \`plutusScriptsV1 : List ByteArray\`
- \`plutusScriptsV2 : List ByteArray\`
- \`plutusScriptsV3 : List ByteArray\`
- \`datums : List (ByteArray × ByteArray)\` (hash → datum)
- \`bootstrapWitnesses : List BootstrapWitness\`

Define the \`VKeyWitness\` and \`BootstrapWitness\` structures per CDDL spec.

**File:** \`Cleanode/Network/ConwayBlock.lean\` lines 106–109" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.5.2] Implement CBOR decoder for extended WitnessSet" \
    "**Parent:** #$S2_3_5

Parse the full witness set from CBOR, including all new fields from Task 2.3.5.1.
The witness set is a CBOR map with well-known integer keys:
- 0: vkey witnesses
- 1: native scripts
- 2: bootstrap witnesses
- 3: Plutus V1 scripts
- 4: datums
- 5: redeemers
- 6: Plutus V2 scripts
- 7: Plutus V3 scripts

Update \`parseWitnessSet\` in \`ConwayBlock.lean\` accordingly.

**File:** \`Cleanode/Network/ConwayBlock.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.5.3] Implement transaction hash computation" \
    "**Parent:** #$S2_3_5

Compute the transaction hash as Blake2b-256 of the CBOR-encoded transaction body.
This hash is what witnesses sign and what identifies the transaction.

- Extract the raw CBOR bytes of the transaction body (already in \`body.rawBytes\`)
- Hash with \`blake2b_256\`
- Use this hash for signature verification and as the canonical TxId

**File:** \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger,crypto" > /dev/null

create_issue "[Task 2.3.5.4] Implement vkey witness verification (Ed25519)" \
    "**Parent:** #$S2_3_5

For each \`VKeyWitness\` in the witness set:
1. Compute \`keyHash = Blake2b-224(vkey)\`
2. Verify \`Ed25519.verify(vkey, txBodyHash, signature)\`
3. Collect verified key hashes

This requires IO (FFI calls to Ed25519), so \`validateSignatures\` needs to become
an IO function or use a pure verification wrapper.

**File:** \`Cleanode/Ledger/Validation.lean\` lines 83–90" \
    "task,phase-2,ledger,crypto" > /dev/null

create_issue "[Task 2.3.5.5] Extract required signers from input addresses" \
    "**Parent:** #$S2_3_5

Implement \`requiredSigners\` properly:
- For Shelley-era addresses: extract the payment key hash (bytes 1–29 of address)
- For script addresses: required signers come from the script's requirements
- Include any \`requiredSigners\` field from the transaction body (Alonzo+)
- Verify that the set of verified key hashes covers all required signers

**File:** \`Cleanode/Ledger/Validation.lean\` lines 66–76" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.5.6] Wire signature validation into validateTransaction" \
    "**Parent:** #$S2_3_5

Replace the stub \`validateSignatures\` (currently always \`.ok ()\`) with the
real implementation that:
1. Computes the tx body hash
2. Verifies all vkey witnesses
3. Checks required signers are covered
4. Returns appropriate \`ValidationError.MissingSignature\` on failure

Note: This changes \`validateTransaction\` from pure to IO. Update the Mempool
integration accordingly.

**Files:** \`Cleanode/Ledger/Validation.lean\`, \`Cleanode/Network/Mempool.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.5.7] Implement bootstrap witness validation (Byron)" \
    "**Parent:** #$S2_3_5

Handle Byron-era bootstrap witnesses which use a different format:
- Public key + signature + chain code + attributes
- Address derivation differs from Shelley
- Required for spending from Byron-era addresses still in the UTxO set

**File:** \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger,crypto" > /dev/null

# ============================================================================
# STORY 2.3.6: Script Validation Pipeline
# ============================================================================

echo "=== Story 2.3.6: Script Validation Pipeline ==="

S2_3_6=$(create_issue \
    "[Story 2.3.6] Script Validation Pipeline" \
    "**Parent:** Epic 2.3 — Transaction Parsing & Validation

**As a** ledger implementer
**I want** native script evaluation and Plutus script validation
**So that** script-locked UTxOs can only be spent when scripts succeed

## Context
- \`validateNativeScripts\` is stubbed (always \`.ok ()\`). The evaluation logic
  (\`evaluateNativeScript\`) exists but is not wired into the validation pipeline.
- \`validatePlutusScripts\` is a placeholder — requires a UPLC interpreter.
- No script data hash validation exists.
- No collateral validation for script transactions.

## Tasks
- [ ] Task 2.3.6.1: Wire native script evaluation into validation pipeline
- [ ] Task 2.3.6.2: Validate script data hash matches witness content
- [ ] Task 2.3.6.3: Implement collateral input validation
- [ ] Task 2.3.6.4: Implement execution budget enforcement
- [ ] Task 2.3.6.5: Design UPLC interpreter integration strategy
- [ ] Task 2.3.6.6: Implement Plutus script context construction
- [ ] Task 2.3.6.7: Implement Plutus V1/V2/V3 script execution

## Acceptance Criteria
- [ ] Native scripts are extracted from witnesses and evaluated
- [ ] Script data hash is validated against redeemers and datums
- [ ] Collateral inputs are validated for script transactions
- [ ] Execution budgets are enforced against protocol parameters
- [ ] Plutus scripts can be executed (at least V2/V3)

## References
- Cardano Ledger Spec: UTXOS rule
- CIP-0032: Alonzo validation
- CIP-0055: Plutus V2
- \`Cleanode/Ledger/Validation.lean\` lines 118–137 (stubs)" \
    "story,phase-2,ledger")

echo "  Created Story 2.3.6: #$S2_3_6"

echo "  Creating tasks for Story 2.3.6..."

create_issue "[Task 2.3.6.1] Wire native script evaluation into validation pipeline" \
    "**Parent:** #$S2_3_6

The \`evaluateNativeScript\` function exists and is correctly implemented but
\`validateNativeScripts\` is a stub. Wire it in:
1. Extract native scripts from the extended WitnessSet
2. For each script-locked input, find the corresponding native script
3. Evaluate with the set of verified signers and current slot
4. Return \`NativeScriptFailure\` on failure

**Files:** \`Cleanode/Ledger/Validation.lean\` lines 106–121" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.6.2] Validate script data hash" \
    "**Parent:** #$S2_3_6

Implement validation that \`scriptDataHash\` in the transaction body matches
the hash of the redeemers and datums in the witness set.

Per the Alonzo spec:
\`scriptDataHash = Blake2b-256(redeemers_cbor || datums_cbor || costModels_cbor)\`

This requires:
- Adding \`scriptDataHash : Option ByteArray\` to \`TransactionBody\`
- Computing the hash from witness set contents
- Comparing and rejecting on mismatch

**Files:** \`Cleanode/Network/ConwayBlock.lean\`, \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.6.3] Implement collateral input validation" \
    "**Parent:** #$S2_3_6

For transactions containing Plutus scripts, validate collateral:
1. Add \`collateralInputs : List TxInput\` to \`TransactionBody\`
2. Add \`collateralReturn : Option TxOutput\` (Babbage+)
3. Add \`totalCollateral : Option Nat\` (Babbage+)
4. Validate collateral inputs exist and have sufficient value
5. Collateral inputs must be pure ADA (no multi-assets)
6. Total collateral >= percentage of tx fee (protocol param)

**Files:** \`Cleanode/Network/ConwayBlock.lean\`, \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.6.4] Implement execution budget enforcement" \
    "**Parent:** #$S2_3_6

Enforce execution unit limits from protocol parameters:
1. Add \`maxTxExUnits : (Nat × Nat)\` (mem, steps) to protocol params
2. Sum execution units from all redeemers in the transaction
3. Reject if total exceeds \`maxTxExUnits\`
4. Add per-block execution unit tracking for block validation

**Files:** \`Cleanode/Ledger/Validation.lean\`, \`Cleanode/Ledger/State.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.6.5] Design UPLC interpreter integration strategy" \
    "**Parent:** #$S2_3_6

Research and design how to integrate Plutus script execution:

**Options:**
1. **Pure Lean UPLC interpreter** — Full formal verification possible but very large effort
2. **FFI to Haskell plutus-core** — Reuse existing implementation via C FFI
3. **FFI to Rust uplc** — Use aiken-lang/uplc crate via C FFI
4. **Hybrid** — Lean interpreter for simple scripts, FFI for complex

Produce a design document with:
- Chosen approach and rationale
- Interface definition (\`evalScript : Script → Datum → Redeemer → ScriptContext → ExBudget → Result\`)
- Performance considerations
- Verification strategy

**Deliverable:** Design doc in \`docs/plutus-integration.md\`" \
    "task,phase-2,ledger,documentation" > /dev/null

create_issue "[Task 2.3.6.6] Implement Plutus script context construction" \
    "**Parent:** #$S2_3_6

Build the \`ScriptContext\` (Plutus \`Data\`) that is passed to Plutus scripts:
- \`TxInfo\`: inputs, reference inputs, outputs, fee, mint, certs, withdrawals, validity range, signatories, redeemers, datums, txId
- \`ScriptPurpose\`: Spending, Minting, Certifying, Rewarding (+ Voting/Proposing for V3)

This is a complex data structure that must exactly match the Plutus specification
for each script language version (V1, V2, V3).

**File:** New file \`Cleanode/Ledger/ScriptContext.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.6.7] Implement Plutus V1/V2/V3 script execution" \
    "**Parent:** #$S2_3_6

Implement actual Plutus script execution using the strategy from Task 2.3.6.5:
1. Deserialize the Plutus script (flat-encoded UPLC)
2. Construct script context per version (V1/V2/V3 have different TxInfo)
3. Apply script to (datum, redeemer, context) arguments
4. Enforce execution budget limits
5. Map evaluation results to \`ValidationError.ScriptFailure\`

Depends on Tasks 2.3.6.5 and 2.3.6.6.

**File:** \`Cleanode/Ledger/Validation.lean\`, potentially new \`Cleanode/Ledger/PlutusEval.lean\`" \
    "task,phase-2,ledger" > /dev/null

# ============================================================================
# STORY 2.3.7: Extended Transaction Body & Validation Rules
# ============================================================================

echo "=== Story 2.3.7: Extended Transaction Body & Validation Rules ==="

S2_3_7=$(create_issue \
    "[Story 2.3.7] Extended Transaction Body & Validation Rules" \
    "**Parent:** Epic 2.3 — Transaction Parsing & Validation

**As a** ledger implementer
**I want** the full transaction body fields and remaining validation rules
**So that** all Cardano ledger rules are enforced

## Context
The current \`TransactionBody\` is simplified (inputs, outputs, fee, rawBytes).
Missing fields: ttl, certs, withdrawals, mint, validity interval, required signers,
network ID, reference inputs, inline datums. Several validation rules that use
these fields are not yet implemented.

## Tasks
- [ ] Task 2.3.7.1: Extend TransactionBody with all Conway-era fields
- [ ] Task 2.3.7.2: Implement min-ADA per output validation
- [ ] Task 2.3.7.3: Implement multi-asset balance validation
- [ ] Task 2.3.7.4: Implement validity interval (TTL) validation
- [ ] Task 2.3.7.5: Implement required signers validation (Alonzo+)
- [ ] Task 2.3.7.6: Implement reference input validation (Babbage+)
- [ ] Task 2.3.7.7: Implement mint/burn policy validation
- [ ] Task 2.3.7.8: Implement certificate validation in state application

## Acceptance Criteria
- [ ] TransactionBody contains all Conway-era fields per CDDL
- [ ] Outputs below min-ADA are rejected
- [ ] Multi-asset values are balanced correctly
- [ ] Expired transactions are rejected
- [ ] Minting/burning requires valid policy scripts
- [ ] Certificates are validated during state application

## References
- Cardano Ledger Spec: UTXO, UTXOW rules (Babbage/Conway)
- \`Cleanode/Network/ConwayBlock.lean\` lines 111–119 (simplified body)" \
    "story,phase-2,ledger")

echo "  Created Story 2.3.7: #$S2_3_7"

echo "  Creating tasks for Story 2.3.7..."

create_issue "[Task 2.3.7.1] Extend TransactionBody with all Conway-era fields" \
    "**Parent:** #$S2_3_7

Extend \`TransactionBody\` to include all fields per Conway CDDL:
- \`ttl : Option Nat\` — transaction time-to-live (slot number)
- \`certs : List Certificate\` — stake/pool/governance certificates
- \`withdrawals : List (ByteArray × Nat)\` — reward withdrawals
- \`mint : List (ByteArray × List (ByteArray × Int))\` — minted/burned assets
- \`validityStart : Option Nat\` — earliest valid slot
- \`requiredSigners : List ByteArray\` — additional required key hashes
- \`networkId : Option Nat\` — network ID tag
- \`referenceInputs : List TxInput\` — reference inputs (read-only)
- \`scriptDataHash : Option ByteArray\` — hash of script-related data
- \`collateralInputs : List TxInput\`
- \`collateralReturn : Option TxOutput\`
- \`totalCollateral : Option Nat\`

Update CBOR parsing to populate these fields.

**File:** \`Cleanode/Network/ConwayBlock.lean\` lines 111–119" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.7.2] Implement min-ADA per output validation" \
    "**Parent:** #$S2_3_7

The \`OutputTooSmall\` error is defined but never thrown. Implement:
1. Use \`minAdaPerUTxO\` from \`Fee.lean\` (needs refinement for Babbage formula)
2. For each output, compute min required ADA based on output size
3. Reject with \`OutputTooSmall\` if output amount < min required
4. Wire into \`validateTransaction\` between balance and fee checks

The Babbage formula: \`max(minUTxOValue, utxoEntrySizeWithoutVal + 160 + numAssets * 28) * coinsPerUTxOByte\`

**Files:** \`Cleanode/Ledger/Validation.lean\`, \`Cleanode/Ledger/Fee.lean\` lines 75–78" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.7.3] Implement multi-asset balance validation" \
    "**Parent:** #$S2_3_7

Current balance validation only checks lovelace. Extend to multi-assets:
1. Define \`Value\` type: lovelace + Map PolicyId (Map AssetName Int)
2. Update \`totalInputValue\` and \`totalOutputValue\` to return \`Value\`
3. Balance check: for each policy+asset, input amount = output amount + mint/burn
4. Lovelace: input amount >= output amount + fee

**Files:** \`Cleanode/Ledger/UTxO.lean\`, \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.7.4] Implement validity interval (TTL) validation" \
    "**Parent:** #$S2_3_7

Validate the transaction's validity interval:
1. If \`ttl\` is set: reject if current slot > ttl
2. If \`validityStart\` is set: reject if current slot < validityStart
3. Add current slot as a parameter to \`validateTransaction\`

Add a new \`ValidationError\` variant: \`Expired (currentSlot ttl : Nat)\`

**File:** \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.7.5] Implement required signers validation (Alonzo+)" \
    "**Parent:** #$S2_3_7

Alonzo introduced an explicit \`requiredSigners\` field in the transaction body.
These key hashes must be present in the witness set's vkey witnesses,
in addition to the signers required by inputs.

1. Parse \`requiredSigners\` from transaction body
2. After signature verification, check all required signers are satisfied
3. Return \`MissingSignature\` for any unsatisfied required signer

**File:** \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.7.6] Implement reference input validation (Babbage+)" \
    "**Parent:** #$S2_3_7

Reference inputs (BIP-0031) are read-only inputs that must exist in the UTxO set
but are not consumed:
1. Parse \`referenceInputs\` from transaction body
2. Validate all reference inputs exist in the UTxO set
3. Reference inputs must NOT overlap with regular inputs
4. Make reference input UTxOs available for script context

**File:** \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.7.7] Implement mint/burn policy validation" \
    "**Parent:** #$S2_3_7

Validate minting/burning of native assets:
1. For each policy ID in the \`mint\` field, locate the corresponding script
2. For native script policies: evaluate the native script
3. For Plutus script policies: execute with \`Minting\` purpose
4. ADA (empty policy ID) can never be minted/burned
5. Minted values must be reflected in the multi-asset balance

**File:** \`Cleanode/Ledger/Validation.lean\`" \
    "task,phase-2,ledger" > /dev/null

create_issue "[Task 2.3.7.8] Implement certificate validation in state application" \
    "**Parent:** #$S2_3_7

\`applyTransaction\` in \`State.lean\` currently doesn't validate certificates:
1. Stake key registration: check deposit is paid, key not already registered
2. Stake key deregistration: check key is registered, refund deposit
3. Delegation: check delegated pool exists and is not retiring
4. Pool registration: check deposit, validate pool params
5. Pool retirement: check pool exists, retirement epoch is valid
6. Conway governance certs: DRep registration, voting

**File:** \`Cleanode/Ledger/State.lean\`" \
    "task,phase-2,ledger" > /dev/null

# ============================================================================
# STORY 2.3.8: Transaction Validation Formal Proofs
# ============================================================================

echo "=== Story 2.3.8: Transaction Validation Formal Proofs ==="

S2_3_8=$(create_issue \
    "[Story 2.3.8] Transaction Validation Formal Proofs" \
    "**Parent:** Epic 2.3 — Transaction Parsing & Validation

**As a** protocol implementer
**I want** formal proofs that validation preserves ledger invariants
**So that** we can guarantee correctness of the validation pipeline

## Context
All current proof scaffolds are trivial (\`True → True\`). Once the implementations
are complete, these need to be replaced with meaningful proofs.

## Tasks
- [ ] Task 2.3.8.1: Prove balance validation ensures value conservation
- [ ] Task 2.3.8.2: Prove UTxO set consistency after transaction application
- [ ] Task 2.3.8.3: Prove state transition correctness
- [ ] Task 2.3.8.4: Prove state invariants are preserved across transitions
- [ ] Task 2.3.8.5: Prove fee monotonicity and bounds

## Acceptance Criteria
- [ ] No trivial \`True → True\` proofs remain in validation code
- [ ] Balance preservation is formally proved
- [ ] UTxO consistency is formally proved
- [ ] State transition correctness has at least partial proofs

## References
- \`Cleanode/Ledger/Validation.lean\` lines 189–198 (trivial proofs)
- \`Cleanode/Ledger/UTxO.lean\` lines 206–210 (trivial proof)
- \`Cleanode/Ledger/State.lean\` lines 289–298 (trivial proofs)" \
    "story,phase-2,ledger,formal-verification")

echo "  Created Story 2.3.8: #$S2_3_8"

echo "  Creating tasks for Story 2.3.8..."

create_issue "[Task 2.3.8.1] Prove balance validation ensures value conservation" \
    "**Parent:** #$S2_3_8

Replace the trivial \`balance_validation_correct\` theorem with a real proof:
- If \`validateBalance utxo body = .ok ()\`, then
  \`totalInputValue utxo body.inputs >= totalOutputValue body.outputs + body.fee\`

**File:** \`Cleanode/Ledger/Validation.lean\` lines 195–198" \
    "task,phase-2,ledger,formal-verification" > /dev/null

create_issue "[Task 2.3.8.2] Prove UTxO set consistency after transaction application" \
    "**Parent:** #$S2_3_8

Replace \`utxo_balance_preservation\` with a real proof:
- After \`applyTx\`, all consumed inputs are removed
- After \`applyTx\`, all new outputs are present
- No other entries are affected
- Size of UTxO set changes correctly

**File:** \`Cleanode/Ledger/UTxO.lean\` lines 206–210" \
    "task,phase-2,ledger,formal-verification" > /dev/null

create_issue "[Task 2.3.8.3] Prove state transition correctness" \
    "**Parent:** #$S2_3_8

Replace \`state_transition_correct\` with a real proof:
- \`applyTransaction\` correctly updates the UTxO set
- Stake pool state is correctly updated by certificates
- Delegation state is correctly updated

**File:** \`Cleanode/Ledger/State.lean\` lines 289–292" \
    "task,phase-2,ledger,formal-verification" > /dev/null

create_issue "[Task 2.3.8.4] Prove state invariants are preserved" \
    "**Parent:** #$S2_3_8

Replace \`state_invariants_preserved\` with a real proof:
- Total ADA is conserved (inputs consumed = outputs created + fees)
- No double-spending across transactions in a block
- Pool registration state is consistent

**File:** \`Cleanode/Ledger/State.lean\` lines 295–298" \
    "task,phase-2,ledger,formal-verification" > /dev/null

create_issue "[Task 2.3.8.5] Prove fee monotonicity and bounds" \
    "**Parent:** #$S2_3_8

The Fee module has some real proofs already. Extend:
- Prove total fee (base + script) is always >= minFee
- Prove fee calculation is deterministic given same inputs
- Prove min-ADA-per-output formula is monotone in output size

**File:** \`Cleanode/Ledger/Fee.lean\`" \
    "task,phase-2,ledger,formal-verification" > /dev/null

# ============================================================================
# STORY 3.1.3: Mempool Validation Hardening
# ============================================================================

echo "=== Story 3.1.3: Mempool Validation Hardening ==="

S3_1_3=$(create_issue \
    "[Story 3.1.3] Mempool Validation Hardening" \
    "**Parent:** Epic 3.1 — Transaction Submission Mini-Protocol

**As a** relay node operator
**I want** the mempool to properly validate and track transactions
**So that** only valid transactions are relayed to peers

## Context
The mempool has several issues:
- \`addTxRaw\` creates a dummy transaction (empty body) instead of parsing CBOR
- Timestamps are hardcoded to 0 (TODO in code), breaking expiry
- No tracking of mempool-internal dependencies (tx spending another mempool tx's output)
- Capacity errors reuse \`TxTooLarge\` instead of a proper error type

## Tasks
- [ ] Task 3.1.3.1: Parse transactions from CBOR in addTxRaw
- [ ] Task 3.1.3.2: Implement proper timestamp tracking
- [ ] Task 3.1.3.3: Track mempool-internal UTxO dependencies
- [ ] Task 3.1.3.4: Add proper mempool capacity error types

## Acceptance Criteria
- [ ] addTxRaw parses and validates the transaction, not just stores raw bytes
- [ ] Timestamps are correctly tracked for expiry
- [ ] Mempool can detect when one unconfirmed tx depends on another

## References
- \`Cleanode/Network/Mempool.lean\` line 135 (TODO), lines 191–208 (dummy tx)" \
    "story,phase-3,ledger,network")

echo "  Created Story 3.1.3: #$S3_1_3"

echo "  Creating tasks for Story 3.1.3..."

create_issue "[Task 3.1.3.1] Parse transactions from CBOR in addTxRaw" \
    "**Parent:** #$S3_1_3

\`Mempool.addTxRaw\` currently creates a dummy \`Transaction\` with empty inputs/outputs.
Fix it to actually parse the CBOR bytes into a proper transaction, then validate.

**File:** \`Cleanode/Network/Mempool.lean\` lines 191–208" \
    "task,phase-3,ledger,network" > /dev/null

create_issue "[Task 3.1.3.2] Implement proper timestamp tracking in mempool" \
    "**Parent:** #$S3_1_3

Replace \`addedAt := 0  -- TODO: use actual timestamp\` with real timestamps.
Use \`IO.monoMsNow\` or equivalent to get current time in milliseconds.
This fixes expiry pruning which currently can't work.

**File:** \`Cleanode/Network/Mempool.lean\` line 135" \
    "task,phase-3,network" > /dev/null

create_issue "[Task 3.1.3.3] Track mempool-internal UTxO dependencies" \
    "**Parent:** #$S3_1_3

When transaction B spends an output of transaction A, and both are in the mempool:
1. Track dependency graph between mempool entries
2. If A is removed (e.g., invalidated), also remove B
3. When building blocks, order transactions respecting dependencies
4. Validate B against a virtual UTxO set that includes A's outputs

**File:** \`Cleanode/Network/Mempool.lean\`" \
    "task,phase-3,ledger,network" > /dev/null

create_issue "[Task 3.1.3.4] Add proper mempool capacity error types" \
    "**Parent:** #$S3_1_3

Currently mempool full conditions reuse \`ValidationError.TxTooLarge 0 0\` which
is misleading. Add proper error handling:
- \`MempoolFull (current max : Nat)\`
- \`MempoolByteLimitExceeded (current max : Nat)\`

Either extend \`ValidationError\` or use a separate \`MempoolError\` type.

**File:** \`Cleanode/Network/Mempool.lean\` lines 128–130" \
    "task,phase-3,network" > /dev/null

# ============================================================================
# Summary
# ============================================================================

echo ""
echo "============================================"
echo "Created issues:"
echo "  Story 2.3.5: #$S2_3_5 — Witness Set Extension & Signature Validation (7 tasks)"
echo "  Story 2.3.6: #$S2_3_6 — Script Validation Pipeline (7 tasks)"
echo "  Story 2.3.7: #$S2_3_7 — Extended Transaction Body & Validation Rules (8 tasks)"
echo "  Story 2.3.8: #$S2_3_8 — Transaction Validation Formal Proofs (5 tasks)"
echo "  Story 3.1.3: #$S3_1_3 — Mempool Validation Hardening (4 tasks)"
echo ""
echo "Total: 5 stories + 31 tasks = 36 issues"
echo "============================================"
