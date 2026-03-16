import Cleanode.Consensus.Praos.ForgeLoop
import Cleanode.Consensus.Praos.BlockForge
import Cleanode.Consensus.Praos.ConsensusState
import Cleanode.Consensus.Praos.StakeDistribution
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Consensus.Praos.SPOKeys
import Cleanode.Config.Genesis
import Cleanode.Network.Mempool

/-!
# Preprod SPO Integration Test

End-to-end test of the block production pipeline on the preprod testnet.
Validates that all components work together:

1. Load SPO keys from TextEnvelope files
2. Build slot clock from genesis parameters
3. Initialize consensus state with preprod params
4. Run leader election for a range of slots
5. Forge a block when elected (with real KES signing)

## Usage

```
cleanode test-forge --preprod --spo-keys ./keys
```

Or programmatically via `runPreprodForgeTest`.

## Expected Key Files

The `--spo-keys` directory should contain:
- `vrf.skey` — VRF signing key (TextEnvelope, 64 bytes)
- `kes.skey` — KES signing key (TextEnvelope, Sum-KES depth 6)
- `node.cert` — Operational certificate (TextEnvelope)
- `cold.vkey` — Cold verification key (TextEnvelope, 32 bytes)

## References
- Cardano SPO documentation
- https://developers.cardano.org/docs/operate-a-stake-pool/
-/

namespace Cleanode.Consensus.Praos.PreprodTest

open Cleanode.Consensus.Praos.ForgeLoop
open Cleanode.Consensus.Praos.BlockForge
open Cleanode.Consensus.Praos.ConsensusState
open Cleanode.Consensus.Praos.StakeDistribution
open Cleanode.Consensus.Praos.LeaderElection
open Cleanode.Config.Genesis

-- ====================
-- = Preprod Genesis  =
-- ====================

/-- Preprod network genesis parameters -/
def preprodGenesis : ShelleyGenesis :=
  { epochLength := 432000,        -- 5 days at 1-second slots
    slotLength := 1,               -- 1 second per slot
    activeSlotsCoeff := { numerator := 1, denominator := 20 },  -- f = 0.05
    securityParam := 2160,         -- k = 2160
    maxLovelaceSupply := 45000000000000000,
    networkMagic := 1,             -- Preprod magic
    networkId := "Testnet",
    protocolParams := some {
      minFeeA := 44,
      minFeeB := 155381,
      maxBlockBodySize := 90112,
      maxBlockHeaderSize := 1100,
      maxTxSize := 16384,
      keyDeposit := 2000000,
      poolDeposit := 500000000,
      eMax := 18,
      nOpt := 500,
      decentralisationParam := 0.0
    } }

/-- Preview network genesis parameters -/
def previewGenesis : ShelleyGenesis :=
  { preprodGenesis with
    networkMagic := 2,
    networkId := "Testnet" }

/-- Mainnet genesis parameters -/
def mainnetGenesis : ShelleyGenesis :=
  { preprodGenesis with
    networkMagic := 764824073,
    networkId := "Mainnet" }

-- ====================
-- = Test Harness     =
-- ====================

/-- Test result -/
structure ForgeTestResult where
  slotsChecked : Nat
  timesElected : Nat
  blocksForged : Nat
  errors : List String

/-- Run a dry-run forge test: check leadership for a range of slots.
    Does NOT connect to the network — uses synthetic stake data. -/
def runDryForgeTest (keyDir : String) (network : String := "Preprod")
    (slotRange : Nat := 100) : IO ForgeTestResult := do
  IO.println s!"[test] Starting dry forge test on {network}"
  IO.println s!"[test] Key directory: {keyDir}"
  IO.println s!"[test] Checking {slotRange} slots for leadership"

  -- Load SPO keys
  let paths := SPOKeys.SPOKeyPaths.default keyDir
  let keysResult ← SPOKeys.loadSPOKeys paths
  match keysResult with
  | .error e =>
    IO.eprintln s!"[test] Failed to load keys: {e}"
    return { slotsChecked := 0, timesElected := 0, blocksForged := 0, errors := [e] }
  | .ok forgeParams => do
    IO.println s!"[test] Keys loaded. Pool ID: {forgeParams.poolId.size} bytes"

    -- Build slot clock
    let clock := match network with
      | "Preprod" => SlotClock.preprod
      | "Preview" => SlotClock.preview
      | _ => SlotClock.mainnet

    -- Get current slot
    let currentSlot ← clock.currentSlot
    IO.println s!"[test] Current slot: {currentSlot}"
    IO.println s!"[test] Current epoch: {clock.slotEpoch currentSlot}"
    IO.println s!"[test] Current KES period: {clock.slotKESPeriod currentSlot}"

    -- Build consensus state with synthetic stake
    -- Give our pool 1% of total stake for testing
    let genesis := match network with
      | "Preprod" => preprodGenesis
      | "Preview" => previewGenesis
      | _ => mainnetGenesis

    let totalStake := 31000000000000000  -- ~31B ADA total delegated
    let poolStake := totalStake / 100    -- 1% of stake
    let stakeSnapshot : StakeSnapshot := {
      poolStakes := [(forgeParams.poolId, poolStake)],
      totalStake := totalStake
    }

    -- Epoch nonce (use zeros for testing — real node gets this from chain)
    let epochNonce := ByteArray.mk (Array.replicate 32 0)
    let cs : ConsensusState := {
      (ConsensusState.initial genesis) with
      epochNonce := epochNonce,
      stakeSnapshot := stakeSnapshot,
      currentEpoch := clock.slotEpoch currentSlot
    }

    -- Check leadership across slots
    let mut elected := 0
    let mut forged := 0
    let mut errors : List String := []

    for i in List.range slotRange do
      let slot := currentSlot + i
      let result := checkLeader forgeParams.vrfSecretKey cs.epochNonce
        slot cs.activeSlotsCoeff poolStake totalStake
      match result with
      | .isLeader _proof _output => do
        elected := elected + 1
        IO.println s!"[test] ELECTED at slot {slot} (offset +{i})"

        -- Try to actually forge (with real KES signing)
        let mempool := Cleanode.Network.Mempool.Mempool.empty {}
        let blockBody := Cleanode.Consensus.Praos.TxSelection.selectTransactions mempool 90112
        let prevHash := ByteArray.mk (Array.replicate 32 0)
        let kesPeriod := clock.slotKESPeriod slot

        let forgeResult ← tryForgeBlockIO forgeParams cs
          (UInt64.ofNat (cs.currentEpoch * cs.epochLength + i)).toNat  -- block number
          slot prevHash blockBody kesPeriod
        match forgeResult with
        | .ok (some block) =>
          forged := forged + 1
          IO.println s!"[test]   Forged block #{block.blockNumber}: header={block.headerBytes.size}B, body={block.bodyBytes.size}B"
        | .ok none =>
          IO.println s!"[test]   Forge returned none (unexpected)"
        | .error e =>
          errors := errors ++ [s!"Slot {slot}: {e}"]
          IO.println s!"[test]   Forge error: {e}"
      | .notLeader => pure ()
      | .invalidPool =>
        errors := errors ++ ["Pool has zero stake"]

    IO.println s!"[test] Results: {slotRange} slots checked, {elected} elected, {forged} forged"
    if errors.length > 0 then
      IO.println s!"[test] Errors: {errors.length}"
      for e in errors do
        IO.eprintln s!"[test]   {e}"

    return {
      slotsChecked := slotRange,
      timesElected := elected,
      blocksForged := forged,
      errors := errors
    }

-- ====================
-- = Validation       =
-- ====================

/-- Validate that SPO keys are consistent and suitable for the target network. -/
def validateSPOSetup (keyDir : String) (network : String := "Preprod") : IO Bool := do
  IO.println s!"[validate] Checking SPO setup for {network}..."

  let paths := SPOKeys.SPOKeyPaths.default keyDir
  -- Check files exist
  let missingFiles ← SPOKeys.validateKeyPaths paths
  if missingFiles.length > 0 then
    IO.eprintln "[validate] Key files missing:"
    for f in missingFiles do
      IO.eprintln s!"  {f}"
    return false

  -- Load keys and check sizes
  let keysResult ← SPOKeys.loadSPOKeys paths
  match keysResult with
  | .error e =>
    IO.eprintln s!"[validate] Key loading failed: {e}"
    return false
  | .ok fp => do
    IO.println s!"[validate] VRF key: {fp.vrfSecretKey.length} bytes secret, {fp.vrfPublicKey.length} bytes public"
    IO.println s!"[validate] KES key: {fp.kesSigningKey.length} bytes"
    IO.println s!"[validate] Pool ID: {fp.poolId.size} bytes"
    IO.println s!"[validate] OpCert: seq={fp.operationalCert.sequenceNumber}, kesPeriod={fp.operationalCert.kesPeriod}"

    -- Check KES period validity
    let clock := match network with
      | "Preprod" => SlotClock.preprod
      | "Preview" => SlotClock.preview
      | _ => SlotClock.mainnet
    let currentSlot ← clock.currentSlot
    let currentKES := clock.slotKESPeriod currentSlot
    let certKES := fp.operationalCert.kesPeriod
    let maxPeriods := 62  -- Sum-KES depth 6 = 64 periods, minus 2 for safety
    if currentKES < certKES then
      IO.eprintln s!"[validate] WARNING: KES period {currentKES} < cert period {certKES}. Clock skew?"
    else if currentKES - certKES > maxPeriods then
      IO.eprintln s!"[validate] ERROR: KES key expired! Current period {currentKES}, cert issued at {certKES} (max evolution: 62)"
      return false
    else
      IO.println s!"[validate] KES period OK: current={currentKES}, cert={certKES}, remaining={maxPeriods - (currentKES - certKES)}"

    IO.println "[validate] All checks passed."
    return true

-- ============================================================
-- = Cross-Validation Guide (#351)                            =
-- = How to validate forged blocks using a cardano-node peer  =
-- ============================================================

/-- Print step-by-step instructions for cross-validating forged blocks
    with a real cardano-node via the N2N peer protocol. -/
def runCrossValidationGuide : IO Unit := do
  IO.println "============================================================"
  IO.println " Cross-Validation: Cleanode <-> cardano-node (Preprod)"
  IO.println "============================================================"
  IO.println ""
  IO.println "This validates that blocks forged by Cleanode are accepted by"
  IO.println "cardano-node through the standard Ouroboros N2N peer protocol."
  IO.println ""
  IO.println "--- Prerequisites ---"
  IO.println ""
  IO.println "1. A synced cardano-node on preprod"
  IO.println "2. Cleanode built with SPO keys (cold, VRF, KES, opcert)"
  IO.println "3. Your pool registered on preprod with non-zero stake"
  IO.println ""
  IO.println "--- Step 1: Start Cleanode as a relay with SPO keys ---"
  IO.println ""
  IO.println "  cleanode relay --preprod \\"
  IO.println "    --spo-keys ./keys \\"
  IO.println "    --listen 3002 \\"
  IO.println "    --socket-path /tmp/cleanode.socket"
  IO.println ""
  IO.println "  Cleanode will:"
  IO.println "    - Sync the chain from bootstrap peers"
  IO.println "    - Build stake distribution from certificates during sync"
  IO.println "    - Run leader election each slot using real VRF"
  IO.println "    - Forge and KES-sign blocks when elected"
  IO.println "    - Announce forged blocks via ChainSync server on port 3002"
  IO.println "    - Serve block bodies via BlockFetch server on port 3002"
  IO.println ""
  IO.println "--- Step 2: Configure cardano-node to peer with Cleanode ---"
  IO.println ""
  IO.println "  Add to your cardano-node topology file (topology.json):"
  IO.println ""
  IO.println "  {"
  IO.println "    \"localRoots\": [{"
  IO.println "      \"accessPoints\": [{"
  IO.println "        \"address\": \"127.0.0.1\","
  IO.println "        \"port\": 3002"
  IO.println "      }],"
  IO.println "      \"advertise\": false,"
  IO.println "      \"valency\": 1,"
  IO.println "      \"trustable\": false"
  IO.println "    }],"
  IO.println "    \"publicRoots\": []"
  IO.println "  }"
  IO.println ""
  IO.println "  Then restart cardano-node with this topology."
  IO.println ""
  IO.println "--- Step 3: Verify the connection ---"
  IO.println ""
  IO.println "  In Cleanode logs, look for:"
  IO.println "    [listen] Accepted inbound peer from 127.0.0.1:XXXXX"
  IO.println "    [inbound] ChainSync: MsgFindIntersect from 127.0.0.1:XXXXX"
  IO.println "    [inbound] ChainSync: MsgRequestNext from 127.0.0.1:XXXXX"
  IO.println ""
  IO.println "  This means cardano-node is subscribed to our ChainSync feed."
  IO.println ""
  IO.println "--- Step 4: Wait for block production ---"
  IO.println ""
  IO.println "  When Cleanode wins a slot and forges a block:"
  IO.println "    [forge] Forged block #N at slot S (VRF proof: ...)"
  IO.println "    [announce] Block N (slot S) sent to 1 peers"
  IO.println ""
  IO.println "  cardano-node will then:"
  IO.println "    1. Receive MsgRollForward via ChainSync (header)"
  IO.println "    2. Send MsgRequestRange via BlockFetch (fetch body)"
  IO.println "    3. Validate the block (VRF, KES, ledger rules)"
  IO.println "    4. Either adopt the block or reject it"
  IO.println ""
  IO.println "--- Step 5: Check validation result ---"
  IO.println ""
  IO.println "  SUCCESS indicators:"
  IO.println "    - cardano-node stays connected (no disconnect)"
  IO.println "    - cardano-cli query tip shows the new block:"
  IO.println "      cardano-cli query tip --testnet-magic 1"
  IO.println "    - cardano-node logs show block adoption"
  IO.println ""
  IO.println "  FAILURE indicators:"
  IO.println "    - cardano-node disconnects immediately after block"
  IO.println "    - cardano-node logs show validation errors:"
  IO.println "      * VRF verification failed -> check VRF key"
  IO.println "      * KES verification failed -> check KES key / opcert"
  IO.println "      * Block from unregistered pool -> check pool registration"
  IO.println "      * Insufficient stake -> check delegation"
  IO.println ""
  IO.println "--- Debugging Tips ---"
  IO.println ""
  IO.println "  1. Enable trace logging on cardano-node:"
  IO.println "     Set TraceBlockFetchDecisions: true in config.json"
  IO.println ""
  IO.println "  2. Check Cleanode's stake snapshot:"
  IO.println "     cleanode query stake-snapshot --socket /tmp/cleanode.socket"
  IO.println ""
  IO.println "  3. Verify KES key hasn't expired:"
  IO.println "     cleanode test-forge --preprod --spo-keys ./keys --validate-only"
  IO.println ""
  IO.println "  4. Check pool registration on-chain:"
  IO.println "     cardano-cli query stake-snapshot --testnet-magic 1 --stake-pool-id <POOL_ID>"
  IO.println ""
  IO.println "============================================================"

end Cleanode.Consensus.Praos.PreprodTest
