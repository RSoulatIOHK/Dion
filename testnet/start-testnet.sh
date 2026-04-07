#!/usr/bin/env bash
# start-testnet.sh — Spin up a local cardano-testnet, register Dion's pool,
# and print the command to start Dion as a block producer.
#
# Usage:
#   cd /path/to/Dion
#   ./testnet/start-testnet.sh
#
# Requirements:
#   - cardano-testnet, cardano-cli in ~/tools/cardano/ (or set CARDANO_BIN)
#   - Dion SPO keys in ./keys/
#   - Dion binary built: lake build

set -euo pipefail

CARDANO_BIN="${CARDANO_BIN:-$HOME/tools/cardano}"
CARDANO_CLI="$CARDANO_BIN/cardano-cli"
CARDANO_TESTNET="$CARDANO_BIN/cardano-testnet"

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TESTNET_DIR="$REPO_ROOT/testnet/local"
KEYS_DIR="$REPO_ROOT/keys"
ENV_FILE="$TESTNET_DIR/env.sh"

MAGIC=42
EPOCH_LENGTH=500       # 500 slots × 1s = ~8 min/epoch; 2 epochs for delegation
SLOT_LENGTH=1          # integer seconds — compatible with Dion's slot clock
ACTIVE_SLOTS_COEFF=0.05

echo "=============================================="
echo "  Dion Local Testnet"
echo "=============================================="
echo "  Repo      : $REPO_ROOT"
echo "  Keys      : $KEYS_DIR"
echo "  Magic     : $MAGIC"
echo "  Epoch     : $EPOCH_LENGTH slots (~$((EPOCH_LENGTH * SLOT_LENGTH / 60)) min)"
echo "  Slot      : ${SLOT_LENGTH}s"
echo "  Delegation ready after: ~$((EPOCH_LENGTH * 2 * SLOT_LENGTH / 60)) min"
echo "=============================================="

mkdir -p "$TESTNET_DIR"

# ── Step 1: Start cardano-testnet in background ──────────────────────────────
echo ""
echo "[1] Starting cardano-testnet in background..."

cd "$TESTNET_DIR"

"$CARDANO_TESTNET" cardano \
  --num-pool-nodes 1 \
  --epoch-length "$EPOCH_LENGTH" \
  --slot-length "$SLOT_LENGTH" \
  --testnet-magic "$MAGIC" \
  --active-slots-coeff "$ACTIVE_SLOTS_COEFF" \
  --max-lovelace-supply 45000000000000000 \
  >"$TESTNET_DIR/testnet.log" 2>&1 &

TESTNET_PID=$!
echo "  PID: $TESTNET_PID"
echo "$TESTNET_PID" > "$TESTNET_DIR/testnet.pid"

# ── Step 2: Wait for socket ──────────────────────────────────────────────────
echo "[2] Waiting for node socket..."
SOCKET=""
for i in $(seq 1 60); do
  # cardano-testnet creates sockets in state-node-spo*/
  SOCKET=$(find "$TESTNET_DIR" -name "node.socket" 2>/dev/null | head -1 || true)
  if [ -n "$SOCKET" ]; then break; fi
  sleep 2
  echo -n "."
done
echo ""

if [ -z "$SOCKET" ]; then
  echo "ERROR: node socket not found after 120s. Check $TESTNET_DIR/testnet.log"
  kill "$TESTNET_PID" 2>/dev/null || true
  exit 1
fi
echo "  Socket: $SOCKET"
export CARDANO_NODE_SOCKET_PATH="$SOCKET"

# ── Step 3: Wait for node to sync genesis ────────────────────────────────────
echo "[3] Waiting for node to produce genesis block..."
for i in $(seq 1 30); do
  TIP=$("$CARDANO_CLI" query tip --testnet-magic "$MAGIC" 2>/dev/null || true)
  SLOT=$(echo "$TIP" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('slot',0))" 2>/dev/null || echo "0")
  if [ "$SLOT" -gt 0 ] 2>/dev/null; then break; fi
  sleep 2
  echo -n "."
done
echo ""
echo "  Tip: $("$CARDANO_CLI" query tip --testnet-magic "$MAGIC" 2>/dev/null || echo '(not ready)')"

# ── Step 4: Extract genesis params ───────────────────────────────────────────
echo "[4] Extracting genesis parameters..."
SHELLEY_GENESIS=$(find "$TESTNET_DIR" -name "shelley-genesis.json" 2>/dev/null | head -1)
if [ -z "$SHELLEY_GENESIS" ]; then
  echo "ERROR: shelley-genesis.json not found in $TESTNET_DIR"
  exit 1
fi

SYSTEM_START=$(python3 -c "
import json, datetime, calendar
d = json.load(open('$SHELLEY_GENESIS'))
s = d['systemStart']
# Parse ISO 8601: 2024-01-01T00:00:00Z
dt = datetime.datetime.strptime(s.rstrip('Z'), '%Y-%m-%dT%H:%M:%S')
print(calendar.timegm(dt.timetuple()))
")
echo "  systemStart: $SYSTEM_START ($(date -r "$SYSTEM_START" -u '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || date -d "@$SYSTEM_START" -u '+%Y-%m-%dT%H:%M:%SZ'))"

# ── Step 5: Find genesis UTxO and fund our payment address ───────────────────
echo "[5] Finding genesis UTxO address..."
GENESIS_ADDR_FILE=$(find "$TESTNET_DIR" -path "*/addresses/user*.addr" 2>/dev/null | head -1 || \
                    find "$TESTNET_DIR" -name "*.addr" 2>/dev/null | head -1)
GENESIS_SKEY_FILE="${GENESIS_ADDR_FILE%.addr}.skey"

if [ -z "$GENESIS_ADDR_FILE" ] || [ ! -f "$GENESIS_ADDR_FILE" ]; then
  echo "  WARNING: Could not find genesis UTxO address file. Manual funding required."
else
  GENESIS_ADDR=$(cat "$GENESIS_ADDR_FILE")
  echo "  Genesis addr: $GENESIS_ADDR"
  UTXO=$("$CARDANO_CLI" query utxo --address "$GENESIS_ADDR" --testnet-magic "$MAGIC" 2>/dev/null || echo "")
  echo "  UTxO: $(echo "$UTXO" | tail -n +3 | head -5)"
fi

# ── Step 6: Register our stake key ───────────────────────────────────────────
echo "[6] Building stake key registration transaction..."

OUR_ADDR=$(cat "$KEYS_DIR/payment.addr" 2>/dev/null || echo "")
OUR_STAKE_VKEY="$KEYS_DIR/stake.vkey"
OUR_STAKE_SKEY="$KEYS_DIR/stake.skey"
OUR_PAYMENT_SKEY="$KEYS_DIR/payment.skey"

if [ -z "$OUR_ADDR" ] || [ ! -f "$OUR_STAKE_VKEY" ]; then
  echo "  WARNING: Payment/stake keys not found in $KEYS_DIR — skipping registration."
  echo "  Generate keys with: ./dion spo keygen --dir ./keys"
else
  # Transfer funds to our payment address first
  if [ -n "$GENESIS_ADDR_FILE" ] && [ -f "$GENESIS_SKEY_FILE" ]; then
    TXIN=$("$CARDANO_CLI" query utxo --address "$GENESIS_ADDR" --testnet-magic "$MAGIC" --out-file /dev/stdout 2>/dev/null | \
           python3 -c "import sys,json; d=json.load(sys.stdin); print(list(d.keys())[0])" 2>/dev/null || echo "")
    if [ -n "$TXIN" ]; then
      echo "  Funding our payment address from genesis UTxO..."
      "$CARDANO_CLI" transaction build \
        --testnet-magic "$MAGIC" \
        --tx-in "$TXIN" \
        --tx-out "$OUR_ADDR+10000000000" \
        --change-address "$GENESIS_ADDR" \
        --out-file "$TESTNET_DIR/fund-tx.body" 2>/dev/null || true

      "$CARDANO_CLI" transaction sign \
        --tx-body-file "$TESTNET_DIR/fund-tx.body" \
        --signing-key-file "$GENESIS_SKEY_FILE" \
        --testnet-magic "$MAGIC" \
        --out-file "$TESTNET_DIR/fund-tx.signed" 2>/dev/null || true

      "$CARDANO_CLI" transaction submit \
        --testnet-magic "$MAGIC" \
        --tx-file "$TESTNET_DIR/fund-tx.signed" 2>/dev/null && \
        echo "  ✓ Funded: 10,000 ADA → $OUR_ADDR" || \
        echo "  WARNING: Funding tx failed — try manually"

      sleep 5  # wait for tx to land
    fi
  fi

  # Register stake key
  "$CARDANO_CLI" stake-address registration-certificate \
    --stake-verification-key-file "$OUR_STAKE_VKEY" \
    --out-file "$TESTNET_DIR/stake-reg.cert" 2>/dev/null || true

  # Build stake pool registration cert
  RELAY_HOST="127.0.0.1"
  RELAY_PORT=3002  # Dion's listen port
  VRF_VKEY="$KEYS_DIR/vrf.vkey"
  COLD_VKEY="$KEYS_DIR/cold.vkey"
  COLD_SKEY="$KEYS_DIR/cold.skey"

  "$CARDANO_CLI" stake-pool registration-certificate \
    --cold-verification-key-file "$COLD_VKEY" \
    --vrf-verification-key-file "$VRF_VKEY" \
    --pool-pledge 1000000000 \
    --pool-cost 340000000 \
    --pool-margin 0.01 \
    --pool-reward-account-verification-key-file "$OUR_STAKE_VKEY" \
    --pool-owner-stake-verification-key-file "$OUR_STAKE_VKEY" \
    --testnet-magic "$MAGIC" \
    --single-host-addr-relay "$RELAY_HOST" \
    --pool-relay-port "$RELAY_PORT" \
    --out-file "$TESTNET_DIR/pool-reg.cert" 2>/dev/null || true

  # Delegation cert (self-delegate to our pool)
  POOL_ID=$("$CARDANO_CLI" stake-pool id \
    --cold-verification-key-file "$COLD_VKEY" 2>/dev/null || echo "")
  echo "  Pool ID: $POOL_ID"

  "$CARDANO_CLI" stake-address delegation-certificate \
    --stake-verification-key-file "$OUR_STAKE_VKEY" \
    --cold-verification-key-file "$COLD_VKEY" \
    --out-file "$TESTNET_DIR/delegation.cert" 2>/dev/null || true

  # Build registration tx (stake key reg + pool reg + delegation)
  UTXO_OUR=$("$CARDANO_CLI" query utxo --address "$OUR_ADDR" --testnet-magic "$MAGIC" --out-file /dev/stdout 2>/dev/null | \
             python3 -c "import sys,json; d=json.load(sys.stdin); print(list(d.keys())[0] if d else '')" 2>/dev/null || echo "")

  if [ -n "$UTXO_OUR" ]; then
    "$CARDANO_CLI" transaction build \
      --testnet-magic "$MAGIC" \
      --tx-in "$UTXO_OUR" \
      --tx-out "$OUR_ADDR+5000000000" \
      --change-address "$OUR_ADDR" \
      --certificate-file "$TESTNET_DIR/stake-reg.cert" \
      --certificate-file "$TESTNET_DIR/pool-reg.cert" \
      --certificate-file "$TESTNET_DIR/delegation.cert" \
      --out-file "$TESTNET_DIR/registration.body" 2>/dev/null || true

    "$CARDANO_CLI" transaction sign \
      --tx-body-file "$TESTNET_DIR/registration.body" \
      --signing-key-file "$OUR_PAYMENT_SKEY" \
      --signing-key-file "$OUR_STAKE_SKEY" \
      --signing-key-file "$COLD_SKEY" \
      --testnet-magic "$MAGIC" \
      --out-file "$TESTNET_DIR/registration.signed" 2>/dev/null || true

    "$CARDANO_CLI" transaction submit \
      --testnet-magic "$MAGIC" \
      --tx-file "$TESTNET_DIR/registration.signed" 2>/dev/null && \
      echo "  ✓ Pool + stake key + delegation registered on-chain!" || \
      echo "  WARNING: Registration tx failed — check logs"
  else
    echo "  WARNING: No UTxO at $OUR_ADDR — skipping registration"
  fi
fi

# ── Step 7: Write env file ────────────────────────────────────────────────────
echo "[7] Writing env file to $ENV_FILE..."
cat > "$ENV_FILE" <<EOF
# Generated by start-testnet.sh — source this before running dion
export CARDANO_NODE_SOCKET_PATH="$SOCKET"
export TESTNET_MAGIC="$MAGIC"
export TESTNET_SYSTEM_START="$SYSTEM_START"
export TESTNET_EPOCH_LENGTH="$EPOCH_LENGTH"
export TESTNET_SOCKET="$SOCKET"
export TESTNET_PID="$TESTNET_PID"
EOF
echo "  ✓ Env written"

# ── Step 8: Get epoch nonce ───────────────────────────────────────────────────
echo "[8] Getting current epoch nonce (needed for Dion --epoch-nonce)..."
sleep 3
EPOCH_NONCE=$("$CARDANO_CLI" query protocol-state --testnet-magic "$MAGIC" 2>/dev/null | \
  python3 -c "
import sys, json, base64
try:
    d = json.load(sys.stdin)
    # Try to get the epochNonce from candidateNonce or epochNonce field
    cs = d.get('candidateNonce', d.get('epochNonce', {}))
    raw = cs.get('contents', '')
    if raw:
        print(raw)
    else:
        # Try direct hex
        print(d.get('epochNonce', {}).get('contents', '0' * 64))
except Exception as e:
    print('0' * 64)
" 2>/dev/null || echo "0000000000000000000000000000000000000000000000000000000000000000")
echo "  Epoch nonce: $EPOCH_NONCE"
echo "export EPOCH_NONCE=\"$EPOCH_NONCE\"" >> "$ENV_FILE"

# ── Step 9: Print Dion start command ─────────────────────────────────────
echo ""
echo "=============================================="
echo "  ✓ Testnet ready!"
echo "=============================================="
echo ""
echo "  Delegation will take effect after 2 epoch boundaries"
echo "  (~$((EPOCH_LENGTH * 2 * SLOT_LENGTH / 60)) minutes from now)"
echo ""
echo "  To start Dion as a block producer, run in another terminal:"
echo ""
echo "    source $ENV_FILE"
echo "    ./testnet/start-dion.sh"
echo ""
echo "  Or manually:"
echo ""
echo "    ./.build/release/dion run \\"
echo "      --testnet-magic $MAGIC \\"
echo "      --system-start $SYSTEM_START \\"
echo "      --epoch-length $EPOCH_LENGTH \\"
echo "      --spo-keys ./keys \\"
echo "      --epoch-nonce $EPOCH_NONCE \\"
echo "      --protocol-major 9 \\"
echo "      --port 3002 \\"
echo "      --peer 127.0.0.1:3001"
echo ""
echo "  Testnet log: $TESTNET_DIR/testnet.log"
echo "  To stop: kill \$(cat $TESTNET_DIR/testnet.pid)"
echo ""
echo "=============================================="

# Keep running so testnet stays alive
wait "$TESTNET_PID"
