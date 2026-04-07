#!/usr/bin/env bash
# spo-setup.sh — One-shot SPO setup for Dion on Preview
# Usage: ./spo-setup.sh [--dir PATH] [--relay HOST:PORT] [--name STR] [--ticker STR]
#                       [--metadata-url URL] [--pledge ADA] [--port N]
set -euo pipefail

# ─── Defaults ──────────────────────────────────────────────────────────────
DIR="$HOME/spo-preview"
RELAY_HOST=""
RELAY_PORT="3001"
POOL_NAME="C/LEAN DION Node"
POOL_TICKER="DION"
POOL_DESC="SPO to test the C/LEAN Dion Node"
POOL_HOMEPAGE="Chut chut! Pas de marque"
METADATA_URL="https://tinyurl.com/27xokufj"
PLEDGE_LOVELACE="1000000000"    # 1000 ADA
COST_LOVELACE="340000000"       # 340 ADA minimum
MARGIN="0.05"
SOCKET="${SOCKET_PATH:-/tmp/dion.socket}"
TESTNET_MAGIC="2"               # Preview
DION="$(dirname "$0")/.lake/build/bin/dion"
CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
NODE_PORT="3001"

# ─── Arg parsing ───────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
  case $1 in
    --dir)          DIR="$2";           shift 2 ;;
    --relay)        IFS=: read -r RELAY_HOST RELAY_PORT <<< "$2"; shift 2 ;;
    --name)         POOL_NAME="$2";     shift 2 ;;
    --ticker)       POOL_TICKER="$2";   shift 2 ;;
    --description)  POOL_DESC="$2";     shift 2 ;;
    --homepage)     POOL_HOMEPAGE="$2"; shift 2 ;;
    --metadata-url) METADATA_URL="$2";  shift 2 ;;
    --pledge)       PLEDGE_LOVELACE="$2"; shift 2 ;;
    --cost)         COST_LOVELACE="$2"; shift 2 ;;
    --socket)       SOCKET="$2";        shift 2 ;;
    --port)         NODE_PORT="$2";     shift 2 ;;
    *) echo "Unknown option: $1"; exit 1 ;;
  esac
done

# ─── Helpers ───────────────────────────────────────────────────────────────
bold()  { printf '\033[1m%s\033[0m' "$*"; }
green() { printf '\033[32m%s\033[0m' "$*"; }
cyan()  { printf '\033[36m%s\033[0m' "$*"; }
red()   { printf '\033[31m%s\033[0m' "$*"; }
step()  { echo; echo "$(bold "▶ $*")"; }
ok()    { echo "  $(green "✓") $*"; }
info()  { echo "  $(cyan "·") $*"; }

ask_relay() {
  if [[ -z "$RELAY_HOST" ]]; then
    echo
    read -rp "  Enter your relay IP or hostname (leave blank for localhost): " RELAY_HOST
    [[ -z "$RELAY_HOST" ]] && RELAY_HOST="127.0.0.1"
    read -rp "  Enter relay port [${RELAY_PORT}]: " inp
    [[ -n "$inp" ]] && RELAY_PORT="$inp"
  fi
}

wait_for_funding() {
  local addr="$1"
  echo
  echo "  $(bold "Fund your payment address on the Preview faucet:")"
  echo "  $(cyan "https://docs.cardano.org/cardano-testnet/tools/faucet/")"
  echo
  echo "  Address: $(bold "$addr")"
  echo
  read -rp "  Press ENTER once you've requested funds (or CTRL+C to abort)..." _
  echo
  echo "  Waiting for UTxO to appear on chain..."
  local attempts=0
  while true; do
    local utxo
    utxo=$($CARDANO_CLI query utxo \
      --socket-path "$SOCKET" \
      --address "$addr" \
      --testnet-magic "$TESTNET_MAGIC" 2>/dev/null || true)
    if echo "$utxo" | grep -q "lovelace"; then
      echo
      ok "Funds received!"
      echo "$utxo"
      break
    fi
    attempts=$((attempts + 1))
    if [[ $attempts -ge 60 ]]; then
      echo
      red "  ✗ Timed out waiting for funds. Is the node running and synced?"
      exit 1
    fi
    printf "."
    sleep 5
  done
}

check_node_socket() {
  if [[ ! -S "$SOCKET" ]]; then
    echo
    red "  ✗ Node socket not found at $SOCKET"
    echo "  Start the node first:"
    echo "    dion run --preview --mithril-sync --spo-keys $DIR --external-addr ${RELAY_HOST}:${NODE_PORT}"
    echo "  then re-run this script with --skip-keygen --skip-metadata"
    exit 1
  fi
}

# ─── Main ──────────────────────────────────────────────────────────────────
echo
echo "$(bold "╔══════════════════════════════════════════╗")"
echo "$(bold "║   Dion SPO Setup — Preview Network   ║")"
echo "$(bold "╚══════════════════════════════════════════╝")"
echo
info "Key directory : $DIR"
info "Pool ticker   : $POOL_TICKER"
info "Metadata URL  : $METADATA_URL"

ask_relay
info "Relay         : ${RELAY_HOST}:${RELAY_PORT}"

# ── Step 1: Keygen ──────────────────────────────────────────────────────────
step "Generating SPO keys"
"$DION" spo keygen --dir "$DIR"

# ── Step 2: Metadata file (local copy for hash) ──────────────────────────────
step "Writing pool metadata"
METADATA_FILE="$DIR/poolMetadata.json"
"$DION" spo metadata \
  --name "$POOL_NAME" \
  --ticker "$POOL_TICKER" \
  --description "$POOL_DESC" \
  --homepage "$POOL_HOMEPAGE" \
  --out "$METADATA_FILE"

METADATA_HASH=$("$DION" spo metadata \
  --name "$POOL_NAME" \
  --ticker "$POOL_TICKER" \
  --description "$POOL_DESC" \
  --homepage "$POOL_HOMEPAGE" \
  --out /dev/null 2>/dev/null | grep "blake2b-256" | awk '{print $NF}')

ok "Metadata hash: $METADATA_HASH"

# ── Step 3: Payment address ──────────────────────────────────────────────────
step "Deriving payment address"
PAYMENT_ADDR=$($CARDANO_CLI address build \
  --payment-verification-key-file "$DIR/payment.vkey" \
  --testnet-magic "$TESTNET_MAGIC")
ok "Payment address: $PAYMENT_ADDR"

# ── Step 4: Wait for node + fund ─────────────────────────────────────────────
step "Checking node socket"
echo
echo "  Start the node in another terminal (if not already running):"
echo "  $(cyan "dion run --preview --mithril-sync --spo-keys $DIR --external-addr ${RELAY_HOST}:${NODE_PORT}")"
echo
read -rp "  Press ENTER once the node is running and synced to tip..." _
check_node_socket
ok "Socket found at $SOCKET"

wait_for_funding "$PAYMENT_ADDR"

# ── Step 5: Get UTxO ─────────────────────────────────────────────────────────
step "Selecting UTxO for registration tx"
TX_IN=$($CARDANO_CLI query utxo \
  --socket-path "$SOCKET" \
  --address "$PAYMENT_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout 2>/dev/null \
  | python3 -c "
import json, sys
u = json.load(sys.stdin)
# Pick the UTxO with the most lovelace
best = max(u.items(), key=lambda x: x[1]['value'].get('lovelace', 0) if isinstance(x[1]['value'], dict) else x[1]['value'])
txid, idx = best[0].rsplit('#', 1) if '#' in best[0] else (list(best[1].keys())[0], '0')
print(best[0])
" 2>/dev/null || \
  $CARDANO_CLI query utxo \
    --socket-path "$SOCKET" \
    --address "$PAYMENT_ADDR" \
    --testnet-magic "$TESTNET_MAGIC" | \
    awk 'NR>2 {print $1"#"$2; exit}')

ok "Using UTxO: $TX_IN"

# ── Step 6: Build certs ──────────────────────────────────────────────────────
step "Building registration certificates"

$CARDANO_CLI stake-address registration-certificate \
  --stake-verification-key-file "$DIR/stake.vkey" \
  --key-reg-deposit-amt 2000000 \
  --out-file "$DIR/stake.cert"
ok "stake.cert"

$CARDANO_CLI stake-pool registration-certificate \
  --cold-verification-key-file "$DIR/pool.vkey" \
  --vrf-verification-key-file "$DIR/vrf.vkey" \
  --pool-pledge "$PLEDGE_LOVELACE" \
  --pool-cost "$COST_LOVELACE" \
  --pool-margin "$MARGIN" \
  --pool-reward-account-verification-key-file "$DIR/stake.vkey" \
  --pool-owner-stake-verification-key-file "$DIR/stake.vkey" \
  --single-host-addr "$RELAY_HOST" \
  --pool-relay-port "$RELAY_PORT" \
  --metadata-url "$METADATA_URL" \
  --metadata-hash "$METADATA_HASH" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$DIR/pool.cert"
ok "pool.cert"

# ── Step 7: Build tx ─────────────────────────────────────────────────────────
step "Building registration transaction"

$CARDANO_CLI transaction build \
  --socket-path "$SOCKET" \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$TX_IN" \
  --change-address "$PAYMENT_ADDR" \
  --certificate-file "$DIR/stake.cert" \
  --certificate-file "$DIR/pool.cert" \
  --witness-override 3 \
  --out-file "$DIR/reg.txbody"
ok "reg.txbody"

# ── Step 8: Sign ─────────────────────────────────────────────────────────────
step "Signing transaction"

$CARDANO_CLI transaction sign \
  --tx-body-file "$DIR/reg.txbody" \
  --signing-key-file "$DIR/payment.skey" \
  --signing-key-file "$DIR/stake.skey" \
  --signing-key-file "$DIR/pool.skey" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$DIR/reg.tx"
ok "reg.tx"

# ── Step 9: Submit ───────────────────────────────────────────────────────────
step "Submitting transaction"

$CARDANO_CLI transaction submit \
  --socket-path "$SOCKET" \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-file "$DIR/reg.tx"
ok "Transaction submitted!"

TX_ID=$($CARDANO_CLI transaction txid --tx-file "$DIR/reg.tx")
echo
echo "$(bold "╔══════════════════════════════════════════╗")"
echo "$(bold "║            Pool registered!              ║")"
echo "$(bold "╚══════════════════════════════════════════╝")"
echo
info "Tx ID     : $TX_ID"
info "Explorer  : https://preview.cexplorer.io/tx/$TX_ID"
echo
echo "$(bold "Start producing blocks:")"
echo "  $(cyan "dion run --preview --spo-keys $DIR --external-addr ${RELAY_HOST}:${NODE_PORT}")"
echo
