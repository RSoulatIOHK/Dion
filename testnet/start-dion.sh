#!/usr/bin/env bash
# start-dion.sh — Start Dion as a block producer against the local testnet.
#
# Run AFTER start-testnet.sh has registered the pool and printed the env file.
#
# Usage:
#   source ./testnet/local/env.sh
#   ./testnet/start-dion.sh
#
# Or without sourcing:
#   ./testnet/start-dion.sh  (will source env.sh automatically)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ENV_FILE="$REPO_ROOT/testnet/local/env.sh"

# Source env if available and vars not already set
if [ -z "${TESTNET_MAGIC:-}" ] && [ -f "$ENV_FILE" ]; then
  echo "[dion] Sourcing $ENV_FILE"
  source "$ENV_FILE"
fi

MAGIC="${TESTNET_MAGIC:-42}"
SYSTEM_START="${TESTNET_SYSTEM_START:?'Run start-testnet.sh first (TESTNET_SYSTEM_START not set)'}"
EPOCH_LENGTH="${TESTNET_EPOCH_LENGTH:-500}"
EPOCH_NONCE="${EPOCH_NONCE:-0000000000000000000000000000000000000000000000000000000000000000}"
SOCKET="${TESTNET_SOCKET:-$REPO_ROOT/testnet/local/dion.socket}"

# Find the Dion binary
DION="$REPO_ROOT/.build/release/dion"
if [ ! -f "$DION" ]; then
  DION="$(which dion 2>/dev/null || echo "")"
fi
if [ -z "$DION" ] || [ ! -f "$DION" ]; then
  echo "ERROR: dion binary not found. Build with: cd $REPO_ROOT && lake build"
  exit 1
fi

# Find the official testnet node socket for peer connection
# cardano-testnet puts the socket in state-node-spo1/
PEER_SOCKET="${CARDANO_NODE_SOCKET_PATH:-}"
TESTNET_DIR="$REPO_ROOT/testnet/local"
OFFICIAL_PORT=$(grep -r "\"port\"" "$TESTNET_DIR" 2>/dev/null | grep -v socket | head -1 | \
  python3 -c "import sys; lines=sys.stdin.read(); import re; m=re.search(r'\"port\"[: ]+(\d+)', lines); print(m.group(1) if m else '3001')" 2>/dev/null || echo "3001")

echo "=============================================="
echo "  Dion Block Producer"
echo "=============================================="
echo "  Binary      : $DION"
echo "  Magic       : $MAGIC"
echo "  System start: $SYSTEM_START"
echo "  Epoch length: $EPOCH_LENGTH slots"
echo "  Peer port   : $OFFICIAL_PORT"
echo "  Protocol    : Babbage (major=9)"
echo "  Keys        : $REPO_ROOT/keys"
echo "=============================================="
echo ""
echo "  Waiting for delegation to take effect (~2 epochs from pool registration)."
echo "  Once delegation is active the forge loop will start producing blocks."
echo ""

exec "$DION" run \
  --testnet-magic "$MAGIC" \
  --system-start "$SYSTEM_START" \
  --epoch-length "$EPOCH_LENGTH" \
  --spo-keys "$REPO_ROOT/keys" \
  --epoch-nonce "$EPOCH_NONCE" \
  --protocol-major 9 \
  --port 3002 \
  --socket-path "$SOCKET" \
  --peer "127.0.0.1:$OFFICIAL_PORT"
