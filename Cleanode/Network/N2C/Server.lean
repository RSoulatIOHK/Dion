import Cleanode.Network.Socket
import Cleanode.Network.Handshake
import Cleanode.Network.N2C.Mux
import Cleanode.Network.N2C.Handshake
import Cleanode.Network.N2C.LocalTxSubmission
import Cleanode.Network.N2C.LocalStateQuery
import Cleanode.Network.N2C.LocalTxMonitor
import Cleanode.Network.Mempool
import Cleanode.Ledger.State

/-!
# Node-to-Client Server

Listens on a Unix domain socket and handles incoming connections from
`cardano-cli` and other local clients. Each connection is handled in its
own async task.

## Usage
```
CARDANO_NODE_SOCKET_PATH=./cleanode.socket cardano-cli query tip --testnet-magic 1
```

## Architecture
- One listening task accepts connections on the Unix socket
- Per-connection task: handshake → MUX dispatch loop → protocol handlers
- Shared state (ledger, mempool) accessed via IO.Ref

## References
- Ouroboros Network Spec Section 3.17 (Node-to-Client)
-/

namespace Cleanode.Network.N2C.Server

open Cleanode.Network.Socket
open Cleanode.Network.Handshake (NetworkMagic)
open Cleanode.Network.N2C.Mux
open Cleanode.Network.N2C.MiniProtocolId
open Cleanode.Network.N2C.Handshake
open Cleanode.Network.N2C.LocalTxSubmission
open Cleanode.Network.N2C.LocalStateQuery
open Cleanode.Network.N2C.LocalTxMonitor
open Cleanode.Network.Mempool
open Cleanode.Ledger.State

-- ====================
-- = Connection State =
-- ====================

/-- Per-connection state for all N2C protocols -/
structure N2CConnectionState where
  lsqState : LSQState
  txMonitorSnapshot : Option MempoolSnapshot

def N2CConnectionState.initial : N2CConnectionState :=
  { lsqState := .Idle, txMonitorSnapshot := none }

-- ====================
-- = Connection Loop  =
-- ====================

/-- Handle a single N2C client connection.
    Runs handshake, then dispatches MUX frames to protocol handlers. -/
partial def handleN2CConnection (sock : Socket)
    (ledgerStateRef : IO.Ref LedgerState)
    (mempoolRef : IO.Ref Mempool)
    (network : NetworkMagic)
    (quiet : Bool := false)
    : IO Unit := do
  let log (msg : String) : IO Unit := if !quiet then IO.eprintln msg else pure ()
  -- Step 1: N2C Handshake
  match ← receiveAndRespondN2CHandshake sock network with
  | .error e =>
    log s!"N2C: Handshake socket error: {e}"
    socket_close sock
    return
  | .ok none =>
    log "N2C: Handshake failed (no common version)"
    socket_close sock
    return
  | .ok (some version) =>
    log s!"N2C: Client connected (version {version})"

    -- Step 2: MUX dispatch loop
    let mut connState := N2CConnectionState.initial
    let mut running := true

    while running do
      match ← receiveN2CMuxFrame sock with
      | .error e =>
        -- Client disconnected or error
        match e with
        | .ReceiveFailed _ => pure ()  -- Normal disconnect
        | _ => log s!"N2C: Socket error: {e}"
        running := false
      | .ok frame =>
        match frame.header.protocolId with
        | .LocalTxSubmission =>
          let cont ← handleTxSubmissionFrame sock frame.payload mempoolRef
          if !cont then running := false
        | .LocalStateQuery =>
          let (cont, newLsqState) ← handleStateQueryFrame sock frame.payload
            ledgerStateRef connState.lsqState network.toNat quiet
          connState := { connState with lsqState := newLsqState }
          if !cont then running := false
        | .LocalTxMonitor =>
          let (cont, newSnap) ← handleTxMonitorFrame sock frame.payload
            mempoolRef connState.txMonitorSnapshot ledgerStateRef
          connState := { connState with txMonitorSnapshot := newSnap }
          if !cont then running := false
        | .ChainSync =>
          log "N2C: LocalChainSync not yet supported"
          running := false
        | .Handshake =>
          log "N2C: Unexpected handshake frame"
          running := false

    log "N2C: Client disconnected"
    socket_close sock

-- ====================
-- = Server Loop      =
-- ====================

/-- Start the N2C server on a Unix domain socket.
    Accepts connections and spawns a handler task for each. -/
partial def n2cServerLoop (socketPath : String)
    (ledgerStateRef : IO.Ref LedgerState)
    (mempoolRef : IO.Ref Mempool)
    (network : NetworkMagic)
    (quiet : Bool := false)
    : IO Unit := do
  let log (msg : String) : IO Unit := if !quiet then IO.eprintln msg else pure ()
  log s!"[n2c] Creating Unix socket at {socketPath}..."

  match ← unix_listen socketPath with
  | .error e =>
    IO.eprintln s!"N2C: Failed to create Unix socket: {e}"
    return
  | .ok listenSock =>
    log "[n2c] Unix socket created, entering accept loop..."
    -- Accept loop
    let mut running := true
    while running do
      match ← unix_accept listenSock with
      | .error e =>
        IO.eprintln s!"N2C: Accept error: {e}"
        running := false
      | .ok clientSock => do
        -- Handle connection inline (not in IO.asTask to avoid external object capture)
        try
          handleN2CConnection clientSock ledgerStateRef mempoolRef network quiet
        catch e =>
          IO.eprintln s!"N2C: Handler error: {e}"

    -- Cleanup
    unix_close_and_unlink listenSock socketPath

end Cleanode.Network.N2C.Server
