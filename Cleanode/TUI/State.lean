/-!
# TUI Shared State

Central state structure written by network threads and read by the TUI renderer.
All updates go through `IO.Ref TUIState` using pure `modify` functions.
-/

namespace Cleanode.TUI.State

/-- A block summary for TUI display (compact, not full block data) -/
structure BlockSummary where
  blockNo   : Nat
  slot      : Nat
  hash      : String       -- Truncated hex (first 16 chars)
  txCount   : Nat
  size      : Nat           -- Block size in bytes
  totalFees : Nat           -- Total fees in lovelace
  era       : Nat
  peerAddr  : String        -- Which peer provided this block
  deriving Repr

/-- A peer summary for TUI display -/
structure PeerSummary where
  address      : String
  status       : String     -- "syncing", "connected", "reconnecting", "cooling"
  blocksSynced : Nat
  lastSeen     : Nat        -- Timestamp ms
  deriving Repr

/-- Central TUI state: written by network threads, read by renderer -/
structure TUIState where
  recentBlocks    : List BlockSummary   -- Last N blocks (newest first)
  mempoolTxCount  : Nat
  mempoolBytes    : Nat
  peers           : List PeerSummary
  tipBlockNo      : Nat
  tipSlot         : Nat
  blocksReceived  : Nat
  rollbacks       : Nat
  startedAt       : Nat                 -- Milliseconds since epoch, for uptime
  networkName     : String
  logs            : List String         -- Last M log lines for status bar
  maxRecentBlocks : Nat := 20
  maxLogs         : Nat := 5
  deriving Repr

/-- Create an empty TUI state -/
def TUIState.empty (networkName : String) (startedAt : Nat) : TUIState :=
  { recentBlocks := []
    mempoolTxCount := 0
    mempoolBytes := 0
    peers := []
    tipBlockNo := 0
    tipSlot := 0
    blocksReceived := 0
    rollbacks := 0
    startedAt := startedAt
    networkName := networkName
    logs := [] }

/-- Add a block to the recent blocks list (newest first, bounded, deduplicated) -/
def TUIState.addBlock (s : TUIState) (b : BlockSummary) : TUIState :=
  -- Skip duplicate blocks (multiple peers may sync the same block)
  if s.recentBlocks.any (fun x => x.blockNo == b.blockNo) then
    { s with tipBlockNo := max s.tipBlockNo b.blockNo
             tipSlot := max s.tipSlot b.slot }
  else
    let blocks := (b :: s.recentBlocks).take s.maxRecentBlocks
    { s with
      recentBlocks := blocks
      tipBlockNo := b.blockNo
      tipSlot := b.slot
      blocksReceived := s.blocksReceived + 1 }

/-- Update or add a peer entry -/
def TUIState.updatePeer (s : TUIState) (addr : String) (status : String) : TUIState :=
  let found := s.peers.any (fun p => p.address == addr)
  if found then
    { s with peers := s.peers.map fun p =>
        if p.address == addr then { p with status := status } else p }
  else
    { s with peers := s.peers ++ [{ address := addr, status := status, blocksSynced := 0, lastSeen := 0 }] }

/-- Increment a peer's synced block count -/
def TUIState.peerSyncedBlock (s : TUIState) (addr : String) : TUIState :=
  { s with peers := s.peers.map fun p =>
      if p.address == addr then { p with blocksSynced := p.blocksSynced + 1 } else p }

/-- Remove a peer from the list -/
def TUIState.removePeer (s : TUIState) (addr : String) : TUIState :=
  { s with peers := s.peers.filter (fun p => p.address != addr) }

/-- Record a rollback -/
def TUIState.addRollback (s : TUIState) : TUIState :=
  { s with rollbacks := s.rollbacks + 1 }

/-- Add a log line (bounded by maxLogs) -/
def TUIState.addLog (s : TUIState) (msg : String) : TUIState :=
  let logs := (s.logs ++ [msg]).reverse.take s.maxLogs |>.reverse
  { s with logs := logs }

/-- Update mempool stats from current mempool state -/
def TUIState.updateMempool (s : TUIState) (txCount bytes : Nat) : TUIState :=
  { s with mempoolTxCount := txCount, mempoolBytes := bytes }

/-- Log helper: routes to TUI state or IO.println depending on mode -/
def tuiLog (tuiRef : Option (IO.Ref TUIState)) (msg : String) : IO Unit :=
  match tuiRef with
  | some ref => ref.modify (·.addLog msg)
  | none => IO.println msg

end Cleanode.TUI.State
