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
  validTxs  : Nat := 0      -- Txs that passed our validation
  failedTxs : Nat := 0      -- Txs that failed our validation
  skippedTxs : Nat := 0     -- Txs skipped (inputs unknown)
  validationErrors : List String := []  -- Per-tx error messages for detail panel
  -- Per-block consensus header validation
  vrfOk     : Bool := false  -- VRF proof verified
  kesOk     : Bool := false  -- KES signature verified
  opCertOk  : Bool := false  -- Operational cert verified
  headerValidated : Bool := false  -- Header validation was attempted
  deriving Repr

/-- A peer summary for TUI display -/
structure PeerSummary where
  address      : String
  status       : String     -- "syncing", "connected", "reconnecting", "cooling"
  blocksSynced : Nat
  lastSeen     : Nat        -- Timestamp ms
  deriving Repr

/-- Consensus validation result for a block header -/
structure ConsensusInfo where
  validatedHeaders : Nat        -- Total headers validated so far
  vrfValid         : Nat        -- VRF proofs that verified correctly
  vrfInvalid       : Nat        -- VRF proofs that failed verification
  kesValid         : Nat        -- KES signatures that verified correctly
  kesInvalid       : Nat        -- KES signatures that failed verification
  opCertValid      : Nat        -- Operational certs that verified
  opCertInvalid    : Nat        -- Operational certs that failed
  currentEpoch     : Nat        -- Current epoch from latest block
  currentKESPeriod : Nat        -- KES period of latest block
  lastIssuerVKey   : String     -- Last block issuer VKey hash (hex, truncated)
  epochNonceHex    : String     -- Current epoch nonce (hex, truncated)
  evolvingNonceHex : String     -- Evolving nonce (hex, truncated)
  deriving Repr

/-- How the node started syncing -/
inductive SyncOrigin where
  | genesis                                    -- Started from genesis (full sync)
  | mithril (epoch : Nat) (immutableFile : Nat) (createdAt : String) (digest : String)  -- From Mithril snapshot
  deriving Repr

/-- Which panel currently has keyboard focus -/
inductive ActivePanel where
  | blocks       -- Arrow keys navigate the block list
  | consensus    -- Consensus detail expanded in sidepanel
  deriving Repr, BEq

/-- What detail view is shown in the right sidepanel -/
inductive DetailView where
  | none         -- Normal view (mempool + consensus summary)
  | blockDetail  -- Selected block's full info + tx list
  | txDetail (txIdx : Nat)  -- A specific transaction's inputs/outputs
  | consensusFull           -- All consensus info expanded
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
  consensus       : ConsensusInfo       -- Consensus validation stats
  -- Interactive state
  paused           : Bool := false      -- When true, block list stops auto-scrolling
  selectedBlockIdx : Nat := 0           -- Cursor in the block list (0 = newest)
  activePanel      : ActivePanel := .blocks
  detailView       : DetailView := .none
  maxRecentBlocks : Nat := 20
  maxLogs         : Nat := 5
  pendingBlocks   : List BlockSummary := []  -- Blocks received while paused (newest first)
  -- Ledger validation counters
  blocksFullyValid : Nat := 0    -- Blocks where all txs passed our rules
  blocksWithFailures : Nat := 0  -- Blocks where at least one tx failed
  totalTxsValidated : Nat := 0   -- Total txs validated across all blocks
  totalTxsFailed : Nat := 0      -- Total txs that failed validation
  syncOrigin : SyncOrigin := .genesis  -- How this sync session started
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
    logs := []
    consensus := {
      validatedHeaders := 0, vrfValid := 0, vrfInvalid := 0,
      kesValid := 0, kesInvalid := 0, opCertValid := 0, opCertInvalid := 0,
      currentEpoch := 0, currentKESPeriod := 0, lastIssuerVKey := "",
      epochNonceHex := "", evolvingNonceHex := ""
    } }

/-- Add a block to the recent blocks list (newest first, bounded, deduplicated) -/
def TUIState.addBlock (s : TUIState) (b : BlockSummary) : TUIState :=
  -- Skip duplicate blocks (multiple peers may sync the same block)
  if s.recentBlocks.any (fun x => x.blockNo == b.blockNo) ||
     s.pendingBlocks.any (fun x => x.blockNo == b.blockNo) then
    { s with tipBlockNo := max s.tipBlockNo b.blockNo
             tipSlot := max s.tipSlot b.slot }
  else if s.paused then
    -- While paused, buffer new blocks — don't touch the visible list
    { s with
      pendingBlocks := (b :: s.pendingBlocks).take s.maxRecentBlocks
      tipBlockNo := b.blockNo
      tipSlot := b.slot
      blocksReceived := s.blocksReceived + 1 }
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

/-- Update consensus validation stats -/
def TUIState.updateConsensus (s : TUIState) (f : ConsensusInfo → ConsensusInfo) : TUIState :=
  { s with consensus := f s.consensus }

/-- Update mempool stats from current mempool state -/
def TUIState.updateMempool (s : TUIState) (txCount bytes : Nat) : TUIState :=
  { s with mempoolTxCount := txCount, mempoolBytes := bytes }

-- ====================
-- = Interactive Nav  =
-- ====================

/-- Move selection up in the block list -/
def TUIState.selectUp (s : TUIState) : TUIState :=
  match s.detailView with
  | .txDetail idx => { s with detailView := .txDetail (if idx > 0 then idx - 1 else 0) }
  | _ =>
    if s.selectedBlockIdx > 0 then { s with selectedBlockIdx := s.selectedBlockIdx - 1 }
    else s

/-- Move selection down in the block list -/
def TUIState.selectDown (s : TUIState) : TUIState :=
  match s.detailView with
  | .txDetail idx => { s with detailView := .txDetail (idx + 1) }
  | _ =>
    let maxIdx := if s.recentBlocks.length > 0 then s.recentBlocks.length - 1 else 0
    if s.selectedBlockIdx < maxIdx then { s with selectedBlockIdx := s.selectedBlockIdx + 1 }
    else s

/-- Press Enter: open detail for selected block, or drill into tx -/
def TUIState.selectEnter (s : TUIState) : TUIState :=
  match s.detailView with
  | .none => { s with detailView := .blockDetail }
  | .blockDetail => { s with detailView := .txDetail 0 }
  | .txDetail _ => s  -- Already at deepest level
  | .consensusFull => s

/-- Press Escape: go back one level -/
def TUIState.selectBack (s : TUIState) : TUIState :=
  match s.detailView with
  | .txDetail _ => { s with detailView := .blockDetail }
  | .blockDetail => { s with detailView := .none }
  | .consensusFull => { s with detailView := .none, activePanel := .blocks }
  | .none => s

/-- Toggle pause mode. On unpause, flush pending blocks into the visible list. -/
def TUIState.togglePause (s : TUIState) : TUIState :=
  if s.paused then
    -- Unpausing: merge pending blocks into visible list
    let merged := (s.pendingBlocks ++ s.recentBlocks).take s.maxRecentBlocks
    { s with paused := false, recentBlocks := merged, pendingBlocks := [], selectedBlockIdx := 0 }
  else
    { s with paused := true }

/-- Toggle consensus detail panel -/
def TUIState.toggleConsensus (s : TUIState) : TUIState :=
  match s.detailView with
  | .consensusFull => { s with detailView := .none, activePanel := .blocks }
  | _ => { s with detailView := .consensusFull, activePanel := .consensus }

/-- Get the currently selected block (if any) -/
def TUIState.selectedBlock (s : TUIState) : Option BlockSummary :=
  s.recentBlocks[s.selectedBlockIdx]?

/-- Log helper: routes to TUI state or IO.println depending on mode -/
def tuiLog (tuiRef : Option (IO.Ref TUIState)) (msg : String) : IO Unit :=
  match tuiRef with
  | some ref => ref.modify (·.addLog msg)
  | none => IO.println msg

end Cleanode.TUI.State
