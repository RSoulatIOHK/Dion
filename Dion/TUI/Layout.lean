import Dion.TUI.Ansi
import Dion.TUI.State
import Dion.TUI.Art

/-!
# TUI Layout

Pure functions that render each panel as a list of fixed-width strings.
All rendering is pure — no IO, no Pigment monad. ANSI codes are inlined
directly into the strings for color.
-/

namespace Dion.TUI.Layout

open Dion.TUI.Ansi
open Dion.TUI.State
open Dion.TUI.Art

-- ========================
-- = Box Drawing Helpers  =
-- ========================

/-- Horizontal line of a given character repeated `n` times -/
def hline (ch : Char) (n : Nat) : String :=
  String.mk (List.replicate n ch)

/-- Top border: ╔═══╗ -/
def topBorder (width : Nat) : String :=
  s!"{Ansi.blue}╔{hline '═' (width - 2)}╗{Ansi.reset}"

/-- Bottom border: ╚═══╝ -/
def bottomBorder (width : Nat) : String :=
  s!"{Ansi.blue}╚{hline '═' (width - 2)}╝{Ansi.reset}"

/-- Mid separator full width: ╠═══╣ -/
def midSeparator (width : Nat) : String :=
  s!"{Ansi.blue}╠{hline '═' (width - 2)}╣{Ansi.reset}"

/-- Mid separator with T-junction: ╠═══╦═══╣ -/
def midSeparatorT (leftWidth rightWidth : Nat) : String :=
  s!"{Ansi.blue}╠{hline '═' (leftWidth - 1)}╦{hline '═' (rightWidth - 2)}╣{Ansi.reset}"

/-- Mid separator with inverted T: ╠═══╩═══╣ -/
def midSeparatorInvT (leftWidth rightWidth : Nat) : String :=
  s!"{Ansi.blue}╠{hline '═' (leftWidth - 1)}╩{hline '═' (rightWidth - 2)}╣{Ansi.reset}"

/-- Mid separator with 3-column top T: ╠═══╦═══╦═══╣ -/
def midSeparatorTriT (lw mw rw : Nat) : String :=
  s!"{Ansi.blue}╠{hline '═' (lw - 1)}╦{hline '═' (mw - 1)}╦{hline '═' (rw - 2)}╣{Ansi.reset}"

/-- Mid separator with 3-column bottom T: ╠═══╩═══╩═══╣ -/
def midSeparatorTriInvT (lw mw rw : Nat) : String :=
  s!"{Ansi.blue}╠{hline '═' (lw - 1)}╩{hline '═' (mw - 1)}╩{hline '═' (rw - 2)}╣{Ansi.reset}"

/-- Mid separator: left two columns merge, right column continues with content: ╠═══╩═══╣ content ║ -/
def midSepMergeLeftWithContent (lw mw : Nat) (rightContent : String) (rw : Nat) : String :=
  let ri := padRight rightContent (rw - 3)
  s!"{Ansi.blue}╠{hline '═' (lw - 1)}╩{hline '═' (mw - 1)}╣{Ansi.reset} {ri}{Ansi.blue}║{Ansi.reset}"

/-- Bottom separator: full-width left + right column end: ╠═══════╩═══╣ -/
def midSepEndRight (leftTotal rw : Nat) : String :=
  s!"{Ansi.blue}╠{hline '═' (leftTotal - 1)}╩{hline '═' (rw - 2)}╣{Ansi.reset}"

/-- Two-column line where left is wide (merged): ║ left ║ right ║ -/
def splitLineWide (left : String) (right : String) (leftTotal rightWidth : Nat) : String :=
  let leftInner := padRight left (leftTotal - 2)
  let rightInner := padRight right (rightWidth - 3)
  s!"{Ansi.blue}║{Ansi.reset} {leftInner}{Ansi.blue}║{Ansi.reset} {rightInner}{Ansi.blue}║{Ansi.reset}"

/-- Left-bordered line: ║ content ║ -/
def boxLine (content : String) (width : Nat) : String :=
  let inner := padRight content (width - 4)
  s!"{Ansi.blue}║{Ansi.reset} {inner} {Ansi.blue}║{Ansi.reset}"

/-- Two-column line: ║ left ║ right ║ -/
def splitLine (left : String) (right : String) (leftWidth rightWidth : Nat) : String :=
  let leftInner := padRight left (leftWidth - 2)
  let rightInner := padRight right (rightWidth - 3)
  s!"{Ansi.blue}║{Ansi.reset} {leftInner}{Ansi.blue}║{Ansi.reset} {rightInner}{Ansi.blue}║{Ansi.reset}"

/-- Three-column line: ║ left ║ mid ║ right ║ -/
def triLine (left : String) (mid : String) (right : String) (lw mw rw : Nat) : String :=
  let li := padRight left (lw - 2)
  let mi := padRight mid (mw - 2)
  let ri := padRight right (rw - 3)
  s!"{Ansi.blue}║{Ansi.reset} {li}{Ansi.blue}║{Ansi.reset} {mi}{Ansi.blue}║{Ansi.reset} {ri}{Ansi.blue}║{Ansi.reset}"

-- ========================
-- = Header               =
-- ========================

/-- Format uptime from milliseconds -/
def formatUptime (nowMs startMs : Nat) : String :=
  let totalSec := (nowMs - startMs) / 1000
  let hours := totalSec / 3600
  let minutes := (totalSec % 3600) / 60
  let seconds := totalSec % 60
  if hours > 0 then s!"{hours}h {minutes}m"
  else if minutes > 0 then s!"{minutes}m {seconds}s"
  else s!"{seconds}s"

/-- Format a number with comma separators for readability -/
def formatNum (n : Nat) : String :=
  let s := toString n
  if s.length <= 3 then s
  else
    let chars := s.toList.reverse
    let grouped := go chars 0
    String.mk grouped.reverse
where
  go : List Char → Nat → List Char
    | [], _ => []
    | c :: cs, i =>
      if i > 0 && i % 3 == 0 then
        ',' :: c :: go cs (i + 1)
      else
        c :: go cs (i + 1)

/-- Render the header section: logo + stats (6 lines) -/
def renderHeader (state : TUIState) (width : Nat) (nowMs : Nat) : List String :=
  let uptime := formatUptime nowMs state.startedAt
  let logoLines := renderLogo
  let syncOriginLabel : String := match (state.syncOrigin : SyncOrigin) with
    | .genesis => s!"{Ansi.dim}Sync: {Ansi.reset}{Ansi.yellow}Genesis{Ansi.reset}"
    | .mithril epoch immFile createdAt digest =>
      let snapshot := s!"epoch {epoch}, immutable #{immFile}"
      let created := if createdAt.length > 0 then s!", {createdAt.take 10}" else ""
      let digestShort := if digest.length > 0 then s!"  {Ansi.dim}digest: {Ansi.reset}{Ansi.cyan}{digest.take 16}..{Ansi.reset}" else ""
      s!"{Ansi.dim}Sync: {Ansi.reset}{Ansi.brightGreen}Mithril{Ansi.reset}{Ansi.dim} ({snapshot}{created}){Ansi.reset}{digestShort}"
  let infoLines := [
    subtitle,
    s!"{Ansi.white}  Network: {Ansi.brightCyan}{state.networkName}{Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Tip: {Ansi.brightYellow}#{formatNum state.tipBlockNo}{Ansi.reset}" ++
      s!"{Ansi.dim}  Slot: {Ansi.reset}{Ansi.cyan}{formatNum state.tipSlot}{Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Uptime: {Ansi.green}{uptime}{Ansi.reset}",
    let unknown := state.blocksReceived - state.blocksFullyValid - state.blocksWithFailures
    s!"{Ansi.white}  Blocks synced: {Ansi.brightYellow}{formatNum state.blocksReceived}{Ansi.reset}" ++
      s!"{Ansi.dim}  ({Ansi.reset}{Ansi.brightGreen}{state.blocksFullyValid} valid{Ansi.reset}" ++
      (if unknown > 0 then s!" {Ansi.dim}{unknown} unknown{Ansi.reset}" else "") ++
      (if state.blocksWithFailures > 0 then s!" {Ansi.red}{state.blocksWithFailures} invalid{Ansi.reset}" else "") ++
      s!"{Ansi.dim}){Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Peers: {Ansi.brightGreen}{state.peers.length}{Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Rollbacks: {Ansi.yellow}{state.rollbacks}{Ansi.reset}",
    -- Leadership schedule: show next scheduled slot and last forged
    let c := state.consensus
    let leaderInfo :=
      if c.leaderSlots.isEmpty && c.lastForgedSlot.isNone then ""
      else
        let nextSlot := c.leaderSlots.find? (· > state.tipSlot)
        let nextStr := match nextSlot with
          | some s => s!"{Ansi.brightYellow}{Ansi.bold}next: slot {formatNum s}{Ansi.reset}"
          | none   => if c.leaderSlots.isEmpty then "" else s!"{Ansi.dim}next: (none this epoch){Ansi.reset}"
        let lastStr := match c.lastForgedSlot with
          | some s => s!"{Ansi.dim}  (last: {formatNum s}){Ansi.reset}"
          | none   => ""
        if nextStr.isEmpty then lastStr
        else s!"  {Ansi.dim}★{Ansi.reset}  {nextStr}{lastStr}"
    s!"{Ansi.white}  {syncOriginLabel}{leaderInfo}"
  ]
  -- Interleave logo and info: logo on left, info on right
  let headerLines := List.range 5 |>.map fun i =>
    let logo := (logoLines[i]?).getD ""
    let info := (infoLines[i]?).getD ""
    -- Logo is ~23 chars wide; pad to 26 then append info
    let logoPadded := padRight logo 30  -- Note: ANSI codes inflate length, use raw padding
    boxLine (s!"{logoPadded}  {info}") width
  headerLines

/-- Format lovelace as ADA with 3 decimal places (e.g., 1234567 → "1.234") -/
def formatAda (lovelace : Nat) : String :=
  let whole := lovelace / 1000000
  let frac := (lovelace % 1000000) / 1000  -- 3 decimal places
  let fracStr := if frac < 10 then s!"00{frac}" else if frac < 100 then s!"0{frac}" else s!"{frac}"
  s!"{formatNum whole}.{fracStr}"

-- ========================
-- = Blocks Panel         =
-- ========================

/-- Render a single block row, with optional selection highlight -/
def renderBlockRow (b : BlockSummary) (rowWidth : Nat) (selected : Bool := false) : String :=
  let hashShort := b.hash.take 12
  let txLabel := if b.txCount == 1 then "tx " else "txs"
  let feesAda := formatAda b.totalFees
  -- Validation indicator:
  --   Green V = fully validated (header + all txs pass)
  --   Red X   = header or tx validation failed
  --   Grey ~  = header OK but some txs skipped (UTxO inputs unknown)
  --   Dim .   = not validated at all
  let valIndicator :=
    if b.failedTxs > 0 then s!"{Ansi.red}X{Ansi.reset}{Ansi.dim}({b.failedTxs}){Ansi.reset}"
    else if b.skippedTxs > 0 then s!"{Ansi.dim}~{Ansi.reset}"
    else if b.headerValidated || b.validTxs > 0 then s!"{Ansi.green}V{Ansi.reset}"
    else s!"{Ansi.dim}.{Ansi.reset}"
  -- Our forged blocks get a gold star prefix, others get the selection cursor
  let ourStar := if b.isOurs then s!"{Ansi.brightYellow}{Ansi.bold}★{Ansi.reset}" else if selected then s!"{Ansi.brightCyan}>{Ansi.reset}" else s!" "
  let content := s!" {ourStar}{valIndicator}" ++
    s!" {if b.isOurs then Ansi.brightYellow else Ansi.brightYellow}#{formatNum b.blockNo}{Ansi.reset}" ++
    s!"  slot {if b.isOurs then Ansi.brightYellow else Ansi.cyan}{formatNum b.slot}{Ansi.reset}" ++
    s!"  {b.txCount} {txLabel}" ++
    s!"  {Ansi.green}{feesAda} A{Ansi.reset}" ++
    (if b.isOurs then s!"{Ansi.brightYellow}{Ansi.bold}  ◄ OUR BLOCK{Ansi.reset}" else s!"{Ansi.dim}  {hashShort}..{Ansi.reset}")
  if selected then
    padRight (s!"{csi}7m" ++ content ++ s!"{csi}27m") rowWidth  -- reverse video
  else if b.isOurs then
    padRight (s!"{Ansi.brightYellow}" ++ content ++ s!"{Ansi.reset}") rowWidth
  else
    padRight content rowWidth

/-- Render the blocks panel (left side) -/
def renderBlockPanel (blocks : List BlockSummary) (width : Nat) (height : Nat)
    (selectedIdx : Nat := 0) (paused : Bool := false) : List String :=
  let pauseTag := if paused then s!"{Ansi.yellow}{Ansi.bold} PAUSED{Ansi.reset}" else ""
  let title := s!"{Ansi.brightCyan}{Ansi.bold}  RECENT BLOCKS{Ansi.reset}{pauseTag}"
  let divider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let visible := blocks.take (height - 2)
  let indices := List.range visible.length
  let blockRows := (indices.zip visible).map fun (i, b) => renderBlockRow b (width - 4) (i == selectedIdx)
  -- Pad to fill height
  let remaining := height - 2 - blockRows.length
  let emptyRows := if blocks.isEmpty then
    [s!"{Ansi.dim}  (waiting for blocks...){Ansi.reset}"] ++ List.replicate (remaining - 1) ""
  else
    List.replicate remaining ""
  [title, divider] ++ blockRows ++ emptyRows.take remaining

-- ========================
-- = Mempool Panel        =
-- ========================

/-- Render the mempool + consensus panel (right side) -/
def renderMempoolPanel (state : TUIState) (width : Nat) (height : Nat) (nowMs : Nat := 0) : List String :=
  let title := s!"{Ansi.brightCyan}{Ansi.bold}  MEMPOOL{Ansi.reset}"
  let divider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let maxMB := 64
  let usedKB := state.mempoolBytes / 1024
  let statsLine := s!"{Ansi.white}  Transactions: {Ansi.brightYellow}{state.mempoolTxCount}{Ansi.reset}" ++
    s!"{Ansi.dim}    Bytes: {Ansi.reset}{Ansi.cyan}{usedKB} KB{Ansi.reset}" ++
    s!"{Ansi.dim} / {maxMB} MB{Ansi.reset}"
  -- Progress bar for mempool capacity
  let barWidth := width - 8
  let fillRatio := if maxMB * 1024 > 0 then (usedKB * barWidth) / (maxMB * 1024) else 0
  let filled := min fillRatio barWidth
  let barFull := String.mk (List.replicate filled '█')
  let barEmpty := String.mk (List.replicate (barWidth - filled) '░')
  let bar := s!"  {Ansi.green}{barFull}{Ansi.dim}{barEmpty}{Ansi.reset}"
  let emptyMsg := if state.mempoolTxCount == 0 then
    s!"{Ansi.dim}  (empty mempool — no txs to relay){Ansi.reset}"
  else ""
  -- Consensus section
  let c : ConsensusInfo := state.consensus
  let consTitle := s!"{Ansi.brightCyan}{Ansi.bold}  CONSENSUS{Ansi.reset}"
  let consDivider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let epochLine := s!"{Ansi.white}  Epoch: {Ansi.brightYellow}{c.currentEpoch}{Ansi.reset}" ++
    s!"{Ansi.dim}  KES Period: {Ansi.reset}{Ansi.cyan}{c.currentKESPeriod}{Ansi.reset}"
  let vrfLine := s!"{Ansi.white}  VRF: {Ansi.brightGreen}{c.vrfValid}{Ansi.reset}{Ansi.dim} valid{Ansi.reset}" ++
    (if c.vrfInvalid > 0 then s!"{Ansi.dim} / {Ansi.reset}{Ansi.red}{c.vrfInvalid} invalid{Ansi.reset}" else "")
  let opCertLine := s!"{Ansi.white}  OpCert: {Ansi.brightGreen}{c.opCertValid}{Ansi.reset}{Ansi.dim} valid{Ansi.reset}" ++
    (if c.opCertInvalid > 0 then s!"{Ansi.dim} / {Ansi.reset}{Ansi.red}{c.opCertInvalid} invalid{Ansi.reset}" else "")
  let kesLine := s!"{Ansi.white}  KES: {Ansi.brightGreen}{c.kesValid}{Ansi.reset}{Ansi.dim} valid{Ansi.reset}" ++
    (if c.kesInvalid > 0 then s!"{Ansi.dim} / {Ansi.reset}{Ansi.red}{c.kesInvalid} invalid{Ansi.reset}" else "")
  let issuerLine := if c.lastIssuerVKey.length > 0 then
    s!"{Ansi.dim}  Issuer: {c.lastIssuerVKey}...{Ansi.reset}"
  else ""
  let nonceLine := if c.epochNonceHex.length > 0 then
    s!"{Ansi.dim}  Nonce: {Ansi.reset}{Ansi.cyan}{c.epochNonceHex}..{Ansi.reset}" ++
    (if c.evolvingNonceHex.length > 0 then s!"{Ansi.dim}  evolving: {Ansi.reset}{Ansi.cyan}{c.evolvingNonceHex}..{Ansi.reset}" else "")
  else ""
  -- Block production banner (shown when we forged or were elected recently)
  let forgeLines : List String :=
    if c.blocksForged == 0 && c.lastElectedSlot.isNone then []
    else
      let msSince := if c.lastElectedMs > 0 && nowMs > c.lastElectedMs then nowMs - c.lastElectedMs else 0
      let isHot := msSince < 10000  -- within 10 seconds
      let isFresh := msSince < 60000  -- within 1 minute
      let slotStr := match c.lastForgedSlot with
        | some s => toString s
        | none   => "?"
      let banner := if isHot then
        -- Flashing gold banner right after forging
        s!"  {Ansi.brightYellow}{Ansi.bold}╔{hline '═' (width - 8)}╗{Ansi.reset}"
      else
        s!"  {Ansi.yellow}{Ansi.dim}╔{hline '─' (width - 8)}╗{Ansi.reset}"
      let label := if isHot then
        s!"{Ansi.brightYellow}{Ansi.bold}  ★  BLOCK FORGED  slot {slotStr}  ★{Ansi.reset}"
      else
        s!"{Ansi.yellow}  ★ forged slot {slotStr}{Ansi.reset}"
      let footer := if isHot then
        s!"  {Ansi.brightYellow}{Ansi.bold}╚{hline '═' (width - 8)}╝{Ansi.reset}"
      else
        s!"  {Ansi.yellow}{Ansi.dim}╚{hline '─' (width - 8)}╝{Ansi.reset}"
      let statsStr := s!"{Ansi.dim}  elected {c.timesElected}× · forged {c.blocksForged} block(s){if isFresh then s!"  ·  {msSince / 1000}s ago" else ""}{Ansi.reset}"
      [banner, padRight label (width - 2), footer, statsStr]
  -- Ledger validation section
  let valTitle := s!"{Ansi.brightCyan}{Ansi.bold}  LEDGER VALIDATION{Ansi.reset}"
  let valDivider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let valBlocks := s!"{Ansi.white}  Blocks: {Ansi.brightGreen}{state.blocksFullyValid}{Ansi.reset}{Ansi.dim} valid{Ansi.reset}" ++
    (if state.blocksWithFailures > 0 then s!"{Ansi.dim} / {Ansi.reset}{Ansi.red}{state.blocksWithFailures} with failures{Ansi.reset}" else "")
  let valTxs := s!"{Ansi.white}  Txs: {Ansi.brightGreen}{state.totalTxsValidated}{Ansi.reset}{Ansi.dim} passed{Ansi.reset}" ++
    (if state.totalTxsFailed > 0 then s!"{Ansi.dim} / {Ansi.reset}{Ansi.red}{state.totalTxsFailed} failed{Ansi.reset}" else "")
  let valRate := if state.totalTxsValidated + state.totalTxsFailed > 0 then
    let total := state.totalTxsValidated + state.totalTxsFailed
    let pct := state.totalTxsValidated * 100 / total
    s!"{Ansi.dim}  Pass rate: {Ansi.reset}{if pct >= 99 then Ansi.brightGreen else if pct >= 90 then Ansi.yellow else Ansi.red}{pct}%{Ansi.reset}"
  else s!"{Ansi.dim}  (no txs validated yet){Ansi.reset}"
  let lines := [title, divider, statsLine, bar, emptyMsg, "",
                consTitle, consDivider, epochLine, vrfLine, opCertLine, kesLine, issuerLine, nonceLine, ""]
              ++ forgeLines
              ++ (if forgeLines.isEmpty then [] else [""])
              ++ [valTitle, valDivider, valBlocks, valTxs, valRate]
  -- Pad to fill height
  lines ++ List.replicate (max 0 (height - lines.length)) ""

-- ========================
-- = Peers Panel          =
-- ========================

/-- Status indicator dot -/
def statusDot (status : String) : String :=
  if status == "syncing" then s!"{Ansi.brightGreen}●{Ansi.reset}"
  else if status == "connected" then s!"{Ansi.green}●{Ansi.reset}"
  else if status == "reconnecting" then s!"{Ansi.yellow}○{Ansi.reset}"
  else if status == "cooling" then s!"{Ansi.blue}○{Ansi.reset}"
  else s!"{Ansi.dim}○{Ansi.reset}"

/-- Render a single peer entry, truncating address to fit -/
def renderPeerEntry (p : PeerSummary) (colWidth : Nat) : String :=
  let dot := statusDot p.status
  -- Reserve space: dot(1) + space(1) + addr + space(1) + status(12) + space(1) + "blk:"(4) + num(~5)
  let addrWidth := max 10 (colWidth - 26)
  let addrTrunc := if p.address.length > addrWidth then p.address.take (addrWidth - 2) ++ ".." else p.address
  let addr := padRight addrTrunc addrWidth
  let stat := padRight p.status 12
  let blocks := s!"{Ansi.dim}blk:{Ansi.reset}{Ansi.white}{p.blocksSynced}{Ansi.reset}"
  padRight (s!"{dot} {addr} {Ansi.dim}{stat}{Ansi.reset}{blocks}") colWidth

/-- Render the peers panel (full width, bottom) -/
def renderPeerPanel (peers : List PeerSummary) (width : Nat) (height : Nat) : List String :=
  let title := s!"{Ansi.brightCyan}{Ansi.bold}  PEERS ({peers.length} connected){Ansi.reset}"
  let divider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  -- Render peers in two columns
  -- Each row: "  " + entry(cw) + "│" + " " + entry(cw) = 4 + 2*cw, must fit in (width - 4)
  let colWidth := (width - 8) / 2
  let peerRows := go peers colWidth
  let lines := [title, divider] ++ peerRows
  -- Pad to fill height
  lines ++ List.replicate (max 0 (height - lines.length)) ""
where
  go : List PeerSummary → Nat → List String
    | [], _ => []
    | [p], cw => [s!"  {renderPeerEntry p cw}"]
    | p1 :: p2 :: rest, cw =>
        s!"  {renderPeerEntry p1 cw}{Ansi.dim}│{Ansi.reset} {renderPeerEntry p2 cw}" :: go rest cw

-- ========================
-- = Status Bar           =
-- ========================

/-- Render the status/log bar at the bottom -/
def renderStatusBar (state : TUIState) (width : Nat) : List String :=
  let helpLine := s!"{Ansi.dim}  [Space]=pause  [↑↓]=select  [Enter]=details  [Esc]=back  [c]=consensus  [q]=quit{Ansi.reset}"
  let logs := state.logs.reverse.take 2 |>.reverse
  let logLines := logs.map fun msg =>
    let content := s!"{Ansi.dim}  {msg}{Ansi.reset}"
    padRight content (width - 4)
  [padRight helpLine (width - 4)] ++ logLines

-- ========================
-- = Block Detail Panel   =
-- ========================

/-- The 30 ledger validation checks, in order -/
private def validationCheckNames : List String :=
  [ "Tx size", "Validity interval", "Input existence",
    "Double-spend", "Balance", "Min UTxO",
    "Withdrawals", "Min fee", "Ed25519 sigs",
    "Native scripts", "Plutus scripts", "Collateral", "ScriptDataHash",
    "Non-empty inputs", "Output size", "Network ID",
    "Collateral ADA-only", "Max collateral ins", "Total collateral",
    "Ref/input disjoint", "Metadata hash", "ExUnits budget",
    "Redeemers", "Datum presence", "Validation tag",
    "Script witnesses", "Cert deposits", "Pool params",
    "Delegatee reg", "Block-level" ]

/-- Classify which check an error string belongs to (by prefix match) -/
private def errorToCheckIdx (err : String) : Option Nat :=
  if err.startsWith "TxTooLarge" then some 0
  else if err.startsWith "ExpiredTx" || err.startsWith "TxNotYetValid" then some 1
  else if err.startsWith "InputNotFound" then some 2
  else if err.startsWith "DoubleSpend" then some 3
  else if err.startsWith "InsufficientFunds" then some 4
  else if err.startsWith "OutputTooSmall" then some 5
  else if err.startsWith "InvalidWithdrawal" then some 6
  else if err.startsWith "FeeBelowMinimum" then some 7
  else if err.startsWith "MissingSignature" then some 8
  else if err.startsWith "NativeScriptFailure" then some 9
  else if err.startsWith "ScriptFailure" then some 10
  else if err.startsWith "CollateralNotFound" || err.startsWith "CollateralIsScriptLocked"
       || err.startsWith "InsufficientCollateral" || err.startsWith "NoCollateral" then some 11
  else if err.startsWith "InvalidScriptDataHash" then some 12
  else if err.startsWith "InputSetEmpty" then some 13
  else if err.startsWith "OutputTooBig" then some 14
  else if err.startsWith "WrongNetwork" then some 15
  else if err.startsWith "CollateralContainsNonADA" then some 16
  else if err.startsWith "TooManyCollateralInputs" then some 17
  else if err.startsWith "IncorrectTotalCollateral" then some 18
  else if err.startsWith "NonDisjointRefInputs" then some 19
  else if err.startsWith "MetadataHashMismatch" then some 20
  else if err.startsWith "ExUnitsTooBig" then some 21
  else if err.startsWith "ExtraRedeemer" || err.startsWith "MissingRedeemer" then some 22
  else if err.startsWith "UnspendableUTxONoDatum" || err.startsWith "MissingRequiredDatum" then some 23
  else if err.startsWith "ValidationTagMismatch" then some 24
  else if err.startsWith "ExtraneousScriptWitness" || err.startsWith "MissingRequiredScript" then some 25
  else if err.startsWith "CertDepositMismatch" || err.startsWith "StakeDeregNonZeroReward" then some 26
  else if err.startsWith "PoolCostTooLow" then some 27
  else if err.startsWith "DelegateeNotRegistered" then some 28
  else if err.startsWith "WrongBlockBodySize" || err.startsWith "TooManyBlockExUnits"
       || err.startsWith "RefScriptsSizeTooBig" || err.startsWith "HeaderProtVerTooHigh" then some 29
  else if err.startsWith "Phase2CollateralInsufficient" then some 11
  else none

/-- Render full-width block detail with validation checklist -/
def renderBlockDetailFull (block : BlockSummary) (width : Nat) (height : Nat) : List String :=
  let title := s!"{Ansi.brightCyan}{Ansi.bold}  BLOCK #{formatNum block.blockNo} DETAIL{Ansi.reset}{Ansi.dim}  [Esc=back]{Ansi.reset}"
  let divider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let eraName := match block.era with
    | 0 => "Byron" | 1 => "Shelley" | 2 => "Allegra" | 3 => "Mary"
    | 4 => "Alonzo" | 5 => "Babbage" | 6 => "Conway" | _ => s!"Era {block.era}"
  let feesAda := formatAda block.totalFees
  let infoLine := s!"{Ansi.white}  Slot: {Ansi.cyan}{formatNum block.slot}{Ansi.reset}" ++
    s!"  {Ansi.white}Era: {Ansi.yellow}{eraName}{Ansi.reset}" ++
    s!"  {Ansi.white}Size: {Ansi.reset}{block.size}B" ++
    s!"  {Ansi.white}Fees: {Ansi.green}{feesAda} ADA{Ansi.reset}" ++
    s!"  {Ansi.dim}Hash: {block.hash}{Ansi.reset}"
  let peerLine := s!"{Ansi.dim}  From: {block.peerAddr}{Ansi.reset}" ++
    s!"  {Ansi.white}Txs: {Ansi.brightYellow}{block.txCount}{Ansi.reset}"
  -- Header validation summary
  let headerLines :=
    if block.headerValidated then
      let vrfSym := if block.vrfOk then s!"{Ansi.green}V{Ansi.reset}" else s!"{Ansi.red}X{Ansi.reset}"
      let kesSym := if block.kesOk then s!"{Ansi.green}V{Ansi.reset}" else s!"{Ansi.red}X{Ansi.reset}"
      let opcSym := if block.opCertOk then s!"{Ansi.green}V{Ansi.reset}" else s!"{Ansi.red}X{Ansi.reset}"
      [s!"  {vrfSym} VRF proof   {kesSym} KES signature   {opcSym} OpCert"]
    else
      [s!"{Ansi.dim}  . Header not validated{Ansi.reset}"]
  -- Transaction validation summary line
  let valSummary :=
    if block.txCount == 0 then
      if block.headerValidated then
        s!"{Ansi.brightGreen}  Empty block — header fully validated{Ansi.reset}"
      else
        s!"{Ansi.dim}  Empty block — not validated{Ansi.reset}"
    else if block.failedTxs > 0 then
      s!"  {Ansi.green}{block.validTxs} passed{Ansi.reset}  {Ansi.red}{block.failedTxs} failed{Ansi.reset}" ++
        (if block.skippedTxs > 0 then s!"  {Ansi.dim}{block.skippedTxs} skipped{Ansi.reset}" else "")
    else if block.skippedTxs > 0 then
      if block.validTxs > 0 then
        s!"  {Ansi.green}{block.validTxs} passed{Ansi.reset}  {Ansi.dim}{block.skippedTxs} skipped (inputs unknown){Ansi.reset}"
      else
        s!"{Ansi.dim}  {block.skippedTxs} tx(s) skipped — UTxO inputs unknown during sync{Ansi.reset}"
    else if block.validTxs > 0 then
      s!"{Ansi.brightGreen}  All {block.validTxs} transactions passed all 30 validation rules{Ansi.reset}"
    else
      s!"{Ansi.dim}  (not validated){Ansi.reset}"
  -- Build the checklist: 2-column layout for 30 checks
  -- Collect which checks failed (from error strings)
  let failedChecks : List Nat := block.validationErrors.filterMap errorToCheckIdx
  let checklistTitle := s!"{Ansi.brightCyan}{Ansi.bold}  VALIDATION CHECKLIST{Ansi.reset}"
  let checklistDiv := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let mkCheckCell (i : Nat) (name : String) : String :=
    let num := if i + 1 < 10 then s!" {i + 1}" else s!"{i + 1}"
    let failCount := failedChecks.filter (· == i) |>.length
    let pad := String.mk (List.replicate (26 - name.length) ' ')
    if failCount > 0 then
      s!"{Ansi.red}X  {num}. {name}{pad}({failCount}){Ansi.reset}"
    else
      s!"{Ansi.green}V  {num}. {name}{pad}{Ansi.reset}"
  -- Split into two columns (13 left, 12 right)
  let colSize := (validationCheckNames.length + 1) / 2
  let checklistRows := (List.range colSize).map fun row =>
    let left := match validationCheckNames[row]? with
      | some name => mkCheckCell row name
      | none => ""
    let rightIdx := row + colSize
    let right := match validationCheckNames[rightIdx]? with
      | some name => mkCheckCell rightIdx name
      | none => ""
    s!"  {left}  {right}"
  -- Error details section
  let errTitle := if block.validationErrors.length > 0 then
    s!"{Ansi.brightCyan}{Ansi.bold}  ERROR DETAILS ({block.validationErrors.length}){Ansi.reset}"
  else ""
  let errDiv := if block.validationErrors.length > 0 then
    s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  else ""
  let maxErrors := max 0 (height - 22)  -- space remaining after checklist
  let errLines := block.validationErrors.take maxErrors |>.map fun e =>
    let truncated := if e.length > (width - 10) then e.take (width - 13) ++ "..." else e
    s!"{Ansi.red}  !{Ansi.reset} {Ansi.dim}{truncated}{Ansi.reset}"
  let moreLine := if block.validationErrors.length > maxErrors then
    s!"{Ansi.dim}  ... and {block.validationErrors.length - maxErrors} more errors{Ansi.reset}"
  else ""
  let lines := [title, divider, infoLine, peerLine, ""] ++ headerLines ++ ["", valSummary, "",
                checklistTitle, checklistDiv] ++ checklistRows ++
               (if errTitle.length > 0 then ["", errTitle, errDiv] ++ errLines ++ [moreLine] else [])
  lines ++ List.replicate (max 0 (height - lines.length)) ""

-- ==============================
-- = Block Info Side Panel      =
-- ==============================

/-- Render selected block info in the right side panel (full height with 30 checks) -/
def renderBlockInfoPanel (block : BlockSummary) (width : Nat) (height : Nat) : List String :=
  let title := s!"{Ansi.brightCyan}{Ansi.bold}  BLOCK #{formatNum block.blockNo}{Ansi.reset}{Ansi.dim}  [Esc]{Ansi.reset}"
  let divider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let eraName := match block.era with
    | 0 => "Byron" | 1 => "Shelley" | 2 => "Allegra" | 3 => "Mary"
    | 4 => "Alonzo" | 5 => "Babbage" | 6 => "Conway" | _ => s!"Era {block.era}"
  let feesAda := formatAda block.totalFees
  let slotLine := s!"{Ansi.white}  Slot: {Ansi.cyan}{formatNum block.slot}{Ansi.reset}  {Ansi.white}Era: {Ansi.yellow}{eraName}{Ansi.reset}"
  let sizeLine := s!"{Ansi.white}  Size: {Ansi.reset}{block.size}B  {Ansi.white}Fees: {Ansi.green}{feesAda} A{Ansi.reset}"
  let txLine := s!"{Ansi.white}  Txs: {Ansi.brightYellow}{block.txCount}{Ansi.reset}" ++
    (if block.validTxs > 0 then s!"  {Ansi.green}{block.validTxs} ok{Ansi.reset}" else "") ++
    (if block.failedTxs > 0 then s!"  {Ansi.red}{block.failedTxs} fail{Ansi.reset}" else "") ++
    (if block.skippedTxs > 0 then s!"  {Ansi.dim}{block.skippedTxs} skip{Ansi.reset}" else "")
  let hashLine := s!"{Ansi.dim}  {block.hash}..{Ansi.reset}"
  let peerLine := s!"{Ansi.dim}  {block.peerAddr}{Ansi.reset}"
  -- Header validation
  let hdrLines := if block.headerValidated then
    let vrfSym := if block.vrfOk then s!"{Ansi.green}V{Ansi.reset}" else s!"{Ansi.red}X{Ansi.reset}"
    let kesSym := if block.kesOk then s!"{Ansi.green}V{Ansi.reset}" else s!"{Ansi.red}X{Ansi.reset}"
    let opcSym := if block.opCertOk then s!"{Ansi.green}V{Ansi.reset}" else s!"{Ansi.red}X{Ansi.reset}"
    [s!"  {vrfSym} VRF  {kesSym} KES  {opcSym} OpCert"]
  else
    [s!"{Ansi.dim}  . Header not validated{Ansi.reset}"]
  -- 30-check validation checklist (shared with full detail view)
  let checkNames := validationCheckNames
  let failedChecks : List Nat := block.validationErrors.filterMap errorToCheckIdx
  let checklistTitle := s!"{Ansi.brightCyan}{Ansi.bold}  CHECKS{Ansi.reset}"
  let checklistDiv := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  -- 2-column check layout: 15 rows × 2 = 30 slots for 30 checks
  -- Pad left-column names to fixed visual width so right column aligns
  let leftPadWidth := 17  -- len("Validity interval") = 17, longest left name
  let mkCell (i : Nat) (padTo : Nat) : String :=
    match checkNames[i]? with
    | none => String.mk (List.replicate (padTo + 5) ' ')
    | some name =>
      let failCount := failedChecks.filter (· == i) |>.length
      let num := if i + 1 < 10 then s!" {i + 1}" else s!"{i + 1}"
      let padding := if name.length < padTo then String.mk (List.replicate (padTo - name.length) ' ') else ""
      let namePad := name ++ padding
      if failCount > 0 then
        s!"{Ansi.red}X{num}.{namePad}{Ansi.reset}{Ansi.dim}({failCount}){Ansi.reset}"
      else if block.txCount == 0 && !block.headerValidated then
        s!"{Ansi.dim}.{num}.{namePad}{Ansi.reset}"
      else
        s!"{Ansi.green}V{num}.{namePad}{Ansi.reset}"
  let colSize := (checkNames.length + 1) / 2  -- 13
  let checkRows := List.range colSize |>.map fun row =>
    let left := mkCell row leftPadWidth
    let right := mkCell (row + colSize) 0
    s!" {left} {right}"
  -- Error details at the bottom if space allows
  let usedLines := 9 + hdrLines.length + 2 + checkRows.length
  let errSpace := max 0 (height - usedLines - 1)
  let errLines := if block.validationErrors.isEmpty || errSpace == 0 then []
  else
    let errs := block.validationErrors.take errSpace |>.map fun e =>
      let truncated := if e.length > (width - 8) then e.take (width - 11) ++ "..." else e
      s!"{Ansi.red}  !{Ansi.reset}{Ansi.dim} {truncated}{Ansi.reset}"
    let more := if block.validationErrors.length > errSpace then
      [s!"{Ansi.dim}  +{block.validationErrors.length - errSpace} more{Ansi.reset}"]
    else []
    [""] ++ errs ++ more
  let lines := [title, divider, slotLine, sizeLine, txLine, hashLine, peerLine, ""] ++
               hdrLines ++ ["", checklistTitle, checklistDiv] ++ checkRows ++ errLines
  lines ++ List.replicate (max 0 (height - lines.length)) ""

-- ==============================
-- = Consensus Detail Panel     =
-- ==============================

/-- Render expanded consensus detail (full sidepanel) -/
def renderConsensusDetail (state : TUIState) (width : Nat) (height : Nat) : List String :=
  let c := state.consensus
  let title := s!"{Ansi.brightCyan}{Ansi.bold}  CONSENSUS DETAIL{Ansi.reset}{Ansi.dim}  [Tab/Esc=back]{Ansi.reset}"
  let divider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let epochLine := s!"{Ansi.white}  Epoch: {Ansi.brightYellow}{c.currentEpoch}{Ansi.reset}"
  let kesLine := s!"{Ansi.white}  KES Period: {Ansi.cyan}{c.currentKESPeriod}{Ansi.reset}"
  let headersLine := s!"{Ansi.white}  Headers validated: {Ansi.brightYellow}{c.validatedHeaders}{Ansi.reset}"
  let sec1 := s!"{Ansi.brightCyan}{Ansi.bold}  VRF{Ansi.reset}"
  let vrfLine := s!"{Ansi.brightGreen}    {c.vrfValid} valid{Ansi.reset}" ++
    (if c.vrfInvalid > 0 then s!"  {Ansi.red}{c.vrfInvalid} invalid{Ansi.reset}" else "")
  let sec2 := s!"{Ansi.brightCyan}{Ansi.bold}  OPERATIONAL CERTIFICATES{Ansi.reset}"
  let opLine := s!"{Ansi.brightGreen}    {c.opCertValid} valid{Ansi.reset}" ++
    (if c.opCertInvalid > 0 then s!"  {Ansi.red}{c.opCertInvalid} invalid{Ansi.reset}" else "")
  let sec3 := s!"{Ansi.brightCyan}{Ansi.bold}  KES SIGNATURES{Ansi.reset}"
  let kesValLine := s!"{Ansi.brightGreen}    {c.kesValid} valid{Ansi.reset}" ++
    (if c.kesInvalid > 0 then s!"  {Ansi.red}{c.kesInvalid} invalid{Ansi.reset}" else "")
  let sec4 := s!"{Ansi.brightCyan}{Ansi.bold}  NONCES{Ansi.reset}"
  let epochNonce := if c.epochNonceHex.length > 0 then
    s!"{Ansi.white}    Epoch:    {Ansi.yellow}{c.epochNonceHex}..{Ansi.reset}"
  else s!"{Ansi.dim}    Epoch:    (not yet computed){Ansi.reset}"
  let evolvNonce := if c.evolvingNonceHex.length > 0 then
    s!"{Ansi.white}    Evolving: {Ansi.cyan}{c.evolvingNonceHex}..{Ansi.reset}"
  else s!"{Ansi.dim}    Evolving: (empty){Ansi.reset}"
  let issuer := if c.lastIssuerVKey.length > 0 then
    s!"{Ansi.dim}  Last issuer: {c.lastIssuerVKey}...{Ansi.reset}"
  else ""
  -- Block production section
  let sec5 := s!"{Ansi.brightYellow}{Ansi.bold}  BLOCK PRODUCTION{Ansi.reset}"
  let electedLine := s!"{Ansi.white}  Times elected: {Ansi.brightYellow}{c.timesElected}{Ansi.reset}"
  let forgedLine  := s!"{Ansi.white}  Blocks forged: {Ansi.brightGreen}{c.blocksForged}{Ansi.reset}"
  let lastSlotLine := match c.lastForgedSlot with
    | some s => s!"{Ansi.white}  Last forged:   {Ansi.brightYellow}slot {s}{Ansi.reset}"
    | none   => s!"{Ansi.dim}  Last forged:   (none yet){Ansi.reset}"
  let prodLines := if c.timesElected > 0 || c.blocksForged > 0 then
    ["", sec5, electedLine, forgedLine, lastSlotLine]
  else
    ["", sec5, s!"{Ansi.dim}  No slots won yet{Ansi.reset}"]
  let lines := [title, divider, epochLine, kesLine, headersLine, "",
                sec1, vrfLine, "", sec2, opLine, "", sec3, kesValLine, "",
                sec4, epochNonce, evolvNonce, "", issuer]
              ++ prodLines
  lines ++ List.replicate (max 0 (height - lines.length)) ""

-- ========================
-- = Full Frame Compose   =
-- ========================

/-- Compose the full TUI frame as a single string.
    Layout:
    - Header (5 rows)
    - Separator
    - Blocks (left) | Mempool (right) — 14 rows
    - Separator
    - Peers — 8 rows
    - Separator
    - Status bar — 3 rows
    - Bottom border -/
def renderFrame (state : TUIState) (nowMs : Nat) (width : Nat := 160) (_height : Nat := 42) : String :=
  let leftWidth := width / 2
  let rightWidth := width - leftWidth
  let panelHeight := 14
  let peerHeight := 10

  -- Determine if we need 3-column layout (block detail open)
  let showBlockInfo := match state.detailView with
    | .blockDetail | .txDetail _ => state.selectedBlock.isSome
    | _ => false

  let allLines := if showBlockInfo then
    -- Extended layout: normal 2-col (unchanged) + extra block info panel on the right
    let col3w := 55  -- extra panel width for block info
    let totalW := width + col3w  -- wider frame when detail is open
    let fullHeight := panelHeight + 1 + peerHeight  -- right column spans full height
    -- Left two panels at normal sizes
    let blockLines := renderBlockPanel state.recentBlocks leftWidth panelHeight
        state.selectedBlockIdx state.paused
    let midLines := match state.detailView with
      | .consensusFull => renderConsensusDetail state rightWidth panelHeight
      | _ => renderMempoolPanel state rightWidth panelHeight nowMs
    let rightLines := match state.selectedBlock with
      | some block => renderBlockInfoPanel block col3w fullHeight
      | none => List.replicate fullHeight ""
    -- Top section: 3 columns (blocks | mempool | block info)
    let topRows := List.range panelHeight |>.map fun i =>
      let l := (blockLines[i]?).getD ""
      let m := (midLines[i]?).getD ""
      let r := (rightLines[i]?).getD ""
      triLine l m r leftWidth rightWidth col3w
    -- Mid separator: left two merge, right continues with content
    let midSepContent := (rightLines[panelHeight]?).getD ""
    let midSep := midSepMergeLeftWithContent leftWidth rightWidth midSepContent col3w
    -- Bottom section: peers (full normal width) | block info continues
    let peerLines := renderPeerPanel state.peers width peerHeight
    let bottomRows := List.range peerHeight |>.map fun i =>
      let l := (peerLines[i]?).getD ""
      let r := (rightLines[panelHeight + 1 + i]?).getD ""
      splitLineWide l r width col3w
    -- Status bar at extended width
    let statusLines := renderStatusBar state totalW
    let statusRows := statusLines.map fun line => boxLine line totalW
    [topBorder totalW] ++ (renderHeader state totalW nowMs)
      ++ [midSeparatorTriT leftWidth rightWidth col3w] ++ topRows
      ++ [midSep] ++ bottomRows
      ++ [midSeparator totalW]
      ++ statusRows
      ++ [bottomBorder totalW]
  else
    -- Normal two-column layout: Blocks | Mempool+Consensus
    let blockLines := renderBlockPanel state.recentBlocks leftWidth panelHeight
        state.selectedBlockIdx state.paused
    let rightLines := match state.detailView with
      | .consensusFull => renderConsensusDetail state rightWidth panelHeight
      | _ => renderMempoolPanel state rightWidth panelHeight nowMs
    let panelRows := List.range panelHeight |>.map fun i =>
      let left := (blockLines[i]?).getD ""
      let right := (rightLines[i]?).getD ""
      splitLine left right leftWidth rightWidth
    let peerLines := renderPeerPanel state.peers width peerHeight
    let peerRows := peerLines.map fun line => boxLine line width
    -- Status bar at normal width
    let statusLines := renderStatusBar state width
    let statusRows := statusLines.map fun line => boxLine line width
    [topBorder width] ++ (renderHeader state width nowMs)
      ++ [midSeparatorT leftWidth rightWidth] ++ panelRows
      ++ [midSeparatorInvT leftWidth rightWidth] ++ peerRows
      ++ [midSeparator width]
      ++ statusRows
      ++ [bottomBorder width]

  -- Join all lines, each followed by clearLine to erase leftover chars
  String.intercalate "\n" (allLines.map fun l => l ++ Ansi.clearLine)

end Dion.TUI.Layout
