import Cleanode.TUI.Ansi
import Cleanode.TUI.State
import Cleanode.TUI.Art

/-!
# TUI Layout

Pure functions that render each panel as a list of fixed-width strings.
All rendering is pure — no IO, no Pigment monad. ANSI codes are inlined
directly into the strings for color.
-/

namespace Cleanode.TUI.Layout

open Cleanode.TUI.Ansi
open Cleanode.TUI.State
open Cleanode.TUI.Art

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

/-- Left-bordered line: ║ content ║ -/
def boxLine (content : String) (width : Nat) : String :=
  let inner := padRight content (width - 4)
  s!"{Ansi.blue}║{Ansi.reset} {inner} {Ansi.blue}║{Ansi.reset}"

/-- Two-column line: ║ left ║ right ║ -/
def splitLine (left : String) (right : String) (leftWidth rightWidth : Nat) : String :=
  let leftInner := padRight left (leftWidth - 2)
  let rightInner := padRight right (rightWidth - 3)
  s!"{Ansi.blue}║{Ansi.reset} {leftInner}{Ansi.blue}║{Ansi.reset} {rightInner}{Ansi.blue}║{Ansi.reset}"

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
  let infoLines := [
    subtitle,
    s!"{Ansi.white}  Network: {Ansi.brightCyan}{state.networkName}{Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Tip: {Ansi.brightYellow}#{formatNum state.tipBlockNo}{Ansi.reset}" ++
      s!"{Ansi.dim}  Slot: {Ansi.reset}{Ansi.cyan}{formatNum state.tipSlot}{Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Uptime: {Ansi.green}{uptime}{Ansi.reset}",
    s!"{Ansi.white}  Blocks synced: {Ansi.brightYellow}{formatNum state.blocksReceived}{Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Peers: {Ansi.brightGreen}{state.peers.length}{Ansi.reset}" ++
      s!"{Ansi.dim}  |  {Ansi.reset}{Ansi.white}Rollbacks: {Ansi.yellow}{state.rollbacks}{Ansi.reset}"
  ]
  -- Interleave logo and info: logo on left, info on right
  let headerLines := List.range 5 |>.map fun i =>
    let logo := (logoLines.get? i).getD ""
    let info := (infoLines.get? i).getD ""
    -- Logo is ~23 chars wide; pad to 26 then append info
    let logoPadded := padRight logo 30  -- Note: ANSI codes inflate length, use raw padding
    boxLine (s!"{logoPadded}  {info}") width
  headerLines

-- ========================
-- = Blocks Panel         =
-- ========================

/-- Render a single block row -/
def renderBlockRow (b : BlockSummary) (rowWidth : Nat) : String :=
  let hashShort := b.hash.take 12
  let txLabel := if b.txCount == 1 then "tx " else "txs"
  let feesAda := b.totalFees / 1000000
  let content := s!"{Ansi.brightYellow}#{formatNum b.blockNo}{Ansi.reset}" ++
    s!"{Ansi.dim}  slot {Ansi.reset}{Ansi.cyan}{formatNum b.slot}{Ansi.reset}" ++
    s!"{Ansi.dim}  {Ansi.reset}{Ansi.white}{b.txCount} {txLabel}{Ansi.reset}" ++
    s!"{Ansi.dim}  {Ansi.reset}{Ansi.green}{feesAda}₳{Ansi.reset}" ++
    s!"{Ansi.dim}  {hashShort}..{Ansi.reset}"
  padRight content rowWidth

/-- Render the blocks panel (left side) -/
def renderBlockPanel (blocks : List BlockSummary) (width : Nat) (height : Nat) : List String :=
  let title := s!"{Ansi.brightCyan}{Ansi.bold}  RECENT BLOCKS{Ansi.reset}"
  let divider := s!"{Ansi.dim}  {hline '─' (width - 6)}{Ansi.reset}"
  let blockRows := blocks.take (height - 2) |>.map fun b =>
    s!"  {renderBlockRow b (width - 4)}"
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
def renderMempoolPanel (state : TUIState) (width : Nat) (height : Nat) : List String :=
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
  let lines := [title, divider, statsLine, bar, emptyMsg, "",
                consTitle, consDivider, epochLine, vrfLine, opCertLine, kesLine, issuerLine]
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
  let logs := state.logs.reverse.take 3 |>.reverse
  logs.map fun msg =>
    let content := s!"{Ansi.dim}  {msg}{Ansi.reset}"
    padRight content (width - 4)

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
def renderFrame (state : TUIState) (nowMs : Nat) (width : Nat := 160) (height : Nat := 42) : String :=
  let leftWidth := width / 2
  let rightWidth := width - leftWidth
  let panelHeight := 14
  let peerHeight := 10

  -- Header
  let header := [topBorder width] ++ renderHeader state width nowMs

  -- Block + Mempool panels side by side
  let blockLines := renderBlockPanel state.recentBlocks leftWidth panelHeight
  let mempoolLines := renderMempoolPanel state rightWidth panelHeight
  let panelRows := List.range panelHeight |>.map fun i =>
    let left := (blockLines.get? i).getD ""
    let right := (mempoolLines.get? i).getD ""
    splitLine left right leftWidth rightWidth

  -- Peers panel
  let peerLines := renderPeerPanel state.peers width peerHeight
  let peerRows := peerLines.map fun line => boxLine line width

  -- Status bar
  let statusLines := renderStatusBar state width
  let statusRows := statusLines.map fun line => boxLine line width

  -- Assemble
  let allLines := header
    ++ [midSeparatorT leftWidth rightWidth]
    ++ panelRows
    ++ [midSeparatorInvT leftWidth rightWidth]
    ++ peerRows
    ++ [midSeparator width]
    ++ statusRows
    ++ [bottomBorder width]

  -- Join all lines, each followed by clearLine to erase leftover chars
  String.intercalate "\n" (allLines.map fun l => l ++ Ansi.clearLine)

end Cleanode.TUI.Layout
