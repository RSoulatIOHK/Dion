import Cleanode.TUI.State

/-!
# Node Status File & Query Commands

The running node periodically writes a JSON status file.
The `cleanode query` subcommands read this file.

## Status File Format
```json
{
  "tip": { "slot": 12345, "block": 6789, "epoch": 500 },
  "peers": [ { "address": "...", "status": "syncing", "blocks": 100 } ],
  "mempool": { "txCount": 5, "bytes": 2048 },
  "sync": { "blocksReceived": 10000, "rollbacks": 2, "uptimeSeconds": 3600 },
  "consensus": { "vrfValid": 9990, "kesValid": 9990, "headersValidated": 10000 }
}
```
-/

namespace Cleanode.CLI.Query

open Cleanode.TUI.State

/-- Default path for the node status file -/
def statusFilePath : String := "./data/node-status.json"

-- ====================
-- = Status Writer    =
-- ====================

/-- Render a JSON object field -/
private def jField (key : String) (value : String) : String :=
  s!"\"{key}\": {value}"

private def jStr (s : String) : String := s!"\"{s}\""
private def jNum (n : Nat) : String := toString n

private def jObj (fields : List String) : String :=
  "{" ++ String.intercalate ", " fields ++ "}"

/-- Render peers as JSON array -/
private def renderPeersJson (peers : List PeerSummary) : String :=
  let entries := peers.map fun p =>
    jObj [jField "address" (jStr p.address), jField "status" (jStr p.status), jField "blocksSynced" (jNum p.blocksSynced)]
  "[" ++ String.intercalate ", " entries ++ "]"

/-- Render TUIState as JSON for the status file -/
def renderStatusJson (state : TUIState) (nowMs : Nat) : String :=
  let uptimeSec := (nowMs - state.startedAt) / 1000
  let tip := jObj [jField "slot" (jNum state.tipSlot), jField "blockNumber" (jNum state.tipBlockNo), jField "epoch" (jNum state.consensus.currentEpoch)]
  let mempool := jObj [jField "txCount" (jNum state.mempoolTxCount), jField "bytes" (jNum state.mempoolBytes)]
  let sync := jObj [jField "blocksReceived" (jNum state.blocksReceived), jField "rollbacks" (jNum state.rollbacks), jField "uptimeSeconds" (jNum uptimeSec)]
  let consensus := jObj [
    jField "headersValidated" (jNum state.consensus.validatedHeaders),
    jField "vrfValid" (jNum state.consensus.vrfValid), jField "vrfInvalid" (jNum state.consensus.vrfInvalid),
    jField "kesValid" (jNum state.consensus.kesValid), jField "kesInvalid" (jNum state.consensus.kesInvalid),
    jField "opCertValid" (jNum state.consensus.opCertValid), jField "opCertInvalid" (jNum state.consensus.opCertInvalid)
  ]
  jObj [jField "tip" tip, jField "network" (jStr state.networkName), jField "peers" (renderPeersJson state.peers), jField "mempool" mempool, jField "sync" sync, jField "consensus" consensus]

/-- Write the status file periodically (call from a background task) -/
partial def statusFileWriterLoop (tuiRef : IO.Ref TUIState) (intervalMs : Nat := 5000) : IO Unit := do
  IO.FS.createDirAll "./data"
  while true do
    IO.sleep (UInt32.ofNat intervalMs)
    let state ← tuiRef.get
    let nowMs ← IO.monoMsNow
    let json := renderStatusJson state nowMs
    IO.FS.writeFile statusFilePath json

-- ====================
-- = Query Commands   =
-- ====================

/-- Read and display the tip from the status file -/
def queryTip : IO Unit := do
  let content ← IO.FS.readFile statusFilePath
  -- Extract just the tip line for clean output
  IO.println content

/-- Read and display peers from the status file -/
def queryPeers : IO Unit := do
  let content ← IO.FS.readFile statusFilePath
  IO.println content

/-- Read and display mempool from the status file -/
def queryMempool : IO Unit := do
  let content ← IO.FS.readFile statusFilePath
  IO.println content

end Cleanode.CLI.Query
