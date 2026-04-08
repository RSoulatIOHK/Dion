import Dion.Network.Socket
import Dion.Network.Multiplexer
import Dion.Network.ChainSync
import Dion.Network.BlockFetch
import Dion.Network.ConwayBlock
import Dion.Network.Mempool

/-!
# BlockFetch Client Helper

High-level client function for fetching blocks.
-/

namespace Dion.Network.BlockFetchClient

open Dion.Network.Socket
open Dion.Network.ChainSync
open Dion.Network.BlockFetch
open Dion.Network.ConwayBlock

/-- Fetch a single block by point. Passes mempoolRef so TxSubmission2
    messages arriving during BlockFetch are handled inline (not dropped).
    Set verbose := false to suppress trace-level prints (e.g. in TUI mode). -/
def fetchBlock (sock : Socket) (point : Point)
    (mempoolRef : Option (IO.Ref Dion.Network.Mempool.Mempool) := none)
    (verbose : Bool := true)
    : IO (Except SocketError (Option ByteArray)) := do
  let log : String → IO Unit := fun msg => if verbose then IO.println msg else pure ()
  log s!"[blockfetch] requesting slot={point.slot} hash={point.hash.size}B"
  match ← sendBlockFetch sock (.MsgRequestRange point point) with
  | .error e => do
      log s!"[blockfetch] send error: {e}"
      return .error e
  | .ok () => do
      log "[blockfetch] sent MsgRequestRange, waiting for StartBatch/NoBlocks..."
      match ← receiveBlockFetch sock ⟨#[]⟩ 65535 mempoolRef with
      | .error e => do
          log s!"[blockfetch] receive error (StartBatch): {e}"
          return .error e
      | .ok none => do
          log "[blockfetch] receive returned none (timeout/decode fail) waiting for StartBatch"
          return .ok none
      | .ok (some result) =>
          log s!"[blockfetch] got: {repr result.message}"
          match result.message with
          | .MsgStartBatch => do
              log "[blockfetch] MsgStartBatch — waiting for block..."
              match ← receiveBlockFetch sock result.leftoverBytes 2000000 mempoolRef with
              | .error e => do
                  log s!"[blockfetch] receive error (MsgBlock): {e}"
                  return .error e
              | .ok none => do
                  log "[blockfetch] receive returned none waiting for MsgBlock"
                  return .ok none
              | .ok (some blockResult) =>
                  match blockResult.message with
                  | .MsgBlock blockBytes => do
                      log s!"[blockfetch] got MsgBlock size={blockBytes.size} — waiting for BatchDone..."
                      match ← receiveBlockFetch sock blockResult.leftoverBytes 65535 mempoolRef with
                      | .error e => do
                          log s!"[blockfetch] receive error (BatchDone): {e}"
                          return .error e
                      | .ok (some doneResult) =>
                          log s!"[blockfetch] BatchDone step got: {repr doneResult.message}"
                          match doneResult.message with
                          | .MsgBatchDone =>
                              log "[blockfetch] complete ✓"
                              return .ok (some blockBytes)
                          | _ => do
                              log "[WARN] Expected BatchDone, got different message"
                              return .ok (some blockBytes)
                      | _ =>
                          log "[blockfetch] no BatchDone frame — returning block anyway"
                          return .ok (some blockBytes)
                  | _ => do
                      log s!"[blockfetch] expected MsgBlock, got: {repr blockResult.message}"
                      return .ok none
          | .MsgNoBlocks => do
              log "[blockfetch] MsgNoBlocks — server has no block at this point"
              return .ok none
          | _ => do
              log s!"[blockfetch] unexpected first message: {repr result.message}"
              return .ok none

end Dion.Network.BlockFetchClient
