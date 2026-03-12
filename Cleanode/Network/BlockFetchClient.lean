import Cleanode.Network.Socket
import Cleanode.Network.Multiplexer
import Cleanode.Network.ChainSync
import Cleanode.Network.BlockFetch
import Cleanode.Network.ConwayBlock

/-!
# BlockFetch Client Helper

High-level client function for fetching blocks.
-/

namespace Cleanode.Network.BlockFetchClient

open Cleanode.Network.Socket
open Cleanode.Network.ChainSync
open Cleanode.Network.BlockFetch
open Cleanode.Network.ConwayBlock

/-- Fetch a single block by point -/
def fetchBlock (sock : Socket) (point : Point) : IO (Except SocketError (Option ByteArray)) := do
  -- Send request
  match ← sendBlockFetch sock (.MsgRequestRange point point) with
  | .error e => do
      IO.println s!"[ERR] BlockFetch send error: {e}"
      return .error e
  | .ok () => do
      -- Receive StartBatch or NoBlocks
      match ← receiveBlockFetch sock ⟨#[]⟩ 65535 with
      | .error e => do
          IO.println s!"[ERR] BlockFetch receive error: {e}"
          return .error e
      | .ok none => do
          IO.println "[ERR] BlockFetch receive returned none (timeout or decode failure)"
          return .ok none
      | .ok (some result) =>
          match result.message with
          | .MsgStartBatch => do
              IO.println "[OK] Got MsgStartBatch, receiving block..."
              -- Receive block
              match ← receiveBlockFetch sock result.leftoverBytes 2000000 with
              | .error e => return .error e
              | .ok none => return .ok none
              | .ok (some blockResult) =>
                  match blockResult.message with
                  | .MsgBlock blockBytes => do
                      -- Receive BatchDone
                      match ← receiveBlockFetch sock blockResult.leftoverBytes 65535 with
                      | .error e => return .error e
                      | .ok (some doneResult) =>
                          match doneResult.message with
                          | .MsgBatchDone => return .ok (some blockBytes)
                          | _ => do
                              IO.println "[WARN] Expected BatchDone, got different message"
                              return .ok (some blockBytes)  -- Return block anyway
                      | _ => return .ok (some blockBytes)  -- Return block anyway
                  | _ => do
                      IO.println "[WARN] Expected MsgBlock"
                      return .ok none
          | .MsgNoBlocks => do
              IO.println "[ERR] Server has no blocks in range"
              return .ok none
          | _ => do
              IO.println "[WARN] Expected MsgStartBatch or MsgNoBlocks"
              return .ok none

end Cleanode.Network.BlockFetchClient
