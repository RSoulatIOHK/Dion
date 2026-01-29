import Cleanode.Network.Basic
import Cleanode.Network.Socket
import Cleanode.Network.Handshake
import Cleanode.Network.Multiplexer
import Cleanode.Network.ChainSync
import Cleanode.Network.Byron
import Cleanode.Network.Shelley
import Cleanode.Network.BlockFetch
import Cleanode.Network.BlockFetchClient
import Cleanode.Network.ConwayBlock
import Cleanode.Network.Crypto
import Cleanode.Network.Bech32
import Cleanode.Storage.BlockStore

open Cleanode.Network
open Cleanode.Network.Socket
open Cleanode.Network.Handshake
open Cleanode.Network.Multiplexer
open Cleanode.Network.ChainSync
open Cleanode.Network.Byron
open Cleanode.Network.Shelley
open Cleanode.Network.BlockFetch
open Cleanode.Network.Crypto
open Cleanode.Network.Bech32
open Cleanode.Network.BlockFetchClient
open Cleanode.Network.ConwayBlock
open Cleanode.Storage.BlockStore

def testHandshake (host : String) (port : UInt16) : IO Unit := do
  IO.println s!"Connecting to {host}:{port}..."

  match ← socket_connect host port with
  | .error e =>
      IO.println s!"✗ Connection failed: {e}"
  | .ok sock => do
      IO.println "✓ Connected!"

      -- Send handshake proposal
      IO.println "Sending handshake proposal..."
      let proposal := createMainnetProposal
      match ← sendHandshake sock proposal with
      | .error e => do
          IO.println s!"✗ Failed to send handshake: {e}"
          socket_close sock
      | .ok _ => do
          IO.println "✓ Handshake sent!"

          -- Receive handshake response
          IO.println "Waiting for handshake response..."

          -- Receive and parse handshake response
          IO.println "Waiting for handshake response..."
          match ← socket_receive sock 1024 with
          | .error e => do
              IO.println s!"✗ Failed to receive: {e}"
              socket_close sock
          | .ok rawData => do
              IO.println s!"✓ Received {rawData.size} bytes"

              -- Properly format hex output
              let hexBytes := rawData.toList.map (fun b =>
                let n := b.toNat
                let hi := n / 16
                let lo := n % 16
                let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
                let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
                String.mk [hiChar, loChar]
              )
              IO.println s!"Raw data: {String.intercalate " " hexBytes}"

              -- Parse MUX frame
              match decodeMuxFrame rawData with
              | none => IO.println "✗ Failed to decode MUX frame"
              | some frame => do
                  IO.println s!"✓ MUX frame decoded:"
                  IO.println s!"  Protocol: {repr frame.header.protocolId}"
                  IO.println s!"  Mode: {repr frame.header.mode}"
                  IO.println s!"  Payload: {frame.payload.size} bytes"

                  -- Try to decode handshake message
                  match decodeHandshakeMessage frame.payload with
                  | none => IO.println "✗ Failed to decode handshake message"
                  | some msg => do
                      IO.println s!"✓ Handshake response: {repr msg}"
                      IO.println "✓ Handshake complete! Ready for ChainSync."

              socket_close sock

def testChainSync (host : String) (port : UInt16) (proposal : HandshakeMessage) (networkName : String) : IO Unit := do
  -- Initialize storage
  initStorage

  -- Check if we have previous sync state
  let resumePoint ← getLastSyncedPoint
  let syncedCount ← countSyncedBlocks
  match resumePoint with
  | none => IO.println s!"Starting fresh sync from genesis on {networkName}"
  | some point => IO.println s!"Resuming sync from slot {point.slot} ({syncedCount} blocks already synced) on {networkName}"

  IO.println s!"Connecting to {host}:{port}..."

  match ← socket_connect host port with
  | .error e =>
      IO.println s!"✗ Connection failed: {e}"
  | .ok sock => do
      IO.println "✓ Connected!"

      -- Step 1: Perform handshake
      IO.println ""
      IO.println "=== Handshake ==="
      match ← sendHandshake sock proposal with
      | .error e => do
          IO.println s!"✗ Failed to send handshake: {e}"
          socket_close sock
      | .ok _ => do
          IO.println "✓ Handshake proposal sent"

          -- Receive handshake response
          match ← socket_receive sock 1024 with
          | .error e => do
              IO.println s!"✗ Failed to receive handshake: {e}"
              socket_close sock
          | .ok rawData => do
              IO.println s!"✓ Received {rawData.size} bytes"
              match decodeMuxFrame rawData with
              | none => do
                  IO.println "✗ Failed to decode handshake MUX frame"
                  socket_close sock
              | some frame => do
                  match decodeHandshakeMessage frame.payload with
                  | none => do
                      IO.println "✗ Failed to decode handshake message"
                      socket_close sock
                  | some msg => do
                      IO.println s!"✓ Handshake complete: {repr msg}"

                      -- Step 2: Find intersection (resume from last point or genesis)
                      IO.println ""
                      IO.println "=== ChainSync - Finding Intersection ==="

                      -- Option: Start from a known recent checkpoint instead of genesis
                      -- Using recent mainnet block to skip early chain history
                      let recentCheckpoint := createCheckpoint 178067736 "7a3eed1504c7d04890a2806697e9d82009e9b140b7a51f0ab24e66bfec43d117"
                      let intersectMsg := findIntersectFromCheckpoint recentCheckpoint

                      -- Or resume from saved state / genesis:
                      -- let intersectMsg := match resumePoint with
                      --   | none => findIntersectGenesis
                      --   | some point => ChainSyncMessage.MsgFindIntersect [point, Point.genesis]
                      match ← sendChainSync sock intersectMsg with
                      | .error e => do
                          IO.println s!"✗ Failed to send FindIntersect: {e}"
                          socket_close sock
                      | .ok _ => do
                          match resumePoint with
                          | none => IO.println "✓ FindIntersect sent (genesis)"
                          | some point => IO.println s!"✓ FindIntersect sent (resuming from slot {point.slot})"

                          -- Receive intersection response
                          match ← socket_receive sock 8192 with
                          | .error e => do
                              IO.println s!"✗ Failed to receive intersection: {e}"
                              socket_close sock
                          | .ok rawData => do
                              IO.println s!"✓ Received {rawData.size} bytes"

                              -- Show hex for debugging
                              let hexBytes := rawData.toList.take 80 |>.map (fun b =>
                                let n := b.toNat
                                let hi := n / 16
                                let lo := n % 16
                                let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
                                let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
                                String.mk [hiChar, loChar]
                              )
                              IO.println s!"Raw (first 80 bytes): {String.intercalate " " hexBytes}"

                              match decodeMuxFrame rawData with
                              | none => do
                                  IO.println "✗ Failed to decode MUX frame"
                                  socket_close sock
                              | some frame => do
                                  IO.println s!"✓ MUX decoded: protocol={repr frame.header.protocolId}, payload={frame.payload.size}B"
                                  match decodeChainSyncMessage frame.payload with
                                  | none => do
                                      IO.println "✗ Failed to decode ChainSync message"
                                      -- Show payload hex
                                      let payloadHex := frame.payload.toList.take 40 |>.map (fun b =>
                                        let n := b.toNat
                                        let hi := n / 16
                                        let lo := n % 16
                                        let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
                                        let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
                                        String.mk [hiChar, loChar]
                                      )
                                      IO.println s!"Payload (first 40 bytes): {String.intercalate " " payloadHex}"
                                      socket_close sock
                                  | some msg => do
                                      IO.println s!"✓ Intersection response: {repr msg}"

                                      -- Step 3: Request first blocks
                                      IO.println ""
                                      IO.println "=== ChainSync - Requesting Blocks ==="
                                      for i in [1:6] do
                                        IO.println s!"\nRequesting block #{i}..."
                                        match ← sendChainSync sock requestNext with
                                        | .error e => do
                                            IO.println s!"✗ Failed to send RequestNext: {e}"
                                            break
                                        | .ok _ => do
                                            match ← socket_receive sock 65535 with
                                            | .error e => do
                                                IO.println s!"✗ Failed to receive block: {e}"
                                                break
                                            | .ok rawData => do
                                                IO.println s!"✓ Received {rawData.size} bytes"
                                                let hexBytes := rawData.toList.take 80 |>.map (fun b =>
                                                  let n := b.toNat
                                                  let hi := n / 16
                                                  let lo := n % 16
                                                  let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
                                                  let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
                                                  String.mk [hiChar, loChar]
                                                )
                                                IO.println s!"Raw (first 80 bytes): {String.intercalate " " hexBytes}"

                                                match decodeMuxFrame rawData with
                                                | none => do
                                                    IO.println "✗ Failed to decode MUX frame"
                                                    break
                                                | some frame => do
                                                    IO.println s!"✓ MUX decoded: payload={frame.payload.size}B"
                                                    let payloadHex := frame.payload.toList.take 60 |>.map (fun b =>
                                                      let n := b.toNat
                                                      let hi := n / 16
                                                      let lo := n % 16
                                                      let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
                                                      let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
                                                      String.mk [hiChar, loChar]
                                                    )
                                                    IO.println s!"Payload (first 60 bytes): {String.intercalate " " payloadHex}"

                                                    match decodeChainSyncMessage frame.payload with
                                                    | none => do
                                                        IO.println "✗ Failed to decode ChainSync message"
                                                        break
                                                    | some (.MsgRollForward header tip) => do
                                                        IO.println s!"✓ Block received!"
                                                        IO.println s!"  Era: {header.era}"
                                                        IO.println s!"  Header bytes: {header.headerBytes.size}"

                                                        -- Parse based on era
                                                        if header.era == 0 then do
                                                          -- Byron era (era 0)
                                                          match extractByronInfo header.headerBytes with
                                                          | none => IO.println "  ⚠ Failed to parse Byron header"
                                                          | some byronInfo => do
                                                              IO.println s!"  Block slot: {byronInfo.slot}"
                                                              IO.println s!"  Block height: {byronInfo.blockNo}"
                                                              match byronInfo.header with
                                                              | none => IO.println "  ⚠ Failed to parse header details"
                                                              | some h => do
                                                                  IO.println s!"  Protocol magic: {h.protocolMagic}"
                                                                  IO.println s!"  Previous hash: {h.prevBlockHash.size} bytes"
                                                                  IO.println s!"  Body proof: {h.bodyProof.size} bytes"

                                                                  -- Show hash prefix
                                                                  if h.prevBlockHash.size >= 8 then
                                                                    let hashHex := h.prevBlockHash.toList.take 8 |>.map (fun b =>
                                                                      let n := b.toNat
                                                                      let hi := n / 16
                                                                      let lo := n % 16
                                                                      let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
                                                                      let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
                                                                      String.mk [hiChar, loChar]
                                                                    )
                                                                    IO.println s!"  Prev hash prefix: {String.intercalate "" hashHex}..."

                                                                  -- Save block to disk
                                                                  saveBlock byronInfo.slot byronInfo.blockNo header.era
                                                                            byronInfo.headerBytes h.prevBlockHash

                                                                  -- Update sync state
                                                                  saveSyncState {
                                                                    lastSlot := UInt64.ofNat byronInfo.slot,
                                                                    lastBlockNo := UInt64.ofNat byronInfo.blockNo,
                                                                    lastHash := h.prevBlockHash
                                                                  }

                                                                  IO.println "  ✓ Block saved to storage"
                                                        else if header.era >= 1 && header.era <= 6 then do
                                                          -- Shelley+ eras (1=Shelley, 2=Allegra, 3=Mary, 4=Alonzo, 5=Babbage, 6=Conway)
                                                          let eraName := match header.era with
                                                            | 1 => "Shelley"
                                                            | 2 => "Allegra"
                                                            | 3 => "Mary"
                                                            | 4 => "Alonzo"
                                                            | 5 => "Babbage"
                                                            | 6 => "Conway"
                                                            | _ => "Unknown"
                                                          IO.println s!"  Era name: {eraName}"

                                                          match extractShelleyInfo header.headerBytes with
                                                          | none => IO.println "  ⚠ Failed to parse Shelley+ header"
                                                          | some shelleyInfo => do
                                                              IO.println s!"  Block slot: {shelleyInfo.slot}"
                                                              IO.println s!"  Block height: {shelleyInfo.blockNo}"
                                                              IO.println s!"  Previous hash: {shelleyInfo.prevBlockHash.size} bytes"
                                                              IO.println s!"  Block body size: {shelleyInfo.blockBodySize} bytes"

                                                              -- Show hash prefix
                                                              if shelleyInfo.prevBlockHash.size >= 8 then
                                                                let hashHex := shelleyInfo.prevBlockHash.toList.take 8 |>.map (fun b =>
                                                                  let n := b.toNat
                                                                  let hi := n / 16
                                                                  let lo := n % 16
                                                                  let hiChar := if hi < 10 then Char.ofNat (48 + hi) else Char.ofNat (87 + hi)
                                                                  let loChar := if lo < 10 then Char.ofNat (48 + lo) else Char.ofNat (87 + lo)
                                                                  String.mk [hiChar, loChar]
                                                                )
                                                                IO.println s!"  Prev hash prefix: {String.intercalate "" hashHex}..."

                                                              -- Save block to disk
                                                              saveBlock shelleyInfo.slot shelleyInfo.blockNo header.era
                                                                        header.headerBytes shelleyInfo.prevBlockHash

                                                              -- Update sync state
                                                              saveSyncState {
                                                                lastSlot := UInt64.ofNat shelleyInfo.slot,
                                                                lastBlockNo := UInt64.ofNat shelleyInfo.blockNo,
                                                                lastHash := shelleyInfo.prevBlockHash
                                                              }

                                                              IO.println "  ✓ Block saved to storage"
                                                        else
                                                          IO.println s!"  ⚠ Unknown era {header.era}"

                                                        IO.println s!"  Chain tip: slot={tip.point.slot}, block={tip.blockNo}"
                                                    | some (.MsgAwaitReply) => do
                                                        IO.println "✓ Caught up! Waiting for new blocks..."
                                                        break
                                                    | some (.MsgRollBackward point tip) => do
                                                        IO.println s!"✓ Rollback to slot {point.slot}"
                                                    | some other => do
                                                        IO.println s!"✓ Received: {repr other}"

                                      socket_close sock

def testBlockFetch (host : String) (port : UInt16) (proposal : HandshakeMessage) (networkName : String) : IO Unit := do
  IO.println s!"Connecting to {host}:{port}..."

  match ← socket_connect host port with
  | .error e =>
      IO.println s!"✗ Connection failed: {e}"
  | .ok sock => do
      IO.println "✓ Connected!"

      -- Step 1: Perform handshake
      IO.println ""
      IO.println "=== Handshake ==="
      match ← sendHandshake sock proposal with
      | .error e => do
          IO.println s!"✗ Failed to send handshake: {e}"
          socket_close sock
      | .ok _ => do
          IO.println "✓ Handshake proposal sent"

          -- Receive handshake response
          match ← socket_receive sock 1024 with
          | .error e => do
              IO.println s!"✗ Failed to receive handshake: {e}"
              socket_close sock
          | .ok rawData => do
              IO.println s!"✓ Received {rawData.size} bytes"
              match decodeMuxFrame rawData with
              | none => do
                  IO.println "✗ Failed to decode handshake MUX frame"
                  socket_close sock
              | some frame => do
                  match decodeHandshakeMessage frame.payload with
                  | none => do
                      IO.println "✗ Failed to decode handshake message"
                      socket_close sock
                  | some msg => do
                      IO.println s!"✓ Handshake complete: {repr msg}"

                      -- Step 2: Use ChainSync to find a recent block
                      IO.println ""
                      IO.println "=== ChainSync - Getting Block Header ==="
                      let recentCheckpoint := createCheckpoint 178067736 "7a3eed1504c7d04890a2806697e9d82009e9b140b7a51f0ab24e66bfec43d117"
                      let intersectMsg := findIntersectFromCheckpoint recentCheckpoint

                      match ← sendChainSync sock intersectMsg with
                      | .error e => do
                          IO.println s!"✗ Failed to send FindIntersect: {e}"
                          socket_close sock
                      | .ok _ => do
                          IO.println "✓ FindIntersect sent"

                          -- Receive intersection response
                          match ← socket_receive sock 8192 with
                          | .error e => do
                              IO.println s!"✗ Failed to receive intersection: {e}"
                              socket_close sock
                          | .ok rawData => do
                              match decodeMuxFrame rawData with
                              | none => do
                                  IO.println "✗ Failed to decode MUX frame"
                                  socket_close sock
                              | some frame => do
                                  match decodeChainSyncMessage frame.payload with
                                  | none => do
                                      IO.println "✗ Failed to decode ChainSync message"
                                      socket_close sock
                                  | some (.MsgIntersectFound point tip) => do
                                      IO.println s!"✓ Intersection found at slot {point.slot}"

                                      -- Step 3: Request the next block header
                                      IO.println "✓ Requesting next block header..."
                                      match ← sendChainSync sock requestNext with
                                      | .error e => do
                                          IO.println s!"✗ Failed to send RequestNext: {e}"
                                          socket_close sock
                                      | .ok _ => do
                                          match ← socket_receive sock 65535 with
                                          | .error e => do
                                              IO.println s!"✗ Failed to receive block header: {e}"
                                              socket_close sock
                                          | .ok rawData => do
                                              match decodeMuxFrame rawData with
                                              | none => do
                                                  IO.println "✗ Failed to decode MUX frame"
                                                  socket_close sock
                                              | some frame => do
                                                  match decodeChainSyncMessage frame.payload with
                                                  | some (.MsgRollBackward rollbackPoint _) => do
                                                      IO.println s!"✓ Rollback to slot {rollbackPoint.slot}"
                                                      IO.println "✓ Requesting next block after rollback..."

                                                      -- Request next block again
                                                      match ← sendChainSync sock requestNext with
                                                      | .error e => do
                                                          IO.println s!"✗ Failed to send RequestNext: {e}"
                                                          socket_close sock
                                                      | .ok _ => do
                                                          match ← socket_receive sock 65535 with
                                                          | .error e => do
                                                              IO.println s!"✗ Failed to receive: {e}"
                                                              socket_close sock
                                                          | .ok rawData2 => do
                                                              match decodeMuxFrame rawData2 with
                                                              | none => do
                                                                  IO.println "✗ Failed to decode MUX frame"
                                                                  socket_close sock
                                                              | some frame2 => do
                                                                  match decodeChainSyncMessage frame2.payload with
                                                                  | some (.MsgRollForward header tip) => do
                                                                      IO.println s!"✓ Got block header!"
                                                                      IO.println s!"  Era: {header.era}, Slot: {tip.point.slot}, Block: {tip.blockNo}"

                                                                      -- Step 4: Use BlockFetch to get full block body
                                                                      IO.println ""
                                                                      IO.println "=== BlockFetch - Requesting Full Block ==="

                                                                      -- Fetch the block
                                                                      let blockPoint := tip.point
                                                                      IO.println s!"✓ BlockFetch request sent for slot {blockPoint.slot}"

                                                                      match ← fetchBlock sock blockPoint with
                                                                      | .error e => do
                                                                          IO.println s!"✗ Failed to fetch block: {e}"
                                                                          socket_close sock
                                                                      | .ok none => do
                                                                          IO.println "✗ No block received"
                                                                          socket_close sock
                                                                      | .ok (some blockBytes) => do
                                                                          -- Parse block body
                                                                          match ← parseConwayBlockBodyIO blockBytes with
                                                                          | none =>
                                                                              IO.println "⚠ Failed to parse block"
                                                                          | some blockBody => do
                                                                              -- Calculate statistics
                                                                              let totalInputs := blockBody.transactions.foldl (fun acc tx => acc + tx.body.inputs.length) 0
                                                                              let totalOutputs := blockBody.transactions.foldl (fun acc tx => acc + tx.body.outputs.length) 0
                                                                              let totalFees := blockBody.transactions.foldl (fun acc tx => acc + tx.body.fee) 0
                                                                              let totalFeesAda := totalFees / 1000000
                                                                              let totalFeesLovelace := totalFees % 1000000

                                                                              -- Compute block hash
                                                                              let blockHash ← computeBlockHash header.headerBytes

                                                                              -- Display block header
                                                                              IO.println ""
                                                                              IO.println "╔════════════════════════════════════════════════════════════╗"
                                                                              IO.println s!"║ Block #{tip.blockNo}   Slot: {blockPoint.slot}"
                                                                              IO.println s!"║ Hash: {blockHash.take 16}...{blockHash.drop (blockHash.length - 16)}"
                                                                              IO.println "╟────────────────────────────────────────────────────────────╢"
                                                                              IO.println s!"║ Size: {blockBytes.size} bytes   Era: {header.era}"
                                                                              IO.println s!"║ Transactions: {blockBody.transactions.length}   Inputs: {totalInputs}   Outputs: {totalOutputs}"
                                                                              IO.println s!"║ Total Fees: {totalFeesAda}.{totalFeesLovelace} ADA"
                                                                              IO.println "╚════════════════════════════════════════════════════════════╝"
                                                                              IO.println ""

                                                                              -- Display transactions
                                                                              if blockBody.transactions.length > 0 then do
                                                                                let mut txNum := 1
                                                                                for tx in blockBody.transactions do
                                                                                  let feeAda := tx.body.fee / 1000000
                                                                                  let feeLovelace := tx.body.fee % 1000000

                                                                                  -- Compute transaction ID
                                                                                  let txId ← computeTxId tx.body.rawBytes

                                                                                  -- Transaction header
                                                                                  IO.println s!"Transaction #{txNum}: {txId.take 16}...{txId.drop (txId.length - 16)}"
                                                                                  IO.println s!"  {tx.body.inputs.length} inputs → {tx.body.outputs.length} outputs   Fee: {feeAda}.{feeLovelace} ADA"

                                                                                  -- Show inputs (full hashes for ≤3 inputs, summary for more)
                                                                                  if tx.body.inputs.length > 0 then do
                                                                                    if tx.body.inputs.length <= 3 then
                                                                                      for input in tx.body.inputs do
                                                                                        let txIdHex := bytesToHex input.txId
                                                                                        IO.println s!"    ← {txIdHex}#{input.outputIndex}"
                                                                                    else
                                                                                      IO.println s!"    ← {tx.body.inputs.length} inputs"

                                                                                  -- Show outputs (full addresses for ≤3 outputs, summary for more)
                                                                                  if tx.body.outputs.length > 0 then do
                                                                                    if tx.body.outputs.length <= 3 then
                                                                                      for output in tx.body.outputs do
                                                                                        let addr := encodeAddress output.address false  -- false = mainnet
                                                                                        let amtAda := output.amount / 1000000
                                                                                        let amtLovelace := output.amount % 1000000
                                                                                        -- Format with proper padding
                                                                                        let paddedLovelace :=
                                                                                          let s := toString amtLovelace
                                                                                          String.join (List.replicate (6 - s.length) "0") ++ s
                                                                                        IO.println s!"    → {addr}"
                                                                                        IO.println s!"       {amtAda}.{paddedLovelace} ADA"

                                                                                        -- Display native assets if present
                                                                                        if output.nativeAssets.length > 0 then
                                                                                          for asset in output.nativeAssets do
                                                                                            let policyHex := bytesToHex asset.policyId
                                                                                            let assetNameHex := bytesToHex asset.assetName
                                                                                            -- Try to decode asset name as ASCII if it's printable
                                                                                            let isPrintable := asset.assetName.toList.all (fun b => b >= 32 && b <= 126)
                                                                                            let assetNameStr :=
                                                                                              if asset.assetName.size > 0 && isPrintable then
                                                                                                String.mk (asset.assetName.toList.map fun b => Char.ofNat b.toNat)
                                                                                              else
                                                                                                assetNameHex
                                                                                            IO.println s!"       + {asset.amount} {assetNameStr}"
                                                                                            IO.println s!"         Policy: {policyHex.take 16}...{policyHex.drop (policyHex.length - 8)}"
                                                                                    else
                                                                                      -- Show total output amount for many outputs
                                                                                      let totalOut := tx.body.outputs.foldl (fun acc out => acc + out.amount) 0
                                                                                      let totalAda := totalOut / 1000000
                                                                                      let totalLov := totalOut % 1000000
                                                                                      let paddedLov :=
                                                                                        let s := toString totalLov
                                                                                        String.join (List.replicate (6 - s.length) "0") ++ s
                                                                                      IO.println s!"    → {tx.body.outputs.length} outputs totaling {totalAda}.{paddedLov} ADA"

                                                                                  IO.println ""
                                                                                  txNum := txNum + 1

                                                                          socket_close sock
                                                                  | other => do
                                                                      IO.println s!"✗ Unexpected message: {repr other}"
                                                                      socket_close sock
                                                  | some (.MsgRollForward header tip) => do
                                                      IO.println s!"✓ Got block header!"
                                                      IO.println s!"  Era: {header.era}, Slot: {tip.point.slot}, Block: {tip.blockNo}"

                                                      -- Step 4: Use BlockFetch to get full block body
                                                      IO.println ""
                                                      IO.println "=== BlockFetch - Requesting Full Block ==="

                                                      -- Fetch the block
                                                      let blockPoint := tip.point
                                                      IO.println s!"✓ BlockFetch request sent for slot {blockPoint.slot}"

                                                      match ← fetchBlock sock blockPoint with
                                                      | .error e => do
                                                          IO.println s!"✗ Failed to fetch block: {e}"
                                                          socket_close sock
                                                      | .ok none => do
                                                          IO.println "✗ No block received"
                                                          socket_close sock
                                                      | .ok (some blockBytes) => do
                                                          IO.println s!"✓ Received full block! Size: {blockBytes.size} bytes"
                                                          IO.println s!"  Header size: {header.headerBytes.size} bytes"
                                                          IO.println s!"  Body size: {blockBytes.size} bytes"

                                                          -- Parse block body to extract transactions
                                                          IO.println ""
                                                          IO.println "=== Parsing Block Transactions ==="
                                                          match ← parseConwayBlockBodyIO blockBytes with
                                                          | none =>
                                                              IO.println "⚠ Failed to parse block body"
                                                          | some blockBody => do
                                                              IO.println s!"✓ Block contains {blockBody.transactions.length} transactions"

                                                              if blockBody.transactions.length > 0 then
                                                                IO.println s!"  (Transaction details: inputs, outputs, fees)"

                                                          socket_close sock
                                                  | other => do
                                                      IO.println s!"✗ Unexpected ChainSync message: {repr other}"
                                                      socket_close sock
                                  | other => do
                                      IO.println s!"✗ Unexpected intersection response: {repr other}"
                                      socket_close sock

def main : IO Unit := do
  IO.println "Cleanode: Formally Verified Cardano Node"
  IO.println "==========================================="
  IO.println ""
  IO.println "Testing Ouroboros BlockFetch Protocol..."
  IO.println ""

  -- ============================================
  -- Network Configuration
  -- ============================================
  -- Uncomment ONE of the following network configurations:

  -- MAINNET (12.8M+ blocks - slow to sync)
  let (host, port) := mainnetBootstrapPeers.head!
  let proposal := createMainnetProposal
  let networkName := "Mainnet"

  -- PREPROD (smaller testnet - faster to sync)
  -- let (host, port) := preprodBootstrapPeers.head!
  -- let proposal := createPreprodProposal
  -- let networkName := "Preprod"

  -- PREVIEW (smallest testnet - fastest to sync)
  -- let (host, port) := previewBootstrapPeers.head!
  -- let proposal := createPreviewProposal
  -- let networkName := "Preview"

  -- ============================================

  -- Choose test to run:
  testBlockFetch host port proposal networkName  -- Fetch full block with transactions
  -- testChainSync host port proposal networkName  -- Just sync headers

  IO.println ""
  IO.println "✓ Test complete!"
