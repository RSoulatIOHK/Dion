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
import Cleanode.Network.TxSubmission2
import Cleanode.Network.Mempool
import Cleanode.Network.PeerSharing
import Cleanode.Network.PeerDb
import Cleanode.Network.PeerConnection
import Cleanode.Network.ConnectionManager
import Cleanode.Network.MuxDispatcher
import Cleanode.Config.Topology
import Cleanode.Storage.BlockStore
import Cleanode.Storage.ChainDB
import Cleanode.Storage.Database
import Pigment

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
open Cleanode.Network.TxSubmission2
open Cleanode.Network.Mempool
open Cleanode.Network.PeerDb
open Cleanode.Network.PeerConnection
open Cleanode.Network.ConnectionManager
open Cleanode.Network.MuxDispatcher
open Cleanode.Network.PeerSharing
open Cleanode.Config.Topology
open Cleanode.Storage.BlockStore
open Cleanode.Storage.ChainDB
open Cleanode.Storage.Database
open Pigment

/-- Atomically check if a key is in the set; if not, add it.
    Returns true if the key was already present (= duplicate).
    Uses swap-based spinlock: `none` means locked, `some list` means unlocked.
    Thread takes ownership by swapping in `none`, then puts the list back. -/
partial def atomicCheckAndMark (ref : IO.Ref (Option (List Nat))) (key : Nat) : IO Bool := do
  -- Acquire: spin until we swap out a `some`
  let rec acquire : IO (List Nat) := do
    match ← ref.swap none with
    | some list => return list
    | none => acquire  -- Another thread holds it, retry
  let list ← acquire
  let alreadySeen := list.contains key
  let newList := if alreadySeen then list else key :: list
  -- Release: put the list back
  ref.set (some newList)
  return alreadySeen

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

/-- Display a full block with colored transaction details -/
def displayBlock (header : Header) (tip : Tip) (blockPoint : Point) (blockBytes : ByteArray) : IO Unit := run do
  match ← parseConwayBlockBodyIO blockBytes with
  | none =>
      println (("⚠ Failed to parse block".style |> yellow))
  | some blockBody => do
      -- Calculate statistics
      let totalInputs := blockBody.transactions.foldl (fun acc tx => acc + tx.body.inputs.length) 0
      let totalOutputs := blockBody.transactions.foldl (fun acc tx => acc + tx.body.outputs.length) 0
      let totalFees := blockBody.transactions.foldl (fun acc tx => acc + tx.body.fee) 0
      let totalFeesAda := totalFees / 1000000
      let totalFeesLovelace := totalFees % 1000000

      -- Compute block hash
      let blockHash ← computeBlockHash header.headerBytes

      -- Display block header with granular colors
      println ("".style)
      println ((("╔" ++ String.join (List.replicate 100 "═") ++ "╗").style |> blue |> bold))

      -- Block number and slot line
      concat [
        ("║ ".style |> blue |> bold),
        ("Block ".style |> white),
        (s!"#{tip.blockNo}".style |> yellow |> bold),
        ("   Slot: ".style |> white),
        (s!"{blockPoint.slot}".style |> cyan |> bold)
      ]

      -- Hash line (full)
      concat [
        ("║ ".style |> blue |> bold),
        ("Hash: ".style |> white),
        (s!"{blockHash}".style |> cyan |> dim)
      ]

      println ((("╟" ++ String.join (List.replicate 100 "─") ++ "╢").style |> blue |> bold))

      -- Size and era line
      concat [
        ("║ ".style |> blue |> bold),
        ("Size: ".style |> white),
        (s!"{blockBytes.size}".style |> magenta),
        (" bytes   Era: ".style |> white),
        (s!"{header.era}".style |> magenta)
      ]

      -- Transactions, inputs, outputs line
      concat [
        ("║ ".style |> blue |> bold),
        ("Transactions: ".style |> white),
        (s!"{blockBody.transactions.length}".style |> yellow),
        ("   Inputs: ".style |> white),
        (s!"{totalInputs}".style |> cyan),
        ("   Outputs: ".style |> white),
        (s!"{totalOutputs}".style |> green)
      ]

      -- Total fees line
      let paddedTotalFees :=
        let s := toString totalFeesLovelace
        String.join (List.replicate (6 - s.length) "0") ++ s
      concat [
        ("║ ".style |> blue |> bold),
        ("Total Fees: ".style |> white),
        (s!"{totalFeesAda}.{paddedTotalFees}".style |> green |> bold),
        (" ₳".style |> green |> bold)
      ]

      println ((("╚" ++ String.join (List.replicate 100 "═") ++ "╝").style |> blue |> bold))
      println ("".style)

      -- Display transactions
      if blockBody.transactions.length > 0 then do
        let mut txNum := 1
        for tx in blockBody.transactions do
          let feeAda := tx.body.fee / 1000000
          let feeLovelace := tx.body.fee % 1000000

          -- Compute transaction ID
          let txId ← computeTxId tx.body.rawBytes

          -- Calculate total input and output amounts
          let _totalOut := tx.body.outputs.foldl (fun acc out => acc + out.amount) 0
          let paddedFee :=
            let s := toString feeLovelace
            String.join (List.replicate (6 - s.length) "0") ++ s

          -- Transaction box header with granular colors
          println ("".style)
          println ((("┌" ++ String.join (List.replicate 100 "─") ++ "┐").style |> blue))

          -- TX header line (full hash)
          concat [
            ("│ ".style |> blue),
            ("TX ".style |> white),
            (s!"#{txNum}".style |> yellow |> bold),
            (": ".style |> white),
            (s!"{txId}".style |> magenta),
            ("  Fee: ".style |> white),
            (s!"{feeAda}.{paddedFee}".style |> green |> bold),
            (" ₳".style |> green |> bold)
          ]

          println ((("├" ++ String.join (List.replicate 100 "─") ++ "┤").style |> blue))

          -- UTXO Graph visualization
          let maxRows := max tx.body.inputs.length tx.body.outputs.length
          let minRows := min tx.body.inputs.length tx.body.outputs.length
          let midRow := minRows / 2

          for i in [:maxRows] do
            -- Input part (left side) - fixed width 25 chars
            let inputPart := if h : i < tx.body.inputs.length then
              let input := tx.body.inputs.get ⟨i, h⟩
              let txHash := bytesToHex input.txId
              let inputStr := s!"{txHash.take 16}...#{input.outputIndex}"
              -- Pad to 25 chars
              inputStr ++ String.join (List.replicate (25 - inputStr.length) " ")
            else
              String.join (List.replicate 25 " ")

            -- Middle connector - fixed width 22 chars
            let connector := if i == midRow then
              s!"│───[{txId.take 8}...]───>│"
            else
              "│                    │"

            -- Output part (right side) - build styled text list
            let outputStyled : List StyledText := if h : i < tx.body.outputs.length then
              let output := tx.body.outputs.get ⟨i, h⟩
              let addr := encodeAddress output.address false
              let amt := output.amount / 1000000
              let amtLov := output.amount % 1000000
              let padLov := let s := toString amtLov
                            String.join (List.replicate (6 - s.length) "0") ++ s

              -- Build colored output: cyan address + green amount
              let addrPart := (s!"{addr.take 20}...".style |> cyan)
              let amtPart := (s!" {amt}.{padLov}₳".style |> green |> bold)
              let assetPart := if output.nativeAssets.length > 0 then
                [(s!" +{output.nativeAssets.length}a".style |> yellow)]
              else
                []
              [addrPart, amtPart] ++ assetPart
            else
              []

            -- Print with individual colors using Pigment's list-based approach
            let parts : List StyledText :=
              [("│ ".style |> blue),
               (inputPart.style |> dim),
               (connector.style |> magenta),
               (" ".style)] ++ outputStyled
            concat parts

            -- Display native assets on separate lines
            if h : i < tx.body.outputs.length then
              let output := tx.body.outputs.get ⟨i, h⟩
              for asset in output.nativeAssets do
                let policyHex := bytesToHex asset.policyId

                -- Decode asset name if printable
                let assetNameDisplay := if asset.assetName.size > 0 then
                  match String.fromUTF8? asset.assetName with
                  | some name =>
                      let isPrintable := name.all fun c => c.toNat >= 32 && c.toNat <= 126
                      if isPrintable && name.length > 0 then
                        s!":{name}"
                      else
                        let nameHex := bytesToHex asset.assetName
                        s!":{nameHex.take 8}..."
                  | none =>
                      let nameHex := bytesToHex asset.assetName
                      s!":{nameHex.take 8}..."
                else
                  ""

                let assetDisplay := s!"  +{asset.amount} {policyHex.take 8}...{assetNameDisplay}"

                -- Empty input and connector for asset lines
                let emptyInput := String.join (List.replicate 25 " ")
                let emptyConnector := "│                    │"
                let assetParts : List StyledText :=
                  [("│ ".style |> blue),
                   (emptyInput.style |> dim),
                   (emptyConnector.style |> magenta),
                   (" ".style),
                   (assetDisplay.style |> yellow)]
                concat assetParts

          -- Display redeemers if present
          let redeemers := tx.witnesses.redeemers
          if redeemers.length > 0 then
            redeemers.forM fun redeemer => do
              let tagStr := match redeemer.tag with
                | .Spend => "Spend"
                | .Mint => "Mint"
                | .Cert => "Cert"
                | .Reward => "Reward"
              let redeemerDisplay := s!"  Redeemer: {tagStr} #{redeemer.index} (mem={redeemer.exUnits.mem}, steps={redeemer.exUnits.steps})"

              -- Empty input and connector for redeemer lines
              let emptyInput := String.join (List.replicate 17 " ")
              let emptyConnector := "│                  │"
              let redeemerParts : List StyledText :=
                [("│ ".style |> blue),
                 (emptyInput.style |> dim),
                 (emptyConnector.style |> magenta),
                 (" ".style),
                 (redeemerDisplay.style |> cyan)]
              concat redeemerParts

          println ((("└" ++ String.join (List.replicate 100 "─") ++ "┘").style |> blue))
          txNum := txNum + 1

/-- Fetch and display a block from a ChainSync RollForward header -/
def fetchAndDisplayBlock (sock : Socket) (header : Header) (tip : Tip)
    (chainDb : Option ChainDB := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none) : IO Bool := do
  -- Extract the actual block point from the header (not the tip)
  -- The headerBytes is tag24-wrapped: d8 18 <bytestring>
  -- Block hash in Cardano = blake2b_256(content inside tag24 bytestring)
  let rawHeaderBytes :=
    if header.headerBytes.size >= 2 &&
       header.headerBytes[0]! == 0xd8 && header.headerBytes[1]! == 0x18 then
      match Cleanode.Network.Cbor.decodeBytes (header.headerBytes.extract 2 header.headerBytes.size) with
      | some result => result.value
      | none => header.headerBytes  -- fallback
    else
      header.headerBytes
  let blockHash ← blake2b_256 rawHeaderBytes
  let blockSlot : UInt64 :=
    if header.era == 0 then
      match extractByronInfo header.headerBytes with
      | some info => UInt64.ofNat info.slot
      | none => tip.point.slot  -- fallback
    else
      match extractShelleyInfo header.headerBytes with
      | some info => UInt64.ofNat info.slot
      | none => tip.point.slot  -- fallback
  let blockPoint : Point := { slot := blockSlot, hash := blockHash }
  -- Deduplicate: atomically check-and-mark block as seen
  let blockNo := tip.blockNo.toNat
  let alreadySeen ← do
    if let some ref := seenBlocks then
      atomicCheckAndMark ref blockNo
    else
      pure false
  if alreadySeen then
    -- Block already claimed by another peer — still fetch to satisfy BlockFetch protocol
    match ← fetchBlock sock blockPoint with
    | .error _ => return false
    | _ => return true
  else
    match ← fetchBlock sock blockPoint with
    | .error e => do
        IO.println s!"✗ Failed to fetch block: {e}"
        return false
    | .ok none => do
        IO.println "✗ No block received"
        return false
    | .ok (some blockBytes) => do
        displayBlock header tip blockPoint blockBytes
        -- Store block in SQLite if ChainDB is available
        if let some cdb := chainDb then
          let slot := blockPoint.slot.toNat
          cdb.addBlock blockNo slot header.era blockHash blockPoint.hash header.headerBytes
          cdb.addBlockBody blockNo blockBytes
          cdb.saveSyncState slot blockNo blockHash
          IO.println s!"  💾 Block #{blockNo} stored in chain.db"
        return true

/-- Encode a KeepAlive response (MsgKeepAliveResponse) -/
def encodeKeepAliveResponse (cookie : UInt16) : ByteArray :=
  -- KeepAlive response: CBOR array [1, cookie] where 1 = MsgKeepAliveResponse
  -- [1, cookie] in CBOR: 82 01 19 XXXX (for 2-byte cookie)
  let cookieHi := UInt8.ofNat ((cookie >>> 8).toNat % 256)
  let cookieLo := UInt8.ofNat (cookie.toNat % 256)
  ⟨#[0x82, 0x01, 0x19, cookieHi, cookieLo]⟩

/-- Send a KeepAlive response back over the multiplexer -/
def sendKeepAliveResponse (sock : Socket) (cookie : UInt16) : IO Unit := do
  let payload := encodeKeepAliveResponse cookie
  let frame ← createFrame .KeepAlive .Initiator payload
  let _ ← socket_send sock (encodeMuxFrame frame)
  pure ()

/-- Extract cookie from KeepAlive payload: CBOR [0, cookie] -/
def extractKeepAliveCookie (payload : ByteArray) : Option UInt16 :=
  -- MsgKeepAlive = [0, cookie] in CBOR
  -- 82 00 19 HH LL  (cookie as 2-byte uint)
  -- 82 00 0X        (cookie as small uint)
  if payload.size >= 3 && payload[0]! == 0x82 && payload[1]! == 0x00 then
    if payload.size >= 5 && payload[2]! == 0x19 then
      some (UInt16.ofNat (payload[3]!.toNat * 256 + payload[4]!.toNat))
    else
      some (UInt16.ofNat payload[2]!.toNat)
  else
    none

/-- Shared peer discovery state (plain data only — safe across threads) -/
structure DiscoveryState where
  discovered : List (String × UInt16)  -- Queue of newly found peers
  known      : List (String × UInt16)  -- All peers we've seen

/-- Receive a ChainSync MUX frame, handling KeepAlive transparently.
    Uses exact reads: 8 bytes for header, then payloadLength bytes for payload.
    Skips TxSubmission2 frames and handles PeerSharing responses inline.
    Loops until a ChainSync frame arrives or an error occurs. -/
partial def receiveChainSyncFrame (sock : Socket)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    : IO (Except String MuxFrame) := do
  -- Read MUX header (exactly 8 bytes)
  match ← socket_receive_exact sock 8 with
  | .error e => return .error s!"Failed to receive MUX header: {e}"
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none => return .error "Failed to decode MUX header"
      | some header => do
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error s!"Failed to receive payload: {e}"
          | .ok payload => do
              if header.protocolId == .KeepAlive then
                -- Auto-respond to KeepAlive
                match extractKeepAliveCookie payload with
                | some cookie => _root_.sendKeepAliveResponse sock cookie
                | none => pure ()
                receiveChainSyncFrame sock discoveryRef
              else if header.protocolId == .TxSubmission2 then
                -- Respond to TxSubmission2 server requests (we have no txs to offer)
                match decodeTxSubmission2Message payload with
                | some (.MsgRequestTxIds blocking _ack _req) =>
                    -- Non-blocking: reply immediately with empty list (spec-compliant)
                    -- Blocking: don't reply — hold the request open (normal for empty mempool)
                    if !blocking then
                      let _ ← sendTxSubmission2 sock (.MsgReplyTxIds [])
                      pure ()
                | some (.MsgRequestTxs _) =>
                    let _ ← sendTxSubmission2 sock (.MsgReplyTxs [])
                    pure ()
                | _ => pure ()  -- MsgDone or decode failure
                receiveChainSyncFrame sock discoveryRef
              else if header.protocolId == .PeerSharing then
                -- Handle PeerSharing responses inline (non-blocking)
                match discoveryRef with
                | some dRef =>
                    match decodePeerSharingMessage payload with
                    | some (.MsgSharePeers peers) => do
                        let peerAddrs := peers.map fun p => (p.host, p.port)
                        if peerAddrs.length > 0 then
                          IO.println s!"  📡 Discovered {peerAddrs.length} peers via PeerSharing"
                          for (h, p) in peerAddrs do
                            IO.println s!"      → {h}:{p}"
                          dRef.modify fun ds =>
                            let newPeers := peerAddrs.filter fun p => !ds.known.any (· == p)
                            { ds with discovered := ds.discovered ++ newPeers }
                        -- Send MsgDone to cleanly close PeerSharing
                        let _ ← sendPeerSharing sock .MsgDone
                    | _ => pure ()  -- Ignore decode failures silently
                | none => pure ()
                receiveChainSyncFrame sock discoveryRef
              else
                return .ok { header := header, payload := payload }

/-- Result of the sync loop: why did it exit? -/
inductive SyncExit where
  | connectionLost (reason : String)  -- Recoverable: relay dropped us
  | protocolError (reason : String)   -- Not recoverable without reconnect
  | done                              -- Clean exit

/-- Continuous sync loop: requests blocks via ChainSync + BlockFetch.
    Returns a SyncExit indicating why it stopped. -/
partial def syncLoop (sock : Socket) (blockCount : Nat) (chainDb : Option ChainDB := none)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none) : IO SyncExit := do
  -- Request next block header
  match ← sendChainSync sock requestNext with
  | .error e =>
      return .connectionLost s!"Failed to send RequestNext: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef with
      | .error e =>
          return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | none => do
              return .protocolError "Failed to decode ChainSync message"
          | some (.MsgRollForward header tip) => do
              let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks
              if ok then
                syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks
              else
                return .protocolError "Failed to fetch block"
          | some (.MsgRollBackward rollbackPoint _) => do
              -- Dedup: only print once across peers (encode as slot + 1B offset)
              let shouldPrint ← do
                if let some ref := seenBlocks then
                  let dup ← atomicCheckAndMark ref (rollbackPoint.slot.toNat + 1_000_000_000)
                  pure (!dup)
                else pure true
              if shouldPrint then
                run do
                  concat [
                    ("↩ Rollback to slot ".style |> yellow),
                    (s!"{rollbackPoint.slot}".style |> yellow |> bold)
                  ]
              syncLoop sock blockCount chainDb discoveryRef seenBlocks
          | some (.MsgAwaitReply) => do
              -- Dedup: only print once across peers (encode as blockCount + 2B offset)
              let shouldPrint ← do
                if let some ref := seenBlocks then
                  let dup ← atomicCheckAndMark ref (blockCount + 2_000_000_000)
                  pure (!dup)
                else pure true
              if shouldPrint then
                run do
                  concat [
                    ("At tip -- waiting for new block...".style |> cyan |> dim)
                  ]
              -- Server will push MsgRollForward when a new block arrives
              -- Keep receiving, handling KeepAlive frames transparently
              match ← receiveChainSyncFrame sock discoveryRef with
              | .error e =>
                  return .connectionLost e
              | .ok frame => do
                  match decodeChainSyncMessage frame.payload with
                  | some (.MsgRollForward header tip) => do
                      let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks
                      if ok then
                        syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks
                      else
                        return .protocolError "Failed to fetch block"
                  | some (.MsgRollBackward rollbackPoint2 _) => do
                      let shouldPrint2 ← do
                        if let some ref := seenBlocks then
                          let dup ← atomicCheckAndMark ref (rollbackPoint2.slot.toNat + 1_000_000_000)
                          pure (!dup)
                        else pure true
                      if shouldPrint2 then
                        run do
                          concat [
                            ("↩ Rollback to slot ".style |> yellow),
                            (s!"{rollbackPoint2.slot}".style |> yellow |> bold)
                          ]
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks
                  | some other => do
                      IO.println s!"Unexpected message: {repr other}"
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks
                  | none =>
                      return .protocolError "Failed to decode ChainSync message"
          | some other => do
              IO.println s!"Unexpected message: {repr other}"
              syncLoop sock blockCount chainDb

/-- Connect, handshake, find tip, intersect, and enter sync loop.
    Returns a SyncExit indicating why the session ended. -/
def connectAndSync (host : String) (port : UInt16) (proposal : HandshakeMessage)
    (chainDb : ChainDB) : IO SyncExit := do
  match ← socket_connect host port with
  | .error e =>
      return .connectionLost s!"Connection failed: {e}"
  | .ok sock => do
      -- Handshake
      match ← sendHandshake sock proposal with
      | .error e =>
          socket_close sock
          return .connectionLost s!"Failed to send handshake: {e}"
      | .ok _ => do
          match ← socket_receive sock 1024 with
          | .error e =>
              socket_close sock
              return .connectionLost s!"Failed to receive handshake: {e}"
          | .ok rawData => do
              match decodeMuxFrame rawData >>= fun f => decodeHandshakeMessage f.payload with
              | none =>
                  socket_close sock
                  return .protocolError "Failed to decode handshake response"
              | some msg => do
                  IO.println s!"  ✓ Handshake: {repr msg}"

                  -- Find chain tip
                  match ← sendChainSync sock findIntersectTip with
                  | .error e =>
                      socket_close sock
                      return .connectionLost s!"Failed to send FindIntersect: {e}"
                  | .ok _ => do
                      match ← socket_receive sock 8192 with
                      | .error e =>
                          socket_close sock
                          return .connectionLost s!"Failed to receive tip: {e}"
                      | .ok rawData => do
                          match decodeMuxFrame rawData >>= fun f => decodeChainSyncMessage f.payload with
                          | some (.MsgIntersectNotFound tip) => do
                              IO.println s!"  ✓ Chain tip at slot {tip.point.slot}"

                              -- Intersect at tip
                              match ← sendChainSync sock (findIntersectAt tip.point) with
                              | .error e =>
                                  socket_close sock
                                  return .connectionLost s!"Failed to intersect: {e}"
                              | .ok _ => do
                                  match ← socket_receive sock 8192 with
                                  | .error e =>
                                      socket_close sock
                                      return .connectionLost s!"Failed to receive intersection: {e}"
                                  | .ok rawData => do
                                      match decodeMuxFrame rawData >>= fun f => decodeChainSyncMessage f.payload with
                                      | some (.MsgIntersectFound point _) => do
                                          IO.println s!"  ✓ Intersected at slot {point.slot}"
                                          let result ← syncLoop sock 0 (some chainDb)
                                          socket_close sock
                                          return result
                                      | _ =>
                                          socket_close sock
                                          return .protocolError "Failed to intersect at tip"
                          | _ =>
                              socket_close sock
                              return .protocolError "Failed to find chain tip"

/-- Reconnection loop: keeps reconnecting on connection loss with backoff -/
partial def reconnectLoop (host : String) (port : UInt16) (proposal : HandshakeMessage)
    (networkName : String) (chainDb : ChainDB) (attempt : Nat := 0) : IO Unit := do
  if attempt > 0 then
    let delaySec := min (attempt * 5) 30  -- 5s, 10s, 15s, ... capped at 30s
    run do
      concat [
        ("⟳ Reconnecting in ".style |> yellow),
        (s!"{delaySec}s".style |> yellow |> bold),
        (s!" (attempt {attempt + 1})...".style |> yellow |> dim)
      ]
    IO.sleep (UInt32.ofNat (delaySec * 1000))
  else
    run do
      println (("=== Following Chain Tip (Ctrl+C to stop) ===".style |> cyan |> bold))

  match ← connectAndSync host port proposal chainDb with
  | .connectionLost reason => do
      run do
        concat [
          ("⚡ Connection lost: ".style |> yellow),
          (reason.style |> yellow |> dim)
        ]
      reconnectLoop host port proposal networkName chainDb (attempt + 1)
  | .protocolError reason => do
      run do
        concat [
          ("✗ Protocol error: ".style |> red |> bold),
          (reason.style |> red)
        ]
      -- Protocol errors are also worth retrying — the relay might behave differently
      reconnectLoop host port proposal networkName chainDb (attempt + 1)
  | .done => pure ()

def testBlockFetch (host : String) (port : UInt16) (proposal : HandshakeMessage) (networkName : String) : IO Unit := do
  -- Open ChainDB once — persists across reconnections
  let chainDb ← ChainDB.open {}
  IO.println "  💾 Chain database opened (data/chain.db)"
  reconnectLoop host port proposal networkName chainDb
  chainDb.close

/-- Receive a MUX frame using exact reads (8-byte header + payload) -/
def receiveMuxFrameExact (sock : Socket) : IO (Except String MuxFrame) := do
  match ← socket_receive_exact sock 8 with
  | .error e => return .error s!"Failed to receive MUX header: {e}"
  | .ok headerBytes =>
      match decodeMuxHeader headerBytes with
      | none => return .error "Failed to decode MUX header"
      | some header => do
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error s!"Failed to receive payload: {e}"
          | .ok payload =>
              return .ok { header := header, payload := payload }


/-- After handshake succeeds: find tip, intersect, and sync.
    Separated to reduce nesting depth (avoids LLVM optimizer crash). -/
def peerFindTipAndSync (sock : Socket) (addrStr : String) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState))
    (seenBlocks : Option (IO.Ref (Option (List Nat)))) : IO SyncExit := do
  -- Find chain tip
  match ← sendChainSync sock findIntersectTip with
  | .error e => return .connectionLost s!"Failed to query tip: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef with
      | .error e => return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | some (.MsgIntersectNotFound tip) => do
              IO.println s!"  ✓ Peer {addrStr}: tip at slot {tip.point.slot}"
              -- Intersect at tip
              match ← sendChainSync sock (findIntersectAt tip.point) with
              | .error e => return .connectionLost s!"Failed to intersect: {e}"
              | .ok _ => do
                  match ← receiveChainSyncFrame sock discoveryRef with
                  | .error e => return .connectionLost e
                  | .ok frame2 => do
                      match decodeChainSyncMessage frame2.payload with
                      | some (.MsgIntersectFound point _) => do
                          IO.println s!"  ✓ Peer {addrStr}: intersected at slot {point.slot}"
                          syncLoop sock 0 (some chainDb) discoveryRef seenBlocks
                      | _ => return .protocolError "Failed to intersect"
          | _ => return .protocolError "Failed to find tip"

/-- Connect to a peer, handshake, find tip, intersect, and sync.
    Returns a SyncExit indicating why it stopped. -/
def peerConnectAndSync (host : String) (port : UInt16) (addrStr : String)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none) : IO SyncExit := do
  match ← socket_connect host port with
  | .error e => return .connectionLost s!"Connection failed: {e}"
  | .ok sock => do
      -- Handshake
      match ← sendHandshake sock proposal with
      | .error e =>
          socket_close sock
          return .connectionLost s!"Handshake send failed: {e}"
      | .ok _ => do
          match ← socket_receive sock 1024 with
          | .error e =>
              socket_close sock
              return .connectionLost s!"Handshake recv failed: {e}"
          | .ok rawData => do
              match decodeMuxFrame rawData >>= fun f => decodeHandshakeMessage f.payload with
              | none =>
                  socket_close sock
                  return .protocolError "Handshake decode failed"
              | some hsMsg => do
                  let peerSharingEnabled := match hsMsg with
                    | .AcceptVersion _ vd => vd.peerSharing == 1
                    | _ => false
                  match hsMsg with
                  | .AcceptVersion vn vd =>
                      IO.println s!"  🤝 Peer {addrStr}: version {vn.value}, peerSharing={vd.peerSharing}, diffusion={vd.initiatorAndResponderDiffusionMode}"
                  | _ =>
                      IO.println s!"  🤝 Peer {addrStr}: handshake response: {repr hsMsg}"
                  let _ ← sendTxSubmission2 sock .MsgInit
                  if peerSharingEnabled && discoveryRef.isSome then
                    IO.println s!"  📡 Peer {addrStr}: PeerSharing enabled, requesting peers..."
                    let _ ← sendPeerSharing sock (.MsgShareRequest 10)
                  let result ← peerFindTipAndSync sock addrStr chainDb discoveryRef seenBlocks
                  socket_close sock
                  return result

/-- Per-peer reconnection loop: connects, syncs, and reconnects on failure with backoff. -/
partial def peerReconnectLoop (host : String) (port : UInt16)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (attempt : Nat := 0) : IO Unit := do
  let addrStr := s!"{host}:{port}"
  if attempt > 0 then
    let delaySec := min (attempt * 5) 30
    IO.println s!"  ⟳ Peer {addrStr}: reconnecting in {delaySec}s (attempt {attempt + 1})..."
    IO.sleep (UInt32.ofNat (delaySec * 1000))

  match ← peerConnectAndSync host port addrStr proposal chainDb discoveryRef seenBlocks with
  | .connectionLost reason => do
      IO.println s!"  ⚡ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks (attempt + 1)
  | .protocolError reason => do
      IO.println s!"  ✗ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks (attempt + 1)
  | .done => pure ()

/-- Peer spawner loop: drains discovered peers and spawns new connection tasks -/
partial def peerSpawnerLoop (discoveryRef : IO.Ref DiscoveryState)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (maxPeers : Nat := 20) : IO Unit := do
  IO.sleep 30000  -- Check every 30 seconds
  let newPeers ← discoveryRef.modifyGet fun ds =>
    (ds.discovered, { ds with discovered := [], known := ds.known ++ ds.discovered })
  if newPeers.isEmpty then
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks maxPeers
  else do
    let knownCount := (← discoveryRef.get).known.length
    let budget := if knownCount >= maxPeers then 0 else maxPeers - knownCount
    let toSpawn := newPeers.take budget
    for (host, port) in toSpawn do
      IO.println s!"  🔗 Connecting to discovered peer {host}:{port}"
      let _ ← IO.asTask (do
        try
          peerReconnectLoop host port proposal chainDb (some discoveryRef) seenBlocks
        catch e =>
          IO.println s!"  ✗ Discovered peer {host}:{port}: {e}")
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks maxPeers

/-- Multi-peer relay node mode -/
def relayNode (proposal : HandshakeMessage) (networkName : String) : IO Unit := do
  run do
    println (("Dion: a Cardano LEAN 4 Relay Node".style |> cyan |> bold))
    println (("====================================".style |> cyan))
    println ("".style)
    println ((s!"Network: {networkName}".style |> blue))
    println ("".style)

  -- Open ChainDB
  let chainDb ← ChainDB.open {}
  IO.println "  💾 Chain database opened (data/chain.db)"

  -- Build topology from bootstrap peers
  let bootstrapPeers := match networkName with
    | "Mainnet" => mainnetBootstrapPeers
    | "Preprod" => preprodBootstrapPeers
    | "Preview" => previewBootstrapPeers
    | _ => mainnetBootstrapPeers

  -- Add curated relay peers (from Cardano peer snapshot — real SPO relays)
  let relayPeers := match networkName with
    | "Mainnet" => mainnetRelayPeers
    | _ => []  -- Only mainnet has curated relays for now
  -- Merge: bootstrap peers + relay peers
  let peerAddrs := bootstrapPeers ++ relayPeers

  IO.println s!"  📡 Bootstrap peers: {bootstrapPeers.length}"
  if relayPeers.length > 0 then
    IO.println s!"  🌐 Relay peers (from peer snapshot): {relayPeers.length}"
  IO.println s!"  📡 Total initial peers: {peerAddrs.length}"

  -- Shared peer discovery state (plain data, safe across threads)
  let discoveryRef ← IO.mkRef ({
    discovered := []
    known := peerAddrs
  } : DiscoveryState)

  -- Shared dedup: tracks block numbers already displayed, prevents duplicate output
  let seenBlocksRef ← IO.mkRef (some ([] : List Nat))

  run do
    println ("".style)
    println (("=== Relay Node Active (Ctrl+C to stop) ===".style |> cyan |> bold))

  -- Launch per-peer reconnect loops — each task owns its connection lifecycle
  let mut tasks : List (Task (Except IO.Error Unit)) := []
  for (host, port) in peerAddrs do
    let task ← IO.asTask (do
      try
        peerReconnectLoop host port proposal chainDb (some discoveryRef) (some seenBlocksRef)
      catch e =>
        IO.println s!"  ✗ Peer {host}:{port}: {e}")
    tasks := tasks ++ [task]

  -- Launch peer discovery spawner
  let spawnerTask ← IO.asTask (do
    try
      peerSpawnerLoop discoveryRef proposal chainDb (some seenBlocksRef)
    catch e =>
      IO.println s!"  ✗ Peer spawner: {e}")
  tasks := tasks ++ [spawnerTask]

  IO.println s!"  Syncing from {peerAddrs.length} bootstrap peers (discovering more)...\n"

  -- Wait for all tasks (they reconnect indefinitely until Ctrl+C)
  for task in tasks do
    let _ ← IO.wait task

  chainDb.close

def main : IO Unit := do
  -- ============================================
  -- Network Configuration
  -- ============================================
  -- Uncomment ONE of the following network configurations:

  -- MAINNET (12.8M+ blocks - slow to sync)
  let proposal := createMainnetProposal
  let networkName := "Mainnet"

  -- PREPROD (smaller testnet - faster to sync)
  -- let proposal := createPreprodProposal
  -- let networkName := "Preprod"

  -- PREVIEW (smallest testnet - fastest to sync)
  -- let proposal := createPreviewProposal
  -- let networkName := "Preview"

  -- ============================================

  -- Run as multi-peer relay node
  relayNode proposal networkName
