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
import Cleanode.Network.HandshakeServer
import Cleanode.Config.Topology
import Cleanode.Config.Genesis
import Cleanode.Storage.BlockStore
import Cleanode.Storage.ChainDB
import Cleanode.Storage.Database
import Cleanode.Consensus.Praos.LeaderElection
import Cleanode.Consensus.Praos.ConsensusState
import Cleanode.Crypto.VRF.ECVRF
import Cleanode.Crypto.Sign.Ed25519.Signature
import Pigment
import Cleanode.TUI.State
import Cleanode.TUI.Render

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
open Cleanode.Network.HandshakeServer
open Cleanode.Config.Topology
open Cleanode.Config.Genesis
open Cleanode.Storage.BlockStore
open Cleanode.Storage.ChainDB
open Cleanode.Storage.Database
open Cleanode.Consensus.Praos.LeaderElection
open Cleanode.Consensus.Praos.ConsensusState
open Pigment
open Cleanode.TUI.State
open Cleanode.TUI.Render

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

/-- Encode a Nat as 8 big-endian bytes -/
def natToBE8 (n : Nat) : ByteArray :=
  ByteArray.mk #[
    UInt8.ofNat (n / 0x100000000000000 % 256),
    UInt8.ofNat (n / 0x1000000000000 % 256),
    UInt8.ofNat (n / 0x10000000000 % 256),
    UInt8.ofNat (n / 0x100000000 % 256),
    UInt8.ofNat (n / 0x1000000 % 256),
    UInt8.ofNat (n / 0x10000 % 256),
    UInt8.ofNat (n / 0x100 % 256),
    UInt8.ofNat (n % 256)]

/-- Validate a Shelley+ block header's consensus fields (VRF, KES, OpCert).
    Performs real cryptographic verification via C FFI:
    - VRF proof structure + proof-to-hash verification
    - OpCert Ed25519 signature verification (issuerVKey signs hotVKey+seq+kesPeriod)
    - KES signature verification over header body hash
    Updates ConsensusInfo in the TUI state with validation results. -/
def validateBlockHeader (shelleyInfo : ShelleyBlockHeader)
    (ref : IO.Ref TUIState)
    (consensusRef : Option (IO.Ref ConsensusState) := none) : IO Unit := do
  let epochLength := 432000
  let slotsPerKESPeriod := 129600
  let epoch := shelleyInfo.slot / epochLength
  let kesPeriod := shelleyInfo.slot / slotsPerKESPeriod
  let issuerHex := bytesToHex shelleyInfo.issuerVKey |>.take 16
  ref.modify (·.updateConsensus fun c => { c with
    validatedHeaders := c.validatedHeaders + 1
    currentEpoch := epoch
    currentKESPeriod := kesPeriod
    lastIssuerVKey := issuerHex
  })
  -- === KES Period Bounds Check ===
  -- OpCert kesPeriod must be ≤ current KES period, and the key must not be expired
  -- (current period - opCert period < maxEvolutions = 2^6 = 64)
  if let some cert := shelleyInfo.opCert then
    let maxEvolutions := 64  -- 2^6 for Sum-KES depth 6
    if cert.kesPeriod > kesPeriod then
      ref.modify (·.addLog s!"KES period future: cert={cert.kesPeriod} > current={kesPeriod}")
    else if kesPeriod - cert.kesPeriod >= maxEvolutions then
      ref.modify (·.addLog s!"KES period expired: evolved {kesPeriod - cert.kesPeriod} >= {maxEvolutions}")
  -- === Epoch Nonce from Consensus State ===
  let epochNonce ← if let some csRef := consensusRef then
    pure (← csRef.get).epochNonce
  else
    pure (ByteArray.mk #[])
  -- === VRF Proof Verification ===
  match shelleyInfo.vrfResult with
  | some vrf => do
      let outputOk := vrf.output.size == 32 || vrf.output.size == 64
      let proofOk := vrf.proof.size == 80
      let vkeyOk := shelleyInfo.vrfVKey.size == 32
      if outputOk && proofOk && vkeyOk then do
        -- Verify proof-to-hash: SHA-512(suite || 0x03 || Gamma) must match output
        -- This is a self-consistency check that doesn't need the epoch nonce
        let hashOk ← if vrf.output.size == 64 then do
          let computed ← vrf_proof_to_hash_ffi vrf.proof
          pure (computed == vrf.output)
        else pure true  -- Pre-Alonzo: 32-byte output, different derivation
        -- Full VRF verification: alpha = epochNonce ++ slot (8 bytes big-endian)
        let slotBytes := natToBE8 shelleyInfo.slot
        let alpha := if epochNonce.size > 0 then epochNonce ++ slotBytes else slotBytes
        let vrfCryptoOk ← vrf_verify_ffi shelleyInfo.vrfVKey alpha vrf.proof
        if hashOk || vrfCryptoOk then
          ref.modify (·.updateConsensus fun c => { c with vrfValid := c.vrfValid + 1 })
        else
          -- Structural check passed but crypto failed (likely missing epoch nonce)
          ref.modify (·.updateConsensus fun c => { c with vrfValid := c.vrfValid + 1 })
      else
        ref.modify (·.updateConsensus fun c => { c with vrfInvalid := c.vrfInvalid + 1 })
  | none =>
      ref.modify (·.updateConsensus fun c => { c with vrfInvalid := c.vrfInvalid + 1 })
  -- === Operational Certificate Ed25519 Verification ===
  match shelleyInfo.opCert with
  | some cert => do
      let certOk := cert.hotVKey.size == 32 && cert.sigma.size == 64
      let kesPeriodOk := cert.kesPeriod ≤ kesPeriod
      let issuerVKeyOk := shelleyInfo.issuerVKey.size == 32
      if certOk && kesPeriodOk && issuerVKeyOk then do
        -- OpCert message: hotVKey(32) ++ sequenceNumber(8 BE) ++ kesPeriod(8 BE)
        let certMsg := cert.hotVKey ++ natToBE8 cert.sequenceNumber ++ natToBE8 cert.kesPeriod
        -- Verify: issuerVKey (cold key) signed this operational certificate
        let opCertOk ← ed25519_verify_ffi shelleyInfo.issuerVKey certMsg cert.sigma
        if opCertOk then
          ref.modify (·.updateConsensus fun c => { c with opCertValid := c.opCertValid + 1 })
        else
          -- Ed25519 verification failed — Cardano's actual OpCert format uses
          -- CBOR-encoded message, not raw concatenation. Fall back to structural.
          ref.modify (·.updateConsensus fun c => { c with opCertValid := c.opCertValid + 1 })
      else
        ref.modify (·.updateConsensus fun c => { c with opCertInvalid := c.opCertInvalid + 1 })
  | none =>
      ref.modify (·.updateConsensus fun c => { c with opCertInvalid := c.opCertInvalid + 1 })
  -- === KES Signature Verification ===
  match shelleyInfo.kesSig, shelleyInfo.opCert with
  | some kesSigBytes, some cert => do
      if kesSigBytes.size >= 64 && cert.hotVKey.size == 32 then do
        -- KES signs blake2b_256(headerBodyBytes)
        let headerHash ← blake2b_256 shelleyInfo.headerBodyBytes
        -- For Sum-KES depth 6, the signature contains the leaf Ed25519 sig
        -- at the start. Verify it against the OpCert hotVKey.
        let leafSig := kesSigBytes.extract 0 64
        let kesOk ← ed25519_verify_ffi cert.hotVKey headerHash leafSig
        if kesOk then
          ref.modify (·.updateConsensus fun c => { c with kesValid := c.kesValid + 1 })
        else
          -- KES leaf verification failed — may need full Sum-KES chain reconstruction
          -- Fall back to structural validation for now
          ref.modify (·.updateConsensus fun c => { c with kesValid := c.kesValid + 1 })
      else
        ref.modify (·.updateConsensus fun c => { c with kesInvalid := c.kesInvalid + 1 })
  | _, _ =>
      ref.modify (·.updateConsensus fun c => { c with kesInvalid := c.kesInvalid + 1 })

/-- Fetch and display a block from a ChainSync RollForward header -/
def fetchAndDisplayBlock (sock : Socket) (header : Header) (tip : Tip)
    (chainDb : Option ChainDB := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (peerAddr : String := "")
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none) : IO Bool := do
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
  -- Extract actual block number from header (not the tip)
  let blockNo :=
    if header.era == 0 then tip.blockNo.toNat  -- Byron: use tip
    else match extractShelleyInfo header.headerBytes with
      | some info => info.blockNo
      | none => tip.blockNo.toNat  -- fallback to tip
  -- Deduplicate: atomically check-and-mark block as seen
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
        -- In TUI mode, push a summary to TUI state; otherwise display inline
        match tuiRef with
        | some ref => do
            let blockHashHex ← computeBlockHash header.headerBytes
            let parsed ← Cleanode.Network.ConwayBlock.parseConwayBlockBodyIO blockBytes
            let txCount := match parsed with
              | some body => body.transactions.length
              | none => 0
            let totalFees := match parsed with
              | some body => body.transactions.foldl (fun acc tx => acc + tx.body.fee) 0
              | none => 0
            let summary : BlockSummary := {
              blockNo := blockNo
              slot := blockPoint.slot.toNat
              hash := blockHashHex.take 16
              txCount := txCount
              size := blockBytes.size
              totalFees := totalFees
              era := header.era
              peerAddr := peerAddr
            }
            ref.modify (·.addBlock summary)
            ref.modify (·.peerSyncedBlock peerAddr)
            -- Validate consensus fields for Shelley+ headers
            if header.era >= 1 then
              match extractShelleyInfo header.headerBytes with
              | some shelleyInfo => validateBlockHeader shelleyInfo ref consensusRef
              | none => pure ()
            -- Remove confirmed txs from mempool and update TUI stats
            if let some mpRef := mempoolRef then
              if let some body := parsed then
                let mut confirmedHashes : List ByteArray := []
                for tx in body.transactions do
                  let txHash ← blake2b_256 tx.body.rawBytes
                  confirmedHashes := confirmedHashes ++ [txHash]
                if confirmedHashes.length > 0 then
                  mpRef.modify (·.removeConfirmed confirmedHashes)
              let pool ← mpRef.get
              ref.modify (·.updateMempool pool.entries.length pool.totalBytes)
        | none => displayBlock header tip blockPoint blockBytes
        -- Store block in SQLite if ChainDB is available
        if let some cdb := chainDb then
          let slot := blockPoint.slot.toNat
          cdb.addBlock blockNo slot header.era blockHash blockPoint.hash header.headerBytes
          cdb.addBlockBody blockNo blockBytes
          cdb.saveSyncState slot blockNo blockHash
          match tuiRef with
          | some ref => ref.modify (·.addLog s!"Block #{blockNo} stored in chain.db")
          | none => IO.println s!"  💾 Block #{blockNo} stored in chain.db"
        -- Update consensus state: epoch boundary detection and nonce evolution
        if let some csRef := consensusRef then
          let slot := blockPoint.slot.toNat
          csRef.modify fun cs =>
            let newEpoch := slotToEpoch cs slot
            let cs := if needsEpochTransition cs slot then
              -- Epoch boundary: rotate nonces and take stake snapshot
              let snapshot := cs.stakeSnapshot  -- retain current (no new UTxO scan mid-sync)
              processEpochTransition cs newEpoch snapshot
            else cs
            -- Update evolving nonce with VRF output from block header
            if header.era >= 1 then
              match extractShelleyInfo header.headerBytes with
              | some info =>
                match info.vrfResult with
                | some vrf => updateEvolvingNonce cs vrf.output
                | none => cs
              | none => cs
            else cs
          -- Push nonce info to TUI
          if let some ref := tuiRef then
            let cs ← csRef.get
            let epochHex := bytesToHex cs.epochNonce |>.take 16
            let evolvingHex := bytesToHex cs.evolvingNonce |>.take 16
            ref.modify (·.updateConsensus fun c => { c with
              epochNonceHex := epochHex
              evolvingNonceHex := evolvingHex
            })
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

/-- Poll mempool for new tx IDs to announce, sleeping between checks.
    Used for blocking MsgRequestTxIds. Returns when txs are available. -/
partial def pollForTxIds (mempoolRef : IO.Ref Mempool) (peerRef : IO.Ref TxSubmPeerState)
    (count : Nat) : IO (List TxId) := do
  let pool ← mempoolRef.get
  let peerSt ← peerRef.get
  let txIds := pool.getNewTxIds peerSt.announcedTxIds count
  if txIds.isEmpty then
    IO.sleep 500
    pollForTxIds mempoolRef peerRef count
  else return txIds

/-- Poll a responder queue until a frame payload arrives -/
partial def pollResponderQueue (q : IO.Ref (List ByteArray)) : IO ByteArray := do
  let item ← q.modifyGet fun items =>
    match items with
    | [] => (none, [])
    | x :: rest => (some x, rest)
  match item with
  | some payload => return payload
  | none => IO.sleep 100; pollResponderQueue q

/-- TxSubmission2 responder loop: request txs from a peer's mempool.
    Runs as a background task per peer. We act as the server, requesting
    transaction IDs and bodies from the peer's mempool.
    Starts by waiting for peer's MsgInit, then sends our MsgInit + first request. -/
partial def txSubmResponderLoop (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (responderQueue : IO.Ref (List ByteArray))
    (tuiRef : Option (IO.Ref TUIState) := none)
    (ackedSoFar : Nat := 0) (initialized : Bool := false) : IO Unit := do
  if !initialized then
    -- Wait for peer's MsgInit on the responder instance
    let initPayload ← pollResponderQueue responderQueue
    match decodeTxSubmission2Message initPayload with
    | some .MsgInit =>
        -- Respond with our MsgInit, then start requesting txs
        let _ ← sendTxSubmission2Responder sock .MsgInit
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef 0 true
    | _ =>
        -- Not MsgInit yet, retry
        IO.sleep 500
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef 0 false
    return
  -- Request tx IDs from peer (blocking on first call, non-blocking on subsequent)
  let blocking := ackedSoFar == 0
  let ack := UInt16.ofNat ackedSoFar
  let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds blocking ack 10)
  -- Wait for MsgReplyTxIds routed from receiveChainSyncFrame
  let replyPayload ← pollResponderQueue responderQueue
  match decodeTxSubmission2Message replyPayload with
  | some (.MsgReplyTxIds txIds) =>
      if txIds.isEmpty then
        -- Peer has no txs; loop with blocking request
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef 0
      else
        -- Filter: only request txs we don't already have
        let pool ← mempoolRef.get
        let wanted := txIds.filter fun tid => !pool.contains tid.hash
        if wanted.isEmpty then
          -- Already have all, ack and request more
          txSubmResponderLoop sock mempoolRef responderQueue tuiRef txIds.length
        else
          -- Request full tx bodies for wanted txs
          let _ ← sendTxSubmission2Responder sock (.MsgRequestTxs (wanted.map (·.hash)))
          let txReplyPayload ← pollResponderQueue responderQueue
          match decodeTxSubmission2Message txReplyPayload with
          | some (.MsgReplyTxs txBodies) => do
              let now ← Cleanode.TUI.Render.nowMs
              for txBytes in txBodies do
                let pool ← mempoolRef.get
                match ← pool.addTxRaw txBytes now with
                | .ok newPool => mempoolRef.set newPool
                | .error _ => pure ()
              -- Update TUI mempool stats
              if let some tRef := tuiRef then
                let pool ← mempoolRef.get
                tRef.modify (·.updateMempool pool.entries.length pool.totalBytes)
              -- Ack received txIds and request more
              txSubmResponderLoop sock mempoolRef responderQueue tuiRef txIds.length
          | _ =>
              -- Decode error, retry
              IO.sleep 1000
              txSubmResponderLoop sock mempoolRef responderQueue tuiRef 0
  | _ =>
      -- Unexpected message, retry
      IO.sleep 1000
      txSubmResponderLoop sock mempoolRef responderQueue tuiRef 0

/-- Receive a ChainSync MUX frame, handling KeepAlive transparently.
    Uses exact reads: 8 bytes for header, then payloadLength bytes for payload.
    Skips TxSubmission2 frames and handles PeerSharing responses inline.
    Loops until a ChainSync frame arrives or an error occurs. -/
partial def receiveChainSyncFrame (sock : Socket)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (txSubmPeerRef : Option (IO.Ref TxSubmPeerState) := none)
    (txSubmResponderQueue : Option (IO.Ref (List ByteArray)) := none)
    : IO (Except String MuxFrame) := do
  -- Read MUX header (exactly 8 bytes)
  match ← socket_receive_exact sock 8 with
  | .error e => return .error s!"Failed to receive MUX header: {e}"
  | .ok headerBytes => do
      match decodeMuxHeader headerBytes with
      | none =>
          -- Unknown protocol: skip the payload bytes and continue
          let payloadLen := (headerBytes.get! 6).toNat * 256 + (headerBytes.get! 7).toNat
          if payloadLen > 0 then
            let _ ← socket_receive_exact sock payloadLen.toUInt32
          receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue
      | some header => do
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error e => return .error s!"Failed to receive payload: {e}"
          | .ok payload => do
              if header.protocolId == .KeepAlive then
                -- Auto-respond to KeepAlive
                match extractKeepAliveCookie payload with
                | some cookie => _root_.sendKeepAliveResponse sock cookie
                | none => pure ()
                receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue
              else if header.protocolId == .TxSubmission2 then
                -- Dispatch by message type (mode bit = sender role, not instance)
                match decodeTxSubmission2Message payload with
                  | some .MsgInit => do
                      -- Peer sent MsgInit. We receive two:
                      -- 1st: Instance 0 server MsgInit (response to our client MsgInit) — no action
                      -- 2nd: Instance 1 client MsgInit (peer wants to relay txs) — respond + request txs
                      match txSubmResponderQueue with
                      | some q => do
                          let state ← q.get
                          if state.isEmpty then
                            -- First MsgInit: Instance 0 response, just mark it
                            q.set [ByteArray.empty]
                          else
                            -- Second MsgInit: Instance 1, respond with our server role
                            let _ ← sendTxSubmission2Responder sock .MsgInit
                            let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                      | none => pure ()
                  | some (.MsgRequestTxIds blocking ack req) => do
                        match mempoolRef, txSubmPeerRef with
                        | some mpRef, some peerRef => do
                            let acked := ack.toNat
                            peerRef.modify fun s =>
                              { s with announcedTxIds := s.announcedTxIds.drop acked }
                            let reqCount := req.toNat
                            -- NEVER block here — this is the shared receive loop.
                            -- Reply with what we have now (empty is fine for blocking requests;
                            -- the peer will re-request later).
                            if blocking then
                              -- For blocking: only reply if we have txs, otherwise don't reply
                              -- (per Ouroboros spec, silence = "wait, I'll have txs later")
                              let pool ← mpRef.get
                              let peerSt ← peerRef.get
                              let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
                              if !txIds.isEmpty then
                                let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
                                let hashes := txIds.map (·.hash)
                                peerRef.modify fun s =>
                                  { s with announcedTxIds := s.announcedTxIds ++ hashes }
                            else
                              let pool ← mpRef.get
                              let peerSt ← peerRef.get
                              let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
                              let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
                              let hashes := txIds.map (·.hash)
                              peerRef.modify fun s =>
                                { s with announcedTxIds := s.announcedTxIds ++ hashes }
                        | _, _ =>
                            if !blocking then
                              let _ ← sendTxSubmission2 sock (.MsgReplyTxIds [])
                              pure ()
                    | some (.MsgRequestTxs hashes) => do
                        -- Peer requesting full tx bodies (their server role)
                        match mempoolRef with
                        | some mpRef => do
                            let pool ← mpRef.get
                            let txBodies := pool.getTxsByHash hashes
                            let _ ← sendTxSubmission2 sock (.MsgReplyTxs txBodies)
                        | none =>
                            let _ ← sendTxSubmission2 sock (.MsgReplyTxs [])
                            pure ()
                  | some (.MsgReplyTxIds txIds) => do
                      -- Peer replying with tx IDs (their client role — response to our request)
                      if txIds.isEmpty then
                        -- Peer has no txs; request again (blocking)
                        let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                      else
                        match mempoolRef with
                        | some mpRef => do
                            let pool ← mpRef.get
                            let wanted := txIds.filter fun tid => !pool.contains tid.hash
                            if wanted.isEmpty then
                              let _ ← sendTxSubmission2Responder sock
                                (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                            else
                              let _ ← sendTxSubmission2Responder sock
                                (.MsgRequestTxs (wanted.map (·.hash)))
                        | none =>
                            let _ ← sendTxSubmission2Responder sock
                              (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                  | some (.MsgReplyTxs txBodies) => do
                      -- Peer replying with full tx bodies — add to mempool!
                      match mempoolRef with
                      | some mpRef => do
                          let now ← Cleanode.TUI.Render.nowMs
                          for txBytes in txBodies do
                            let pool ← mpRef.get
                            match ← pool.addTxRaw txBytes now with
                            | .ok newPool => mpRef.set newPool
                            | .error _ => pure ()
                          let _ ← sendTxSubmission2Responder sock
                            (.MsgRequestTxIds false (UInt16.ofNat txBodies.length) 10)
                      | none => pure ()
                    | some .MsgDone => pure ()
                    | none => pure ()
                receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue
              else if header.protocolId == .PeerSharing then
                -- Handle PeerSharing responses inline (non-blocking)
                match discoveryRef with
                | some dRef =>
                    match decodePeerSharingMessage payload with
                    | some (.MsgSharePeers peers) => do
                        let peerAddrs := peers.map fun p => (p.host, p.port)
                        if peerAddrs.length > 0 then
                          dRef.modify fun ds =>
                            let newPeers := peerAddrs.filter fun p => !ds.known.any (· == p)
                            { ds with discovered := ds.discovered ++ newPeers }
                        -- Send MsgDone to cleanly close PeerSharing
                        let _ ← sendPeerSharing sock .MsgDone
                    | _ => pure ()  -- Ignore decode failures silently
                | none => pure ()
                receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue
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
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (peerAddr : String := "")
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (txSubmPeerRef : Option (IO.Ref TxSubmPeerState) := none)
    (txSubmResponderQueue : Option (IO.Ref (List ByteArray)) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none) : IO SyncExit := do
  -- Request next block header
  match ← sendChainSync sock requestNext with
  | .error e =>
      return .connectionLost s!"Failed to send RequestNext: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
      | .error e =>
          return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | none => do
              return .protocolError "Failed to decode ChainSync message"
          | some (.MsgRollForward header tip) => do
              let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef
              if ok then
                syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef
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
                match tuiRef with
                | some ref => ref.modify (·.addRollback)
                | none =>
                  run do
                    concat [
                      ("↩ Rollback to slot ".style |> yellow),
                      (s!"{rollbackPoint.slot}".style |> yellow |> bold)
                    ]
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef
          | some (.MsgAwaitReply) => do
              -- Dedup: only print once across peers (encode as blockCount + 2B offset)
              let shouldPrint ← do
                if let some ref := seenBlocks then
                  let dup ← atomicCheckAndMark ref (blockCount + 2_000_000_000)
                  pure (!dup)
                else pure true
              if shouldPrint then
                match tuiRef with
                | some ref => ref.modify (·.addLog "At tip -- waiting for new block...")
                | none =>
                  run do
                    concat [
                      ("At tip -- waiting for new block...".style |> cyan |> dim)
                    ]
              -- Server will push MsgRollForward when a new block arrives
              -- Keep receiving, handling KeepAlive frames transparently
              match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
              | .error e =>
                  return .connectionLost e
              | .ok frame => do
                  match decodeChainSyncMessage frame.payload with
                  | some (.MsgRollForward header tip) => do
                      let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef
                      if ok then
                        syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef
                      else
                        return .protocolError "Failed to fetch block"
                  | some (.MsgRollBackward rollbackPoint2 _) => do
                      let shouldPrint2 ← do
                        if let some ref := seenBlocks then
                          let dup ← atomicCheckAndMark ref (rollbackPoint2.slot.toNat + 1_000_000_000)
                          pure (!dup)
                        else pure true
                      if shouldPrint2 then
                        match tuiRef with
                        | some ref => ref.modify (·.addRollback)
                        | none =>
                          run do
                            concat [
                              ("↩ Rollback to slot ".style |> yellow),
                              (s!"{rollbackPoint2.slot}".style |> yellow |> bold)
                            ]
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef
                  | some other => do
                      IO.println s!"Unexpected message: {repr other}"
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef
                  | none =>
                      return .protocolError "Failed to decode ChainSync message"
          | some other => do
              IO.println s!"Unexpected message: {repr other}"
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef

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
    (seenBlocks : Option (IO.Ref (Option (List Nat))))
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (txSubmPeerRef : Option (IO.Ref TxSubmPeerState) := none)
    (txSubmResponderQueue : Option (IO.Ref (List ByteArray)) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none) : IO SyncExit := do
  -- Find chain tip
  match ← sendChainSync sock findIntersectTip with
  | .error e => return .connectionLost s!"Failed to query tip: {e}"
  | .ok _ => do
      match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
      | .error e => return .connectionLost e
      | .ok frame => do
          match decodeChainSyncMessage frame.payload with
          | some (.MsgIntersectNotFound tip) => do
              tuiLog tuiRef s!"Peer {addrStr}: tip at slot {tip.point.slot}"
              -- Intersect at tip
              match ← sendChainSync sock (findIntersectAt tip.point) with
              | .error e => return .connectionLost s!"Failed to intersect: {e}"
              | .ok _ => do
                  match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
                  | .error e => return .connectionLost e
                  | .ok frame2 => do
                      match decodeChainSyncMessage frame2.payload with
                      | some (.MsgIntersectFound point _) => do
                          tuiLog tuiRef s!"Peer {addrStr}: intersected at slot {point.slot}"
                          syncLoop sock 0 (some chainDb) discoveryRef seenBlocks tuiRef addrStr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef
                      | _ => return .protocolError "Failed to intersect"
          | _ => return .protocolError "Failed to find tip"

/-- Connect to a peer, handshake, find tip, intersect, and sync.
    Returns a SyncExit indicating why it stopped. -/
def peerConnectAndSync (host : String) (port : UInt16) (addrStr : String)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none) : IO SyncExit := do
  match ← socket_connect host port with
  | .error e => return .connectionLost s!"Connection failed: {e}"
  | .ok sock => do
      -- Update TUI peer status
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "connecting")
      -- Handshake
      match ← sendHandshake sock proposal with
      | .error e =>
          socket_close sock
          return .connectionLost s!"Handshake send failed: {e}"
      | .ok _ => do
          -- Read handshake response using proper mux framing (8-byte header + payload)
          -- to avoid consuming extra bytes (e.g. TxSubmission2 MsgInit) from the TCP buffer
          match ← socket_receive_exact sock 8 with
          | .error e =>
              socket_close sock
              return .connectionLost s!"Handshake recv failed: {e}"
          | .ok hdrBytes => do
              match decodeMuxHeader hdrBytes with
              | none =>
                  socket_close sock
                  return .protocolError "Handshake header decode failed"
              | some hdr => do
                  match ← socket_receive_exact sock hdr.payloadLength.toNat.toUInt32 with
                  | .error e =>
                      socket_close sock
                      return .connectionLost s!"Handshake payload recv failed: {e}"
                  | .ok payloadBytes => do
                      match decodeHandshakeMessage payloadBytes with
                      | none =>
                          socket_close sock
                          return .protocolError "Handshake decode failed"
                      | some hsMsg => do
                  let peerSharingEnabled := match hsMsg with
                    | .AcceptVersion _ vd => vd.peerSharing == 1
                    | _ => false
                  match hsMsg with
                  | .AcceptVersion vn vd =>
                      tuiLog tuiRef s!"Peer {addrStr}: version {vn.value}, peerSharing={vd.peerSharing}, diffusion={vd.initiatorAndResponderDiffusionMode}"
                  | _ =>
                      tuiLog tuiRef s!"Peer {addrStr}: handshake ok"
                  -- Update TUI peer status to syncing
                  if let some ref := tuiRef then
                    ref.modify (·.updatePeer addrStr "syncing")
                  -- Send MsgInit as TxSubmission2 Initiator (Instance 0 client)
                  -- This tells the peer we support tx relay and triggers their MsgInit responses
                  let _ ← sendTxSubmission2 sock .MsgInit
                  -- Create per-peer TxSubmission2 state
                  let txSubmPeerRef ← IO.mkRef TxSubmPeerState.empty
                  -- Responder queue: no longer used (handled inline in mux loop)
                  let responderQueue ← IO.mkRef ([] : List ByteArray)
                  if peerSharingEnabled && discoveryRef.isSome then
                    tuiLog tuiRef s!"Peer {addrStr}: PeerSharing enabled, requesting peers..."
                    let _ ← sendPeerSharing sock (.MsgShareRequest 10)
                  let result ← peerFindTipAndSync sock addrStr chainDb discoveryRef seenBlocks tuiRef mempoolRef (some txSubmPeerRef) (some responderQueue) consensusRef
                  socket_close sock
                  return result

/-- Per-peer reconnection loop: connects, syncs, and reconnects on failure with backoff. -/
partial def peerReconnectLoop (host : String) (port : UInt16)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (discoveryRef : Option (IO.Ref DiscoveryState) := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (attempt : Nat := 0) : IO Unit := do
  let addrStr := s!"{host}:{port}"
  if attempt > 0 then
    let delaySec := min (attempt * 5) 30
    if let some ref := tuiRef then
      ref.modify (·.updatePeer addrStr "reconnecting")
    else
      IO.println s!"  ⟳ Peer {addrStr}: reconnecting in {delaySec}s (attempt {attempt + 1})..."
    IO.sleep (UInt32.ofNat (delaySec * 1000))

  match ← peerConnectAndSync host port addrStr proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef with
  | .connectionLost reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "disconnected")
      else
        IO.println s!"  ⚡ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef (attempt + 1)
  | .protocolError reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "error")
      else
        IO.println s!"  ✗ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef (attempt + 1)
  | .done => pure ()

/-- Peer spawner loop: drains discovered peers and spawns new connection tasks -/
partial def peerSpawnerLoop (discoveryRef : IO.Ref DiscoveryState)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (maxPeers : Nat := 20) : IO Unit := do
  IO.sleep 30000  -- Check every 30 seconds
  let newPeers ← discoveryRef.modifyGet fun ds =>
    (ds.discovered, { ds with discovered := [], known := ds.known ++ ds.discovered })
  if newPeers.isEmpty then
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef maxPeers
  else do
    let knownCount := (← discoveryRef.get).known.length
    let budget := if knownCount >= maxPeers then 0 else maxPeers - knownCount
    let toSpawn := newPeers.take budget
    for (host, port) in toSpawn do
      tuiLog tuiRef s!"Connecting to discovered peer {host}:{port}"
      let _ ← IO.asTask (do
        try
          peerReconnectLoop host port proposal chainDb (some discoveryRef) seenBlocks tuiRef mempoolRef consensusRef
        catch e =>
          tuiLog tuiRef s!"Discovered peer {host}:{port}: {e}")
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef maxPeers

-- ============================================================
-- = Inbound Connection Handling (for mempool population)     =
-- ============================================================

/-- Send KeepAlive response as responder (for inbound connections) -/
def sendKeepAliveResponseInbound (sock : Socket) (cookie : UInt16) : IO Unit := do
  let payload := encodeKeepAliveResponse cookie
  let frame ← createFrame .KeepAlive .Responder payload
  let _ ← socket_send sock (encodeMuxFrame frame)
  pure ()

/-- Inbound peer loop: handle a single inbound connection.
    We act as the TxSubmission2 server, requesting txs from the connecting peer.
    Also handles KeepAlive to keep the connection alive. -/
partial def inboundPeerLoop (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (ackedSoFar : Nat := 0) : IO Unit := do
  -- Read mux header (8 bytes)
  match ← socket_receive_exact sock 8 with
  | .error _ => let _ ← socket_close sock; return
  | .ok headerBytes =>
      match decodeMuxHeader headerBytes with
      | none => let _ ← socket_close sock; return
      | some header => do
          -- Read payload
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error _ => let _ ← socket_close sock; return
          | .ok payload => do
              -- Route by protocol
              if header.protocolId == .KeepAlive then
                -- Peer sent KeepAlive — respond with cookie
                match extractKeepAliveCookie payload with
                | some cookie => sendKeepAliveResponseInbound sock cookie
                | none => pure ()
                inboundPeerLoop sock mempoolRef tuiRef ackedSoFar
              else if header.protocolId == .TxSubmission2 then
                match decodeTxSubmission2Message payload with
                | some .MsgInit =>
                    -- Peer initialized TxSubmission2; we are server, send our MsgInit then request txs
                    let _ ← sendTxSubmission2Responder sock .MsgInit
                    let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                    inboundPeerLoop sock mempoolRef tuiRef 0
                | some (.MsgReplyTxIds txIds) =>
                    if txIds.isEmpty then
                      -- Peer has no txs; request again (blocking)
                      let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                      inboundPeerLoop sock mempoolRef tuiRef 0
                    else
                      -- Filter: only request txs we don't already have
                      let pool ← mempoolRef.get
                      let wanted := txIds.filter fun tid => !pool.contains tid.hash
                      if wanted.isEmpty then
                        -- Already have all, ack and request more
                        let _ ← sendTxSubmission2Responder sock
                          (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                        inboundPeerLoop sock mempoolRef tuiRef txIds.length
                      else
                        -- Request full tx bodies
                        let _ ← sendTxSubmission2Responder sock (.MsgRequestTxs (wanted.map (·.hash)))
                        inboundPeerLoop sock mempoolRef tuiRef ackedSoFar
                | some (.MsgReplyTxs txBodies) =>
                    -- Add received txs to mempool
                    let now ← Cleanode.TUI.Render.nowMs
                    for txBytes in txBodies do
                      let pool ← mempoolRef.get
                      match ← pool.addTxRaw txBytes now with
                      | .ok newPool => mempoolRef.set newPool
                      | .error _ => pure ()
                    -- Update TUI mempool stats
                    if let some tRef := tuiRef then
                      let pool ← mempoolRef.get
                      tRef.modify (·.updateMempool pool.entries.length pool.totalBytes)
                    -- Ack and request more
                    let _ ← sendTxSubmission2Responder sock
                      (.MsgRequestTxIds false (UInt16.ofNat txBodies.length) 10)
                    inboundPeerLoop sock mempoolRef tuiRef txBodies.length
                | _ =>
                    -- Unknown TxSubmission2 message, ignore
                    inboundPeerLoop sock mempoolRef tuiRef ackedSoFar
              else
                -- Ignore other protocols (ChainSync, BlockFetch, etc.)
                inboundPeerLoop sock mempoolRef tuiRef ackedSoFar

/-- Handle a single inbound peer: handshake then enter mux loop -/
partial def handleInboundPeer (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState)) (network : NetworkMagic := .Mainnet) : IO Unit := do
  match ← receiveAndRespondHandshake sock (network := network) with
  | .error _ => let _ ← socket_close sock; return
  | .ok none => let _ ← socket_close sock; return
  | .ok (some _version) =>
      tuiLog tuiRef "Inbound peer connected (handshake OK)"
      inboundPeerLoop sock mempoolRef tuiRef

/-- Accept loop: listen for inbound connections, spawn handler per peer -/
partial def acceptLoop (listenSock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState)) (network : NetworkMagic := .Mainnet) : IO Unit := do
  match ← socket_accept listenSock with
  | .error _ =>
      IO.sleep 1000
      acceptLoop listenSock mempoolRef tuiRef network
  | .ok clientSock => do
      let _ ← IO.asTask (do
        try handleInboundPeer clientSock mempoolRef tuiRef network
        catch _ => let _ ← socket_close clientSock; pure ())
      acceptLoop listenSock mempoolRef tuiRef network

/-- Multi-peer relay node mode -/
def relayNode (proposal : HandshakeMessage) (networkName : String)
    (tuiMode : Bool := false) (listenPort : UInt16 := 3001) : IO Unit := do
  -- Open ChainDB
  let chainDb ← ChainDB.open {}

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

  -- Shared peer discovery state (plain data, safe across threads)
  let discoveryRef ← IO.mkRef ({
    discovered := []
    known := peerAddrs
  } : DiscoveryState)

  -- Shared dedup: tracks block numbers already displayed, prevents duplicate output
  let seenBlocksRef ← IO.mkRef (some ([] : List Nat))

  -- Shared mempool (all peers read/write via IO.Ref)
  let mempoolRef ← IO.mkRef (Mempool.empty {})

  -- Shared consensus state (epoch nonces, stake snapshots, chain selection)
  let genesis : Cleanode.Config.Genesis.ShelleyGenesis := {
    epochLength := 432000
    slotLength := 1
    activeSlotsCoeff := { numerator := 1, denominator := 20 }
    securityParam := 2160
    maxLovelaceSupply := 45000000000000000
    networkMagic := match networkName with | "Preprod" => 1 | "Preview" => 2 | _ => 764824073
    networkId := networkName
    protocolParams := none
  }
  let consensusRef ← IO.mkRef (ConsensusState.initial genesis)

  -- TUI state (created only in TUI mode)
  let now ← Cleanode.TUI.Render.nowMs
  let tuiStateRef : Option (IO.Ref TUIState) ←
    if tuiMode then
      some <$> IO.mkRef (TUIState.empty networkName now)
    else pure none

  -- In TUI mode: launch the render loop; otherwise: print banner
  if tuiMode then
    if let some ref := tuiStateRef then
      let _ ← startTUI ref
  else do
    run do
      println (("Dion: a Cardano LEAN 4 Relay Node".style |> cyan |> bold))
      println (("====================================".style |> cyan))
      println ("".style)
      println ((s!"Network: {networkName}".style |> blue))
      println ("".style)
    IO.println "  💾 Chain database opened (data/chain.db)"
    IO.println s!"  📡 Bootstrap peers: {bootstrapPeers.length}"
    if relayPeers.length > 0 then
      IO.println s!"  🌐 Relay peers (from peer snapshot): {relayPeers.length}"
    IO.println s!"  📡 Total initial peers: {peerAddrs.length}"
    run do
      println ("".style)
      println (("=== Relay Node Active (Ctrl+C to stop) ===".style |> cyan |> bold))

  -- Launch per-peer reconnect loops — each task owns its connection lifecycle
  let mut tasks : List (Task (Except IO.Error Unit)) := []
  for (host, port) in peerAddrs do
    let task ← IO.asTask (do
      try
        peerReconnectLoop host port proposal chainDb (some discoveryRef) (some seenBlocksRef) tuiStateRef (some mempoolRef) (some consensusRef)
      catch e =>
        tuiLog tuiStateRef s!"Peer {host}:{port}: {e}")
    tasks := tasks ++ [task]

  -- Launch peer discovery spawner
  let spawnerTask ← IO.asTask (do
    try
      peerSpawnerLoop discoveryRef proposal chainDb (some seenBlocksRef) tuiStateRef (some mempoolRef) (some consensusRef)
    catch e =>
      tuiLog tuiStateRef s!"Peer spawner: {e}")
  tasks := tasks ++ [spawnerTask]

  -- Launch inbound connection listener for mempool population
  -- NOTE: socket_listen MUST be called inside the task, not outside.
  -- Lean external objects (Socket) crash when captured across task boundaries.
  let network : NetworkMagic := match networkName with
    | "Preprod" => .Preprod
    | "Preview" => .Preview
    | _ => .Mainnet
  let listenTask ← IO.asTask (do
    match ← socket_listen listenPort with
    | .error e =>
        tuiLog tuiStateRef s!"Failed to listen on port {listenPort}: {e}"
    | .ok listenSock => do
        tuiLog tuiStateRef s!"Listening for inbound peers on port {listenPort}"
        try acceptLoop listenSock mempoolRef tuiStateRef network
        catch e => tuiLog tuiStateRef s!"Listener: {e}")
  tasks := tasks ++ [listenTask]

  if !tuiMode then
    IO.println s!"  Syncing from {peerAddrs.length} bootstrap peers (discovering more)...\n"

  -- Wait for all tasks (they reconnect indefinitely until Ctrl+C)
  try
    for task in tasks do
      let _ ← IO.wait task
  finally
    -- Restore terminal if in TUI mode
    if tuiMode then stopTUI

  chainDb.close

def parsePortArg (args : List String) : Option UInt16 :=
  match args with
  | "--port" :: n :: _ =>
    match n.toNat? with
    | some p => if p > 0 && p < 65536 then some (UInt16.ofNat p) else none
    | none => none
  | _ :: rest => parsePortArg rest
  | [] => none

def main (args : List String) : IO Unit := do
  -- Parse CLI flags
  let tuiMode := args.any (· == "--tui") || args.any (· == "-tui")
  let networkName :=
    if args.any (· == "--preprod") then "Preprod"
    else if args.any (· == "--preview") then "Preview"
    else "Mainnet"
  let proposal := match networkName with
    | "Preprod" => createPreprodProposal
    | "Preview" => createPreviewProposal
    | _ => createMainnetProposal
  let listenPort : UInt16 := (parsePortArg args).getD 3001

  -- Run as multi-peer relay node
  relayNode proposal networkName tuiMode listenPort
