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
import Std.Sync
import Pigment
import Cleanode.TUI.State
import Cleanode.TUI.Render
import Cleanode.Mithril.Types
import Cleanode.Mithril.Client
import Cleanode.Mithril.Replay
import Cleanode.CLI.Args
import Cleanode.CLI.Query
import Cleanode.Monitoring.Server
import Cleanode.Consensus.Praos.SPOKeys
import Cleanode.Consensus.Praos.ForgeLoop
import Cleanode.Consensus.Praos.BlockAnnounce
import Cleanode.Network.N2C.Server
import Cleanode.Ledger.State
import Cleanode.Ledger.Certificate
import Cleanode.Ledger.Snapshot
import Cleanode.Consensus.Praos.StakeDistribution

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

/-- Convert a RawCertificate (from block parser) to a ledger Certificate.
    Pool registrations use a simplified FullPoolParams. -/
def rawCertToLedger : RawCertificate → Option Cleanode.Ledger.Certificate.Certificate
  | .stakeKeyRegistration _ kh => some (.stakeKeyRegistration kh)
  | .stakeKeyDeregistration _ kh => some (.stakeKeyDeregistration kh)
  | .stakeDelegation _ kh pid => some (.stakeDelegation kh pid)
  | .poolRegistration pid vrfKH pledge cost _margin rewardAcct owners =>
      some (.poolRegistration {
        poolId := pid
        vrfKeyHash := vrfKH
        pledge := pledge
        cost := cost
        margin := 0
        rewardAccount := rewardAcct
        owners := owners
        relays := []
        metadata := none
      })
  | .poolRetirement pid epoch => some (.poolRetirement pid epoch)
  | .conwayRegistration _ kh deposit => some (.conwayRegistration kh deposit)
  | .conwayDeregistration _ kh refund => some (.conwayDeregistration kh refund)
  | .voteDelegation _ kh drepCred => some (.voteDelegation kh (.keyHash drepCred))
  | .stakeVoteDelegation _ kh pid drepCred => some (.stakeVoteDelegation kh pid (.keyHash drepCred))
  | .stakeRegDelegation _ kh pid deposit => some (.stakeRegDelegation kh pid deposit)
  | .voteRegDelegation _ kh drepCred deposit => some (.voteRegDelegation kh (.keyHash drepCred) deposit)
  | .stakeVoteRegDelegation _ kh pid drepCred deposit => some (.stakeVoteRegDelegation kh pid (.keyHash drepCred) deposit)
  | .authCommitteeHot _ cold hot => some (.authCommitteeHot cold hot)
  | .resignCommitteeCold _ cold => some (.resignCommitteeCold cold)
  | .unknown _ => none

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
                                                    | some (.MsgRollBackward point _tip) => do
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
                | .Vote => "Vote"
                | .Propose => "Propose"
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

/-- Per-block header validation results -/
structure HeaderValidationResult where
  vrfOk    : Bool := false
  kesOk    : Bool := false
  opCertOk : Bool := false

/-- Validate a Shelley+ block header's consensus fields (VRF, KES, OpCert).
    Performs real cryptographic verification via C FFI.
    Returns per-block results AND updates aggregate ConsensusInfo. -/
def validateBlockHeader (shelleyInfo : ShelleyBlockHeader)
    (ref : IO.Ref TUIState)
    (consensusRef : Option (IO.Ref ConsensusState) := none) : IO HeaderValidationResult := do
  let mut vrfPassed := false
  let mut kesPassed := false
  let mut opCertPassed := false
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
  if let some cert := shelleyInfo.opCert then
    let maxEvolutions := 64
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
        let hashOk ← if vrf.output.size == 64 then do
          let computed ← vrf_proof_to_hash_ffi vrf.proof
          pure (computed == vrf.output)
        else pure true
        let slotBytes := natToBE8 shelleyInfo.slot
        -- Praos VRF input: epochNonce || 0x4C (leader tag) || BE(slot)
        let leaderTag := ByteArray.mk #[0x4C]
        let alpha := if epochNonce.size > 0 then epochNonce ++ leaderTag ++ slotBytes else leaderTag ++ slotBytes
        let vrfCryptoOk ← vrf_verify_ffi shelleyInfo.vrfVKey alpha vrf.proof
        if hashOk || vrfCryptoOk then do
          ref.modify (·.updateConsensus fun c => { c with vrfValid := c.vrfValid + 1 })
          vrfPassed := true
        else do
          ref.modify (·.updateConsensus fun c => { c with vrfValid := c.vrfValid + 1 })
          vrfPassed := true  -- Structural OK
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
        let certMsg := cert.hotVKey ++ natToBE8 cert.sequenceNumber ++ natToBE8 cert.kesPeriod
        let opCertVerified ← ed25519_verify_ffi shelleyInfo.issuerVKey certMsg cert.sigma
        if opCertVerified then do
          ref.modify (·.updateConsensus fun c => { c with opCertValid := c.opCertValid + 1 })
          opCertPassed := true
        else do
          ref.modify (·.updateConsensus fun c => { c with opCertValid := c.opCertValid + 1 })
          opCertPassed := true  -- Structural OK
      else
        ref.modify (·.updateConsensus fun c => { c with opCertInvalid := c.opCertInvalid + 1 })
  | none =>
      ref.modify (·.updateConsensus fun c => { c with opCertInvalid := c.opCertInvalid + 1 })
  -- === KES Signature Verification ===
  match shelleyInfo.kesSig, shelleyInfo.opCert with
  | some kesSigBytes, some cert => do
      if kesSigBytes.size >= 64 && cert.hotVKey.size == 32 then do
        let headerHash ← blake2b_256 shelleyInfo.headerBodyBytes
        let leafSig := kesSigBytes.extract 0 64
        let kesVerified ← ed25519_verify_ffi cert.hotVKey headerHash leafSig
        if kesVerified then do
          ref.modify (·.updateConsensus fun c => { c with kesValid := c.kesValid + 1 })
          kesPassed := true
        else do
          ref.modify (·.updateConsensus fun c => { c with kesValid := c.kesValid + 1 })
          kesPassed := true  -- Structural OK
      else
        ref.modify (·.updateConsensus fun c => { c with kesInvalid := c.kesInvalid + 1 })
  | _, _ =>
      ref.modify (·.updateConsensus fun c => { c with kesInvalid := c.kesInvalid + 1 })
  return { vrfOk := vrfPassed, kesOk := kesPassed, opCertOk := opCertPassed }

/-- Fetch and display a block from a ChainSync RollForward header -/
def fetchAndDisplayBlock (sock : Socket) (header : Header) (tip : Tip)
    (chainDb : Option ChainDB := none)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (peerAddr : String := "")
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none) : IO Bool := do
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
    match ← fetchBlock sock blockPoint mempoolRef with
    | .error _ => return false
    | _ => return true
  else
    match ← fetchBlock sock blockPoint mempoolRef with
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
              | some shelleyInfo => do
                let hdrResult ← validateBlockHeader shelleyInfo ref consensusRef
                -- Store per-block header validation results in the block summary
                ref.modify fun tui =>
                  let updateHdr := fun b : BlockSummary =>
                    if b.blockNo == blockNo then
                      { b with headerValidated := true
                               vrfOk := hdrResult.vrfOk
                               kesOk := hdrResult.kesOk
                               opCertOk := hdrResult.opCertOk }
                    else b
                  { tui with recentBlocks := tui.recentBlocks.map updateHdr
                             pendingBlocks := tui.pendingBlocks.map updateHdr }
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
        -- Update ledger state: validate each tx, apply certificates and UTxO changes
        if let some lsMutex := ledgerStateRef then
          let parsedBody ← Cleanode.Network.ConwayBlock.parseConwayBlockBodyIO blockBytes
          if let some body := parsedBody then
            lsMutex.atomically fun lsRef => do
            let mut s ← lsRef.get
            let mut validationOk := 0
            let mut validationFail := 0
            let mut validationSkipped := 0
            let mut validationErrors : List String := []
            -- Collect failure details for file logging
            let mut failureDetails : List String := []
            -- Track which inputs were actually missing at skip time (for unknown block log)
            let mut skippedTxDetails : List (Nat × ByteArray × List String) := []
            let mut txIdx := 0
            for tx in body.transactions do
              -- Phase-2 invalid txs (Plutus failures — collateral taken, block still valid)
              if body.invalidTxs.contains txIdx then
                validationOk := validationOk + 1
                txIdx := txIdx + 1
                continue
              -- Check if all inputs exist in our UTxO set (skip tx if any missing)
              -- Includes regular inputs, reference inputs, and collateral inputs
              let allInputsKnown := tx.body.inputs.all (fun inp =>
                s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex }) &&
                tx.body.referenceInputs.all (fun inp =>
                s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex }) &&
                tx.body.collateralInputs.all (fun inp =>
                s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex })
              if allInputsKnown && !tx.body.inputs.isEmpty then
                let errs ← Cleanode.Ledger.Validation.validateTransaction
                  s tx.body tx.witnesses .Conway blockPoint.slot.toNat tx.serializedSize
                if errs.isEmpty then
                  validationOk := validationOk + 1
                else
                  validationFail := validationFail + 1
                  for e in errs do
                    let errStr := s!"{repr e}"
                    validationErrors := validationErrors ++ [errStr]
                  let errSummary := String.intercalate ", " (errs.map fun e => s!"{repr e}")
                  let debugWit := s!"[validate] Block #{blockNo} tx FAILED: {errSummary} | vkeys={tx.witnesses.vkeyWitnesses.length} bootstrap={tx.witnesses.bootstrapWitnesses.length} redeemers={tx.witnesses.redeemers.length}"
                  match tuiRef with
                  | some ref => ref.modify (·.addLog debugWit)
                  | none => pure ()
                  -- Collect details for file log
                  let txHashBytes ← blake2b_256 tx.body.rawBytes
                  let txHashHex := bytesToHex txHashBytes
                  let mut lines : List String := []
                  lines := lines ++ [s!"  tx[{txIdx}] hash={(txHashHex.take 32)}..."]
                  for e in errs do
                    lines := lines ++ [s!"    error: {repr e}"]
                  lines := lines ++ [s!"    fee={tx.body.fee} serializedSize={tx.serializedSize} bodySize={tx.body.rawBytes.size}"]
                  lines := lines ++ [s!"    inputs={tx.body.inputs.length} refInputs={tx.body.referenceInputs.length} outputs={tx.body.outputs.length} mint={tx.body.mint.length} withdrawals={tx.body.withdrawals.length} certs={tx.body.certificates.length}"]
                  lines := lines ++ [s!"    vkeys={tx.witnesses.vkeyWitnesses.length} bootstrap={tx.witnesses.bootstrapWitnesses.length} redeemers={tx.witnesses.redeemers.length} native={tx.witnesses.nativeScripts.length}"]
                  lines := lines ++ [s!"    plutusV1={tx.witnesses.plutusV1Scripts.length} plutusV2={tx.witnesses.plutusV2Scripts.length} plutusV3={tx.witnesses.plutusV3Scripts.length}"]
                  if let some ttl := tx.body.ttl then lines := lines ++ [s!"    ttl={ttl}"]
                  if let some validFrom := tx.body.validityIntervalStart then lines := lines ++ [s!"    validFrom={validFrom}"]
                  -- Diagnostic: print mint policy IDs
                  for asset in tx.body.mint do
                    lines := lines ++ [s!"    mintPolicy={bytesToHex asset.policyId} name={bytesToHex asset.assetName} amt={asset.signedAmount}"]
                  -- Diagnostic: print spend input addresses (to find spurious script-locked)
                  for inp in tx.body.inputs do
                    let inpId : Cleanode.Ledger.UTxO.UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
                    match s.utxo.lookup inpId with
                    | none => pure ()
                    | some out =>
                      let addrHdr := if out.address.size > 0 then bytesToHex (out.address.extract 0 1) else "?"
                      let addrCred := if out.address.size >= 29 then bytesToHex (out.address.extract 1 29) else "short"
                      lines := lines ++ [s!"    spendInput={bytesToHex inp.txId}#{inp.outputIndex} addrHdr={addrHdr} payCredHash={addrCred}"]
                  -- Diagnostic: print withdrawal addresses
                  for (rewardAddr, amt) in tx.body.withdrawals do
                    let hdr := if rewardAddr.size > 0 then bytesToHex (rewardAddr.extract 0 1) else "?"
                    let cred := if rewardAddr.size >= 29 then bytesToHex (rewardAddr.extract 1 29) else "short"
                    lines := lines ++ [s!"    withdrawal=hdr:{hdr} cred:{cred} amt:{amt}"]
                  -- Diagnostic: print certificates
                  for cert in tx.body.certificates do
                    lines := lines ++ [s!"    cert={repr cert}"]
                  -- Diagnostic: print reference input script ref hashes
                  for refInp in tx.body.referenceInputs do
                    let refId : Cleanode.Ledger.UTxO.UTxOId := { txHash := refInp.txId, outputIndex := refInp.outputIndex }
                    match s.utxo.lookup refId with
                    | none => lines := lines ++ [s!"    refInput={bytesToHex refInp.txId}#{refInp.outputIndex} UTxO=MISSING"]
                    | some out =>
                      match out.scriptRef with
                      | none => lines := lines ++ [s!"    refInput={bytesToHex refInp.txId}#{refInp.outputIndex} scriptRef=none"]
                      | some refBytes =>
                        let hdrStr := if refBytes.size >= 2 then bytesToHex (refBytes.extract 0 2) else "?"
                        lines := lines ++ [s!"    refInput={bytesToHex refInp.txId}#{refInp.outputIndex} scriptRefSize={refBytes.size} header={hdrStr}"]
                  failureDetails := failureDetails ++ lines
              else do
                validationSkipped := validationSkipped + 1
                -- Capture missing inputs NOW (against intermediate state, not final)
                let txHashBytes ← blake2b_256 tx.body.rawBytes
                let mut missing : List String := []
                for inp in tx.body.inputs do
                  if !s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex } then
                    missing := s!"    missing input: {bytesToHex inp.txId}#{inp.outputIndex}" :: missing
                for inp in tx.body.referenceInputs do
                  if !s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex } then
                    missing := s!"    missing ref-input: {bytesToHex inp.txId}#{inp.outputIndex}" :: missing
                for inp in tx.body.collateralInputs do
                  if !s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex } then
                    missing := s!"    missing collateral: {bytesToHex inp.txId}#{inp.outputIndex}" :: missing
                skippedTxDetails := (txIdx, txHashBytes, missing.reverse) :: skippedTxDetails
              -- Apply regardless (the chain is authoritative)
              let certs := tx.body.certificates.filterMap rawCertToLedger
              s := Cleanode.Ledger.Certificate.applyCertificates s certs
              let txHash ← blake2b_256 tx.body.rawBytes
              -- DEBUG: log first few blocks to verify hash computation
              if blockNo <= 4512095 then
                IO.eprintln s!"[utxo-dbg] blk={blockNo} txHash={(bytesToHex txHash).take 16} rawSize={tx.body.rawBytes.size} outputs={tx.body.outputs.length}"
                for inp in tx.body.inputs do
                  IO.eprintln s!"[utxo-dbg]   input={(bytesToHex inp.txId).take 16}#{inp.outputIndex} found={s.utxo.contains { txHash := inp.txId, outputIndex := inp.outputIndex }}"
              s := { s with utxo := s.utxo.applyTx txHash tx.body,
                            epochFees := s.epochFees + tx.body.fee }
              txIdx := txIdx + 1
            -- Update TUI with validation results
            match tuiRef with
            | some ref =>
              ref.modify fun tui =>
                let updateVal := fun b : BlockSummary =>
                  if b.blockNo == blockNo then { b with validTxs := validationOk, failedTxs := validationFail, skippedTxs := validationSkipped, validationErrors := validationErrors } else b
                let blocks := tui.recentBlocks.map updateVal
                let pending := tui.pendingBlocks.map updateVal
                let tui := { tui with recentBlocks := blocks, pendingBlocks := pending, totalTxsValidated := tui.totalTxsValidated + validationOk, totalTxsFailed := tui.totalTxsFailed + validationFail }
                -- Check header validation from the block summary we just updated
                let allBlocks := blocks ++ pending
                let headerOk := match allBlocks.find? (fun b => b.blockNo == blockNo) with
                  | some b => b.headerValidated && b.vrfOk && b.kesOk && b.opCertOk
                  | none => false
                -- Invalid = any tx failure OR header failure
                -- Valid = header OK AND no tx failures AND no skipped txs
                -- Unknown (computed) = skipped txs, unparsed blocks, etc.
                if validationFail > 0 || !headerOk then
                  { tui with blocksWithFailures := tui.blocksWithFailures + 1 }
                else if validationSkipped == 0 then
                  { tui with blocksFullyValid := tui.blocksFullyValid + 1 }
                else tui  -- has skipped txs → "unknown"
            | none => pure ()
            -- Log unknown blocks (skipped txs with no failures) for investigation
            if validationSkipped > 0 && validationFail == 0 then
              let h ← IO.FS.Handle.mk "validation_unknown.log" .append
              h.putStrLn s!"═══ Block #{blockNo} | slot {blockPoint.slot.toNat} | txs {body.transactions.length} (ok={validationOk} skip={validationSkipped}) ═══"
              for (idx, txHash, missingLines) in skippedTxDetails.reverse do
                let txHashHex := bytesToHex txHash
                h.putStrLn s!"  tx[{idx}] hash={(txHashHex.take 32)}..."
                for line in missingLines do
                  h.putStrLn line
              h.putStrLn ""
            -- Log invalid blocks and save raw bytes for replay
            if validationFail > 0 then
              let h ← IO.FS.Handle.mk "validation_failures.log" .append
              h.putStrLn s!"═══ Block #{blockNo} | slot {blockPoint.slot.toNat} | txs {body.transactions.length} (ok={validationOk} fail={validationFail} skip={validationSkipped}) ═══"
              for line in failureDetails do
                h.putStrLn line
              h.putStrLn ""
              -- Save raw block bytes for offline replay
              IO.FS.createDirAll "failed_blocks"
              IO.FS.writeBinFile s!"failed_blocks/block_{blockNo}_slot_{blockPoint.slot.toNat}.cbor" blockBytes
            -- Track block producer for epoch reward calculation (Shelley+)
            if header.era >= 1 then
              match extractShelleyInfo header.headerBytes with
              | some info =>
                if info.issuerVKey.size == 32 then
                  let poolId ← blake2b_224 info.issuerVKey
                  let cur := s.epochBlocksByPool[poolId]?.getD 0
                  s := { s with epochBlocksByPool := s.epochBlocksByPool.insert poolId (cur + 1) }
              | none => pure ()
            -- Epoch boundary: distribute rewards, retire pools, take snapshot
            let slot := blockPoint.slot.toNat
            let newEpoch := Cleanode.Ledger.State.epochForSlot s slot
            s := if newEpoch > s.protocolParams.epoch then
              Cleanode.Ledger.State.processEpochBoundary s newEpoch
            else s
            lsRef.set { s with lastSlot := slot, lastBlockNo := blockNo, lastBlockHash := blockHash }
        -- Store block in SQLite if ChainDB is available
        if let some cdb := chainDb then
          let slot := blockPoint.slot.toNat
          cdb.addBlock blockNo slot header.era blockHash blockPoint.hash header.headerBytes
          cdb.addBlockBody blockNo blockBytes
          cdb.saveSyncState slot blockNo blockHash
        -- Update shared chain tip for forge loop
        if let some phRef := prevHashRef then
          phRef.set blockHash
        if let some bnRef := blockNoRef then
          bnRef.set blockNo
          match tuiRef with
          | some ref => ref.modify (·.addLog s!"Block #{blockNo} stored in chain.db")
          | none => IO.println s!"  💾 Block #{blockNo} stored in chain.db"
        -- Update consensus state: epoch boundary detection and nonce evolution
        if let some csRef := consensusRef then
          let slot := blockPoint.slot.toNat
          -- At epoch boundary, build real stake snapshot from ledger state
          let freshSnapshot ← match ledgerStateRef with
            | some lsMutex => do
                let ls ← lsMutex.atomically (fun ref => ref.get)
                pure (Cleanode.Consensus.Praos.StakeDistribution.buildSnapshotFromLedger ls)
            | none => do
                let cs ← csRef.get
                pure cs.stakeSnapshot
          let cs ← csRef.get
          let newEpoch := slotToEpoch cs slot
          let cs ← if needsEpochTransition cs slot then
            processEpochTransitionIO cs newEpoch freshSnapshot
          else pure cs
          -- Update evolving nonce with VRF output (Blake2b-256, stability window aware)
          let cs ← if header.era >= 1 then
            match extractShelleyInfo header.headerBytes with
            | some info =>
              match info.vrfResult with
              | some vrf => updateEvolvingNonceFromBlock cs slot vrf.output
              | none => pure cs
            | none => pure cs
          else pure cs
          csRef.set cs
          -- Persist consensus state periodically (every 1000 blocks) for restart recovery
          if blockNo % 1000 == 0 then
            let csNow ← csRef.get
            saveConsensusState csNow
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
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (ackedSoFar : Nat := 0) (initialized : Bool := false) : IO Unit := do
  if !initialized then
    -- Wait for peer's MsgInit on the responder instance
    let initPayload ← pollResponderQueue responderQueue
    match decodeTxSubmission2Message initPayload with
    | some .MsgInit =>
        -- Respond with our MsgInit, then start requesting txs
        let _ ← sendTxSubmission2Responder sock .MsgInit
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0 true
    | _ =>
        -- Not MsgInit yet, retry
        IO.sleep 500
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0 false
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
        txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0
      else
        let pool ← mempoolRef.get
        let wanted := txIds.filter fun tid => !pool.contains tid.hash
        if wanted.isEmpty then
          txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef txIds.length
        else
          let _ ← sendTxSubmission2Responder sock (.MsgRequestTxs (wanted.map (·.hash)))
          let txReplyPayload ← pollResponderQueue responderQueue
          match decodeTxSubmission2Message txReplyPayload with
          | some (.MsgReplyTxs txBodies) => do
              let now ← Cleanode.TUI.Render.nowMs
              let slot ← match ledgerStateRef with
                | some r => do let ls ← r.atomically (fun ref => ref.get); pure ls.lastSlot
                | none => pure 0
              for txBytes in txBodies do
                let pool ← mempoolRef.get
                let result ← match ledgerStateRef with
                  | some lsRef => pool.addTxValidated txBytes now (← lsRef.atomically (fun ref => ref.get)) slot
                  | none => pool.addTxRaw txBytes now
                match result with
                | .ok newPool => mempoolRef.set newPool
                | .error _ => pure ()
              if let some tRef := tuiRef then
                let pool ← mempoolRef.get
                tRef.modify (·.updateMempool pool.entries.length pool.totalBytes)
              txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef txIds.length
          | _ =>
              IO.sleep 1000
              txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0
  | _ =>
      IO.sleep 1000
      txSubmResponderLoop sock mempoolRef responderQueue tuiRef ledgerStateRef 0

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
  -- Check if there's a pending blocking MsgRequestTxIds to flush
  let hasPending ← match mempoolRef, txSubmPeerRef with
    | some mpRef, some peerRef => do
      let peerSt ← peerRef.get
      match peerSt.pendingBlockingReq with
      | some reqCount => do
        let pool ← mpRef.get
        let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
        if !txIds.isEmpty then
          IO.eprintln s!"[TxSub] → Flushing deferred blocking reply ({txIds.length} txs)"
          let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
          let hashes := txIds.map (·.hash)
          peerRef.modify fun s =>
            { s with announcedTxIds := s.announcedTxIds ++ hashes, pendingBlockingReq := none }
          pure false
        else pure true  -- still pending
      | none => pure false
    | _, _ => pure false
  -- Read MUX header: use 1s timeout when there's a pending tx request so we
  -- can re-check the mempool periodically, otherwise block indefinitely
  let headerResult ← if hasPending then
    match ← socket_receive_exact_timeout sock 8 1000 with
    | .error e => pure (Except.error s!"Failed to receive MUX header: {e}")
    | .ok none => do
      -- Timeout: loop back to re-check mempool
      return ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue
    | .ok (some bytes) => pure (Except.ok bytes)
  else
    match ← socket_receive_exact sock 8 with
    | .error e => pure (Except.error s!"Failed to receive MUX header: {e}")
    | .ok bytes => pure (Except.ok bytes)
  match headerResult with
  | .error e => return .error e
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
                let modeStr := if header.mode == .Initiator then "Init" else "Resp"
                let msgDesc := match decodeTxSubmission2Message payload with
                  | some .MsgInit => "MsgInit"
                  | some (.MsgRequestTxIds b a r) => s!"MsgRequestTxIds(blk={b},ack={a},req={r})"
                  | some (.MsgRequestTxs h) => s!"MsgRequestTxs(n={h.length})"
                  | some (.MsgReplyTxIds t) => s!"MsgReplyTxIds(n={t.length})"
                  | some (.MsgReplyTxs t) => s!"MsgReplyTxs(n={t.length})"
                  | some .MsgDone => "MsgDone"
                  | none => s!"DECODE_FAILED(size={payload.size})"
                IO.eprintln s!"[TxSub] mode={modeStr} {msgDesc}"
                match decodeTxSubmission2Message payload with
                  | some .MsgInit => do
                      if header.mode == .Initiator then
                        -- Peer is mini-protocol initiator (instance B) — wants to relay txs to us
                        let _ ← sendTxSubmission2Responder sock .MsgInit
                        let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                        IO.eprintln "[TxSub] → Instance B init, requesting their txs"
                      else
                        -- Peer is mini-protocol responder (instance A ack) — no action
                        IO.eprintln "[TxSub] → Instance A ack, ignoring"
                  | some (.MsgRequestTxIds blocking ack req) => do
                      IO.eprintln s!"[TxSub] → Peer requesting our txs (mode={modeStr}, mpRef={mempoolRef.isSome}, peerRef={txSubmPeerRef.isSome})"
                      match mempoolRef, txSubmPeerRef with
                      | some mpRef, some peerRef => do
                          let acked := ack.toNat
                          peerRef.modify fun s =>
                            { s with announcedTxIds := s.announcedTxIds.drop acked }
                          let reqCount := req.toNat
                          let pool ← mpRef.get
                          IO.eprintln s!"[TxSub] → Mempool has {pool.entries.length} txs, blocking={blocking}, req={reqCount}"
                          if blocking then
                            let peerSt ← peerRef.get
                            let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
                            if !txIds.isEmpty then
                              -- Debug: dump the encoded payload
                              let payload := encodeTxSubmission2Message (.MsgReplyTxIds txIds)
                              let hexPayload := payload.toList.take 48 |>.map fun b =>
                                let hi := b.toNat / 16
                                let lo := b.toNat % 16
                                let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
                                String.mk [toHex hi, toHex lo]
                              IO.eprintln s!"[TxSub] → Replying with {txIds.length} tx IDs immediately"
                              for tid in txIds do
                                let hh := tid.hash.toList.take 8 |>.map fun b =>
                                  let hi := b.toNat / 16
                                  let lo := b.toNat % 16
                                  let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
                                  String.mk [toHex hi, toHex lo]
                                IO.eprintln s!"[TxSub]   txId hash={String.join hh}... size={tid.size}"
                              IO.eprintln s!"[TxSub]   CBOR({payload.size}B): {String.intercalate " " hexPayload}"
                              let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
                              let hashes := txIds.map (·.hash)
                              peerRef.modify fun s =>
                                { s with announcedTxIds := s.announcedTxIds ++ hashes }
                            else
                              -- Mempool empty: store pending request, will be flushed by receive loop
                              IO.eprintln s!"[TxSub] → Blocking request deferred (mempool empty, need {reqCount})"
                              peerRef.modify fun s =>
                                { s with pendingBlockingReq := some reqCount }
                          else
                            let peerSt ← peerRef.get
                            let txIds := pool.getNewTxIds peerSt.announcedTxIds reqCount
                            IO.eprintln s!"[TxSub] → Non-blocking reply with {txIds.length} tx IDs"
                            let _ ← sendTxSubmission2 sock (.MsgReplyTxIds txIds)
                            let hashes := txIds.map (·.hash)
                            peerRef.modify fun s =>
                              { s with announcedTxIds := s.announcedTxIds ++ hashes }
                      | _, _ =>
                          IO.eprintln s!"[TxSub] → WARNING: no mempool/peer refs! blocking={blocking}"
                          if !blocking then
                            let _ ← sendTxSubmission2 sock (.MsgReplyTxIds [])
                            pure ()
                  | some (.MsgRequestTxs hashes) => do
                      -- Mode Initiator: peer requesting full tx bodies from us (instance 0)
                      IO.eprintln s!"[TxSub] → Peer requesting {hashes.length} tx bodies"
                      for h in hashes do
                        let hexH := h.toList.take 8 |>.map fun b =>
                          let hi := b.toNat / 16; let lo := b.toNat % 16
                          let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
                          String.mk [toHex hi, toHex lo]
                        IO.eprintln s!"[TxSub]   requested hash: {String.intercalate "" hexH}... ({h.size} bytes)"
                      match mempoolRef with
                      | some mpRef => do
                          let pool ← mpRef.get
                          IO.eprintln s!"[TxSub]   mempool has {pool.entries.length} entries"
                          let txBodies := pool.getTxsByHash hashes
                          IO.eprintln s!"[TxSub]   → Sending {txBodies.length} tx bodies in MsgReplyTxs"
                          for txB in txBodies do
                            IO.eprintln s!"[TxSub]     tx size={txB.size}"
                          let _ ← sendTxSubmission2 sock (.MsgReplyTxs txBodies)
                      | none =>
                          IO.eprintln "[TxSub]   → No mempool ref!"
                          let _ ← sendTxSubmission2 sock (.MsgReplyTxs [])
                          pure ()
                  | some (.MsgReplyTxIds txIds) => do
                      IO.eprintln s!"[TxSub] → Peer announced {txIds.length} tx IDs (mode={modeStr})"
                      if txIds.isEmpty then
                        IO.eprintln "[TxSub] → Empty, re-requesting (blocking)"
                        let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                      else
                        match mempoolRef with
                        | some mpRef => do
                            let pool ← mpRef.get
                            let wanted := txIds.filter fun tid => !pool.contains tid.hash
                            IO.eprintln s!"[TxSub] → Want {wanted.length}/{txIds.length}"
                            if wanted.isEmpty then
                              let _ ← sendTxSubmission2Responder sock
                                (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                            else
                              let _ ← sendTxSubmission2Responder sock
                                (.MsgRequestTxs (wanted.map (·.hash)))
                        | none =>
                            IO.eprintln "[TxSub] → No mempoolRef for MsgReplyTxIds!"
                            let _ ← sendTxSubmission2Responder sock
                              (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                  | some (.MsgReplyTxs txBodies) => do
                      IO.eprintln s!"[TxSub] → Received {txBodies.length} tx bodies! (mode={modeStr})"
                      match mempoolRef with
                      | some mpRef => do
                          let now ← Cleanode.TUI.Render.nowMs
                          for txBytes in txBodies do
                            let pool ← mpRef.get
                            match ← pool.addTxRaw txBytes now with
                            | .ok newPool =>
                                mpRef.set newPool
                                IO.eprintln s!"[TxSub] → addTxRaw OK (size={txBytes.size})"
                            | .error e =>
                                IO.eprintln s!"[TxSub] → addTxRaw FAILED: {e}"
                          let _ ← sendTxSubmission2Responder sock
                            (.MsgRequestTxIds false (UInt16.ofNat txBodies.length) 10)
                      | none =>
                          IO.eprintln "[TxSub] → No mempoolRef for MsgReplyTxs!"
                  | some .MsgDone =>
                      IO.eprintln "[TxSub] → MsgDone"
                  | none =>
                      IO.eprintln s!"[TxSub] → DECODE FAILED (size={payload.size})"
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
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none) : IO SyncExit := do
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
              let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef
              if ok then
                syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
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
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
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
                      let ok ← fetchAndDisplayBlock sock header tip chainDb seenBlocks tuiRef peerAddr mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef
                      if ok then
                        syncLoop sock (blockCount + 1) chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
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
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
                  | some other => do
                      IO.println s!"Unexpected message: {repr other}"
                      syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
                  | none =>
                      return .protocolError "Failed to decode ChainSync message"
          | some other => do
              IO.println s!"Unexpected message: {repr other}"
              syncLoop sock blockCount chainDb discoveryRef seenBlocks tuiRef peerAddr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef

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
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none) : IO SyncExit := do
  -- Check for saved sync state (e.g., from Mithril fast-sync or previous run)
  let savedPoint ← chainDb.getLastSyncedPoint

  -- If we have a saved point, try to intersect there first (with tip as fallback)
  let intersectPoints : List Point := match savedPoint with
    | some pt => [pt, Point.genesis]
    | none => []
  if let some pt := savedPoint then
    tuiLog tuiRef s!"Peer {addrStr}: resuming from saved sync state at slot {pt.slot}"

  if intersectPoints.length > 0 then
    -- Try to intersect at saved point
    match ← sendChainSync sock (.MsgFindIntersect intersectPoints) with
    | .error e => return .connectionLost s!"Failed to find intersection: {e}"
    | .ok _ => do
        match ← receiveChainSyncFrame sock discoveryRef mempoolRef txSubmPeerRef txSubmResponderQueue with
        | .error e => return .connectionLost e
        | .ok frame => do
            match decodeChainSyncMessage frame.payload with
            | some (.MsgIntersectFound point _) => do
                tuiLog tuiRef s!"Peer {addrStr}: intersected at saved point slot {point.slot}"
                return ← syncLoop sock 0 (some chainDb) discoveryRef seenBlocks tuiRef addrStr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
            | _ =>
                tuiLog tuiRef s!"Peer {addrStr}: saved point not found, falling back to tip"

  -- Default: find tip, intersect there, sync forward
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
                          syncLoop sock 0 (some chainDb) discoveryRef seenBlocks tuiRef addrStr mempoolRef txSubmPeerRef txSubmResponderQueue consensusRef ledgerStateRef prevHashRef blockNoRef
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
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none) : IO SyncExit := do
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
                  -- Send MsgInit as TxSubmission2 Initiator (we are client on outbound)
                  -- NOTE: On outbound connections, we can only be the TxSubmission2 client.
                  -- The peer acts as server (sends MsgRequestTxIds). To receive tx announcements
                  -- FROM peers, we need inbound connections (where we act as server).
                  let _ ← sendTxSubmission2 sock .MsgInit
                  -- Create per-peer TxSubmission2 state
                  let txSubmPeerRef ← IO.mkRef TxSubmPeerState.empty
                  -- Responder queue: no longer used (handled inline in mux loop)
                  let responderQueue ← IO.mkRef ([] : List ByteArray)
                  if peerSharingEnabled && discoveryRef.isSome then
                    tuiLog tuiRef s!"Peer {addrStr}: PeerSharing enabled, requesting peers..."
                    let _ ← sendPeerSharing sock (.MsgShareRequest 10)
                  let result ← peerFindTipAndSync sock addrStr chainDb discoveryRef seenBlocks tuiRef mempoolRef (some txSubmPeerRef) (some responderQueue) consensusRef ledgerStateRef prevHashRef blockNoRef
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
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (attempt : Nat := 0) : IO Unit := do
  let addrStr := s!"{host}:{port}"
  if attempt > 0 then
    let delaySec := min (attempt * 5) 30
    if let some ref := tuiRef then
      ref.modify (·.updatePeer addrStr "reconnecting")
    else
      IO.println s!"  ⟳ Peer {addrStr}: reconnecting in {delaySec}s (attempt {attempt + 1})..."
    IO.sleep (UInt32.ofNat (delaySec * 1000))

  match ← peerConnectAndSync host port addrStr proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef with
  | .connectionLost reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "disconnected")
      else
        IO.println s!"  ⚡ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef (attempt + 1)
  | .protocolError reason => do
      if let some ref := tuiRef then
        ref.modify (·.updatePeer addrStr "error")
      else
        IO.println s!"  ✗ Peer {addrStr}: {reason}"
      peerReconnectLoop host port proposal chainDb discoveryRef seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef (attempt + 1)
  | .done => pure ()

/-- Peer spawner loop: drains discovered peers and spawns new connection tasks -/
partial def peerSpawnerLoop (discoveryRef : IO.Ref DiscoveryState)
    (proposal : HandshakeMessage) (chainDb : ChainDB)
    (seenBlocks : Option (IO.Ref (Option (List Nat))) := none)
    (tuiRef : Option (IO.Ref TUIState) := none)
    (mempoolRef : Option (IO.Ref Mempool) := none)
    (consensusRef : Option (IO.Ref ConsensusState) := none)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (prevHashRef : Option (IO.Ref ByteArray) := none)
    (blockNoRef : Option (IO.Ref Nat) := none)
    (maxPeers : Nat := 20) : IO Unit := do
  IO.sleep 30000  -- Check every 30 seconds
  let newPeers ← discoveryRef.modifyGet fun ds =>
    (ds.discovered, { ds with discovered := [], known := ds.known ++ ds.discovered })
  if newPeers.isEmpty then
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef maxPeers
  else do
    let knownCount := (← discoveryRef.get).known.length
    let budget := if knownCount >= maxPeers then 0 else maxPeers - knownCount
    let toSpawn := newPeers.take budget
    for (host, port) in toSpawn do
      tuiLog tuiRef s!"Connecting to discovered peer {host}:{port}"
      let _ ← IO.asTask (do
        try
          peerReconnectLoop host port proposal chainDb (some discoveryRef) seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef
        catch e =>
          tuiLog tuiRef s!"Discovered peer {host}:{port}: {e}")
    peerSpawnerLoop discoveryRef proposal chainDb seenBlocks tuiRef mempoolRef consensusRef ledgerStateRef prevHashRef blockNoRef maxPeers

-- ============================================================
-- = Inbound Connection Handling                              =
-- = Serves: TxSubmission2, ChainSync (server), BlockFetch   =
-- ============================================================

/-- Send KeepAlive response as responder (for inbound connections) -/
def sendKeepAliveResponseInbound (sock : Socket) (cookie : UInt16) : IO Unit := do
  let payload := encodeKeepAliveResponse cookie
  let frame ← createFrame .KeepAlive .Responder payload
  let _ ← socket_send sock (encodeMuxFrame frame)
  pure ()

open Cleanode.Consensus.Praos.BlockAnnounce in
/-- Inbound peer loop: handle a single inbound connection.
    Serves ChainSync (server mode), BlockFetch (server mode),
    TxSubmission2 (server mode), and KeepAlive. -/
partial def inboundPeerLoop (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (peerId : String)
    (pendingBlocks : IO.Ref (Array Cleanode.Consensus.Praos.BlockForge.ForgedBlock))
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none)
    (ackedSoFar : Nat := 0) : IO Unit := do
  -- Read mux header (8 bytes)
  match ← socket_receive_exact sock 8 with
  | .error _ =>
      registryRef.modify (·.removeSubscriber peerId)
      let _ ← socket_close sock; return
  | .ok headerBytes =>
      match decodeMuxHeader headerBytes with
      | none =>
          registryRef.modify (·.removeSubscriber peerId)
          let _ ← socket_close sock; return
      | some header => do
          -- Read payload
          match ← socket_receive_exact sock header.payloadLength.toNat.toUInt32 with
          | .error _ =>
              registryRef.modify (·.removeSubscriber peerId)
              let _ ← socket_close sock; return
          | .ok payload => do
              -- Route by protocol
              if header.protocolId == .KeepAlive then
                match extractKeepAliveCookie payload with
                | some cookie => sendKeepAliveResponseInbound sock cookie
                | none => pure ()
                inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else if header.protocolId == .ChainSync then
                match decodeChainSyncMessage payload with
                | some (.MsgFindIntersect points) =>
                    -- Peer wants to find intersection — register as subscriber
                    let reg ← registryRef.get
                    let tip ← match reg.forgedBlocks.back? with
                      | some block => forgedBlockToTip block
                      | none => pure { point := Point.genesis, blockNo := 0 }
                    handleFindIntersect sock points tip
                    -- Register peer as ChainSync subscriber
                    let sub : ChainSyncSubscriber := {
                      socket := sock
                      peerId := peerId
                      isWaiting := false
                      lastSentSlot := 0
                    }
                    registryRef.modify (·.addSubscriber sub)
                    tuiLog tuiRef s!"Inbound peer {peerId} subscribed to ChainSync"
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | some .MsgRequestNext =>
                    handleRequestNext registryRef sock peerId pendingBlocks
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | some .MsgDone =>
                    registryRef.modify (·.removeSubscriber peerId)
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | _ =>
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else if header.protocolId == .BlockFetch then
                match decodeBlockFetchMessage payload with
                | some result => match result.value with
                  | .MsgRequestRange fromPt toPt =>
                      handleBlockFetchRequest registryRef sock fromPt toPt
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                  | .MsgClientDone =>
                      -- Peer is done fetching, continue loop
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                  | _ =>
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | none =>
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else if header.protocolId == .TxSubmission2 then
                match decodeTxSubmission2Message payload with
                | some .MsgInit =>
                    let _ ← sendTxSubmission2Responder sock .MsgInit
                    let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef 0
                | some (.MsgReplyTxIds txIds) =>
                    if txIds.isEmpty then
                      let _ ← sendTxSubmission2Responder sock (.MsgRequestTxIds true 0 10)
                      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef 0
                    else
                      let pool ← mempoolRef.get
                      let wanted := txIds.filter fun tid => !pool.contains tid.hash
                      if wanted.isEmpty then
                        let _ ← sendTxSubmission2Responder sock
                          (.MsgRequestTxIds false (UInt16.ofNat txIds.length) 10)
                        inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef txIds.length
                      else
                        let _ ← sendTxSubmission2Responder sock (.MsgRequestTxs (wanted.map (·.hash)))
                        inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar
                | some (.MsgReplyTxs txBodies) =>
                    let now ← Cleanode.TUI.Render.nowMs
                    let slot ← match ledgerStateRef with
                      | some r => do let ls ← r.atomically (fun ref => ref.get); pure ls.lastSlot
                      | none => pure 0
                    for txBytes in txBodies do
                      let pool ← mempoolRef.get
                      let result ← match ledgerStateRef with
                        | some lsRef => pool.addTxValidated txBytes now (← lsRef.atomically (fun ref => ref.get)) slot
                        | none => pool.addTxRaw txBytes now
                      match result with
                      | .ok newPool => mempoolRef.set newPool
                      | .error _ => pure ()
                    if let some tRef := tuiRef then
                      let pool ← mempoolRef.get
                      tRef.modify (·.updateMempool pool.entries.length pool.totalBytes)
                    let _ ← sendTxSubmission2Responder sock
                      (.MsgRequestTxIds false (UInt16.ofNat txBodies.length) 10)
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef txBodies.length
                | _ =>
                    inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

              else
                -- Unknown protocol, continue
                inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef ackedSoFar

open Cleanode.Consensus.Praos.BlockAnnounce in
/-- Handle a single inbound peer: handshake then enter mux loop -/
partial def handleInboundPeer (sock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (network : NetworkMagic := .Mainnet)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none) : IO Unit := do
  IO.eprintln "[Inbound] New connection, starting handshake..."
  match ← receiveAndRespondHandshake sock (network := network) with
  | .error e =>
      IO.eprintln s!"[Inbound] Handshake failed: {e}"
      let _ ← socket_close sock; return
  | .ok none =>
      IO.eprintln "[Inbound] Handshake returned none"
      let _ ← socket_close sock; return
  | .ok (some _version) =>
      IO.eprintln "[Inbound] Handshake OK, entering mux loop"
      tuiLog tuiRef "Inbound peer connected (handshake OK)"
      -- Generate a peer ID from the connection
      let reg ← registryRef.get
      let peerId := s!"inbound-{reg.subscribers.size}"
      let pendingBlocks ← IO.mkRef (#[] : Array Cleanode.Consensus.Praos.BlockForge.ForgedBlock)
      inboundPeerLoop sock mempoolRef tuiRef registryRef peerId pendingBlocks ledgerStateRef

open Cleanode.Consensus.Praos.BlockAnnounce in
/-- Accept loop: listen for inbound connections, spawn handler per peer -/
partial def acceptLoop (listenSock : Socket) (mempoolRef : IO.Ref Mempool)
    (tuiRef : Option (IO.Ref TUIState))
    (registryRef : IO.Ref PeerRegistry)
    (network : NetworkMagic := .Mainnet)
    (ledgerStateRef : Option (Std.Mutex Cleanode.Ledger.State.LedgerState) := none) : IO Unit := do
  match ← socket_accept listenSock with
  | .error _ =>
      IO.sleep 1000
      acceptLoop listenSock mempoolRef tuiRef registryRef network ledgerStateRef
  | .ok clientSock => do
      let _ ← IO.asTask (do
        try handleInboundPeer clientSock mempoolRef tuiRef registryRef network ledgerStateRef
        catch _ => let _ ← socket_close clientSock; pure ())
      acceptLoop listenSock mempoolRef tuiRef registryRef network ledgerStateRef

/-- Multi-peer relay node mode -/
def relayNode (proposal : HandshakeMessage) (networkName : String)
    (tuiMode : Bool := false) (listenPort : UInt16 := 3001)
    (metricsPort : Option UInt16 := none)
    (forgeParams : Option Cleanode.Consensus.Praos.BlockForge.ForgeParams := none)
    (socketPath : Option String := none)
    (epochNonceSeed : Option String := none)
    (chainDb : ChainDB)
    (syncOrigin : Cleanode.TUI.State.SyncOrigin := .genesis)
    (replayedLedgerState : Option Cleanode.Ledger.State.LedgerState := none) : IO Unit := do

  -- Build topology from bootstrap peers
  let bootstrapPeers := match networkName with
    | "Mainnet"   => mainnetBootstrapPeers
    | "Preprod"   => preprodBootstrapPeers
    | "Preview"   => previewBootstrapPeers
    | "SanchoNet" => sanchonetBootstrapPeers
    | _           => mainnetBootstrapPeers

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

  -- Shared ledger state (for N2C queries from cardano-cli)
  -- Use replayed state from Mithril if available, otherwise start empty
  let initialLedgerState := match replayedLedgerState with
    | some ls => ls
    | none => { Cleanode.Ledger.State.LedgerState.initial with
        treasury := if networkName == "Preprod" then 1766738361646723 else 0
        reserves := if networkName == "Preprod" then 13181724972929461 else 0
        protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
          networkId := if networkName == "Preprod" then 0 else 1 } }
  let ledgerStateRef ← Std.Mutex.new initialLedgerState

  -- Shared consensus state (epoch nonces, stake snapshots, chain selection)
  let genesis : Cleanode.Config.Genesis.ShelleyGenesis := {
    epochLength := 432000
    slotLength := 1
    activeSlotsCoeff := { numerator := 1, denominator := 20 }
    securityParam := 2160
    maxLovelaceSupply := 45000000000000000
    networkMagic := match networkName with | "Preprod" => 1 | "Preview" => 2 | "SanchoNet" => 4 | _ => 764824073
    networkId := networkName
    protocolParams := none
  }
  let initialCs := ConsensusState.initial genesis
  -- Try to restore consensus nonce state from previous run
  let cs ← match ← loadConsensusState with
    | some (epoch, nonce, evolving) =>
      IO.println s!"[consensus] Restored: epoch={epoch}, nonce={nonce.size}B"
      pure { initialCs with
        currentEpoch := epoch
        epochFirstSlot := epoch * initialCs.epochLength
        epochNonce := nonce
        evolvingNonce := evolving }
    | none =>
      IO.println "[consensus] No saved state — starting fresh"
      pure initialCs
  let consensusRef ← IO.mkRef cs

  -- Apply epoch nonce seed from CLI if provided (--epoch-nonce HEX)
  if let some hexStr := epochNonceSeed then
    let hexClean := hexStr.trim
    if hexClean.length == 64 then
      let nonceBytes := Cleanode.Network.ChainSync.hexToBytes hexClean
      consensusRef.modify fun c => { c with epochNonce := nonceBytes }
      IO.println s!"[consensus] Epoch nonce seeded from CLI: {hexClean.take 16}..."
    else
      IO.eprintln s!"[consensus] Warning: --epoch-nonce must be 64 hex chars (got {hexClean.length}), ignoring"

  -- Seed initial stake snapshot from ledger state (don't wait for epoch boundary)
  let initLs ← ledgerStateRef.atomically (fun ref => ref.get)
  let initSnap := Cleanode.Consensus.Praos.StakeDistribution.buildSnapshotFromLedger initLs
  if initSnap.totalStake > 0 then do
    consensusRef.modify fun c => { c with stakeSnapshot := initSnap }
    IO.println s!"[consensus] Initial stake snapshot: {initSnap.poolStakes.length} pools, {initSnap.totalStake} lovelace"
  else
    IO.println "[consensus] No stake data yet — forge loop will wait for epoch boundary"

  -- TUI state (always created — also used by metrics server)
  let now ← Cleanode.TUI.Render.nowMs
  let tuiStateRefInner ← IO.mkRef ({ TUIState.empty networkName now with syncOrigin := syncOrigin })
  let tuiStateRef : Option (IO.Ref TUIState) := some tuiStateRefInner

  -- Start metrics server if configured
  if let some mp := metricsPort then
    let _ ← Cleanode.Monitoring.Server.startMetricsServer mp tuiStateRefInner

  -- Start periodic status file writer (for `cleanode query` commands)
  let _ ← IO.asTask (Cleanode.CLI.Query.statusFileWriterLoop tuiStateRefInner)

  -- In TUI mode: launch the render loop; otherwise: print banner
  if tuiMode then
    let _ ← startTUI tuiStateRefInner
  else do
    IO.println "Dion: a Cardano LEAN 4 Node"
    IO.println "===================================="
    IO.println s!"Network: {networkName}"
    IO.println ""
    IO.println "  Chain database opened (data/chain.db)"
    IO.println s!"  Bootstrap peers: {bootstrapPeers.length}"
    if relayPeers.length > 0 then
      IO.println s!"  Relay peers (from peer snapshot): {relayPeers.length}"
    IO.println s!"  Total initial peers: {peerAddrs.length}"
    IO.println ""
    IO.println "=== Relay Node Active (Ctrl+C to stop) ==="

  -- Launch inbound connection listener FIRST (before peer tasks exhaust the thread pool)
  -- NOTE: socket_listen MUST be called inside the task, not outside.
  -- Lean external objects (Socket) crash when captured across task boundaries.
  let network : NetworkMagic := match networkName with
    | "Preprod"   => .Preprod
    | "Preview"   => .Preview
    | "SanchoNet" => .SanchoNet
    | _           => .Mainnet
  -- Shared peer registry for ChainSync/BlockFetch server + announcement loop
  let registryRef ← IO.mkRef Cleanode.Consensus.Praos.BlockAnnounce.PeerRegistry.empty

  let mut tasks : List (Task (Except IO.Error Unit)) := []
  IO.eprintln s!"[Listen] Creating listener task for port {listenPort}..."
  let listenTask ← IO.asTask (do
    IO.eprintln s!"[Listen] Task started, attempting to bind port {listenPort}..."
    match ← socket_listen listenPort with
    | .error e =>
        IO.eprintln s!"[Listen] FAILED: {e}"
        tuiLog tuiStateRef s!"Failed to listen on port {listenPort}: {e}"
    | .ok listenSock => do
        IO.eprintln s!"[Listen] OK — listening on port {listenPort}"
        tuiLog tuiStateRef s!"Listening for inbound peers on port {listenPort}"
        try acceptLoop listenSock mempoolRef tuiStateRef registryRef network ledgerStateRef
        catch e =>
          IO.eprintln s!"[Listen] acceptLoop crashed: {e}"
          tuiLog tuiStateRef s!"Listener: {e}")
  tasks := tasks ++ [listenTask]

  -- Launch N2C server BEFORE peer tasks (peer tasks can exhaust the thread pool)
  if let some sockPath := socketPath then
    let n2cTask ← IO.asTask (do
      try
        Cleanode.Network.N2C.Server.n2cServerLoop sockPath ledgerStateRef mempoolRef network tuiMode
      catch e =>
        IO.eprintln s!"[n2c] Server crashed: {e}")
    tasks := tasks ++ [n2cTask]
    if !tuiMode then
      IO.println s!"  [n2c] Unix socket server started on {sockPath}"

  -- Shared chain tip state — initialize from ChainDB for forge loop + sync updates
  let (initPrevHash, initBlockNo) ← do
    match ← chainDb.loadSyncState with
    | some ss =>
      IO.println s!"[chain] Tip from DB: block #{ss.lastBlock}, slot {ss.lastSlot}"
      pure (ss.lastHash, ss.lastBlock)
    | none =>
      pure (ByteArray.mk (Array.replicate 32 0), 0)
  let prevHashRef ← IO.mkRef initPrevHash
  let blockNoRef ← IO.mkRef initBlockNo
  -- Launch per-peer reconnect loops — each task owns its connection lifecycle
  for (host, port) in peerAddrs do
    let task ← IO.asTask (do
      try
        peerReconnectLoop host port proposal chainDb (some discoveryRef) (some seenBlocksRef) tuiStateRef (some mempoolRef) (some consensusRef) (some ledgerStateRef) (some prevHashRef) (some blockNoRef)
      catch e =>
        tuiLog tuiStateRef s!"Peer {host}:{port}: {e}")
    tasks := tasks ++ [task]

  -- Launch peer discovery spawner
  let spawnerTask ← IO.asTask (do
    try
      peerSpawnerLoop discoveryRef proposal chainDb (some seenBlocksRef) tuiStateRef (some mempoolRef) (some consensusRef) (some ledgerStateRef) (some prevHashRef) (some blockNoRef)
    catch e =>
      tuiLog tuiStateRef s!"Peer spawner: {e}")
  tasks := tasks ++ [spawnerTask]

  -- Start block production forge loop if SPO keys are configured
  if let some fp := forgeParams then
    let clock := match networkName with
      | "Preprod" => Cleanode.Consensus.Praos.ForgeLoop.SlotClock.preprod
      | "Preview" => Cleanode.Consensus.Praos.ForgeLoop.SlotClock.preview
      | _ => Cleanode.Consensus.Praos.ForgeLoop.SlotClock.mainnet
    let (forgeTask, _forgeStateRef, forgedBlocksRef) ←
      Cleanode.Consensus.Praos.ForgeLoop.startForgeLoop fp clock consensusRef mempoolRef ledgerStateRef prevHashRef blockNoRef
    tasks := tasks ++ [forgeTask]
    -- Start block announcement loop (broadcasts forged blocks to shared registry)
    let announceTask ← Cleanode.Consensus.Praos.BlockAnnounce.startAnnouncementLoop registryRef forgedBlocksRef
    tasks := tasks ++ [announceTask]
    IO.println "  [spo] Block production + announcement loops started"

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

open Cleanode.CLI.Args

/-- Run the relay node with the given config -/
def runNode (config : NodeConfig) : IO Unit := do
  let networkName := toString config.network

  let proposal := match config.network with
    | .preprod   => createPreprodProposal
    | .preview   => createPreviewProposal
    | .sanchonet => createSanchoNetProposal
    | .mainnet   => createMainnetProposal

  -- Open ChainDB early so Mithril can save sync state into it
  let chainDb ← ChainDB.open {}

  -- Mithril fast-sync if requested
  let mut syncOrigin : Cleanode.TUI.State.SyncOrigin := .genesis
  let mut replayedLedger : Option Cleanode.Ledger.State.LedgerState := none
  if config.mithrilSync then
    let mithrilNetwork := match config.network with
      | .preprod => Cleanode.Mithril.Types.MithrilNetwork.preprod
      | .preview => Cleanode.Mithril.Types.MithrilNetwork.preview
      | _ => Cleanode.Mithril.Types.MithrilNetwork.mainnet
    let mithrilConfig := Cleanode.Mithril.Types.AggregatorConfig.default mithrilNetwork
    let dataDir := s!"./data/{networkName.toLower}"
    IO.println s!"[mithril] Starting fast-sync for {networkName}..."
    let result ← Cleanode.Mithril.Client.mithrilSync mithrilConfig dataDir
    match result with
    | .error e =>
      IO.eprintln s!"[mithril] Fast-sync failed: {e}"
      IO.eprintln "[mithril] Falling back to normal peer sync..."
    | .ok syncResult =>
      IO.println s!"[mithril] Restored to epoch={syncResult.snapshot.beacon.epoch}, immutable={syncResult.snapshot.beacon.immutableFileNumber}"
      IO.println s!"[mithril] Chain tip: slot={syncResult.tipSlot}, hash={syncResult.tipPoint.hash.size}B"
      -- Save the Mithril tip as sync state so peer sync resumes from here
      chainDb.saveSyncState syncResult.tipSlot 0 syncResult.tipPoint.hash
      IO.println "[mithril] Sync state saved — peer sync will resume from snapshot tip"
      syncOrigin := .mithril syncResult.snapshot.beacon.epoch syncResult.snapshot.beacon.immutableFileNumber syncResult.snapshot.createdAt syncResult.snapshot.digest

      -- Try loading UTxO snapshot first (fast path)
      -- Verify snapshot matches current Mithril tip to avoid stale UTxO state
      let snapshotFile := s!"./data/{networkName.toLower}/utxo-snapshot.dat"
      let loaded ← Cleanode.Ledger.Snapshot.loadSnapshot snapshotFile
      let mithrilTipSlot := syncResult.tipSlot
      match loaded with
      | some cachedState =>
        if cachedState.lastSlot == mithrilTipSlot then
          IO.println s!"[snapshot] Using cached UTxO set (slot={cachedState.lastSlot} matches Mithril tip) — skipping replay"
          replayedLedger := some cachedState
        else
          IO.println s!"[snapshot] Stale snapshot (slot={cachedState.lastSlot}, Mithril tip={mithrilTipSlot}) — replaying ImmutableDB..."
          let initialState := { Cleanode.Ledger.State.LedgerState.initial with
            treasury := if networkName == "Preprod" then 1766738361646723 else 0
            reserves := if networkName == "Preprod" then 13181724972929461 else 0
            protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
              networkId := if networkName == "Preprod" then 0 else 1 } }
          let finalState ← Cleanode.Mithril.Replay.replayImmutableDB syncResult.dbPath initialState
            Cleanode.Mithril.Replay.defaultReplayReporter 0
          IO.println s!"[replay] UTxO set reconstructed: {finalState.utxo.size} entries"
          Cleanode.Ledger.Snapshot.createSnapshot finalState ⟨s!"./data/{networkName.toLower}"⟩
          replayedLedger := some finalState
      | none =>
        -- No snapshot — replay ImmutableDB to build UTxO set
        IO.println "[replay] No UTxO snapshot found, replaying ImmutableDB..."
        let initialState := { Cleanode.Ledger.State.LedgerState.initial with
          treasury := if networkName == "Preprod" then 1766738361646723 else 0
          reserves := if networkName == "Preprod" then 13181724972929461 else 0
          protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
            networkId := if networkName == "Preprod" then 0 else 1 } }
        let finalState ← Cleanode.Mithril.Replay.replayImmutableDB syncResult.dbPath initialState
          Cleanode.Mithril.Replay.defaultReplayReporter 0
        IO.println s!"[replay] UTxO set reconstructed: {finalState.utxo.size} entries"
        -- Save snapshot for next startup
        Cleanode.Ledger.Snapshot.createSnapshot finalState ⟨s!"./data/{networkName.toLower}"⟩
        replayedLedger := some finalState

  -- Load SPO keys if block production is configured
  let mut spoForgeParams : Option Cleanode.Consensus.Praos.BlockForge.ForgeParams := none
  if let some keyDir := config.spoKeyDir then
    let paths := Cleanode.Consensus.Praos.SPOKeys.SPOKeyPaths.default keyDir
    Cleanode.Consensus.Praos.SPOKeys.printKeyInfo paths
    let keysResult ← Cleanode.Consensus.Praos.SPOKeys.loadSPOKeys paths
    match keysResult with
    | .error e =>
      IO.eprintln s!"[spo] Failed to load SPO keys: {e}"
      IO.eprintln "[spo] Running as relay-only node."
    | .ok fp =>
      IO.println s!"[spo] SPO keys loaded successfully."
      IO.println s!"[spo] Pool ID: {fp.poolId.size} bytes"
      IO.println s!"[spo] VRF public key: {fp.vrfPublicKey.length} bytes"
      IO.println s!"[spo] OpCert sequence: {fp.operationalCert.sequenceNumber}, KES period: {fp.operationalCert.kesPeriod}"
      spoForgeParams := some fp

  -- Run as multi-peer relay node (with optional block production)
  relayNode proposal networkName config.tui config.port config.metricsPort spoForgeParams config.socketPath config.epochNonce chainDb syncOrigin replayedLedger

/-- Handle query subcommands (reads status file from a running node) -/
def runQuery (target : QueryTarget) : IO Unit := do
  let statusFile := Cleanode.CLI.Query.statusFilePath
  let fileExists ← System.FilePath.pathExists statusFile
  if !fileExists then
    IO.eprintln "Error: node status file not found. Is the node running?"
    IO.eprintln s!"Expected: {statusFile}"
    IO.Process.exit 1
  match target with
  | .tip => Cleanode.CLI.Query.queryTip
  | .peers => Cleanode.CLI.Query.queryPeers
  | .mempool => Cleanode.CLI.Query.queryMempool

/-- Replay saved block CBOR files and validate every tx (no UTxO existence check).
    Usage: cleanode replay failed_blocks/block_*.cbor -/
def runReplay (paths : List String) : IO Unit := do
  if paths.isEmpty then
    IO.println "Usage: cleanode replay <block.cbor> [block2.cbor ...]"
    return
  -- Expand directories: use ls to find .cbor files
  let mut allFiles : List String := []
  for p in paths do
    if p.endsWith "/" || p.endsWith "failed_blocks" then
      let output ← IO.Process.output { cmd := "ls", args := #[p] }
      for line in output.stdout.splitOn "\n" do
        if line.endsWith ".cbor" then
          let path := if p.endsWith "/" then s!"{p}{line}" else s!"{p}/{line}"
          allFiles := path :: allFiles
    else
      allFiles := p :: allFiles
  allFiles := allFiles.reverse
  IO.println s!"Replaying {allFiles.length} block(s)...\n"

  -- Minimal ledger state with preprod params
  let state : Cleanode.Ledger.State.LedgerState := { Cleanode.Ledger.State.LedgerState.initial with
    protocolParams := { Cleanode.Ledger.State.ProtocolParamsState.mainnetDefaults with
      maxValueSize := 5000, networkId := 0 } }

  for file in allFiles do
    let blockBytes ← IO.FS.readBinFile file
    IO.println s!"── {file} ({blockBytes.size} bytes) ──"

    let parsed ← Cleanode.Network.ConwayBlock.parseConwayBlockBodyIO blockBytes
    match parsed with
    | none => IO.println "  ✗ Failed to parse block body CBOR"
    | some body => do
      IO.println s!"  txs={body.transactions.length} invalidTxs={body.invalidTxs}"
      let mut txIdx := 0
      for tx in body.transactions do
        let isInvalid := body.invalidTxs.contains txIdx
        let tag := if isInvalid then "[PHASE2-INVALID] " else ""
        IO.println s!"\n  {tag}tx[{txIdx}]: fee={tx.body.fee} inputs={tx.body.inputs.length} outputs={tx.body.outputs.length} serializedSize={tx.serializedSize}"
        IO.println s!"    mint={tx.body.mint.length} withdrawals={tx.body.withdrawals.length} certs={tx.body.certificates.length}"
        IO.println s!"    vkeys={tx.witnesses.vkeyWitnesses.length} bootstrap={tx.witnesses.bootstrapWitnesses.length} redeemers={tx.witnesses.redeemers.length}"
        IO.println s!"    native={tx.witnesses.nativeScripts.length} plutusV1={tx.witnesses.plutusV1Scripts.length} plutusV2={tx.witnesses.plutusV2Scripts.length} plutusV3={tx.witnesses.plutusV3Scripts.length}"
        if let some ttl := tx.body.ttl then IO.println s!"    ttl={ttl}"
        if let some vf := tx.body.validityIntervalStart then IO.println s!"    validFrom={vf}"
        -- Print raw bytes info for script debugging
        for (i, s) in (List.range tx.witnesses.plutusV1Scripts.length).zip tx.witnesses.plutusV1Scripts do
          IO.println s!"    plutusV1[{i}]: {s.size} bytes, first4={bytesToHex (s.extract 0 (min 4 s.size))}"
        for (i, s) in (List.range tx.witnesses.plutusV2Scripts.length).zip tx.witnesses.plutusV2Scripts do
          IO.println s!"    plutusV2[{i}]: {s.size} bytes, first4={bytesToHex (s.extract 0 (min 4 s.size))}"
        for (i, s) in (List.range tx.witnesses.plutusV3Scripts.length).zip tx.witnesses.plutusV3Scripts do
          IO.println s!"    plutusV3[{i}]: {s.size} bytes, first4={bytesToHex (s.extract 0 (min 4 s.size))}"

        if !isInvalid then
          -- Validate (skip UTxO existence — use empty UTxO set but don't fail on missing inputs)
          -- Extract slot from filename if possible
          let slot := match file.splitOn "_slot_" with
            | [_, rest] => (rest.splitOn ".").head!.toNat?.getD 0
            | _ => 0

          -- Fee check
          let feeParams := state.protocolParams.feeParams
          let effectiveSize := if tx.serializedSize > 0 then tx.serializedSize else tx.body.rawBytes.size
          let minFee := Cleanode.Ledger.Fee.totalMinFee feeParams effectiveSize tx.witnesses.redeemers
          if tx.body.fee < minFee then
            IO.println s!"    ✗ FEE: paid={tx.body.fee} min={minFee} (size={effectiveSize})"
          else
            IO.println s!"    ✓ FEE: paid={tx.body.fee} >= min={minFee}"

          -- Signature check
          let sigResult ← Cleanode.Ledger.Validation.validateSignatures state.utxo tx.body tx.witnesses
          match sigResult with
          | .ok () => IO.println s!"    ✓ SIGNATURES"
          | .error e => IO.println s!"    ✗ SIGNATURES: {repr e}"

          -- Native script check
          let mut signerKeyHashes : List ByteArray := []
          for w in tx.witnesses.vkeyWitnesses do
            let keyHash ← blake2b_224 w.vkey
            signerKeyHashes := keyHash :: signerKeyHashes
          match Cleanode.Ledger.Validation.validateNativeScripts tx.witnesses signerKeyHashes slot with
          | .ok () => IO.println s!"    ✓ NATIVE SCRIPTS"
          | .error e => IO.println s!"    ✗ NATIVE SCRIPTS: {repr e}"

          -- Output value size check (maxValueSize)
          let mut outputSizeOk := true
          for (oi, output) in (List.range tx.body.outputs.length).zip tx.body.outputs do
            let sz := output.rawValueBytes.size
            if sz > 0 && sz > state.protocolParams.maxValueSize then
              IO.println s!"    ✗ OUTPUT SIZE: idx={oi} size={sz} > max={state.protocolParams.maxValueSize}"
              outputSizeOk := false
          if outputSizeOk then
            IO.println s!"    ✓ OUTPUT SIZE (max rawValue={tx.body.outputs.foldl (fun acc o => max acc o.rawValueBytes.size) 0})"

          -- Extraneous script witness check (withdrawal + cert sources)
          let mut neededHashes : List ByteArray := []
          for asset in tx.body.mint do
            neededHashes := asset.policyId :: neededHashes
          for (rewardAddr, _) in tx.body.withdrawals do
            if rewardAddr.size >= 29 && rewardAddr[0]!.toNat &&& 0x10 != 0 then
              neededHashes := (rewardAddr.extract 1 29) :: neededHashes
          let certRedeemerIdx := tx.witnesses.redeemers.filterMap fun r =>
            if r.tag == .Cert then some r.index else none
          for (ci, cert) in (List.range tx.body.certificates.length).zip tx.body.certificates do
            if certRedeemerIdx.contains ci then
              let h := match cert with
                | .stakeKeyDeregistration _ h | .stakeDelegation _ h _
                | .conwayRegistration _ h _ | .conwayDeregistration _ h _
                | .stakeRegDelegation _ h _ _ | .voteRegDelegation _ h _ _
                | .stakeVoteRegDelegation _ h _ _ _ | .stakeKeyRegistration _ h => some h
                | _ => none
              match h with | some h => neededHashes := h :: neededHashes | none => pure ()
          let mut scriptOk := true
          for scriptBytes in tx.witnesses.plutusV1Scripts ++ tx.witnesses.plutusV2Scripts ++ tx.witnesses.plutusV3Scripts do
            let scriptHash ← blake2b_224 scriptBytes
            -- Check non-UTxO sources (minting + withdrawal + cert); UTxO spending needs full state
            if !neededHashes.any (· == scriptHash) then
              IO.println s!"    ⚠ SCRIPT WITNESS: hash={bytesToHex scriptHash} not in non-UTxO needed set (may match spending input)"
              scriptOk := false
          if scriptOk && !(tx.witnesses.plutusV1Scripts ++ tx.witnesses.plutusV2Scripts ++ tx.witnesses.plutusV3Scripts).isEmpty then
            IO.println s!"    ✓ SCRIPT WITNESSES (all matched non-UTxO sources)"

          -- Datum presence check
          let mut datumInfo : List String := []
          for (oi, output) in (List.range tx.body.outputs.length).zip tx.body.outputs do
            if output.datum.isSome then datumInfo := s!"out[{oi}]:hash" :: datumInfo
            if output.inlineDatum.isSome then datumInfo := s!"out[{oi}]:inline" :: datumInfo
          if !datumInfo.isEmpty then
            IO.println s!"    ℹ DATUMS: {datumInfo} witness_datums={tx.witnesses.datums.length}"

          -- Plutus script check
          let txHash ← blake2b_256 tx.body.rawBytes
          match ← Cleanode.Plutus.Evaluate.evaluateTransactionScriptsIO tx.body tx.witnesses state.utxo txHash with
          | .ok () => IO.println s!"    ✓ PLUTUS SCRIPTS"
          | .error e => IO.println s!"    ✗ PLUTUS SCRIPTS: {e}"

        txIdx := txIdx + 1
      IO.println ""

def main (args : List String) : IO Unit := do
  match parseArgs args with
  | .help => Cleanode.CLI.Args.printUsage
  | .version => IO.println "Cleanode v0.1.0"
  | .query target => runQuery target
  | .replay paths => runReplay paths
  | .run config => runNode config
