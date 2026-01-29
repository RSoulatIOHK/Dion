import Cleanode.Network.Cbor

/-!
# Conway Era Block Body Parser

Conway (and Babbage) blocks have a specific structure containing transactions.

## Block Body Format

The block body is a CBOR array with 4-5 elements:
```
[
  transaction_bodies,         -- Array of transaction bodies
  transaction_witness_sets,   -- Array of witness sets (signatures, scripts, etc.)
  auxiliary_data_set,         -- Map from tx index to auxiliary data
  invalid_transactions        -- Set of invalid transaction indices (optional)
]
```

Each transaction body contains:
- Inputs (UTxOs being spent)
- Outputs (new UTxOs being created)
- Fee amount
- TTL (time-to-live)
- Certificates (stake pool operations, etc.)
- Withdrawals (rewards)
- Update proposals
- Metadata hash
- Validity interval
- Mint/burn operations
- Script data hash
- Collateral inputs
- Required signers
- Network ID
- Collateral return
- Total collateral
- Reference inputs

## References
- CIP-0009: Babbage block format
- cardano-ledger Conway specs
-/

namespace Cleanode.Network.ConwayBlock

open Cleanode.Network.Cbor

-- ==============
-- = Core Types =
-- ==============

/-- Transaction input (reference to a UTxO) -/
structure TxInput where
  txId : ByteArray      -- Transaction hash (32 bytes)
  outputIndex : Nat     -- Index of output in that transaction

instance : Repr TxInput where
  reprPrec i _ := s!"TxInput(txId={i.txId.size}B, index={i.outputIndex})"

/-- Native asset (non-ADA token) -/
structure NativeAsset where
  policyId : ByteArray    -- Policy ID (28 bytes)
  assetName : ByteArray   -- Asset name (up to 32 bytes)
  amount : Nat            -- Token amount

instance : Repr NativeAsset where
  reprPrec a _ := s!"NativeAsset(policy={a.policyId.size}B, name={a.assetName.size}B, amount={a.amount})"

/-- Transaction output (new UTxO) -/
structure TxOutput where
  address : ByteArray         -- Cardano address (variable length)
  amount : Nat                -- Lovelace amount
  datum : Option ByteArray    -- Optional datum for smart contracts
  nativeAssets : List NativeAsset  -- Native tokens (non-ADA assets)

instance : Repr TxOutput where
  reprPrec o _ := s!"TxOutput(addr={o.address.size}B, amount={o.amount}, assets={o.nativeAssets.length})"

/-- Transaction body (core transaction data) -/
structure TransactionBody where
  inputs : List TxInput
  outputs : List TxOutput
  fee : Nat
  rawBytes : ByteArray  -- Raw CBOR bytes for computing TxId
  -- Simplified for now - full structure has many more fields

instance : Repr TransactionBody where
  reprPrec tx _ := s!"TxBody(inputs={tx.inputs.length}, outputs={tx.outputs.length}, fee={tx.fee})"

/-- Complete transaction with witnesses -/
structure Transaction where
  body : TransactionBody
  witnessSetSize : Nat  -- Size of witness set (we won't parse it fully yet)

instance : Repr Transaction where
  reprPrec tx _ := s!"Transaction({repr tx.body}, witnesses={tx.witnessSetSize}B)"

/-- Parsed Conway block body -/
structure ConwayBlockBody where
  transactions : List Transaction
  invalidTxs : List Nat  -- Indices of invalid transactions

instance : Repr ConwayBlockBody where
  reprPrec b _ := s!"ConwayBlockBody(txCount={b.transactions.length}, invalidCount={b.invalidTxs.length})"

-- ==============
-- = Parsing    =
-- ==============

/-- Parse multi-asset value (native tokens)
    Format: [lovelace_amount, { policy_id => { asset_name => amount } }] -/
partial def parseMultiAsset (bs : ByteArray) : Option (DecodeResult (Nat × List NativeAsset)) := do
  -- Multi-asset is an array: [lovelace, assets_map]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- Element 0: Lovelace amount
  let r2 ← decodeUInt r1.remaining
  let lovelace := r2.value

  -- Element 1: Map of policy IDs to asset maps
  let r3 ← decodeMapHeader r2.remaining
  let policyCount := r3.value

  let mut remaining := r3.remaining
  let mut assets : List NativeAsset := []

  -- Parse each policy ID
  for _ in [0:policyCount] do
    match decodeBytes remaining with
    | none => break
    | some policyResult => do
        let policyId := policyResult.value

        -- Parse asset map for this policy
        match decodeMapHeader policyResult.remaining with
        | none => break
        | some assetMapResult => do
            let assetCount := assetMapResult.value
            remaining := assetMapResult.remaining

            -- Parse each asset name and amount
            for _ in [0:assetCount] do
              match decodeBytes remaining with
              | none => break
              | some nameResult => do
                  let assetName := nameResult.value

                  match decodeUInt nameResult.remaining with
                  | none => break
                  | some amountResult => do
                      let asset : NativeAsset := {
                        policyId := policyId,
                        assetName := assetName,
                        amount := amountResult.value
                      }
                      assets := asset :: assets
                      remaining := amountResult.remaining

  some { value := (lovelace, assets.reverse), remaining := remaining }

/-- Parse transaction input -/
partial def parseTxInput (bs : ByteArray) : Option (DecodeResult TxInput) := do
  -- Input is: [tx_hash, output_index]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- Transaction hash (32 bytes)
  let r2 ← decodeBytes r1.remaining
  let txId := r2.value

  -- Output index
  let r3 ← decodeUInt r2.remaining
  let outputIndex := r3.value

  some {
    value := { txId := txId, outputIndex := outputIndex },
    remaining := r3.remaining
  }

/-- Parse transaction output -/
partial def parseTxOutput (bs : ByteArray) : Option (DecodeResult TxOutput) := do
  -- Output can be:
  -- Pre-Alonzo: [address, amount]
  -- Post-Alonzo: [address, amount, datum_option?]
  -- Post-Babbage: { 0: address, 1: amount, 2: datum_option?, 3: script_ref? }

  -- Check if it's a map (Babbage+) or array (earlier eras)
  let firstByte := bs[0]!
  let major := firstByte >>> 5

  if major == 5 then do  -- Map format (Babbage+)
    let r1 ← decodeMapHeader bs
    let mut remaining := r1.remaining
    let mut address := ⟨#[]⟩
    let mut amount := 0
    let mut datum := none
    let mut nativeAssets : List NativeAsset := []

    -- Parse map entries
    for _ in [0:r1.value] do
      match decodeUInt remaining with
      | none => break
      | some keyResult => do
          match keyResult.value with
          | 0 => do  -- Address
              match decodeBytes keyResult.remaining with
              | none => break
              | some addrResult => do
                  address := addrResult.value
                  remaining := addrResult.remaining
          | 1 => do  -- Amount (could be int or array for multi-asset)
              match decodeUInt keyResult.remaining with
              | some amtResult => do
                  amount := amtResult.value
                  remaining := amtResult.remaining
              | none =>  -- Multi-asset value
                  match parseMultiAsset keyResult.remaining with
                  | some multiResult => do
                      amount := multiResult.value.1
                      nativeAssets := multiResult.value.2
                      remaining := multiResult.remaining
                  | none =>  -- Failed to parse, skip it
                      match skipCborValue keyResult.remaining with
                      | some after => remaining := after
                      | none => break
          | _ => do  -- Skip other fields
              match skipCborValue keyResult.remaining with
              | some after => remaining := after
              | none => break

    some { value := { address := address, amount := amount, datum := datum, nativeAssets := nativeAssets }, remaining := remaining }
  else do  -- Array format (pre-Babbage)
    let r1 ← decodeArrayHeader bs
    if r1.value < 2 then none
    else do
      -- Address
      let r2 ← decodeBytes r1.remaining

      -- Amount (try int first, then multi-asset)
      let mut amount := 0
      let mut nativeAssets : List NativeAsset := []
      let r3 ← match decodeUInt r2.remaining with
        | some result => some result
        | none => do  -- Multi-asset amount
            match parseMultiAsset r2.remaining with
            | some multiResult => do
                amount := multiResult.value.1
                nativeAssets := multiResult.value.2
                some { value := 0, remaining := multiResult.remaining }
            | none =>  -- Failed to parse, skip it
                let afterAmount ← skipCborValue r2.remaining
                some { value := 0, remaining := afterAmount }

      -- If we got the amount from decodeUInt, use it
      if amount == 0 && r3.value != 0 then
        amount := r3.value

      -- Skip any remaining fields (datum, etc.)
      let mut remaining := r3.remaining
      for _ in [2:r1.value] do
        match skipCborValue remaining with
        | some after => remaining := after
        | none => break

      some {
        value := { address := r2.value, amount := amount, datum := none, nativeAssets := nativeAssets },
        remaining := remaining
      }

/-- Parse transaction body from CBOR map (Babbage+) -/
partial def parseTransactionBodyMap (bs : ByteArray) : Option (DecodeResult TransactionBody) := do
  let r1 ← decodeMapHeader bs
  let mapSize := r1.value

  let mut remaining := r1.remaining
  let mut inputs : List TxInput := []
  let mut outputs : List TxOutput := []
  let mut fee : Nat := 0

  -- Parse each key-value pair in the map
  for _ in [0:mapSize] do
    -- Read the key
    match decodeUInt remaining with
    | none => break
    | some keyResult => do
        let key := keyResult.value
        remaining := keyResult.remaining

        match key with
        | 0 => do  -- Inputs array (may be wrapped in tag 258 for sets)
            -- Try to skip tag if present (tag 258 = set wrapper)
            let afterTag := match skipTag remaining with
              | some tagResult => tagResult.remaining  -- Skip the tag
              | none => remaining  -- No tag, use as-is

            match decodeArrayHeader afterTag with
            | none => break
            | some arrResult => do
                let inputCount := arrResult.value
                remaining := arrResult.remaining

                for _ in [0:inputCount] do
                  match parseTxInput remaining with
                  | none => break
                  | some inputResult => do
                      inputs := inputResult.value :: inputs
                      remaining := inputResult.remaining
        | 1 => do  -- Outputs array
            match decodeArrayHeader remaining with
            | none => break
            | some arrResult => do
                let outputCount := arrResult.value
                remaining := arrResult.remaining

                for _ in [0:outputCount] do
                  match parseTxOutput remaining with
                  | none => break
                  | some outputResult => do
                      outputs := outputResult.value :: outputs
                      remaining := outputResult.remaining
        | 2 => do  -- Fee
            match decodeUInt remaining with
            | none => break
            | some feeResult => do
                fee := feeResult.value
                remaining := feeResult.remaining
        | _ => do  -- Skip unknown fields
            match skipCborValue remaining with
            | none => break
            | some afterSkip => remaining := afterSkip

  -- Extract raw bytes (from start to where remaining starts)
  let rawByteCount := bs.size - remaining.size
  let rawBytes := bs.extract 0 rawByteCount

  some {
    value := {
      inputs := inputs.reverse,
      outputs := outputs.reverse,
      fee := fee,
      rawBytes := rawBytes
    },
    remaining := remaining
  }

/-- Parse transaction body from CBOR array (Shelley/Allegra/Mary/Alonzo) -/
partial def parseTransactionBodyArray (bs : ByteArray) : Option (DecodeResult TransactionBody) := do
  -- Array format: [inputs, outputs, fee, ttl?, certificates?, withdrawals?, ...]
  let r1 ← decodeArrayHeader bs
  if r1.value < 3 then none  -- Must have at least inputs, outputs, fee

  let mut remaining := r1.remaining

  -- Element 0: Inputs array
  match decodeArrayHeader remaining with
  | none => none
  | some inputsResult => do
      let mut inputs : List TxInput := []
      remaining := inputsResult.remaining

      for _ in [0:inputsResult.value] do
        match parseTxInput remaining with
        | none => break  -- Skip failed input parse
        | some inputResult => do
            inputs := inputResult.value :: inputs
            remaining := inputResult.remaining

      -- Element 1: Outputs array
      match decodeArrayHeader remaining with
      | none => none
      | some outputsResult => do
          let mut outputs : List TxOutput := []
          remaining := outputsResult.remaining

          for _ in [0:outputsResult.value] do
            match parseTxOutput remaining with
            | none => break  -- Skip failed output parse
            | some outputResult => do
                outputs := outputResult.value :: outputs
                remaining := outputResult.remaining

          -- Element 2: Fee
          match decodeUInt remaining with
          | none => none
          | some feeResult => do
              remaining := feeResult.remaining

              -- Skip remaining optional fields
              for _ in [3:r1.value] do
                match skipCborValue remaining with
                | some after => remaining := after
                | none => break

              -- Extract raw bytes (from start to where remaining starts)
              let rawByteCount := bs.size - remaining.size
              let rawBytes := bs.extract 0 rawByteCount

              some {
                value := {
                  inputs := inputs.reverse,
                  outputs := outputs.reverse,
                  fee := feeResult.value,
                  rawBytes := rawBytes
                },
                remaining := remaining
              }

/-- Parse transaction body from CBOR map or array -/
partial def parseTransactionBody (bs : ByteArray) : Option (DecodeResult TransactionBody) := do
  -- Transaction bodies can be either maps (Babbage+) or arrays (Shelley/Allegra/Mary/Alonzo)
  if bs.size == 0 then none
  else
    let firstByte := bs[0]!
    let major := firstByte >>> 5

    if major == 5 then
      -- Map format (Babbage/Conway)
      parseTransactionBodyMap bs
    else if major == 4 then
      -- Array format (Shelley/Allegra/Mary/Alonzo)
      parseTransactionBodyArray bs
    else
      none

/-- Parse array of transaction bodies -/
partial def parseTransactionBodiesIO (bs : ByteArray) : IO (Option (List TransactionBody)) := do
  -- The body should be a CBOR array of transaction bodies
  match decodeArrayHeader bs with
  | none => return none
  | some r1 => do
      let txCount := r1.value
      let mut remaining := r1.remaining
      let mut bodies : List TransactionBody := []

      for _ in [0:txCount] do
        -- Parse each transaction body using the existing parser
        match parseTransactionBody remaining with
        | none =>
            -- If parsing fails, skip this malformed transaction
            match skipCborValue remaining with
            | none => break
            | some afterTx => remaining := afterTx
        | some txResult =>
            bodies := txResult.value :: bodies
            remaining := txResult.remaining

      return some bodies.reverse

/-- Parse array of transaction bodies (non-IO version for compatibility) -/
partial def parseTransactionBodies (bs : ByteArray) : Option (List TransactionBody) := do
  let r1 ← decodeArrayHeader bs
  let txCount := r1.value

  let mut remaining := r1.remaining
  let mut bodies : List TransactionBody := []

  for _ in [0:txCount] do
    -- Parse each transaction body
    match parseTransactionBody remaining with
    | none =>
        -- Skip malformed transactions
        match skipCborValue remaining with
        | none => break
        | some afterTx => remaining := afterTx
    | some txResult =>
        bodies := txResult.value :: bodies
        remaining := txResult.remaining

  some bodies.reverse

/-- Parse Conway/Babbage block (full block, not just body) -/
def parseConwayBlockBodyIO (bs : ByteArray) : IO (Option ConwayBlockBody) := do
  -- First unwrap the tag24 CBOR wrapper if present (0xd8 0x18)
  let blockBytes ←
    if bs.size >= 2 && bs[0]! == 0xd8 && bs[1]! == 0x18 then do
      match decodeBytes (bs.extract 2 bs.size) with
      | some r => pure r.value
      | none => pure bs
    else
      pure bs

  -- Block might be wrapped as [era_id, actual_block] or just [header, body_array]
  -- Modern blocks (Alonzo+) are [era, [header, tx_bodies, witnesses, aux, invalid]]
  -- Where era is a small int (0-7) and the actual block has 5 elements
  match decodeArrayHeader blockBytes with
  | none => return none
  | some r1 => do
      -- If we have 2 elements and first is a small int, it's [era, actual_block]
      let actualBlock ←
        if r1.value == 2 then do
          -- Try to decode era indicator
          match decodeUInt r1.remaining with
          | some eraResult =>
              if eraResult.value <= 10 then  -- Era IDs are small (0-10)
                pure eraResult.remaining
              else
                pure r1.remaining
          | none => pure r1.remaining
        else
          pure r1.remaining

      -- Now decode the actual block structure
      match decodeArrayHeader actualBlock with
      | none => return none
      | some blockArray => do
          -- Modern format: [header, tx_bodies, witnesses, aux, invalid] (5 elements)
          if blockArray.value >= 5 then do
            -- Skip header (element 0)
            match skipCborValue blockArray.remaining with
            | none => return none
            | some afterHeader => do
                -- Element 1 is tx_bodies array directly
                match ← parseTransactionBodiesIO afterHeader with
                  | none =>
                      return some {
                        transactions := [],
                        invalidTxs := []
                      }
                  | some txBodies =>
                      let transactions := txBodies.map (fun body =>
                        { body := body, witnessSetSize := 0 }
                      )
                      return some {
                        transactions := transactions,
                        invalidTxs := []
                      }
          else
            -- Old format not supported yet
            return none

/-- Parse Conway/Babbage block body (non-IO version for compatibility) -/
partial def parseConwayBlockBody (bs : ByteArray) : Option ConwayBlockBody := do
  -- First unwrap the tag24 CBOR wrapper if present
  let blockBytes :=
    if bs.size >= 2 && bs[0]! == 0xd8 && bs[1]! == 0x18 then
      -- tag24 wrapper: skip 2 bytes, then decode the bytestring
      match decodeBytes (bs.extract 2 bs.size) with
      | some r => r.value
      | none => bs
    else
      bs

  -- Block body is an array: [tx_bodies, tx_witness_sets, aux_data?, invalid_txs?]
  let r1 ← decodeArrayHeader blockBytes
  if r1.value < 2 then none

  -- Element 0: Transaction bodies (array)
  match parseTransactionBodies r1.remaining with
  | none =>
      some {
        transactions := [],
        invalidTxs := []
      }
  | some txBodies =>
      let transactions := txBodies.map (fun body =>
        { body := body, witnessSetSize := 0 }
      )
      some {
        transactions := transactions,
        invalidTxs := []
      }

end Cleanode.Network.ConwayBlock
