import Cleanode.Network.Cbor
import Cleanode.Network.CborCursor

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
open Cleanode.Network.CborCursor

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

/-- Redeemer tag indicating what it's validating -/
inductive RedeemerTag where
  | Spend    -- Validating a script input
  | Mint     -- Validating a minting policy
  | Cert     -- Validating a certificate
  | Reward   -- Validating a reward withdrawal
  deriving Repr, BEq

/-- Execution units (memory and CPU steps) -/
structure ExUnits where
  mem : Nat
  steps : Nat
  deriving Repr

/-- Redeemer for Plutus script execution -/
structure Redeemer where
  tag : RedeemerTag
  index : Nat              -- Which input/mint/cert this redeemer applies to
  data : ByteArray         -- Plutus data (CBOR encoded)
  exUnits : ExUnits        -- Execution budget used

instance : Repr Redeemer where
  reprPrec r _ := s!"Redeemer({repr r.tag}, idx={r.index}, data={r.data.size}B, mem={r.exUnits.mem}, steps={r.exUnits.steps})"

/-- Witness set containing signatures, scripts, and redeemers -/
structure WitnessSet where
  redeemers : List Redeemer
  -- We could add more fields here: vkey_witnesses, scripts, datums, etc.
  deriving Repr

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
  witnesses : WitnessSet

instance : Repr Transaction where
  reprPrec tx _ := s!"Transaction({repr tx.body}, redeemers={tx.witnesses.redeemers.length})"

/-- Parsed Conway block body -/
structure ConwayBlockBody where
  transactions : List Transaction
  invalidTxs : List Nat  -- Indices of invalid transactions

instance : Repr ConwayBlockBody where
  reprPrec b _ := s!"ConwayBlockBody(txCount={b.transactions.length}, invalidCount={b.invalidTxs.length})"

-- ==========================
-- = Cursor-Based Parsing   =
-- ==========================

/-- Parse multi-asset value (native tokens)
    Format: [lovelace_amount, { policy_id => { asset_name => amount } }] -/
partial def parseMultiAssetC (c : Cursor) : Option (CResult (Nat × List NativeAsset)) := do
  let r1 ← CborCursor.decodeArrayHeader c
  if r1.value != 2 then none

  let r2 ← CborCursor.decodeUInt r1.cursor
  let lovelace := r2.value

  let r3 ← CborCursor.decodeMapHeader r2.cursor
  let policyCount := r3.value

  let mut cur := r3.cursor
  let mut assets : List NativeAsset := []

  for _ in [0:policyCount] do
    match CborCursor.decodeBytes cur with
    | none => break
    | some policyResult => do
        let policyId := policyResult.value
        match CborCursor.decodeMapHeader policyResult.cursor with
        | none => break
        | some assetMapResult => do
            let assetCount := assetMapResult.value
            cur := assetMapResult.cursor
            for _ in [0:assetCount] do
              match CborCursor.decodeBytes cur with
              | none => break
              | some nameResult => do
                  match CborCursor.decodeUInt nameResult.cursor with
                  | none => break
                  | some amountResult => do
                      assets := { policyId, assetName := nameResult.value, amount := amountResult.value } :: assets
                      cur := amountResult.cursor

  some { value := (lovelace, assets.reverse), cursor := cur }

/-- Parse transaction input -/
partial def parseTxInputC (c : Cursor) : Option (CResult TxInput) := do
  let r1 ← CborCursor.decodeArrayHeader c
  if r1.value != 2 then none
  let r2 ← CborCursor.decodeBytes r1.cursor
  let r3 ← CborCursor.decodeUInt r2.cursor
  some { value := { txId := r2.value, outputIndex := r3.value }, cursor := r3.cursor }

/-- Parse transaction output -/
partial def parseTxOutputC (c : Cursor) : Option (CResult TxOutput) := do
  if c.remaining == 0 then none
  let major := c.peek >>> 5

  if major == 5 then do  -- Map format (Babbage+)
    let r1 ← CborCursor.decodeMapHeader c
    let mut cur := r1.cursor
    let mut address := ⟨#[]⟩
    let mut amount := 0
    let mut nativeAssets : List NativeAsset := []

    for _ in [0:r1.value] do
      match CborCursor.decodeUInt cur with
      | none => break
      | some keyResult => do
          match keyResult.value with
          | 0 => do  -- Address
              match CborCursor.decodeBytes keyResult.cursor with
              | none => break
              | some addrResult => do
                  address := addrResult.value
                  cur := addrResult.cursor
          | 1 => do  -- Amount (int or multi-asset)
              match CborCursor.decodeUInt keyResult.cursor with
              | some amtResult => do
                  amount := amtResult.value
                  cur := amtResult.cursor
              | none =>
                  match parseMultiAssetC keyResult.cursor with
                  | some multiResult => do
                      amount := multiResult.value.1
                      nativeAssets := multiResult.value.2
                      cur := multiResult.cursor
                  | none =>
                      match skipValue keyResult.cursor with
                      | some after => cur := after
                      | none => break
          | _ => do
              match skipValue keyResult.cursor with
              | some after => cur := after
              | none => break

    some { value := { address, amount, datum := none, nativeAssets }, cursor := cur }
  else do  -- Array format (pre-Babbage)
    let r1 ← CborCursor.decodeArrayHeader c
    if r1.value < 2 then none
    else do
      let r2 ← CborCursor.decodeBytes r1.cursor
      let mut amount := 0
      let mut nativeAssets : List NativeAsset := []
      let r3 ← match CborCursor.decodeUInt r2.cursor with
        | some result => some result
        | none => do
            match parseMultiAssetC r2.cursor with
            | some multiResult => do
                amount := multiResult.value.1
                nativeAssets := multiResult.value.2
                some { value := 0, cursor := multiResult.cursor }
            | none =>
                let after ← skipValue r2.cursor
                some { value := 0, cursor := after }

      if amount == 0 && r3.value != 0 then
        amount := r3.value

      let mut cur := r3.cursor
      for _ in [2:r1.value] do
        match skipValue cur with
        | some after => cur := after
        | none => break

      some { value := { address := r2.value, amount, datum := none, nativeAssets }, cursor := cur }

/-- Parse a single redeemer from CBOR array [tag, index, data, ex_units] -/
partial def parseRedeemerC (c : Cursor) : Option (CResult Redeemer) := do
  let r1 ← CborCursor.decodeArrayHeader c
  if r1.value != 4 then none

  let tagResult ← CborCursor.decodeUInt r1.cursor
  let tag := match tagResult.value with
    | 0 => RedeemerTag.Spend
    | 1 => RedeemerTag.Mint
    | 2 => RedeemerTag.Cert
    | 3 => RedeemerTag.Reward
    | _ => RedeemerTag.Spend

  let indexResult ← CborCursor.decodeUInt tagResult.cursor

  -- Extract raw Plutus data bytes
  let dataStart := indexResult.cursor
  let afterData ← skipValue dataStart
  let dataBytes := extractBetween dataStart afterData

  let exUnitsResult ← CborCursor.decodeArrayHeader afterData
  if exUnitsResult.value != 2 then none
  let memResult ← CborCursor.decodeUInt exUnitsResult.cursor
  let stepsResult ← CborCursor.decodeUInt memResult.cursor

  some {
    value := {
      tag, index := indexResult.value, data := dataBytes,
      exUnits := { mem := memResult.value, steps := stepsResult.value }
    },
    cursor := stepsResult.cursor
  }

/-- Parse redeemers array from witness set -/
partial def parseRedeemersC (c : Cursor) : Option (CResult (List Redeemer)) := do
  let r1 ← CborCursor.decodeArrayHeader c
  let count := r1.value

  let rec parseLoop (cur : Cursor) (acc : List Redeemer) (n : Nat) : Option (CResult (List Redeemer)) :=
    if n == 0 then some { value := acc.reverse, cursor := cur }
    else match parseRedeemerC cur with
      | some r => parseLoop r.cursor (r.value :: acc) (n - 1)
      | none => none

  parseLoop r1.cursor [] count

/-- Parse witness set from CBOR map -/
partial def parseWitnessSetC (c : Cursor) : Option (CResult WitnessSet) := do
  let r1 ← CborCursor.decodeMapHeader c
  let mapSize := r1.value

  let rec parseMapEntries (cur : Cursor) (redeemers : List Redeemer) (n : Nat) : Option (CResult (List Redeemer)) :=
    if n == 0 then some { value := redeemers, cursor := cur }
    else match CborCursor.decodeUInt cur with
      | some keyResult =>
          if keyResult.value == 5 then
            match parseRedeemersC keyResult.cursor with
            | some redResult =>
                -- Skip past the redeemers value to advance cursor
                match skipValue keyResult.cursor with
                | some afterValue => parseMapEntries afterValue redResult.value (n - 1)
                | none => none
            | none => none
          else
            match skipValue keyResult.cursor with
            | some afterValue => parseMapEntries afterValue redeemers (n - 1)
            | none => none
      | none => none

  match parseMapEntries r1.cursor [] mapSize with
  | some r => some { value := { redeemers := r.value }, cursor := r.cursor }
  | none => none

/-- Parse transaction body from CBOR map (Babbage+) -/
partial def parseTransactionBodyMapC (c : Cursor) : Option (CResult TransactionBody) := do
  let bodyStart := c
  let r1 ← CborCursor.decodeMapHeader c
  let mapSize := r1.value

  let mut cur := r1.cursor
  let mut inputs : List TxInput := []
  let mut outputs : List TxOutput := []
  let mut fee : Nat := 0

  for _ in [0:mapSize] do
    match CborCursor.decodeUInt cur with
    | none => break
    | some keyResult => do
        let key := keyResult.value
        cur := keyResult.cursor

        match key with
        | 0 => do  -- Inputs array (may be wrapped in tag 258)
            let afterTag := match CborCursor.skipTag cur with
              | some tagResult => tagResult.cursor
              | none => cur
            match CborCursor.decodeArrayHeader afterTag with
            | none => break
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  match parseTxInputC cur with
                  | none => break
                  | some inputResult => do
                      inputs := inputResult.value :: inputs
                      cur := inputResult.cursor
        | 1 => do  -- Outputs array
            match CborCursor.decodeArrayHeader cur with
            | none => break
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  match parseTxOutputC cur with
                  | none => break
                  | some outputResult => do
                      outputs := outputResult.value :: outputs
                      cur := outputResult.cursor
        | 2 => do  -- Fee
            match CborCursor.decodeUInt cur with
            | none => break
            | some feeResult => do
                fee := feeResult.value
                cur := feeResult.cursor
        | _ => do
            match skipValue cur with
            | none => break
            | some afterSkip => cur := afterSkip

  let rawBytes := extractBetween bodyStart cur

  some {
    value := { inputs := inputs.reverse, outputs := outputs.reverse, fee, rawBytes },
    cursor := cur
  }

/-- Parse transaction body from CBOR array (Shelley/Allegra/Mary/Alonzo) -/
partial def parseTransactionBodyArrayC (c : Cursor) : Option (CResult TransactionBody) := do
  let bodyStart := c
  let r1 ← CborCursor.decodeArrayHeader c
  if r1.value < 3 then none

  let mut cur := r1.cursor

  -- Element 0: Inputs array
  match CborCursor.decodeArrayHeader cur with
  | none => none
  | some inputsResult => do
      let mut inputs : List TxInput := []
      cur := inputsResult.cursor

      for _ in [0:inputsResult.value] do
        match parseTxInputC cur with
        | none => break
        | some inputResult => do
            inputs := inputResult.value :: inputs
            cur := inputResult.cursor

      -- Element 1: Outputs array
      match CborCursor.decodeArrayHeader cur with
      | none => none
      | some outputsResult => do
          let mut outputs : List TxOutput := []
          cur := outputsResult.cursor

          for _ in [0:outputsResult.value] do
            match parseTxOutputC cur with
            | none => break
            | some outputResult => do
                outputs := outputResult.value :: outputs
                cur := outputResult.cursor

          -- Element 2: Fee
          match CborCursor.decodeUInt cur with
          | none => none
          | some feeResult => do
              cur := feeResult.cursor

              -- Skip remaining optional fields
              for _ in [3:r1.value] do
                match skipValue cur with
                | some after => cur := after
                | none => break

              let rawBytes := extractBetween bodyStart cur

              some {
                value := { inputs := inputs.reverse, outputs := outputs.reverse, fee := feeResult.value, rawBytes },
                cursor := cur
              }

/-- Parse transaction body from CBOR map or array -/
partial def parseTransactionBodyC (c : Cursor) : Option (CResult TransactionBody) := do
  if c.remaining == 0 then none
  let major := c.peek >>> 5
  if major == 5 then parseTransactionBodyMapC c
  else if major == 4 then parseTransactionBodyArrayC c
  else none

/-- Parse array of transaction bodies -/
partial def parseTransactionBodiesC (c : Cursor) : Option (CResult (List TransactionBody)) := do
  let r1 ← CborCursor.decodeArrayHeader c
  let txCount := r1.value
  let mut cur := r1.cursor
  let mut bodies : List TransactionBody := []

  for _ in [0:txCount] do
    match parseTransactionBodyC cur with
    | none =>
        match skipValue cur with
        | none => break
        | some afterTx => cur := afterTx
    | some txResult =>
        bodies := txResult.value :: bodies
        cur := txResult.cursor

  some { value := bodies.reverse, cursor := cur }

/-- Parse array of witness sets -/
partial def parseWitnessSetsC (c : Cursor) : Option (CResult (List WitnessSet)) := do
  let r1 ← CborCursor.decodeArrayHeader c
  let witnessCount := r1.value
  let mut cur := r1.cursor
  let mut witnessSets : List WitnessSet := []

  for _ in [0:witnessCount] do
    match parseWitnessSetC cur with
    | none =>
        witnessSets := { redeemers := [] } :: witnessSets
        match skipValue cur with
        | none => break
        | some afterWitness => cur := afterWitness
    | some witnessResult =>
        witnessSets := witnessResult.value :: witnessSets
        cur := witnessResult.cursor

  some { value := witnessSets.reverse, cursor := cur }

-- ==========================
-- = Top-Level Entry Points =
-- ==========================

/-- Parse Conway/Babbage block (full block, not just body) — cursor-based, zero-copy -/
def parseConwayBlockBodyIO (bs : ByteArray) : IO (Option ConwayBlockBody) := do
  let c0 := Cursor.mk' bs

  -- First unwrap the tag24 CBOR wrapper if present (0xd8 0x18)
  let blockCursor ←
    if bs.size >= 2 && bs[0]! == 0xd8 && bs[1]! == 0x18 then do
      match CborCursor.decodeBytes (c0.advance 2) with
      | some r => pure (Cursor.mk' r.value)  -- Unwrapped: new cursor on inner bytes
      | none => pure c0
    else
      pure c0

  -- Block might be wrapped as [era_id, actual_block] or just [header, body_array]
  match CborCursor.decodeArrayHeader blockCursor with
  | none => return none
  | some r1 => do
      let actualCursor ←
        if r1.value == 2 then do
          match CborCursor.decodeUInt r1.cursor with
          | some eraResult =>
              if eraResult.value <= 10 then pure eraResult.cursor
              else pure r1.cursor
          | none => pure r1.cursor
        else
          pure r1.cursor

      match CborCursor.decodeArrayHeader actualCursor with
      | none => return none
      | some blockArray => do
          if blockArray.value >= 5 then do
            -- Skip header (element 0)
            match skipValue blockArray.cursor with
            | none => return none
            | some afterHeader => do
                -- Element 1: Parse tx_bodies array
                match parseTransactionBodiesC afterHeader with
                  | none =>
                      return some { transactions := [], invalidTxs := [] }
                  | some txBodiesResult =>
                      -- Element 2: Parse witnesses array
                      match parseWitnessSetsC txBodiesResult.cursor with
                      | none =>
                          let transactions := txBodiesResult.value.map (fun body =>
                            { body := body, witnesses := { redeemers := [] } }
                          )
                          return some { transactions, invalidTxs := [] }
                      | some witnessResult =>
                          let transactions := List.zipWith (fun body witnesses =>
                            { body := body, witnesses := witnesses }
                          ) txBodiesResult.value witnessResult.value
                          let transactions :=
                            if txBodiesResult.value.length > witnessResult.value.length then
                              transactions ++ (txBodiesResult.value.drop witnessResult.value.length).map (fun body =>
                                { body := body, witnesses := { redeemers := [] } }
                              )
                            else transactions
                          return some { transactions, invalidTxs := [] }
          else
            return none

/-- Parse Conway/Babbage block body (non-IO version for compatibility) -/
partial def parseConwayBlockBody (bs : ByteArray) : Option ConwayBlockBody := do
  let c0 := Cursor.mk' bs

  let blockCursor :=
    if bs.size >= 2 && bs[0]! == 0xd8 && bs[1]! == 0x18 then
      match CborCursor.decodeBytes (c0.advance 2) with
      | some r => Cursor.mk' r.value
      | none => c0
    else
      c0

  let r1 ← CborCursor.decodeArrayHeader blockCursor
  if r1.value < 2 then none

  match parseTransactionBodiesC r1.cursor with
  | none => some { transactions := [], invalidTxs := [] }
  | some txBodiesResult =>
      let transactions := txBodiesResult.value.map (fun body =>
        { body := body, witnesses := { redeemers := [] } }
      )
      some { transactions, invalidTxs := [] }

-- ==========================
-- = Legacy API Compat      =
-- ==========================

-- The old ByteArray-based API functions are kept for files that still
-- import ConwayBlock types but don't call parse functions directly.
-- The old Cbor.lean decode functions remain available via `open Cleanode.Network.Cbor`.

end Cleanode.Network.ConwayBlock
