import Cleanode.Network.Cbor

/-!
# Byron Era Transaction Structure

Byron transactions follow the original Cardano ledger format (pre-Shelley).

## Byron Transaction Format (CDDL)
```
byron_tx = [
  [* byron_tx_in],    -- Inputs
  [* byron_tx_out],   -- Outputs
  attributes          -- Transaction attributes (map)
]

byron_tx_in = [uint, tagged_bytes]  -- (type_tag, CBOR-tagged reference)
byron_tx_out = [address, uint]       -- (encoded address, lovelace amount)
```

## Byron Block Body
```
byron_block_body = [
  [* byron_tx_payload],   -- Transaction payloads
  ssc_payload,             -- SSC (shared seed computation) data
  dlg_payload,             -- Delegation certificates
  update_payload           -- Update proposals/votes
]
```

## References
- cardano-ledger Byron CDDL spec
-/

namespace Cleanode.Network.ByronTx

open Cleanode.Network.Cbor

-- ====================
-- = Byron Tx Types   =
-- ====================

/-- Byron transaction input -/
structure ByronTxInput where
  txId : ByteArray      -- Transaction hash (32 bytes)
  outputIndex : Nat     -- Index of output being spent
  deriving BEq

instance : Repr ByronTxInput where
  reprPrec i _ := s!"ByronTxInput(txId={i.txId.size}B, idx={i.outputIndex})"

/-- Byron transaction output -/
structure ByronTxOutput where
  address : ByteArray   -- Encoded Byron address
  amount : Nat          -- Lovelace amount
  deriving BEq

instance : Repr ByronTxOutput where
  reprPrec o _ := s!"ByronTxOutput(addr={o.address.size}B, amount={o.amount})"

/-- Byron transaction -/
structure ByronTransaction where
  inputs : List ByronTxInput
  outputs : List ByronTxOutput
  attributes : ByteArray   -- Raw CBOR attributes

instance : Repr ByronTransaction where
  reprPrec t _ := s!"ByronTx(inputs={t.inputs.length}, outputs={t.outputs.length})"

/-- Byron transaction witness (signature proof) -/
structure ByronTxWitness where
  witnessType : Nat        -- Witness type tag
  witnessData : ByteArray  -- Signature or script proof

instance : Repr ByronTxWitness where
  reprPrec w _ := s!"ByronTxWitness(type={w.witnessType}, data={w.witnessData.size}B)"

/-- Byron transaction payload (tx + witnesses) -/
structure ByronTxPayload where
  transaction : ByronTransaction
  witnesses : List ByronTxWitness

instance : Repr ByronTxPayload where
  reprPrec p _ := s!"ByronTxPayload(tx={repr p.transaction}, witnesses={p.witnesses.length})"

/-- Byron block body -/
structure ByronBlockBody where
  txPayloads : List ByronTxPayload
  sscPayload : ByteArray      -- Raw SSC data
  dlgPayload : ByteArray      -- Raw delegation data
  updatePayload : ByteArray   -- Raw update data

instance : Repr ByronBlockBody where
  reprPrec b _ := s!"ByronBlockBody(txs={b.txPayloads.length})"

-- ====================
-- = Parsing          =
-- ====================

/-- Parse a Byron transaction input -/
partial def parseByronTxInput (bs : ByteArray) : Option (DecodeResult ByronTxInput) := do
  -- Byron inputs are: [type_tag, tagged_cbor_data]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- Type tag (usually 0 for regular tx input)
  let r2 ← decodeUInt r1.remaining

  -- Tagged CBOR data: tag 24 wrapping [txId, outputIndex]
  let afterTag := match skipTag r2.remaining with
    | some r => r.remaining
    | none => r2.remaining

  -- Decode the wrapped bytes
  let r3 ← decodeBytes afterTag
  -- Inside the bytes: [txId_hash, output_index]
  let r4 ← decodeArrayHeader r3.value
  if r4.value != 2 then none

  let r5 ← decodeBytes r4.remaining
  let txId := r5.value

  let r6 ← decodeUInt r5.remaining
  let outputIndex := r6.value

  some { value := { txId := txId, outputIndex := outputIndex }, remaining := r3.remaining }

/-- Parse a Byron transaction output -/
partial def parseByronTxOutput (bs : ByteArray) : Option (DecodeResult ByronTxOutput) := do
  -- Byron outputs are: [address, amount]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- Address (CBOR-encoded Byron address)
  let addrStart := r1.remaining
  let afterAddr ← skipCborValue r1.remaining
  let addrLen := addrStart.size - afterAddr.size
  let address := addrStart.extract 0 addrLen

  -- Amount (lovelace)
  let r3 ← decodeUInt afterAddr
  let amount := r3.value

  some { value := { address := address, amount := amount }, remaining := r3.remaining }

/-- Parse a Byron transaction -/
partial def parseByronTransaction (bs : ByteArray) : Option (DecodeResult ByronTransaction) := do
  -- Byron tx: [inputs, outputs, attributes]
  let r1 ← decodeArrayHeader bs
  if r1.value != 3 then none

  -- Parse inputs array
  let r2 ← decodeArrayHeader r1.remaining
  let inputCount := r2.value
  let mut remaining := r2.remaining
  let mut inputs : List ByronTxInput := []

  for _ in [0:inputCount] do
    match parseByronTxInput remaining with
    | none =>
        match skipCborValue remaining with
        | some after => remaining := after
        | none => break
    | some r =>
        inputs := r.value :: inputs
        remaining := r.remaining

  -- Parse outputs array
  let r3 ← decodeArrayHeader remaining
  let outputCount := r3.value
  remaining := r3.remaining
  let mut outputs : List ByronTxOutput := []

  for _ in [0:outputCount] do
    match parseByronTxOutput remaining with
    | none =>
        match skipCborValue remaining with
        | some after => remaining := after
        | none => break
    | some r =>
        outputs := r.value :: outputs
        remaining := r.remaining

  -- Attributes (raw CBOR map)
  let attrStart := remaining
  let afterAttrs ← skipCborValue remaining
  let attrLen := attrStart.size - afterAttrs.size
  let attributes := attrStart.extract 0 attrLen

  some {
    value := {
      inputs := inputs.reverse,
      outputs := outputs.reverse,
      attributes := attributes
    },
    remaining := afterAttrs
  }

/-- Parse a Byron transaction witness -/
partial def parseByronTxWitness (bs : ByteArray) : Option (DecodeResult ByronTxWitness) := do
  -- Witness: [type_tag, witness_data]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  let r2 ← decodeUInt r1.remaining
  let witnessType := r2.value

  -- Witness data (tagged CBOR)
  let dataStart := r2.remaining
  let afterData ← skipCborValue r2.remaining
  let dataLen := dataStart.size - afterData.size
  let witnessData := dataStart.extract 0 dataLen

  some { value := { witnessType := witnessType, witnessData := witnessData }, remaining := afterData }

/-- Parse a Byron transaction payload (tx + witnesses) -/
partial def parseByronTxPayload (bs : ByteArray) : Option (DecodeResult ByronTxPayload) := do
  -- Payload: [transaction, [witnesses]]
  let r1 ← decodeArrayHeader bs
  if r1.value != 2 then none

  -- Parse transaction
  let txResult ← parseByronTransaction r1.remaining

  -- Parse witnesses array
  let r2 ← decodeArrayHeader txResult.remaining
  let witnessCount := r2.value
  let mut remaining := r2.remaining
  let mut witnesses : List ByronTxWitness := []

  for _ in [0:witnessCount] do
    match parseByronTxWitness remaining with
    | none =>
        match skipCborValue remaining with
        | some after => remaining := after
        | none => break
    | some r =>
        witnesses := r.value :: witnesses
        remaining := r.remaining

  some {
    value := {
      transaction := txResult.value,
      witnesses := witnesses.reverse
    },
    remaining := remaining
  }

/-- Parse Byron block body -/
partial def parseByronBlockBody (bs : ByteArray) : Option ByronBlockBody := do
  -- Block body: [tx_payloads, ssc, dlg, update]
  let r1 ← decodeArrayHeader bs
  if r1.value < 4 then none

  -- Element 0: Transaction payloads (wrapped in another array/tag structure)
  -- Byron wraps txs as: [tag24([tx_payload_1, tx_payload_2, ...])]
  let txsStart := r1.remaining
  let mut remaining := r1.remaining
  let mut txPayloads : List ByronTxPayload := []

  -- Try to parse transaction payloads
  match skipTag remaining with
  | some tagResult =>
      -- Tagged: skip tag, decode inner bytes, then parse
      match decodeBytes tagResult.remaining with
      | some innerResult =>
          match decodeArrayHeader innerResult.value with
          | some arrResult =>
              let mut txRemaining := arrResult.remaining
              for _ in [0:arrResult.value] do
                match parseByronTxPayload txRemaining with
                | none =>
                    match skipCborValue txRemaining with
                    | some after => txRemaining := after
                    | none => break
                | some r =>
                    txPayloads := r.value :: txPayloads
                    txRemaining := r.remaining
              remaining := innerResult.remaining
          | none => remaining := innerResult.remaining
      | none =>
          match skipCborValue remaining with
          | some after => remaining := after
          | none => pure ()
  | none =>
      -- Not tagged, try direct array
      match skipCborValue remaining with
      | some after => remaining := after
      | none => pure ()

  -- Skip to get raw bytes for SSC, DLG, Update
  let sscStart := remaining
  let afterSsc ← skipCborValue remaining
  let sscPayload := sscStart.extract 0 (sscStart.size - afterSsc.size)

  let dlgStart := afterSsc
  let afterDlg ← skipCborValue afterSsc
  let dlgPayload := dlgStart.extract 0 (dlgStart.size - afterDlg.size)

  let updStart := afterDlg
  let afterUpd ← skipCborValue afterDlg
  let updatePayload := updStart.extract 0 (updStart.size - afterUpd.size)

  some {
    txPayloads := txPayloads.reverse,
    sscPayload := sscPayload,
    dlgPayload := dlgPayload,
    updatePayload := updatePayload
  }

end Cleanode.Network.ByronTx
