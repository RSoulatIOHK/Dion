import Dion.Network.ConwayBlock
import Dion.Network.CborCursor
import Dion.Ledger.UTxO
import Dion.Ledger.Value

/-!
# Plutus Script Context

Constructs the `ScriptContext` that is passed as the final argument to Plutus scripts.
The context is encoded as Plutus Data (the `PlutusData` type below).

## Conway ScriptContext Structure (PlutusV3)

```
ScriptContext = Constr 0 [txInfo, redeemer, scriptInfo]
TxInfo = Constr 0 [inputs, refInputs, outputs, fee, mint, certs, wdrl,
                    validRange, signatories, redeemers, datums, id]
ScriptPurpose:
  Minting   = Constr 0 [policyId]
  Spending  = Constr 1 [txOutRef, datum]
  Rewarding = Constr 2 [credential]
  Certifying = Constr 3 [index, cert]
  Voting    = Constr 4 [voter]
  Proposing = Constr 5 [index, procedure]
```

## References
- Plutus Core Spec: ScriptContext encoding
- CIP-0069: PlutusV3 script context
-/

namespace Dion.Plutus.ScriptContext

open Dion.Network.ConwayBlock
open Dion.Ledger.UTxO
open Dion.Ledger.Value

-- ====================
-- = Plutus Data      =
-- ====================

/-- Plutus Data type — the universal data encoding for Plutus scripts.
    This mirrors the on-chain `Data` type exactly. -/
inductive PlutusData where
  | Constr (tag : Nat) (fields : List PlutusData)
  | Map (entries : List (PlutusData × PlutusData))
  | List (items : List PlutusData)
  | Integer (value : Int)
  | ByteString (bytes : ByteArray)
  deriving BEq

partial def PlutusData.repr : PlutusData → String
  | .Constr tag fields =>
    let fs := fields.map PlutusData.repr |>.intersperse ", " |> String.join
    s!"Constr({tag}, [{fs}])"
  | .Map entries =>
    let es := entries.map (fun (k, v) => s!"({PlutusData.repr k}, {PlutusData.repr v})")
      |>.intersperse ", " |> String.join
    s!"Map([{es}])"
  | .List items =>
    let is_ := items.map PlutusData.repr |>.intersperse ", " |> String.join
    s!"List([{is_}])"
  | .Integer v => s!"I({v})"
  | .ByteString bs => s!"B({bs.size}B)"

instance : Repr PlutusData where
  reprPrec d _ := PlutusData.repr d

-- ==============================
-- = PlutusData CBOR Decoder    =
-- ==============================

mutual
  /-- Decode a single PlutusData value from a cursor position. -/
  private partial def decodePD (c : Dion.Network.CborCursor.Cursor) :
      Option (Dion.Network.CborCursor.CResult PlutusData) :=
    if c.remaining == 0 then none
    else
      let major := c.peek.toNat >>> 5
      match major with
      | 0 =>
        Dion.Network.CborCursor.decodeUInt c |>.map fun r =>
          { value := .Integer (Int.ofNat r.value), cursor := r.cursor }
      | 1 =>
        Dion.Network.CborCursor.decodeHead c |>.map fun r =>
          { value := .Integer (-1 - Int.ofNat r.value.2), cursor := r.cursor }
      | 2 =>
        Dion.Network.CborCursor.decodeBytes c |>.map fun r =>
          { value := .ByteString r.value, cursor := r.cursor }
      | 4 =>
        Dion.Network.CborCursor.decodeArrayHeader c >>= fun hdr =>
        decodePDList hdr.value hdr.cursor |>.map fun (items, cur') =>
          { value := .List items, cursor := cur' }
      | 5 =>
        Dion.Network.CborCursor.decodeMapHeader c >>= fun hdr =>
        decodePDPairs hdr.value hdr.cursor |>.map fun (pairs, cur') =>
          { value := .Map pairs, cursor := cur' }
      | 6 =>
        Dion.Network.CborCursor.skipTag c >>= fun r =>
        let tag := r.value
        if tag >= 121 && tag <= 127 then
          Dion.Network.CborCursor.decodeArrayHeader r.cursor >>= fun hdr =>
          decodePDList hdr.value hdr.cursor |>.map fun (fields, cur') =>
            { value := .Constr (tag - 121) fields, cursor := cur' }
        else if tag >= 1280 && tag <= 1400 then
          Dion.Network.CborCursor.decodeArrayHeader r.cursor >>= fun hdr =>
          decodePDList hdr.value hdr.cursor |>.map fun (fields, cur') =>
            { value := .Constr (tag - 1280 + 7) fields, cursor := cur' }
        else if tag == 102 then
          -- Alternative encoding: tag 102 wrapping [constrIdx, [fields]]
          Dion.Network.CborCursor.decodeArrayHeader r.cursor >>= fun outer =>
          if outer.value < 2 then none
          else
            Dion.Network.CborCursor.decodeUInt outer.cursor >>= fun tagR =>
            Dion.Network.CborCursor.decodeArrayHeader tagR.cursor >>= fun fHdr =>
            decodePDList fHdr.value fHdr.cursor |>.map fun (fields, cur') =>
              { value := .Constr tagR.value fields, cursor := cur' }
        else none
      | _ => none

  /-- Decode n PlutusData items (n=9999 = indefinite). -/
  private partial def decodePDList (n : Nat) (c : Dion.Network.CborCursor.Cursor) :
      Option (List PlutusData × Dion.Network.CborCursor.Cursor) :=
    if n == 9999 && Dion.Network.CborCursor.isBreak c then some ([], c.advance 1)
    else if n == 0 then some ([], c)
    else
      decodePD c >>= fun r =>
      decodePDList (if n == 9999 then 9999 else n - 1) r.cursor |>.map fun (rest, cur') =>
        (r.value :: rest, cur')

  /-- Decode n key-value pairs of PlutusData (n=9999 = indefinite). -/
  private partial def decodePDPairs (n : Nat) (c : Dion.Network.CborCursor.Cursor) :
      Option (List (PlutusData × PlutusData) × Dion.Network.CborCursor.Cursor) :=
    if n == 9999 && Dion.Network.CborCursor.isBreak c then some ([], c.advance 1)
    else if n == 0 then some ([], c)
    else
      decodePD c >>= fun k =>
      decodePD k.cursor >>= fun v =>
      decodePDPairs (if n == 9999 then 9999 else n - 1) v.cursor |>.map fun (rest, cur') =>
        ((k.value, v.value) :: rest, cur')
end -- mutual

/-- Decode PlutusData from a CBOR-encoded byte array.
    Returns `none` on malformed input; falls back to `.ByteString` at call sites. -/
def decodePlutusDataFromCbor (bs : ByteArray) : Option PlutusData :=
  decodePD (Dion.Network.CborCursor.Cursor.mk' bs) |>.map (·.value)

-- ==============================
-- = Certificate PlutusData     =
-- ==============================

/-- Encode a RawCertificate as PlutusData (used in both TxInfo certs list and ScriptInfo). -/
def encodeCertPlutusData (cert : RawCertificate) : PlutusData :=
  match cert with
  | .stakeKeyRegistration _ kh => .Constr 0 [.Constr 0 [.ByteString kh]]
  | .stakeKeyDeregistration _ kh => .Constr 1 [.Constr 0 [.ByteString kh]]
  | .stakeDelegation _ kh poolId => .Constr 2 [.Constr 0 [.ByteString kh], .ByteString poolId]
  | .poolRegistration poolId vrfHash _ _ _ _ _ =>
      .Constr 3 [.ByteString poolId, .ByteString vrfHash]
  | .poolRetirement poolId epoch =>
      .Constr 4 [.ByteString poolId, .Integer (Int.ofNat epoch)]
  | .conwayRegistration _ kh deposit =>
      .Constr 5 [.Constr 0 [.ByteString kh], .Integer (Int.ofNat deposit)]
  | .conwayDeregistration _ kh refund =>
      .Constr 6 [.Constr 0 [.ByteString kh], .Integer (Int.ofNat refund)]
  | .voteDelegation _ kh drepCred =>
      .Constr 7 [.Constr 0 [.ByteString kh], .ByteString drepCred]
  | .stakeVoteDelegation _ kh poolId drepCred =>
      .Constr 8 [.Constr 0 [.ByteString kh], .ByteString poolId, .ByteString drepCred]
  | .stakeRegDelegation _ kh poolId deposit =>
      .Constr 9 [.Constr 0 [.ByteString kh], .ByteString poolId, .Integer (Int.ofNat deposit)]
  | .voteRegDelegation _ kh drepCred deposit =>
      .Constr 10 [.Constr 0 [.ByteString kh], .ByteString drepCred, .Integer (Int.ofNat deposit)]
  | .stakeVoteRegDelegation _ kh poolId drepCred deposit =>
      .Constr 11 [.Constr 0 [.ByteString kh], .ByteString poolId, .ByteString drepCred, .Integer (Int.ofNat deposit)]
  | .authCommitteeHot _ cold hot =>
      .Constr 12 [.ByteString cold, .ByteString hot]
  | .resignCommitteeCold _ cold =>
      .Constr 13 [.ByteString cold]
  | .unknown _ => .Constr 14 []

-- ====================
-- = Script Purpose   =
-- ====================

/-- What a Plutus script is validating -/
inductive ScriptPurpose where
  | Spending (txOutRef : UTxOId) (datum : Option PlutusData)
  | Minting (policyId : ByteArray)
  | Rewarding (rewardAddr : ByteArray)
  | Certifying (index : Nat) (cert : PlutusData)
  | Voting (voter : PlutusData)
  | Proposing (index : Nat) (procedure : PlutusData)

-- ====================
-- = Data Encoding    =
-- ====================

/-- Encode a ByteArray as PlutusData -/
def encodeByteArray (bs : ByteArray) : PlutusData :=
  .ByteString bs

/-- Encode a Nat as PlutusData -/
def encodeNat (n : Nat) : PlutusData :=
  .Integer (Int.ofNat n)

/-- Encode a TxInput as PlutusData TxOutRef: Constr 0 [Constr 0 [txId], index] -/
def encodeTxOutRef (inp : TxInput) : PlutusData :=
  .Constr 0 [.Constr 0 [.ByteString inp.txId], .Integer (Int.ofNat inp.outputIndex)]

/-- Encode UTxOId as PlutusData -/
def encodeUTxOId (id : UTxOId) : PlutusData :=
  .Constr 0 [.Constr 0 [.ByteString id.txHash], .Integer (Int.ofNat id.outputIndex)]

/-- Encode a Value as PlutusData: Map of (policyId → Map of (assetName → amount))
    ADA is encoded as policyId="" assetName="" -/
def encodeValue (v : Value) : PlutusData :=
  let adaEntry : PlutusData × PlutusData :=
    (.ByteString (ByteArray.mk #[]),
     .Map [(.ByteString (ByteArray.mk #[]), .Integer (Int.ofNat v.lovelace))])
  let assets := v.toNativeAssets
  let policyIds := assets.map (·.policyId) |>.eraseDups
  let assetEntries := policyIds.map fun pid =>
    let policyAssets := assets.filter (·.policyId == pid)
    let inner := policyAssets.map fun na =>
      (.ByteString na.assetName, .Integer (Int.ofNat na.amount))
    (PlutusData.ByteString pid, PlutusData.Map inner)
  .Map (adaEntry :: assetEntries)

/-- Encode a TxOutput as PlutusData:
    Constr 0 [address, value, datum, referenceScript] -/
def encodeTxOutput (output : TxOutput) : PlutusData :=
  let value := encodeValue (Value.fromTxOutput output)
  let datum := match output.datum with
    | some d => .Constr 0 [.ByteString d]  -- DatumHash
    | none => .Constr 1 []                  -- NoDatum
  .Constr 0 [.ByteString output.address, value, datum, .Constr 1 []]

/-- Encode an input with its resolved UTxO as PlutusData:
    Constr 0 [txOutRef, txOut] -/
def encodeTxInInfo (inp : TxInput) (output : TxOutput) : PlutusData :=
  .Constr 0 [encodeTxOutRef inp, encodeTxOutput output]

/-- Encode a validity interval as PlutusData:
    Constr 0 [lowerBound, upperBound]
    where bound = Constr 0/1 [Constr 0/1 [value]] -/
def encodeValidityRange (validFrom : Option Nat) (ttl : Option Nat) : PlutusData :=
  let lower := match validFrom with
    | some s => .Constr 0 [.Constr 1 [.Integer (Int.ofNat s)], .Constr 1 []]  -- Finite, inclusive
    | none => .Constr 0 [.Constr 0 [], .Constr 1 []]                          -- NegInf
  let upper := match ttl with
    | some s => .Constr 0 [.Constr 1 [.Integer (Int.ofNat s)], .Constr 1 []]  -- Finite, inclusive
    | none => .Constr 0 [.Constr 2 [], .Constr 1 []]                          -- PosInf
  .Constr 0 [lower, upper]

-- ====================
-- = TxInfo Builder   =
-- ====================

/-- Build the TxInfo PlutusData from a transaction and its resolved inputs.
    `resolvedInputs`: list of (TxInput, TxOutput) for regular inputs
    `resolvedRefInputs`: list of (TxInput, TxOutput) for reference inputs
    `txHash`: the transaction ID (blake2b_256 of body) -/
def buildTxInfo (body : TransactionBody) (witnesses : WitnessSet)
    (resolvedInputs : List (TxInput × TxOutput))
    (resolvedRefInputs : List (TxInput × TxOutput))
    (txHash : ByteArray) : PlutusData :=
  let inputs := .List (resolvedInputs.map fun (inp, out) => encodeTxInInfo inp out)
  let refInputs := .List (resolvedRefInputs.map fun (inp, out) => encodeTxInInfo inp out)
  let outputs := .List (body.outputs.map encodeTxOutput)
  let fee := encodeValue (Value.lovelaceOnly body.fee)
  let mint := encodeValue (Value.fromNativeAssets body.mint)
  let certs := .List (body.certificates.map encodeCertPlutusData)
  let withdrawals := .Map (body.withdrawals.map fun (addr, amt) =>
    (.ByteString addr, .Integer (Int.ofNat amt)))
  let validRange := encodeValidityRange body.validityIntervalStart body.ttl
  let signatories := .List (body.requiredSigners.map fun h => .ByteString h)
  let redeemers := PlutusData.Map (witnesses.redeemers.map fun r =>
    let tagIdx := match r.tag with
      | .Spend => 1 | .Mint => 0 | .Cert => 3 | .Reward => 2 | .Vote => 4 | .Propose => 5
    let key := PlutusData.Constr tagIdx [.Integer (Int.ofNat r.index)]
    let rdmrData := decodePlutusDataFromCbor r.data |>.getD (.ByteString r.data)
    let val := PlutusData.Constr 0 [rdmrData,
      PlutusData.Constr 0 [.Integer (Int.ofNat r.exUnits.mem),
                            .Integer (Int.ofNat r.exUnits.steps)]]
    (key, val))
  -- Collect datums: witness datums + inline datums from resolved inputs
  -- Keys are raw CBOR bytes (datum hash lookup requires IO; left for future work)
  let witnessDatums := witnesses.datums.map fun d =>
    (.ByteString d, decodePlutusDataFromCbor d |>.getD (.ByteString d))
  let inlineDatums := resolvedInputs.filterMap fun (_, out) =>
    out.inlineDatum.map fun d =>
      (.ByteString d, decodePlutusDataFromCbor d |>.getD (.ByteString d))
  let datums := .Map (witnessDatums ++ inlineDatums)
  let txId := .Constr 0 [.ByteString txHash]
  .Constr 0 [inputs, refInputs, outputs, fee, mint, certs, withdrawals,
             validRange, signatories, redeemers, datums, txId]

/-- Build a full ScriptContext for a given script purpose.
    PlutusV3: Constr 0 [txInfo, redeemer, scriptInfo] -/
def buildScriptContext (body : TransactionBody) (witnesses : WitnessSet)
    (resolvedInputs : List (TxInput × TxOutput))
    (resolvedRefInputs : List (TxInput × TxOutput))
    (txHash : ByteArray) (redeemer : PlutusData)
    (purpose : ScriptPurpose) : PlutusData :=
  let txInfo := buildTxInfo body witnesses resolvedInputs resolvedRefInputs txHash
  let scriptInfo := match purpose with
    | .Minting policyId => .Constr 0 [.ByteString policyId]
    | .Spending ref datum =>
      let datumArg := match datum with
        | some d => .Constr 0 [d]
        | none => .Constr 1 []
      .Constr 1 [encodeUTxOId ref, datumArg]
    | .Rewarding addr =>
      -- Reward address: 1-byte header + 28-byte credential hash
      -- Header bit 4 set → ScriptCredential; clear → PubKeyCredential
      let credHash := if addr.size >= 29 then addr.extract 1 29 else addr
      let isScript := addr.size > 0 && (addr[0]! &&& 0x10) != 0
      let cred := if isScript then .Constr 1 [.ByteString credHash]
                              else .Constr 0 [.ByteString credHash]
      .Constr 2 [cred]
    | .Certifying idx cert => .Constr 3 [.Integer (Int.ofNat idx), cert]
    | .Voting voter => .Constr 4 [voter]
    | .Proposing idx proc => .Constr 5 [.Integer (Int.ofNat idx), proc]
  .Constr 0 [txInfo, redeemer, scriptInfo]

/-- Build a V1/V2 ScriptContext: Constr 0 [txInfo, scriptPurpose]
    Key differences from V3:
    - Only 2 top-level fields (no embedded redeemer)
    - Spending purpose has no datum field
    - Rewarding purpose takes StakingCredential (StakingHash (Credential)) not bare Credential -/
def buildScriptContextV2 (body : TransactionBody) (witnesses : WitnessSet)
    (resolvedInputs : List (TxInput × TxOutput))
    (resolvedRefInputs : List (TxInput × TxOutput))
    (txHash : ByteArray) (purpose : ScriptPurpose) : PlutusData :=
  let txInfo := buildTxInfo body witnesses resolvedInputs resolvedRefInputs txHash
  let scriptPurpose := match purpose with
    | .Minting policyId => .Constr 0 [.ByteString policyId]
    | .Spending ref _ => .Constr 1 [encodeUTxOId ref]  -- V1/V2: no datum in SpendingPurpose
    | .Rewarding addr =>
      -- V2 Rewarding takes StakingCredential = StakingHash (Credential)
      let credHash := if addr.size >= 29 then addr.extract 1 29 else addr
      let isScript := addr.size > 0 && (addr[0]! &&& 0x10) != 0
      let cred := if isScript then .Constr 1 [.ByteString credHash]
                              else .Constr 0 [.ByteString credHash]
      .Constr 2 [.Constr 0 [cred]]  -- Rewarding (StakingHash credential)
    | .Certifying _idx cert => .Constr 3 [cert]  -- V2: Certifying DCert (no index)
    | .Voting voter => .Constr 4 [voter]
    | .Proposing idx proc => .Constr 5 [.Integer (Int.ofNat idx), proc]
  .Constr 0 [txInfo, scriptPurpose]

/-- Resolve inputs from UTxO set for script context construction -/
def resolveInputs (utxo : UTxOSet) (inputs : List TxInput) : List (TxInput × TxOutput) :=
  inputs.filterMap fun inp =>
    let id : UTxOId := { txHash := inp.txId, outputIndex := inp.outputIndex }
    match utxo.lookup id with
    | some output => some (inp, output)
    | none => none

end Dion.Plutus.ScriptContext
