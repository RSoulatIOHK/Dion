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
  amount : Nat            -- Token amount (absolute value for outputs; for mint, use signedAmount)
  signedAmount : Int := amount  -- Signed amount (negative = burn, used for minting field)

instance : Repr NativeAsset where
  reprPrec a _ := s!"NativeAsset(policy={a.policyId.size}B, name={a.assetName.size}B, amount={a.amount})"

/-- Transaction output (new UTxO) -/
structure TxOutput where
  address : ByteArray         -- Cardano address (variable length)
  amount : Nat                -- Lovelace amount
  datum : Option ByteArray    -- Optional datum hash (32 bytes) from output key 2
  inlineDatum : Option ByteArray  -- Inline datum CBOR from output key 3 (Babbage+)
  scriptRef : Option ByteArray    -- Reference script from output key 4 (Babbage+)
  nativeAssets : List NativeAsset  -- Native tokens (non-ADA assets)
  rawValueBytes : ByteArray := ByteArray.empty  -- Raw CBOR of the value field (key 1)
  rawOutputBytes : ByteArray := ByteArray.empty -- Raw CBOR of the entire output (for minUTxO)

instance : Repr TxOutput where
  reprPrec o _ := s!"TxOutput(addr={o.address.size}B, amount={o.amount}, assets={o.nativeAssets.length})"

/-- Redeemer tag indicating what it's validating -/
inductive RedeemerTag where
  | Spend    -- Validating a script input
  | Mint     -- Validating a minting policy
  | Cert     -- Validating a certificate
  | Reward   -- Validating a reward withdrawal
  | Vote     -- Conway: validating a vote (tag=4)
  | Propose  -- Conway: validating a proposal (tag=5)
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

/-- VKey witness: Ed25519 public key + signature over the tx hash -/
structure VKeyWitness where
  vkey : ByteArray       -- Ed25519 public key (32 bytes)
  signature : ByteArray  -- Ed25519 signature (64 bytes)

instance : Repr VKeyWitness where
  reprPrec w _ := s!"VKeyWitness(vkey={w.vkey.size}B, sig={w.signature.size}B)"

/-- Witness set containing signatures, scripts, and redeemers -/
structure WitnessSet where
  vkeyWitnesses : List VKeyWitness := []  -- Key 0
  nativeScripts : List ByteArray := []     -- Key 1 (raw CBOR per script)
  bootstrapWitnesses : List ByteArray := [] -- Key 2 (raw CBOR per witness)
  plutusV1Scripts : List ByteArray := []   -- Key 3 (raw script bytes)
  datums : List ByteArray := []            -- Key 4 (plutus_data, raw CBOR per datum)
  redeemers : List Redeemer                -- Key 5
  plutusV2Scripts : List ByteArray := []   -- Key 6 (raw script bytes)
  plutusV3Scripts : List ByteArray := []   -- Key 7 (raw script bytes)
  rawRedeemersCbor : ByteArray := ByteArray.empty  -- Raw CBOR of redeemers value (for scriptDataHash)
  rawDatumsCbor : ByteArray := ByteArray.empty     -- Raw CBOR of datums value (for scriptDataHash)
  rawSize : Nat := 0                               -- Raw CBOR size of this witness set

instance : Repr WitnessSet where
  reprPrec w _ := s!"WitnessSet(vkeys={w.vkeyWitnesses.length}, native={w.nativeScripts.length}, datums={w.datums.length}, redeemers={w.redeemers.length}, plutus={w.plutusV1Scripts.length + w.plutusV2Scripts.length + w.plutusV3Scripts.length})"

/-- Raw certificate parsed from block CBOR.
    Kept lightweight to avoid circular dependencies with Ledger.Certificate.
    Convert to `Cleanode.Ledger.Certificate.Certificate` at the call site. -/
inductive RawCertificate where
  | stakeKeyRegistration (credIsScript : Bool) (credHash : ByteArray)
  | stakeKeyDeregistration (credIsScript : Bool) (credHash : ByteArray)
  | stakeDelegation (credIsScript : Bool) (credHash : ByteArray) (poolId : ByteArray)
  | poolRegistration (poolId : ByteArray) (vrfKeyHash : ByteArray)
      (pledge cost margin : Nat) (rewardAccount : ByteArray)
      (owners : List ByteArray)
  | poolRetirement (poolId : ByteArray) (epoch : Nat)
  | conwayRegistration (credIsScript : Bool) (credHash : ByteArray) (deposit : Nat)  -- type 7
  | conwayDeregistration (credIsScript : Bool) (credHash : ByteArray) (refund : Nat)  -- type 8
  | voteDelegation (credIsScript : Bool) (credHash : ByteArray) (drepCred : ByteArray)  -- type 9
  | stakeVoteDelegation (credIsScript : Bool) (credHash : ByteArray) (poolId : ByteArray) (drepCred : ByteArray)  -- type 10
  | stakeRegDelegation (credIsScript : Bool) (credHash : ByteArray) (poolId : ByteArray) (deposit : Nat)  -- type 11
  | voteRegDelegation (credIsScript : Bool) (credHash : ByteArray) (drepCred : ByteArray) (deposit : Nat)  -- type 12
  | stakeVoteRegDelegation (credIsScript : Bool) (credHash : ByteArray) (poolId : ByteArray) (drepCred : ByteArray) (deposit : Nat)  -- type 13
  | authCommitteeHot (coldIsScript : Bool) (coldCredHash : ByteArray) (hotCredHash : ByteArray)  -- type 14
  | resignCommitteeCold (coldIsScript : Bool) (coldCredHash : ByteArray)  -- type 15
  | unknown (certType : Nat)

instance : Repr RawCertificate where
  reprPrec
    | .stakeKeyRegistration _ _, _ => "StakeKeyReg"
    | .stakeKeyDeregistration _ _, _ => "StakeKeyDereg"
    | .stakeDelegation _ _ _, _ => "StakeDelegation"
    | .poolRegistration _ _ _ _ _ _ _, _ => "PoolRegistration"
    | .poolRetirement _ e, _ => s!"PoolRetirement(epoch={e})"
    | .conwayRegistration _ _ d, _ => s!"ConwayRegistration(deposit={d})"
    | .conwayDeregistration _ _ r, _ => s!"ConwayDeregistration(refund={r})"
    | .voteDelegation _ _ _, _ => "VoteDelegation"
    | .stakeVoteDelegation _ _ _ _, _ => "StakeVoteDelegation"
    | .stakeRegDelegation _ _ _ d, _ => s!"StakeRegDelegation(deposit={d})"
    | .voteRegDelegation _ _ _ d, _ => s!"VoteRegDelegation(deposit={d})"
    | .stakeVoteRegDelegation _ _ _ _ d, _ => s!"StakeVoteRegDelegation(deposit={d})"
    | .authCommitteeHot _ _ _, _ => "AuthCommitteeHot"
    | .resignCommitteeCold _ _, _ => "ResignCommitteeCold"
    | .unknown t, _ => s!"UnknownCert({t})"

structure TransactionBody where
  inputs : List TxInput                            -- Key 0
  outputs : List TxOutput                          -- Key 1
  fee : Nat                                        -- Key 2
  ttl : Option Nat := none                         -- Key 3: time-to-live (upper slot bound)
  certificates : List RawCertificate               -- Key 4
  withdrawals : List (ByteArray × Nat) := []       -- Key 5: reward_account → lovelace
  auxiliaryDataHash : Option ByteArray := none      -- Key 7
  validityIntervalStart : Option Nat := none        -- Key 8: lower slot bound
  mint : List NativeAsset := []                     -- Key 9: minting/burning
  scriptDataHash : Option ByteArray := none         -- Key 11
  collateralInputs : List TxInput := []             -- Key 13
  requiredSigners : List ByteArray := []            -- Key 14
  collateralReturn : Option TxOutput := none        -- Key 16
  totalCollateral : Option Nat := none              -- Key 17
  referenceInputs : List TxInput := []              -- Key 18
  votingProcedures : Option ByteArray := none       -- Key 19 (raw CBOR)
  proposalProcedures : Option ByteArray := none     -- Key 20 (raw CBOR)
  currentTreasuryValue : Option Nat := none         -- Key 21
  donation : Option Nat := none                     -- Key 22
  rawBytes : ByteArray  -- Raw CBOR bytes for computing TxId

instance : Repr TransactionBody where
  reprPrec tx _ := s!"TxBody(inputs={tx.inputs.length}, outputs={tx.outputs.length}, fee={tx.fee}, certs={tx.certificates.length})"

/-- Complete transaction with witnesses -/
structure Transaction where
  body : TransactionBody
  witnesses : WitnessSet
  serializedSize : Nat := 0  -- Full serialized tx size (body + witnesses + valid + aux)

instance : Repr Transaction where
  reprPrec tx _ := s!"Transaction({repr tx.body}, redeemers={tx.witnesses.redeemers.length}, size={tx.serializedSize})"

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
    let mut datumHash : Option ByteArray := none
    let mut inlineDatum : Option ByteArray := none
    let mut scriptRef : Option ByteArray := none
    let mut rawValueBytes : ByteArray := ByteArray.empty

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
              let valueStart := keyResult.cursor
              match CborCursor.decodeUInt keyResult.cursor with
              | some amtResult => do
                  amount := amtResult.value
                  rawValueBytes := extractBetween valueStart amtResult.cursor
                  cur := amtResult.cursor
              | none =>
                  match parseMultiAssetC keyResult.cursor with
                  | some multiResult => do
                      amount := multiResult.value.1
                      nativeAssets := multiResult.value.2
                      rawValueBytes := extractBetween valueStart multiResult.cursor
                      cur := multiResult.cursor
                  | none =>
                      match skipValue keyResult.cursor with
                      | some after => cur := after
                      | none => break
          | 2 => do  -- Datum option: [0, datumHash] or [1, inlineDatum]
              -- CDDL: datum_option = [0, hash32] / [1, data]
              match CborCursor.decodeArrayHeader keyResult.cursor with
              | some arrR => do
                  match CborCursor.decodeUInt arrR.cursor with
                  | some tagR => do
                      if tagR.value == 0 then
                        -- datum hash
                        match CborCursor.decodeBytes tagR.cursor with
                        | some hashR => do datumHash := some hashR.value; cur := hashR.cursor
                        | none => match skipValue tagR.cursor with
                          | some after => cur := after
                          | none => break
                      else
                        -- inline datum (tag 1): capture raw CBOR
                        let start := tagR.cursor
                        match skipValue start with
                        | some after => do
                            inlineDatum := some (extractBetween start after)
                            cur := after
                        | none => break
                  | none => match skipValue keyResult.cursor with
                      | some after => cur := after
                      | none => break
              | none => match skipValue keyResult.cursor with
                  | some after => cur := after
                  | none => break
          | 3 => do  -- Script ref: tag 24 (CBOR-encoded script)
              let start := keyResult.cursor
              match skipValue start with
              | some after => do
                  scriptRef := some (extractBetween start after)
                  cur := after
              | none => break
          | _ => do
              match skipValue keyResult.cursor with
              | some after => cur := after
              | none => break

    let rawOutputBytes := extractBetween c cur
    some { value := { address, amount, datum := datumHash, inlineDatum, scriptRef, nativeAssets, rawValueBytes, rawOutputBytes }, cursor := cur }
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
      -- Alonzo array format: [address, value, datum_hash?]
      -- Parse the 3rd element as a 32-byte datum hash if present
      let mut datumHash : Option ByteArray := none
      if r1.value >= 3 then
        match CborCursor.decodeBytes cur with
        | some hashR =>
          if hashR.value.size == 32 then
            datumHash := some hashR.value
          cur := hashR.cursor
        | none =>
          match skipValue cur with
          | some after => cur := after
          | none => pure ()
      -- Skip any remaining elements beyond the 3rd
      for _ in [3:r1.value] do
        match skipValue cur with
        | some after => cur := after
        | none => break

      let rawOutputBytes := extractBetween c cur
      some { value := { address := r2.value, amount, datum := datumHash, inlineDatum := none, scriptRef := none, nativeAssets, rawOutputBytes }, cursor := cur }

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
    | 4 => RedeemerTag.Vote
    | _ => RedeemerTag.Propose

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

/-- Parse a single redeemer from Conway map entry: key=[tag,index], value=[data, exunits] -/
partial def parseRedeemerMapEntryC (c : Cursor) : Option (CResult Redeemer) := do
  -- Key: [tag, index]
  let keyArr ← CborCursor.decodeArrayHeader c
  if keyArr.value != 2 then none
  let tagResult ← CborCursor.decodeUInt keyArr.cursor
  let tag := match tagResult.value with
    | 0 => RedeemerTag.Spend
    | 1 => RedeemerTag.Mint
    | 2 => RedeemerTag.Cert
    | 3 => RedeemerTag.Reward
    | 4 => RedeemerTag.Vote
    | _ => RedeemerTag.Propose
  let indexResult ← CborCursor.decodeUInt tagResult.cursor
  -- Value: [data, ex_units]
  let valArr ← CborCursor.decodeArrayHeader indexResult.cursor
  if valArr.value != 2 then none
  let dataStart := valArr.cursor
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

/-- Parse redeemers from witness set — handles both array (Alonzo/Babbage) and map (Conway) formats -/
partial def parseRedeemersC (c : Cursor) : Option (CResult (List Redeemer)) := do
  if c.remaining == 0 then none
  let majorType := (c.peek >>> 5).toNat
  if majorType == 4 then
    -- Array format: [* [tag, index, data, exunits]]
    let r1 ← CborCursor.decodeArrayHeader c
    let count := r1.value
    let rec parseLoop (cur : Cursor) (acc : List Redeemer) (n : Nat) : Option (CResult (List Redeemer)) :=
      if n == 0 then some { value := acc.reverse, cursor := cur }
      else match parseRedeemerC cur with
        | some r => parseLoop r.cursor (r.value :: acc) (n - 1)
        | none => none
    parseLoop r1.cursor [] count
  else if majorType == 5 then
    -- Map format (Conway): { [tag, index] => [data, exunits] }
    let r1 ← CborCursor.decodeMapHeader c
    let count := r1.value
    let rec parseMapLoop (cur : Cursor) (acc : List Redeemer) (n : Nat) : Option (CResult (List Redeemer)) :=
      if n == 0 then some { value := acc.reverse, cursor := cur }
      else if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then
        some { value := acc.reverse, cursor := cur.advance 1 }
      else match parseRedeemerMapEntryC cur with
        | some r => parseMapLoop r.cursor (r.value :: acc) (n - 1)
        | none => none
    parseMapLoop r1.cursor [] count
  else none

/-- Parse a single VKey witness: [vkey(32B), sig(64B)] -/
partial def parseVKeyWitnessC (c : Cursor) : Option (CResult VKeyWitness) := do
  -- Skip tag if present (shouldn't be, but just in case)
  let c := match CborCursor.skipTag c with
    | some tagResult => tagResult.cursor
    | none => c
  let r1 ← CborCursor.decodeArrayHeader c
  if r1.value != 2 && r1.value != 9999 then none
  let vkeyR ← CborCursor.decodeBytes r1.cursor
  let sigR ← CborCursor.decodeBytes vkeyR.cursor
  -- For indefinite-length arrays, skip break code
  let finalCur := if r1.value == 9999 && sigR.cursor.remaining ≥ 1 && sigR.cursor.peek.toNat == 0xFF then
    sigR.cursor.advance 1
  else sigR.cursor
  some { value := { vkey := vkeyR.value, signature := sigR.value }, cursor := finalCur }

/-- Parse array of VKey witnesses -/
partial def parseVKeyWitnessesC (c : Cursor) : Option (CResult (List VKeyWitness)) := do
  -- Skip CBOR tag 258 (set semantics) if present
  let c := match CborCursor.skipTag c with
    | some tagResult => tagResult.cursor
    | none => c
  let r1 ← CborCursor.decodeArrayHeader c
  let count := r1.value
  let rec go (cur : Cursor) (acc : List VKeyWitness) (n : Nat) : Option (CResult (List VKeyWitness)) :=
    if n == 0 then some { value := acc.reverse, cursor := cur }
    -- For indefinite-length arrays, stop at break code (0xFF)
    else if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then
      some { value := acc.reverse, cursor := cur.advance 1 }
    else match parseVKeyWitnessC cur with
      | some r => go r.cursor (r.value :: acc) (n - 1)
      | none => none
  go r1.cursor [] count

/-- Parse an array of raw CBOR items (each item stored as raw bytes) -/
partial def parseRawArrayC (c : Cursor) : Option (CResult (List ByteArray)) := do
  let r1 ← CborCursor.decodeArrayHeader c
  let mut cur := r1.cursor
  let mut items : List ByteArray := []
  for _ in [0:r1.value] do
    -- Stop at break code for indefinite-length arrays
    if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then
      cur := cur.advance 1
      break
    let start := cur
    match skipValue cur with
    | some after => do
        items := extractBetween start after :: items
        cur := after
    | none => break
  some { value := items.reverse, cursor := cur }

/-- Parse witness set from CBOR map — handles all witness keys 0-7 -/
partial def parseWitnessSetC (c : Cursor) : Option (CResult WitnessSet) := do
  let r1 ← CborCursor.decodeMapHeader c
  let mapSize := r1.value

  let mut cur := r1.cursor
  let mut vkeys : List VKeyWitness := []
  let mut nativeScripts : List ByteArray := []
  let mut datums : List ByteArray := []
  let mut bootstrapWits : List ByteArray := []
  let mut plutusV1 : List ByteArray := []
  let mut redeemers : List Redeemer := []
  let mut plutusV2 : List ByteArray := []
  let mut plutusV3 : List ByteArray := []
  let mut rawRedeemersCbor : ByteArray := ByteArray.empty
  let mut rawDatumsCbor : ByteArray := ByteArray.empty

  for _ in [0:mapSize] do
    -- Stop at break code for indefinite-length maps (0xBF ... 0xFF)
    if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then
      cur := cur.advance 1
      break
    match CborCursor.decodeUInt cur with
    | none => break
    | some keyResult => do
        let key := keyResult.value
        cur := keyResult.cursor
        match key with
        | 0 => do  -- vkey_witnesses
            match parseVKeyWitnessesC cur with
            | some vkR => do
                vkeys := vkR.value
                cur := vkR.cursor
            | none =>
                match skipValue cur with
                | some after => cur := after
                | none => break
        | 1 => do  -- native_scripts
            match parseRawArrayC cur with
            | some r => do nativeScripts := r.value; cur := r.cursor
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
        | 2 => do  -- bootstrap_witnesses
            match parseRawArrayC cur with
            | some r => do bootstrapWits := r.value; cur := r.cursor
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
        | 3 => do  -- plutus_v1_scripts
            match parseRawArrayC cur with
            | some r => do plutusV1 := r.value; cur := r.cursor
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
        | 4 => do  -- plutus_data (datums)
            let datumsStart := cur
            match parseRawArrayC cur with
            | some r => do
                datums := r.value
                rawDatumsCbor := extractBetween datumsStart r.cursor
                cur := r.cursor
            | none => match skipValue cur with
                | some after =>
                    rawDatumsCbor := extractBetween datumsStart after
                    cur := after
                | none => break
        | 5 => do  -- redeemers
            let redeemersStart := cur
            match parseRedeemersC cur with
            | some redR => do
                redeemers := redR.value
                rawRedeemersCbor := extractBetween redeemersStart redR.cursor
                cur := redR.cursor
            | none =>
                match skipValue cur with
                | some after =>
                    rawRedeemersCbor := extractBetween redeemersStart after
                    cur := after
                | none => break
        | 6 => do  -- plutus_v2_scripts
            match parseRawArrayC cur with
            | some r => do plutusV2 := r.value; cur := r.cursor
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
        | 7 => do  -- plutus_v3_scripts
            match parseRawArrayC cur with
            | some r => do plutusV3 := r.value; cur := r.cursor
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
        | _ => do
            match skipValue cur with
            | some after => cur := after
            | none => break

  some {
    value := {
      vkeyWitnesses := vkeys, nativeScripts, datums,
      bootstrapWitnesses := bootstrapWits,
      plutusV1Scripts := plutusV1, redeemers,
      plutusV2Scripts := plutusV2, plutusV3Scripts := plutusV3,
      rawRedeemersCbor, rawDatumsCbor
    },
    cursor := cur
  }

/-- Parse a single certificate from CBOR array [certType, ...params].
    Shelley-era types 0-4 are parsed; Conway types 5+ are returned as `unknown`. -/
partial def parseCertificateC (c : Cursor) : Option (CResult RawCertificate) := do
  let r1 ← CborCursor.decodeArrayHeader c
  let arrLen := r1.value
  if arrLen < 1 then none
  let r2 ← CborCursor.decodeUInt r1.cursor
  let certType := r2.value
  let mut cur := r2.cursor
  match certType with
  | 0 => do  -- stake_registration: [0, stake_credential]
      -- stake_credential = [0, key_hash] or [1, script_hash]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      some { value := .stakeKeyRegistration (credType.value == 1) hashR.value, cursor := hashR.cursor }
  | 1 => do  -- stake_deregistration: [1, stake_credential]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      some { value := .stakeKeyDeregistration (credType.value == 1) hashR.value, cursor := hashR.cursor }
  | 2 => do  -- stake_delegation: [2, stake_credential, pool_keyhash]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let poolR ← CborCursor.decodeBytes hashR.cursor
      some { value := .stakeDelegation (credType.value == 1) hashR.value poolR.value, cursor := poolR.cursor }
  | 3 => do  -- pool_registration: [3, pool_params...]
      -- pool_params: operator(bytes), vrf_keyhash(bytes), pledge(uint), cost(uint),
      --   margin(tag30 rational), reward_account(bytes), pool_owners(set), relays, metadata
      let operR ← CborCursor.decodeBytes cur          -- operator (pool ID)
      let vrfR ← CborCursor.decodeBytes operR.cursor  -- VRF key hash
      let pledgeR ← CborCursor.decodeUInt vrfR.cursor  -- pledge
      let costR ← CborCursor.decodeUInt pledgeR.cursor  -- cost
      -- margin is a tag 30 rational [num, den] — skip it
      let afterMargin ← skipValue costR.cursor
      let rewardR ← CborCursor.decodeBytes afterMargin  -- reward account
      -- owners is a set (tag 258 array or plain array)
      let afterOwnersTag := match CborCursor.skipTag rewardR.cursor with
        | some tagR => tagR.cursor
        | none => rewardR.cursor
      let ownersArrR ← CborCursor.decodeArrayHeader afterOwnersTag
      let mut ownersCur := ownersArrR.cursor
      let mut owners : List ByteArray := []
      for _ in [0:ownersArrR.value] do
        match CborCursor.decodeBytes ownersCur with
        | some owR => owners := owR.value :: owners; ownersCur := owR.cursor
        | none => break
      -- Skip relays and metadata
      let afterRelays ← skipValue ownersCur
      let afterMeta ← skipValue afterRelays
      some { value := .poolRegistration operR.value vrfR.value pledgeR.value costR.value 0 rewardR.value owners, cursor := afterMeta }
  | 4 => do  -- pool_retirement: [4, pool_keyhash, epoch]
      let poolR ← CborCursor.decodeBytes cur
      let epochR ← CborCursor.decodeUInt poolR.cursor
      some { value := .poolRetirement poolR.value epochR.value, cursor := epochR.cursor }
  | 7 => do  -- conwayRegistration: [7, stake_credential, coin]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let depositR ← CborCursor.decodeUInt hashR.cursor
      some { value := .conwayRegistration (credType.value == 1) hashR.value depositR.value, cursor := depositR.cursor }
  | 8 => do  -- conwayDeregistration: [8, stake_credential, coin]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let refundR ← CborCursor.decodeUInt hashR.cursor
      some { value := .conwayDeregistration (credType.value == 1) hashR.value refundR.value, cursor := refundR.cursor }
  | 9 => do  -- voteDelegation: [9, stake_credential, drep_credential]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let drepR ← CborCursor.decodeArrayHeader hashR.cursor
      let drepType ← CborCursor.decodeUInt drepR.cursor
      -- DRep can be [0, keyhash], [1, scripthash], [2], [3]
      if drepR.value >= 2 then
        let drepHashR ← CborCursor.decodeBytes drepType.cursor
        some { value := .voteDelegation (credType.value == 1) hashR.value drepHashR.value, cursor := drepHashR.cursor }
      else
        some { value := .voteDelegation (credType.value == 1) hashR.value ByteArray.empty, cursor := drepType.cursor }
  | 10 => do  -- stakeVoteDelegation: [10, stake_credential, pool_keyhash, drep_credential]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let poolR ← CborCursor.decodeBytes hashR.cursor
      let drepR ← CborCursor.decodeArrayHeader poolR.cursor
      let drepType ← CborCursor.decodeUInt drepR.cursor
      if drepR.value >= 2 then
        let drepHashR ← CborCursor.decodeBytes drepType.cursor
        some { value := .stakeVoteDelegation (credType.value == 1) hashR.value poolR.value drepHashR.value, cursor := drepHashR.cursor }
      else
        some { value := .stakeVoteDelegation (credType.value == 1) hashR.value poolR.value ByteArray.empty, cursor := drepType.cursor }
  | 11 => do  -- stakeRegDelegation: [11, stake_credential, pool_keyhash, coin]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let poolR ← CborCursor.decodeBytes hashR.cursor
      let depositR ← CborCursor.decodeUInt poolR.cursor
      some { value := .stakeRegDelegation (credType.value == 1) hashR.value poolR.value depositR.value, cursor := depositR.cursor }
  | 12 => do  -- voteRegDelegation: [12, stake_credential, drep_credential, coin]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let drepR ← CborCursor.decodeArrayHeader hashR.cursor
      let drepType ← CborCursor.decodeUInt drepR.cursor
      let afterDrep ← if drepR.value >= 2 then
        let drepHashR ← CborCursor.decodeBytes drepType.cursor
        some (drepHashR.value, drepHashR.cursor)
      else
        some (ByteArray.empty, drepType.cursor)
      let depositR ← CborCursor.decodeUInt afterDrep.2
      some { value := .voteRegDelegation (credType.value == 1) hashR.value afterDrep.1 depositR.value, cursor := depositR.cursor }
  | 13 => do  -- stakeVoteRegDelegation: [13, stake_credential, pool_keyhash, drep_credential, coin]
      let credR ← CborCursor.decodeArrayHeader cur
      let credType ← CborCursor.decodeUInt credR.cursor
      let hashR ← CborCursor.decodeBytes credType.cursor
      let poolR ← CborCursor.decodeBytes hashR.cursor
      let drepR ← CborCursor.decodeArrayHeader poolR.cursor
      let drepType ← CborCursor.decodeUInt drepR.cursor
      let afterDrep ← if drepR.value >= 2 then
        let drepHashR ← CborCursor.decodeBytes drepType.cursor
        some (drepHashR.value, drepHashR.cursor)
      else
        some (ByteArray.empty, drepType.cursor)
      let depositR ← CborCursor.decodeUInt afterDrep.2
      some { value := .stakeVoteRegDelegation (credType.value == 1) hashR.value poolR.value afterDrep.1 depositR.value, cursor := depositR.cursor }
  | 14 => do  -- authCommitteeHot: [14, cold_credential, hot_credential]
      let coldR ← CborCursor.decodeArrayHeader cur
      let coldType ← CborCursor.decodeUInt coldR.cursor
      let coldHashR ← CborCursor.decodeBytes coldType.cursor
      let hotR ← CborCursor.decodeArrayHeader coldHashR.cursor
      let _hotType ← CborCursor.decodeUInt hotR.cursor
      let hotHashR ← CborCursor.decodeBytes _hotType.cursor
      some { value := .authCommitteeHot (coldType.value == 1) coldHashR.value hotHashR.value, cursor := hotHashR.cursor }
  | 15 => do  -- resignCommitteeCold: [15, cold_credential]
      let coldR ← CborCursor.decodeArrayHeader cur
      let coldType ← CborCursor.decodeUInt coldR.cursor
      let coldHashR ← CborCursor.decodeBytes coldType.cursor
      some { value := .resignCommitteeCold (coldType.value == 1) coldHashR.value, cursor := coldHashR.cursor }
  | _ => do  -- unknown cert types — skip remaining fields
      for _ in [1:arrLen] do
        match skipValue cur with
        | some after => cur := after
        | none => break
      some { value := .unknown certType, cursor := cur }

/-- Parse transaction body from CBOR map (Babbage+) -/
partial def parseTransactionBodyMapC (c : Cursor) : Option (CResult TransactionBody) := do
  let bodyStart := c
  let r1 ← CborCursor.decodeMapHeader c
  let mapSize := r1.value

  let mut cur := r1.cursor
  let mut inputs : List TxInput := []
  let mut outputs : List TxOutput := []
  let mut fee : Nat := 0
  let mut ttl : Option Nat := none
  let mut certs : List RawCertificate := []
  let mut withdrawals : List (ByteArray × Nat) := []
  let mut auxDataHash : Option ByteArray := none
  let mut validityStart : Option Nat := none
  let mut mint : List NativeAsset := []
  let mut scriptDataHash : Option ByteArray := none
  let mut collateralInputs : List TxInput := []
  let mut requiredSigners : List ByteArray := []
  let mut collateralReturn : Option TxOutput := none
  let mut totalCollateral : Option Nat := none
  let mut referenceInputs : List TxInput := []
  let mut votingProcs : Option ByteArray := none
  let mut proposalProcs : Option ByteArray := none
  let mut treasuryVal : Option Nat := none
  let mut donation : Option Nat := none

  for _ in [0:mapSize] do
    -- Stop at break code for indefinite-length maps
    if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then
      cur := cur.advance 1
      break
    match CborCursor.decodeUInt cur with
    | none => break
    | some keyResult => do
        let key := keyResult.value
        let valueStart := keyResult.cursor
        cur := keyResult.cursor

        match key with
        | 0 => do  -- Inputs array (may be wrapped in tag 258)
            let afterTag := match CborCursor.skipTag cur with
              | some tagResult => tagResult.cursor
              | none => cur
            match CborCursor.decodeArrayHeader afterTag with
            | none => pure ()
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then cur := cur.advance 1; break
                  match parseTxInputC cur with
                  | none => break
                  | some inputResult => do
                      inputs := inputResult.value :: inputs
                      cur := inputResult.cursor
        | 1 => do  -- Outputs array
            match CborCursor.decodeArrayHeader cur with
            | none => pure ()
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then cur := cur.advance 1; break
                  match parseTxOutputC cur with
                  | none =>
                      -- Output failed to parse: skip the CBOR value but add a placeholder
                      -- to keep output indices aligned (UTxO IDs depend on index position)
                      match skipValue cur with
                      | some after =>
                          outputs := { address := ByteArray.empty, amount := 0, datum := none, inlineDatum := none, scriptRef := none, nativeAssets := [] } :: outputs
                          cur := after
                      | none => break
                  | some outputResult => do
                      outputs := outputResult.value :: outputs
                      cur := outputResult.cursor
        | 2 => do  -- Fee
            match CborCursor.decodeUInt cur with
            | none => pure ()
            | some feeResult => do
                fee := feeResult.value
                cur := feeResult.cursor
        | 3 => do  -- TTL
            match CborCursor.decodeUInt cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some ttlResult => do
                ttl := some ttlResult.value
                cur := ttlResult.cursor
        | 4 => do  -- Certificates array (may be wrapped in tag 258)
            let afterTag := match CborCursor.skipTag cur with
              | some tagResult => tagResult.cursor
              | none => cur
            match CborCursor.decodeArrayHeader afterTag with
            | none => break
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  match parseCertificateC cur with
                  | none =>
                      match skipValue cur with
                      | some after => cur := after
                      | none => break
                  | some certResult => do
                      certs := certResult.value :: certs
                      cur := certResult.cursor
        | 5 => do  -- Withdrawals map: {reward_account => lovelace}
            match CborCursor.decodeMapHeader cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some mapR => do
                cur := mapR.cursor
                for _ in [0:mapR.value] do
                  match CborCursor.decodeBytes cur with
                  | none => break
                  | some addrR =>
                      match CborCursor.decodeUInt addrR.cursor with
                      | none => break
                      | some amtR => do
                          withdrawals := (addrR.value, amtR.value) :: withdrawals
                          cur := amtR.cursor
        | 7 => do  -- Auxiliary data hash
            match CborCursor.decodeBytes cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some hashR => do
                auxDataHash := some hashR.value
                cur := hashR.cursor
        | 8 => do  -- Validity interval start
            match CborCursor.decodeUInt cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some startResult => do
                validityStart := some startResult.value
                cur := startResult.cursor
        | 9 => do  -- Mint: {policy_id => {asset_name => int}}
            match CborCursor.decodeMapHeader cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some policyMapR => do
                cur := policyMapR.cursor
                for _ in [0:policyMapR.value] do
                  match CborCursor.decodeBytes cur with
                  | none => break
                  | some policyR =>
                      match CborCursor.decodeMapHeader policyR.cursor with
                      | none => break
                      | some assetMapR => do
                          cur := assetMapR.cursor
                          for _ in [0:assetMapR.value] do
                            match CborCursor.decodeBytes cur with
                            | none => break
                            | some nameR =>
                                -- Amounts can be negative (burns) — major type 1 in CBOR.
                                let (amount, signedAmt, afterAmt) :=
                                  match CborCursor.decodeUInt nameR.cursor with
                                  | some amtR => (amtR.value, (amtR.value : Int), amtR.cursor)
                                  | none =>
                                    -- Try negative integer (major type 1): value = -1 - n
                                    match CborCursor.decodeHead nameR.cursor with
                                    | some h =>
                                      if h.value.1 == 1 then
                                        let magnitude := h.value.2 + 1
                                        (magnitude, -(magnitude : Int), h.cursor)
                                      else match skipValue nameR.cursor with
                                        | some after => (0, 0, after)
                                        | none => (0, 0, nameR.cursor)
                                    | _ => match skipValue nameR.cursor with
                                        | some after => (0, 0, after)
                                        | none => (0, 0, nameR.cursor)
                                mint := { policyId := policyR.value, assetName := nameR.value, amount, signedAmount := signedAmt } :: mint
                                cur := afterAmt
        | 11 => do  -- Script data hash (CDDL key 11)
            match CborCursor.decodeBytes cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some hashR => do
                scriptDataHash := some hashR.value
                cur := hashR.cursor
        | 13 => do  -- Collateral inputs (CDDL key 13, may be wrapped in tag 258)
            let afterTag := match CborCursor.skipTag cur with
              | some tagResult => tagResult.cursor
              | none => cur
            match CborCursor.decodeArrayHeader afterTag with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  match parseTxInputC cur with
                  | none => break
                  | some inputResult => do
                      collateralInputs := inputResult.value :: collateralInputs
                      cur := inputResult.cursor
        | 14 => do  -- Required signers (CDDL key 14, may be wrapped in tag 258)
            let afterTag := match CborCursor.skipTag cur with
              | some tagResult => tagResult.cursor
              | none => cur
            match CborCursor.decodeArrayHeader afterTag with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  match CborCursor.decodeBytes cur with
                  | none => break
                  | some hashR => do
                      requiredSigners := hashR.value :: requiredSigners
                      cur := hashR.cursor
        | 16 => do  -- Collateral return output (CDDL key 16)
            match parseTxOutputC cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some outResult => do
                collateralReturn := some outResult.value
                cur := outResult.cursor
        | 17 => do  -- Total collateral (CDDL key 17)
            match CborCursor.decodeUInt cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some colR => do
                totalCollateral := some colR.value
                cur := colR.cursor
        | 18 => do  -- Reference inputs (CDDL key 18, may be wrapped in tag 258)
            let afterTag := match CborCursor.skipTag cur with
              | some tagResult => tagResult.cursor
              | none => cur
            match CborCursor.decodeArrayHeader afterTag with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some arrResult => do
                cur := arrResult.cursor
                for _ in [0:arrResult.value] do
                  match parseTxInputC cur with
                  | none => break
                  | some inputResult => do
                      referenceInputs := inputResult.value :: referenceInputs
                      cur := inputResult.cursor
        | 19 => do  -- Voting procedures (CDDL key 19, store raw CBOR)
            let start := cur
            match skipValue cur with
            | none => break
            | some after => do
                votingProcs := some (extractBetween start after)
                cur := after
        | 20 => do  -- Proposal procedures (CDDL key 20, store raw CBOR)
            let start := cur
            match skipValue cur with
            | none => break
            | some after => do
                proposalProcs := some (extractBetween start after)
                cur := after
        | 21 => do  -- Current treasury value (CDDL key 21)
            match CborCursor.decodeUInt cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some tR => do
                treasuryVal := some tR.value
                cur := tR.cursor
        | 22 => do  -- Donation (CDDL key 22)
            match CborCursor.decodeUInt cur with
            | none => match skipValue cur with
                | some after => cur := after
                | none => break
            | some dR => do
                donation := some dR.value
                cur := dR.cursor
        | _ => do
            match skipValue cur with
            | none => break
            | some afterSkip => cur := afterSkip

        -- Recovery: if an inner loop broke midway through a compound value,
        -- cur may be stuck inside it. Always verify cur reached the true end.
        match skipValue valueStart with
        | some afterVal =>
            if cur.pos < afterVal.pos then
              cur := afterVal  -- Inner parse was incomplete, skip to end of value
        | none => break

  -- Use skipValue from the start to get the definitive end position.
  -- The parsing loop may break early (leaving `cur` mid-map), but skipValue
  -- always correctly skips the entire CBOR map structure.
  let endCur ← skipValue bodyStart
  let rawBytes := extractBetween bodyStart endCur

  some {
    value := {
      inputs := inputs.reverse, outputs := outputs.reverse, fee, ttl,
      certificates := certs.reverse, withdrawals := withdrawals.reverse,
      auxiliaryDataHash := auxDataHash, validityIntervalStart := validityStart,
      mint := mint.reverse, scriptDataHash,
      collateralInputs := collateralInputs.reverse,
      requiredSigners := requiredSigners.reverse,
      collateralReturn, totalCollateral,
      referenceInputs := referenceInputs.reverse,
      votingProcedures := votingProcs, proposalProcedures := proposalProcs,
      currentTreasuryValue := treasuryVal, donation, rawBytes
    },
    cursor := endCur
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
            | none =>
                match skipValue cur with
                | some after =>
                    outputs := { address := ByteArray.empty, amount := 0, datum := none, inlineDatum := none, scriptRef := none, nativeAssets := [] } :: outputs
                    cur := after
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
                value := { inputs := inputs.reverse, outputs := outputs.reverse, fee := feeResult.value, certificates := [], rawBytes },
                cursor := cur
              }

/-- Parse transaction body from CBOR map or array -/
partial def parseTransactionBodyC (c : Cursor) : Option (CResult TransactionBody) := do
  if c.remaining == 0 then none
  let major := c.peek >>> 5
  if major == 5 then parseTransactionBodyMapC c
  else if major == 4 then parseTransactionBodyArrayC c
  else none

/-- Parse array of transaction bodies, returning each body with its raw CBOR size -/
partial def parseTransactionBodiesC (c : Cursor) : Option (CResult (List (TransactionBody × Nat))) := do
  let r1 ← CborCursor.decodeArrayHeader c
  let txCount := r1.value
  let mut cur := r1.cursor
  let mut bodies : List (TransactionBody × Nat) := []

  for _ in [0:txCount] do
    -- Stop at break code for indefinite-length arrays
    if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then
      cur := cur.advance 1
      break
    let startPos := cur.pos
    match parseTransactionBodyC cur with
    | none =>
        match skipValue cur with
        | none => break
        | some afterTx => cur := afterTx
    | some txResult =>
        let rawSize := txResult.cursor.pos - startPos
        bodies := (txResult.value, rawSize) :: bodies
        cur := txResult.cursor

  some { value := bodies.reverse, cursor := cur }

/-- Parse array of witness sets, returning each with its raw CBOR size -/
partial def parseWitnessSetsC (c : Cursor) : Option (CResult (List (WitnessSet × Nat))) := do
  let r1 ← CborCursor.decodeArrayHeader c
  let witnessCount := r1.value
  let mut cur := r1.cursor
  let mut witnessSets : List (WitnessSet × Nat) := []

  for _ in [0:witnessCount] do
    -- Stop at break code for indefinite-length arrays
    if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then
      cur := cur.advance 1
      break
    let startPos := cur.pos
    match parseWitnessSetC cur with
    | none =>
        witnessSets := ({ redeemers := [] }, 0) :: witnessSets
        match skipValue cur with
        | none => break
        | some afterWitness => cur := afterWitness
    | some witnessResult =>
        let rawSize := witnessResult.cursor.pos - startPos
        witnessSets := (witnessResult.value, rawSize) :: witnessSets
        cur := witnessResult.cursor

  some { value := witnessSets.reverse, cursor := cur }

/-- Parse the invalid_txs array (element 4 of block body): list of tx indices that failed phase-2 validation -/
private def parseInvalidTxIndices (c : Cursor) : List Nat :=
  match CborCursor.decodeArrayHeader c with
  | none => []
  | some arrR => go arrR.cursor arrR.value []
where
  go (cur : Cursor) : (remaining : Nat) → List Nat → List Nat
    | 0, acc => acc.reverse
    | remaining + 1, acc =>
      if cur.remaining ≥ 1 && cur.peek.toNat == 0xFF then acc.reverse
      else match CborCursor.decodeUInt cur with
        | some r => go r.cursor remaining (r.value :: acc)
        | none => acc.reverse

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

  -- Block might be:
  -- (a) [era_id, [header, tx_bodies, witnesses, aux_data, invalid_txs]] (2-element wrapped)
  -- (b) [header, tx_bodies, witnesses, aux_data, invalid_txs] (5-element direct)
  match CborCursor.decodeArrayHeader blockCursor with
  | none => return none
  | some r1 => do
      -- Determine the cursor pointing to the first element of the 5-element block body
      let blockBodyCursor ←
        if r1.value == 2 then do
          -- Format (a): skip era_id, then decode inner 5-element array
          match CborCursor.decodeUInt r1.cursor with
          | some eraResult =>
              if eraResult.value <= 10 then do
                match CborCursor.decodeArrayHeader eraResult.cursor with
                | some innerArr =>
                    if innerArr.value >= 5 then pure (some innerArr.cursor)
                    else pure none
                | none => pure none
              else pure none
          | none => pure none
        else if r1.value >= 5 then
          -- Format (b): already at the first element of the 5-element block
          pure (some r1.cursor)
        else
          pure none

      match blockBodyCursor with
      | none => return none
      | some elemCursor => do
          -- Skip header (element 0)
          match skipValue elemCursor with
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
                      let transactions := txBodiesResult.value.map (fun (body, bodySize) =>
                        -- tx = [body, witnesses, isValid, auxData] — overhead = isValid (1B) + null auxData (1B) = 2 bytes
                        { body := body, witnesses := { redeemers := [] }, serializedSize := bodySize + 2 : Transaction }
                      )
                      return some { transactions, invalidTxs := [] }
                  | some witnessResult =>
                      let transactions := List.zipWith (fun (body, bodySize) (witnesses, witSize) =>
                        -- Full serialized tx size: body CBOR + witness CBOR + 2 bytes overhead (isValid + null auxData)
                        let fullSize := bodySize + witSize + 2
                        { body := body, witnesses := witnesses, serializedSize := fullSize : Transaction }
                      ) txBodiesResult.value witnessResult.value
                      let transactions :=
                        if txBodiesResult.value.length > witnessResult.value.length then
                          transactions ++ (txBodiesResult.value.drop witnessResult.value.length).map (fun (body, bodySize) =>
                            { body := body, witnesses := { redeemers := [] }, serializedSize := bodySize + 2 : Transaction }
                          )
                        else transactions
                      -- Element 3: Skip auxiliary_data (map of tx_index → metadata)
                      let afterAux := match skipValue witnessResult.cursor with
                        | some c => c
                        | none => witnessResult.cursor
                      -- Element 4: Parse invalid_txs array (indices of phase-2 failed txs)
                      let invalidTxs := parseInvalidTxIndices afterAux
                      return some { transactions, invalidTxs }

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
      let transactions := txBodiesResult.value.map (fun (body, bodySize) =>
        { body := body, witnesses := { redeemers := [] }, serializedSize := bodySize + 2 : Transaction }
      )
      some { transactions, invalidTxs := [] }

-- ==========================
-- = Standalone Tx Parser   =
-- ==========================

/-- Parse a standalone transaction (as broadcast on the network / submitted via N2C).
    A Cardano transaction on the wire is a CBOR array: [body, witnesses, ?aux_data]
    Returns (body, witnesses, rawBodyBytes) or none on parse failure. -/
def parseStandaloneTx (bs : ByteArray)
    : Option (TransactionBody × WitnessSet × ByteArray) := do
  let c0 := Cursor.mk' bs
  -- Unwrap tag24 if present
  let cur :=
    if bs.size >= 2 && bs[0]! == 0xd8 && bs[1]! == 0x18 then
      match CborCursor.decodeBytes (c0.advance 2) with
      | some r => Cursor.mk' r.value
      | none => c0
    else c0
  -- Must be an array of at least 2 elements
  let r1 ← CborCursor.decodeArrayHeader cur
  if r1.value < 2 then none
  -- Parse body (record start offset so we can extract raw bytes)
  let bodyStart := r1.cursor.pos
  let bodyResult ← parseTransactionBodyC r1.cursor
  let bodyEnd := bodyResult.cursor.pos
  let rawBodyBytes := bs.extract bodyStart bodyEnd
  -- Parse witness set
  let witnessResult ← parseWitnessSetC bodyResult.cursor
  some (bodyResult.value, witnessResult.value, rawBodyBytes)

-- ==========================
-- = Native Script Parser   =
-- ==========================

/-- Native script AST matching Cardano's timelock scripts.
    CBOR format: [type, ...params] -/
inductive NativeScriptCbor where
  | requireSignature (keyHash : ByteArray)           -- type 0
  | requireAllOf (scripts : List NativeScriptCbor)   -- type 1
  | requireAnyOf (scripts : List NativeScriptCbor)   -- type 2
  | requireMOfN (m : Nat) (scripts : List NativeScriptCbor) -- type 3
  | requireTimeBefore (slot : Nat)                   -- type 4 (InvalidHereafter)
  | requireTimeAfter (slot : Nat)                    -- type 5 (InvalidBefore)

/-- Parse a native script from raw CBOR bytes.
    Format: [type, ...params] where type determines the structure. -/
partial def parseNativeScriptCbor (data : ByteArray) : Option NativeScriptCbor :=
  let c := Cursor.mk' data
  parseNativeScriptC c |>.map (·.value)
where
  parseNativeScriptC (c : Cursor) : Option (CResult NativeScriptCbor) := do
    let r1 ← CborCursor.decodeArrayHeader c
    if r1.value < 1 then none
    let r2 ← CborCursor.decodeUInt r1.cursor
    let scriptType := r2.value
    match scriptType with
    | 0 => do  -- RequireSignature: [0, keyHash]
      let r3 ← CborCursor.decodeBytes r2.cursor
      some { value := .requireSignature r3.value, cursor := r3.cursor }
    | 1 => do  -- RequireAllOf: [1, [script...]]
      let (scripts, afterScripts) ← parseScriptArray r2.cursor
      some { value := .requireAllOf scripts, cursor := afterScripts }
    | 2 => do  -- RequireAnyOf: [2, [script...]]
      let (scripts, afterScripts) ← parseScriptArray r2.cursor
      some { value := .requireAnyOf scripts, cursor := afterScripts }
    | 3 => do  -- RequireMOfN: [3, m, [script...]]
      let r3 ← CborCursor.decodeUInt r2.cursor
      let m := r3.value
      let (scripts, afterScripts) ← parseScriptArray r3.cursor
      some { value := .requireMOfN m scripts, cursor := afterScripts }
    | 4 => do  -- InvalidBefore (RequireTimeAfter): [4, slot] — tx valid FROM this slot
      let r3 ← CborCursor.decodeUInt r2.cursor
      some { value := .requireTimeAfter r3.value, cursor := r3.cursor }
    | 5 => do  -- InvalidHereafter (RequireTimeBefore): [5, slot] — tx invalid FROM this slot
      let r3 ← CborCursor.decodeUInt r2.cursor
      some { value := .requireTimeBefore r3.value, cursor := r3.cursor }
    | _ => none
  parseScriptArray (c : Cursor) : Option (List NativeScriptCbor × Cursor) := do
    let r1 ← CborCursor.decodeArrayHeader c
    let mut cur := r1.cursor
    let mut scripts : List NativeScriptCbor := []
    for _ in [0:r1.value] do
      match parseNativeScriptC cur with
      | some r => do scripts := scripts ++ [r.value]; cur := r.cursor
      | none =>
        match skipValue cur with
        | some after => cur := after
        | none => break
    some (scripts, cur)

-- ==========================
-- = Script Hash Helpers    =
-- ==========================

/-- Compute the hash of a script from its raw CBOR bytes.
    Script hash = Blake2b-224(0x00 ++ scriptBytes) for native scripts
    Script hash = Blake2b-224(0x01 ++ scriptBytes) for PlutusV1
    Script hash = Blake2b-224(0x02 ++ scriptBytes) for PlutusV2
    Script hash = Blake2b-224(0x03 ++ scriptBytes) for PlutusV3
    The version prefix byte is prepended before hashing. -/
def scriptHashPrefix (scriptBytes : ByteArray) (versionByte : UInt8) : ByteArray :=
  let prefixed := ByteArray.mk #[versionByte] ++ scriptBytes
  prefixed

-- ==========================
-- = Legacy API Compat      =
-- ==========================

-- The old ByteArray-based API functions are kept for files that still
-- import ConwayBlock types but don't call parse functions directly.
-- The old Cbor.lean decode functions remain available via `open Cleanode.Network.Cbor`.

-- ==========================
-- = Tx CBOR Splitter       =
-- ==========================

/-- Components of a raw transaction CBOR, split for block body encoding.
    A Conway tx is `[body, witnesses, isValid, auxData/null]`. -/
structure TxComponents where
  bodyRawBytes     : ByteArray   -- Raw CBOR of tx body (for tx_bodies array)
  witnessRawBytes  : ByteArray   -- Raw CBOR of witness set (for witness_sets array)
  auxDataRawBytes  : Option ByteArray  -- Raw CBOR of auxiliary data (if present, not null)
  isValid          : Bool        -- true if tx is valid (field 2)

/-- Split a raw transaction CBOR into its constituent parts.
    tx = [body, witness_set, is_valid, auxiliary_data / null]
    Returns none if the CBOR doesn't parse as a 4-element array. -/
partial def splitTxCbor (rawTx : ByteArray) : Option TxComponents := do
  let c0 := Cursor.mk' rawTx
  let r1 ← CborCursor.decodeArrayHeader c0
  if r1.value < 3 then none  -- Need at least body, witnesses, isValid

  -- Element 0: tx body
  let bodyStart := r1.cursor
  let afterBody ← skipValue bodyStart
  let bodyBytes := extractBetween bodyStart afterBody

  -- Element 1: witness set
  let witnessStart := afterBody
  let afterWitness ← skipValue witnessStart
  let witnessBytes := extractBetween witnessStart afterWitness

  -- Element 2: isValid (CBOR bool: 0xf5=true, 0xf4=false; or uint 1/0)
  let (valid, afterValid) ←
    if afterWitness.remaining > 0 then
      let b := afterWitness.peek
      if b == 0xf5 then      -- CBOR true
        some (true, afterWitness.advance 1)
      else if b == 0xf4 then -- CBOR false
        some (false, afterWitness.advance 1)
      else
        match CborCursor.decodeUInt afterWitness with
        | some uintResult => some (uintResult.value != 0, uintResult.cursor)
        | none => none
    else none

  -- Element 3: auxiliary data (may be null = 0xf6, or actual data)
  let auxData ←
    if r1.value >= 4 then do
      let auxStart := afterValid
      -- Check for CBOR null (simple value 22 = 0xf6)
      if auxStart.remaining > 0 && auxStart.peek == 0xf6 then
        some none  -- null auxiliary data
      else
        let afterAux ← skipValue auxStart
        some (some (extractBetween auxStart afterAux))
    else
      some none

  some { bodyRawBytes := bodyBytes, witnessRawBytes := witnessBytes,
         auxDataRawBytes := auxData, isValid := valid }

end Cleanode.Network.ConwayBlock
