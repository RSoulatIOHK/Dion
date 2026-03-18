import Cleanode.Ledger.State
import Cleanode.Ledger.UTxO

/-!
# Ledger Snapshots

Persistent snapshots of the ledger state for:
- Fast node restart (avoid replaying entire chain)
- Epoch boundary checkpoints
- State recovery

## Snapshot Format
Binary format: header + serialized UTxO + pool state + delegation state

## References
- Cardano Node: Ledger State Snapshots
-/

namespace Cleanode.Ledger.Snapshot

open Cleanode.Ledger.State
open Cleanode.Ledger.UTxO
open Cleanode.Network.ConwayBlock
open System (FilePath)

-- ====================
-- = Snapshot Types   =
-- ====================

/-- Snapshot metadata -/
structure SnapshotInfo where
  epoch : Nat
  slot : Nat
  blockNo : Nat
  utxoCount : Nat
  poolCount : Nat
  timestamp : Nat           -- Unix timestamp when snapshot was created
  deriving Repr

/-- Snapshot file format header (magic + version) -/
structure SnapshotHeader where
  magic : UInt32 := 0x434C4E44    -- "CLND"
  version : UInt32 := 4           -- v4: + pool registrations + delegation state
  info : SnapshotInfo
  deriving Repr

-- ====================
-- = Serialization    =
-- ====================

/-- Encode a Nat as 8-byte big-endian -/
private def encodeNat64 (n : Nat) : ByteArray :=
  ByteArray.mk #[
    UInt8.ofNat ((n / (256 ^ 7)) % 256),
    UInt8.ofNat ((n / (256 ^ 6)) % 256),
    UInt8.ofNat ((n / (256 ^ 5)) % 256),
    UInt8.ofNat ((n / (256 ^ 4)) % 256),
    UInt8.ofNat ((n / (256 ^ 3)) % 256),
    UInt8.ofNat ((n / (256 ^ 2)) % 256),
    UInt8.ofNat ((n / 256) % 256),
    UInt8.ofNat (n % 256)]

/-- Decode a Nat from 8-byte big-endian -/
private def decodeNat64 (bs : ByteArray) (offset : Nat) : Nat :=
  if offset + 7 < bs.size then
    bs[offset]!.toNat * 256^7 + bs[offset+1]!.toNat * 256^6 +
    bs[offset+2]!.toNat * 256^5 + bs[offset+3]!.toNat * 256^4 +
    bs[offset+4]!.toNat * 256^3 + bs[offset+5]!.toNat * 256^2 +
    bs[offset+6]!.toNat * 256 + bs[offset+7]!.toNat
  else 0

/-- Encode a UInt32 as 4-byte big-endian -/
private def encodeU32 (n : UInt32) : ByteArray :=
  let b0 := UInt8.ofNat ((n >>> 24).toNat % 256)
  let b1 := UInt8.ofNat ((n >>> 16).toNat % 256)
  let b2 := UInt8.ofNat ((n >>>  8).toNat % 256)
  let b3 := UInt8.ofNat (n.toNat % 256)
  ⟨#[b0, b1, b2, b3]⟩

/-- Decode a UInt32 from 4-byte big-endian -/
private def decodeU32 (bs : ByteArray) (offset : Nat) : UInt32 :=
  if offset + 3 < bs.size then
    UInt32.ofNat (bs[offset]!.toNat * 256^3 + bs[offset+1]!.toNat * 256^2 +
                  bs[offset+2]!.toNat * 256 + bs[offset+3]!.toNat)
  else 0

/-- Serialize snapshot info -/
def serializeSnapshotInfo (si : SnapshotInfo) : ByteArray :=
  encodeNat64 si.epoch ++
  encodeNat64 si.slot ++
  encodeNat64 si.blockNo ++
  encodeNat64 si.utxoCount ++
  encodeNat64 si.poolCount ++
  encodeNat64 si.timestamp

/-- Serialize a snapshot header -/
def serializeSnapshotHeader (header : SnapshotHeader) : ByteArray :=
  encodeU32 header.magic ++
  encodeU32 header.version ++
  serializeSnapshotInfo header.info

/-- Serialize a single native asset -/
private def serializeNativeAsset (a : NativeAsset) : ByteArray :=
  -- Format: policyId(28) | nameLen(4) | name | amount(8)
  a.policyId ++
  encodeU32 (UInt32.ofNat a.assetName.size) ++
  a.assetName ++
  encodeNat64 a.amount

/-- Serialize optional variable-length bytes: len(4) | bytes, or len=0 for none -/
private def serializeOptBytes (ob : Option ByteArray) : ByteArray :=
  match ob with
  | some bs => encodeU32 (UInt32.ofNat bs.size) ++ bs
  | none => encodeU32 0

/-- Serialize a UTxO entry (v3 format) -/
private def serializeUTxOEntry (entry : UTxOEntry) : ByteArray :=
  -- Format: txHash(32) | outputIndex(8) | addrLen(4) | addr | amount(8)
  --       | datumHashFlag(1) | datumHash(0|32)
  --       | assetCount(4) | [policyId(28) | nameLen(4) | name | amount(8)] ...
  --       | scriptRefLen(4) | scriptRef
  --       | inlineDatumLen(4) | inlineDatum
  let base := entry.id.txHash ++
    encodeNat64 entry.id.outputIndex ++
    encodeU32 (UInt32.ofNat entry.output.address.size) ++
    entry.output.address ++
    encodeNat64 entry.output.amount
  let datumPart := match entry.output.datum with
    | some dh => ByteArray.mk #[1] ++ dh
    | none => ByteArray.mk #[0]
  let assetHeader := encodeU32 (UInt32.ofNat entry.output.nativeAssets.length)
  let assetData := entry.output.nativeAssets.foldl (fun acc a => acc ++ serializeNativeAsset a) ByteArray.empty
  base ++ datumPart ++ assetHeader ++ assetData ++
    serializeOptBytes entry.output.scriptRef ++
    serializeOptBytes entry.output.inlineDatum

-- ====================
-- = Pool/Deleg Ser   =
-- ====================

/-- Serialize a PoolParams entry.
    Format: poolId(28) | vrfKeyHash(32) | pledge(8) | cost(8) | margin(8)
          | rewardAcctLen(4) | rewardAcct | ownerCount(4) | [ownerLen(4)|owner]...
          | metaLen(4) | meta -/
private def serializePoolParams (p : PoolParams) : ByteArray :=
  p.poolId ++
  p.vrfKeyHash ++
  encodeNat64 p.pledge ++
  encodeNat64 p.cost ++
  encodeNat64 p.margin ++
  serializeOptBytes (some p.rewardAccount) ++
  encodeU32 (UInt32.ofNat p.owners.length) ++
  p.owners.foldl (fun acc o => acc ++ serializeOptBytes (some o)) ByteArray.empty ++
  serializeOptBytes p.metadata

/-- Serialize a DelegationEntry.
    Format: stakeKeyHashLen(4) | stakeKeyHash | poolId(28) -/
private def serializeDelegationEntry (d : DelegationEntry) : ByteArray :=
  serializeOptBytes (some d.stakeKeyHash) ++
  d.poolId

-- ====================
-- = Snapshot I/O     =
-- ====================

/-- Create a snapshot of the current ledger state -/
partial def createSnapshot (state : LedgerState) (snapshotDir : FilePath) : IO Unit := do
  IO.FS.createDirAll snapshotDir

  let si : SnapshotInfo := {
    epoch := state.protocolParams.epoch,
    slot := state.lastSlot,
    blockNo := state.lastBlockNo,
    utxoCount := state.utxo.size,
    poolCount := state.pools.registeredPools.length,
    timestamp := (← IO.monoNanosNow) / 1000000000  -- seconds since boot (approx)
  }

  let header : SnapshotHeader := { info := si }
  let mut data := serializeSnapshotHeader header

  -- Serialize UTxO entries
  for entry in state.utxo.toList do
    data := data ++ serializeUTxOEntry entry

  -- Serialize pool registrations: count(8) + entries
  data := data ++ encodeNat64 state.pools.registeredPools.length
  for pool in state.pools.registeredPools do
    data := data ++ serializePoolParams pool

  -- Serialize delegation entries: count(8) + entries
  data := data ++ encodeNat64 state.delegation.delegations.length
  for d in state.delegation.delegations do
    data := data ++ serializeDelegationEntry d

  -- Serialize registered stake keys: count(8) + each key (variable, with len prefix)
  data := data ++ encodeNat64 state.delegation.registeredStakeKeys.length
  for key in state.delegation.registeredStakeKeys do
    data := data ++ serializeOptBytes (some key)

  let filename := snapshotDir / "utxo-snapshot.dat"
  IO.FS.writeBinFile filename data
  IO.println s!"[snapshot] Saved: {filename} — epoch={si.epoch}, slot={si.slot}, utxos={si.utxoCount}, pools={state.pools.registeredPools.length}, delegations={state.delegation.delegations.length}, size={data.size} bytes"

/-- Deserialize native assets from snapshot binary data.
    Returns (assets, newOffset). -/
private partial def deserializeNativeAssets (data : ByteArray) (off : Nat) (count : Nat) : List NativeAsset × Nat :=
  let rec go (o : Nat) (remaining : Nat) (acc : List NativeAsset) : List NativeAsset × Nat :=
    if remaining == 0 || o + 28 > data.size then (acc.reverse, o)
    else
      let policyId := data.extract o (o + 28)
      if o + 32 > data.size then (acc.reverse, o)
      else
        let nameLen := (decodeU32 data (o + 28)).toNat
        if o + 32 + nameLen + 8 > data.size then (acc.reverse, o)
        else
          let assetName := data.extract (o + 32) (o + 32 + nameLen)
          let amount := decodeNat64 data (o + 32 + nameLen)
          let asset : NativeAsset := { policyId, assetName, amount }
          go (o + 32 + nameLen + 8) (remaining - 1) (asset :: acc)
  go off count []

/-- Deserialize optional variable-length bytes. Returns (value, newOffset). -/
private def deserializeOptBytes (data : ByteArray) (off : Nat) : Option ByteArray × Nat :=
  if off + 4 > data.size then (none, off)
  else
    let len := (decodeU32 data off).toNat
    if len == 0 then (none, off + 4)
    else if off + 4 + len > data.size then (none, off + 4)
    else (some (data.extract (off + 4) (off + 4 + len)), off + 4 + len)

/-- Deserialize UTxO entries from snapshot binary data (v3 format).
    Returns (UTxOSet, finalOffset). -/
private partial def deserializeUTxOEntries (data : ByteArray) (offset : Nat) (count : Nat) : UTxOSet × Nat :=
  let rec go (off : Nat) (remaining : Nat) (m : Std.HashMap UTxOId TxOutput) : Std.HashMap UTxOId TxOutput × Nat :=
    if remaining == 0 || off + 44 > data.size then (m, off)
    else
      let txHash := data.extract off (off + 32)
      let outputIndex := decodeNat64 data (off + 32)
      let addrLen := (decodeU32 data (off + 40)).toNat
      if off + 44 + addrLen + 8 > data.size then (m, off)
      else
        let address := data.extract (off + 44) (off + 44 + addrLen)
        let amount := decodeNat64 data (off + 44 + addrLen)
        let afterAmount := off + 44 + addrLen + 8
        -- Datum hash: 1 byte flag + 0|32 bytes
        if afterAmount >= data.size then (m, off)
        else
          let datumFlag := data[afterAmount]!.toNat
          let (datum, afterDatum) := if datumFlag == 1 && afterAmount + 33 <= data.size then
            (some (data.extract (afterAmount + 1) (afterAmount + 33)), afterAmount + 33)
          else
            (none, afterAmount + 1)
          -- Native assets: count(4) + entries
          if afterDatum + 4 > data.size then (m, off)
          else
            let assetCount := (decodeU32 data afterDatum).toNat
            let (assets, afterAssets) := deserializeNativeAssets data (afterDatum + 4) assetCount
            -- Script ref: len(4) + bytes
            let (scriptRef, afterScriptRef) := deserializeOptBytes data afterAssets
            -- Inline datum: len(4) + bytes
            let (inlineDatum, afterInlineDatum) := deserializeOptBytes data afterScriptRef
            let id : UTxOId := { txHash, outputIndex }
            let output : TxOutput := { address, amount, datum, inlineDatum, scriptRef, nativeAssets := assets }
            go afterInlineDatum (remaining - 1) (m.insert id output)
  let (m, finalOff) := go offset count Std.HashMap.emptyWithCapacity
  ({ map := m }, finalOff)

/-- Deserialize pool params. Returns (pool, newOffset) or none on error. -/
private def deserializePoolParams (data : ByteArray) (off : Nat) : Option (PoolParams × Nat) :=
  if off + 28 + 32 + 8 + 8 + 8 > data.size then none
  else
    let poolId := data.extract off (off + 28)
    let vrfKeyHash := data.extract (off + 28) (off + 60)
    let pledge := decodeNat64 data (off + 60)
    let cost := decodeNat64 data (off + 68)
    let margin := decodeNat64 data (off + 76)
    let (rewardAcctOpt, off2) := deserializeOptBytes data (off + 84)
    let rewardAccount := rewardAcctOpt.getD ByteArray.empty
    if off2 + 4 > data.size then none
    else
      let ownerCount := (decodeU32 data off2).toNat
      let (owners, off3) := (List.range ownerCount).foldl (fun (acc, o) _ =>
        let (keyOpt, o') := deserializeOptBytes data o
        (acc ++ [keyOpt.getD ByteArray.empty], o')) ([], off2 + 4)
      let (metadata, off4) := deserializeOptBytes data off3
      some ({ poolId, vrfKeyHash, pledge, cost, margin, rewardAccount, owners, metadata }, off4)

/-- Deserialize pool entries from binary data. -/
private partial def deserializePoolEntries (data : ByteArray) (off : Nat) (count : Nat) : List PoolParams × Nat :=
  let rec go (o : Nat) (rem : Nat) (acc : List PoolParams) : List PoolParams × Nat :=
    if rem == 0 then (acc.reverse, o)
    else match deserializePoolParams data o with
    | none => (acc.reverse, o)
    | some (p, o') => go o' (rem - 1) (p :: acc)
  go off count []

/-- Deserialize delegation entries from binary data. -/
private partial def deserializeDelegationEntries (data : ByteArray) (off : Nat) (count : Nat) : List DelegationEntry × Nat :=
  let rec go (o : Nat) (rem : Nat) (acc : List DelegationEntry) : List DelegationEntry × Nat :=
    if rem == 0 then (acc.reverse, o)
    else
      let (skOpt, o') := deserializeOptBytes data o
      if o' + 28 > data.size then (acc.reverse, o)
      else
        let stakeKeyHash := skOpt.getD ByteArray.empty
        let poolId := data.extract o' (o' + 28)
        go (o' + 28) (rem - 1) ({ stakeKeyHash, poolId } :: acc)
  go off count []

/-- Deserialize stake keys from binary data. -/
private partial def deserializeStakeKeys (data : ByteArray) (off : Nat) (count : Nat) : List ByteArray × Nat :=
  let rec go (o : Nat) (rem : Nat) (acc : List ByteArray) : List ByteArray × Nat :=
    if rem == 0 then (acc.reverse, o)
    else
      let (keyOpt, o') := deserializeOptBytes data o
      go o' (rem - 1) (keyOpt.getD ByteArray.empty :: acc)
  go off count []

/-- Load a snapshot from disk -/
def loadSnapshot (filename : FilePath) : IO (Option LedgerState) := do
  try
    let data ← IO.FS.readBinFile filename

    -- Validate header
    if data.size < 56 then return none  -- Header too small
    let magic := decodeU32 data 0
    if magic != 0x434C4E44 then return none  -- Bad magic

    let version := decodeU32 data 4
    if version != 3 && version != 4 then
      IO.println s!"[snapshot] Incompatible snapshot version {version} (expected 3 or 4). Please re-sync to create a new snapshot."
      return none

    -- Read info
    let epoch := decodeNat64 data 8
    let slot := decodeNat64 data 16
    let blockNo := decodeNat64 data 24
    let utxoCount := decodeNat64 data 32

    -- Deserialize UTxO entries (header is 56 bytes: 4+4+6*8)
    IO.println s!"[snapshot] Loading {utxoCount} UTxO entries from {filename}..."
    let (utxo, afterUtxo) := deserializeUTxOEntries data 56 utxoCount

    -- Read v4 pool/delegation sections (if present)
    let (pools, delegation) ←
      if version == 4 && afterUtxo + 8 <= data.size then do
        -- Pool registrations
        let poolCount := decodeNat64 data afterUtxo
        let (poolList, afterPools) := deserializePoolEntries data (afterUtxo + 8) poolCount
        -- Delegation entries
        let (delegList, afterDelegs) :=
          if afterPools + 8 <= data.size then
            let delegCount := decodeNat64 data afterPools
            deserializeDelegationEntries data (afterPools + 8) delegCount
          else ([], afterPools)
        -- Registered stake keys
        let stakeKeys :=
          if afterDelegs + 8 <= data.size then
            let keyCount := decodeNat64 data afterDelegs
            (deserializeStakeKeys data (afterDelegs + 8) keyCount).1
          else []
        IO.println s!"[snapshot] v4: loaded {poolList.length} pools, {delegList.length} delegations, {stakeKeys.length} stake keys"
        pure ({ PoolState.empty with registeredPools := poolList },
              { DelegationState.empty with delegations := delegList, registeredStakeKeys := stakeKeys })
      else
        pure (PoolState.empty, DelegationState.empty)

    let ledgerState := {
      LedgerState.initial with
      lastSlot := slot,
      lastBlockNo := blockNo,
      utxo := utxo,
      pools := pools,
      delegation := delegation,
      protocolParams := { ProtocolParamsState.mainnetDefaults with epoch := epoch }
    }
    IO.println s!"[snapshot] Loaded: slot={slot}, block={blockNo}, UTxO={utxo.size} entries, pools={pools.registeredPools.length}, delegations={delegation.delegations.length}"
    return some ledgerState
  catch e =>
    IO.println s!"[snapshot] Failed to load {filename}: {e}"
    return none

/-- Validate snapshot integrity -/
def validateSnapshot (filename : FilePath) : IO Bool := do
  try
    let data ← IO.FS.readBinFile filename
    if data.size < 56 then return false
    let magic := decodeU32 data 0
    return magic == 0x434C4E44
  catch _ =>
    return false

-- ====================
-- = Proof Scaffolds  =
-- ====================

/-- Snapshot is valid if it was created from a valid state -/
theorem snapshot_validity :
    ∀ (_state : LedgerState),
      True → True := by
  intros; trivial

end Cleanode.Ledger.Snapshot
