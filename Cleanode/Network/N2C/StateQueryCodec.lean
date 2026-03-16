import Cleanode.Network.Cbor

/-!
# Query Encoding/Decoding for LocalStateQuery

The query encoding has two layers:

## Outer Query wrapper (ouroboros-consensus Query type)
- `[0, blockQuery]`  — BlockQuery: delegated to HardFork block-level query
- `[1]`              — GetSystemStart
- `[2]`              — GetChainBlockNo
- `[3]`              — GetChainPoint

## HardFork BlockQuery (inner)
- `[0, eraQuery]`    — QueryIfCurrent: era-indexed query
- `[1, anytimeQ, eraIdx]` — QueryAnytime (3-element)
- `[2, hfQuery]`     — QueryHardFork: meta queries about the hard fork

### QueryIfCurrent era indexing (NS encoding)
`[1, [1, [1, [1, [1, [1, [0, actualQuery]]]]]]]`
Each `[1, ...]` skips one era, `[0, query]` enters the target era.
Conway is era 6 (Byron=0 .. Conway=6), so 6 levels of `[1, ...]`.

### QueryHardFork sub-queries
- `[0]`  — GetInterpreter (era history)
- `[1]`  — GetCurrentEra

## References
- ouroboros-consensus: Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
- Ouroboros Network Spec Section 3.13
-/

namespace Cleanode.Network.N2C.StateQueryCodec

open Cleanode.Network.Cbor

-- ====================
-- = Top-level Query  =
-- ====================

/-- Top-level query type from MsgQuery payload -/
inductive TopLevelQuery where
  | BlockQuery (payload : ByteArray)     -- [0, blockQuery]
  | GetSystemStart                        -- [1]
  | GetChainBlockNo                       -- [2]
  | GetChainPoint                         -- [3]

/-- Decode the outer Query wrapper -/
partial def decodeTopLevelQuery (bs : ByteArray) : Option TopLevelQuery := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining
  match r2.value with
  | 0 => some (.BlockQuery r2.remaining)
  | 1 => some .GetSystemStart
  | 2 => some .GetChainBlockNo
  | 3 => some .GetChainPoint
  | _ => none

-- ====================
-- = HardFork Query   =
-- ====================

/-- HardFork block-level query type -/
inductive HardForkQuery where
  | QueryIfCurrent (eraDepth : Nat) (innerQuery : ByteArray)
  | QueryHardFork (subQuery : Nat)    -- 0=GetInterpreter, 1=GetCurrentEra
  | QueryAnytime (query : Nat) (eraIndex : Nat)

/-- Decode the HardFork block query.
    V_16+ uses flat era indexing: [0, [eraIndex, innerQuery]]
    Older versions used telescoping NS: [0, [1, [1, ..., [0, query]]]] -/
partial def decodeHardForkQuery (bs : ByteArray) : Option HardForkQuery := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining
  match r2.value with
  | 0 => -- QueryIfCurrent: try flat format [eraIndex, innerQuery] first
    let r3 ← decodeArrayHeader r2.remaining
    if r3.value == 2 then
      let r4 ← decodeUInt r3.remaining
      -- If tag >= 2, it's a flat era index (V_16+)
      if r4.value >= 2 then
        some (.QueryIfCurrent r4.value r4.remaining)
      else
        -- Could be flat (era 0 or 1) or telescoping NS
        -- Try telescoping: tag 0 = enter era, tag 1 = skip era
        match r4.value with
        | 0 => some (.QueryIfCurrent 0 r4.remaining)
        | 1 => peelEraLayers r4.remaining 1
        | _ => none
    else none
  | 2 => -- QueryHardFork: [2, subQuery]
    let r3 ← decodeArrayHeader r2.remaining
    let r4 ← decodeUInt r3.remaining
    some (.QueryHardFork r4.value)
  | _ => none
where
  peelEraLayers (bs : ByteArray) (depth : Nat) : Option HardForkQuery := do
    let r ← decodeArrayHeader bs
    if r.value != 2 then none
    let r2 ← decodeUInt r.remaining
    match r2.value with
    | 0 => some (.QueryIfCurrent depth r2.remaining)
    | 1 =>
      if depth > 10 then none
      else peelEraLayers r2.remaining (depth + 1)
    | _ => none

-- ====================
-- = Era Wrapping     =
-- ====================

/-- Number of eras to skip for Conway (Byron=0, Shelley=1, ..., Conway=6) -/
def conwayEraDepth : Nat := 6

/-- Wrap a result in the QueryIfCurrent result envelope.
    Produces: [1, [1, [1, ..., [0, result]]]] -/
def wrapQueryIfCurrentResult (eraDepth : Nat) (resultBytes : ByteArray) : ByteArray :=
  -- Build from inside out: [0, result]
  let inner := encodeArrayHeader 2 ++ encodeUInt 0 ++ resultBytes
  -- Wrap with [1, ...] for each era to skip
  (List.range eraDepth).foldl (fun acc _ =>
    encodeArrayHeader 2 ++ encodeUInt 1 ++ acc
  ) inner

/-- Convenience: wrap for Conway era -/
def wrapConwayResult (resultBytes : ByteArray) : ByteArray :=
  wrapQueryIfCurrentResult conwayEraDepth resultBytes

-- ====================
-- = Era History      =
-- ====================

/-- Encode RelativeTime as Pico integer (seconds * 10^12).
    toCBOR @Pico encodes the underlying Integer (MkFixed n → toCBOR n).
    Uses bignum encoding for values > 2^64. -/
def encodeRelativeTime (seconds : Nat) : ByteArray :=
  encodeBigUInt (seconds * 1000000000000)

/-- Encode a Bound: [relativeTime, slotNo, epochNo] -/
def encodeBound (timeSec slot epoch : Nat) : ByteArray :=
  encodeArrayHeader 3 ++ encodeRelativeTime timeSec ++ encodeUInt slot ++ encodeUInt epoch

/-- Encode EraEnd with a bound (Maybe Bound = Just bound, encoded directly) -/
def encodeEraEnd (timeSec slot epoch : Nat) : ByteArray :=
  encodeBound timeSec slot epoch

/-- Encode EraEnd unbounded (Maybe Bound = Nothing, encoded as CBOR null) -/
def encodeEraUnbounded : ByteArray :=
  ByteArray.mk #[0xf6]  -- CBOR null

/-- Encode SafeZone: [0, safeFromTip, [0]] for StandardSafeZone.
    The third element is SafeBeforeEpoch encoded as [0] (NoSafeBeforeEpoch). -/
def encodeSafeZone (n : Nat) : ByteArray :=
  encodeArrayHeader 3 ++ encodeUInt 0 ++ encodeUInt n
    ++ (encodeArrayHeader 1 ++ encodeUInt 0)

/-- Encode EraParams: [epochSize, slotLengthMs, safeZone, genesisWin]
    SlotLength is encoded as milliseconds (via slotLengthToMillisec).
    GenesisWindow is Word64 (number of slots). -/
def encodeEraParams (epochSize : Nat) (slotLengthMs : Nat) (safeZone : Nat) (genesisWin : Nat) : ByteArray :=
  encodeArrayHeader 4
    ++ encodeUInt epochSize
    ++ encodeUInt slotLengthMs
    ++ encodeSafeZone safeZone
    ++ encodeUInt genesisWin

/-- Encode a single EraSummary: [startBound, endBound, eraParams] -/
def encodeEraSummary (startTime startSlot startEpoch : Nat)
    (endBound : ByteArray) (epochSize slotLengthMs safeZone genesisWin : Nat) : ByteArray :=
  encodeArrayHeader 3
    ++ encodeBound startTime startSlot startEpoch
    ++ endBound
    ++ encodeEraParams epochSize slotLengthMs safeZone genesisWin

/-- Encode the full mainnet era history (Summary).
    7 eras: Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway.
    SlotLength in ms. GenesisWindow = 3*k where k=2160 for mainnet. -/
def encodeMainnetEraHistory : ByteArray :=
  let k := 2160
  let genesisWin := 3 * k  -- 6480
  encodeArrayHeader 7
  -- Byron: epoch 0-207, slot len 20000ms, epoch size 21600, safeZone=4320
  ++ encodeEraSummary 0 0 0
       (encodeEraEnd 89856000 4492800 208)
       21600 20000 4320 genesisWin
  -- Shelley: epoch 208-235
  ++ encodeEraSummary 89856000 4492800 208
       (encodeEraEnd 101952000 16588800 236)
       432000 1000 129600 genesisWin
  -- Allegra: epoch 236-250
  ++ encodeEraSummary 101952000 16588800 236
       (encodeEraEnd 108432000 23068800 251)
       432000 1000 129600 genesisWin
  -- Mary: epoch 251-289
  ++ encodeEraSummary 108432000 23068800 251
       (encodeEraEnd 125280000 39916800 290)
       432000 1000 129600 genesisWin
  -- Alonzo: epoch 290-364
  ++ encodeEraSummary 125280000 39916800 290
       (encodeEraEnd 157680000 72316800 365)
       432000 1000 129600 genesisWin
  -- Babbage: epoch 365-518
  ++ encodeEraSummary 157680000 72316800 365
       (encodeEraEnd 224208000 138844800 519)
       432000 1000 129600 genesisWin
  -- Conway: epoch 519+ (ongoing)
  ++ encodeEraSummary 224208000 138844800 519
       encodeEraUnbounded
       432000 1000 129600 genesisWin

/-- Encode the preprod testnet era history (Summary).
    7 eras: Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway.
    System start: 2022-06-01T00:00:00Z, k=2160.
    Source: docs/preprod-era-history.md -/
def encodePreprodEraHistory : ByteArray :=
  let genesisWin := 3 * 2160  -- 6480
  encodeArrayHeader 7
  -- Byron: epoch 0-3, slot 0-86399, time 0-1727999
  ++ encodeEraSummary 0 0 0
       (encodeEraEnd 1728000 86400 4)
       21600 20000 4320 genesisWin
  -- Shelley: epoch 4 (1 epoch)
  ++ encodeEraSummary 1728000 86400 4
       (encodeEraEnd 2160000 518400 5)
       432000 1000 129600 genesisWin
  -- Allegra: epoch 5 (1 epoch)
  ++ encodeEraSummary 2160000 518400 5
       (encodeEraEnd 2592000 950400 6)
       432000 1000 129600 genesisWin
  -- Mary: epoch 6 (1 epoch)
  ++ encodeEraSummary 2592000 950400 6
       (encodeEraEnd 3024000 1382400 7)
       432000 1000 129600 genesisWin
  -- Alonzo: epoch 7-11 (5 epochs)
  ++ encodeEraSummary 3024000 1382400 7
       (encodeEraEnd 5184000 3542400 12)
       432000 1000 129600 genesisWin
  -- Babbage: epoch 12-162 (151 epochs)
  ++ encodeEraSummary 5184000 3542400 12
       (encodeEraEnd 70416000 68774400 163)
       432000 1000 129600 genesisWin
  -- Conway: epoch 163+ (ongoing)
  ++ encodeEraSummary 70416000 68774400 163
       encodeEraUnbounded
       432000 1000 129600 genesisWin

-- ====================
-- = Query Types      =
-- ====================

/-- Known Shelley-based era query constructors (tags from encodeShelleyQuery) -/
inductive ShelleyQuery where
  | GetLedgerTip          -- tag 0: current tip point
  | GetEpochNo            -- tag 1: current epoch number
  | GetCurrentPParams     -- tag 3: protocol parameters
  | GetStakeDistribution  -- tag 5: pool stake distribution
  | GetUTxOByAddress      -- tag 6: UTxOs for addresses [6, addrs]
  | Unknown (tag : Nat)
  deriving Repr

/-- Decode the inner Shelley-era query from unwrapped bytes.
    Returns the query type and any remaining payload. -/
partial def decodeShelleyQuery (bs : ByteArray) : Option (ShelleyQuery × ByteArray) := do
  -- Queries are either:
  -- [tag] for simple queries, or
  -- [tag, payload] for parameterized queries
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining
  match r2.value with
  | 0 => some (.GetLedgerTip, r2.remaining)
  | 1 => some (.GetEpochNo, r2.remaining)
  | 3 => some (.GetCurrentPParams, r2.remaining)
  | 5 => some (.GetStakeDistribution, r2.remaining)
  | 6 => some (.GetUTxOByAddress, r2.remaining)
  | n => some (.Unknown n, r2.remaining)

/-- Decode address set from GetFilteredUTxO query payload.
    Format: [tag258, [addr1, addr2, ...]] or just [addr1, addr2, ...] -/
partial def decodeAddressSet (bs : ByteArray) : Option (List ByteArray) := do
  -- Try to decode: could be a tagged set (tag 258) or direct array
  -- tag 258 = CBOR tag for mathematical finite set
  let firstByte := bs[0]?
  match firstByte with
  | some b =>
    if b.toNat >= 0xC6 then do  -- CBOR tag
      -- Skip the tag, decode the array inside
      let r1 ← decodeUInt (bs.extract 1 bs.size)  -- tag number
      decodeAddressArray r1.remaining
    else
      decodeAddressArray bs
  | none => some []
where
  decodeAddressArray (bs : ByteArray) : Option (List ByteArray) := do
    let r ← decodeArrayHeader bs
    let count := r.value
    let mut remaining := r.remaining
    let mut addrs : List ByteArray := []
    for _ in List.range count do
      let r2 ← decodeBytes remaining
      addrs := r2.value :: addrs
      remaining := r2.remaining
    some addrs.reverse

end Cleanode.Network.N2C.StateQueryCodec
