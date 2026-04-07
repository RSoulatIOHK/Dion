import Dion.Network.Cbor
import Dion.Network.N2C.Mux
import Dion.Network.N2C.StateQueryCodec
import Dion.Network.N2C.PParamsCodec
import Dion.Network.N2C.UTxOCodec
import Dion.Ledger.State
import Std.Sync
import Dion.Storage.ChainDB

/-!
# LocalStateQuery Mini-Protocol (N2C Protocol 7)

Handles `cardano-cli query` commands by serving ledger state queries over the
local Unix socket.

## State Machine
StIdle → StAcquiring → StAcquired ⇄ StQuerying → StIdle → StDone

## Messages
- MsgAcquire(point): [0, point]    — client acquires state
- MsgAcquired: [1]                 — server confirms
- MsgFailure(reason): [2, reason]  — server rejects
- MsgQuery(query): [3, query]      — client sends query
- MsgResult(result): [4, result]   — server responds
- MsgReAcquire(point): [5, point]  — client re-acquires
- MsgRelease: [6]                  — client releases
- MsgDone: [7]                     — client is done

## References
- Ouroboros Network Spec Section 3.13
-/

namespace Dion.Network.N2C.LocalStateQuery

open Dion.Network.Cbor
open Dion.Network.N2C.Mux
open Dion.Network.N2C.MiniProtocolId
open Dion.Network.N2C.StateQueryCodec
open Dion.Network.N2C.PParamsCodec
open Dion.Network.N2C.UTxOCodec
open Dion.Network.Multiplexer (Mode)
open Dion.Network.Socket
open Dion.Ledger.State
open Dion.Ledger.UTxO

-- ====================
-- = Messages         =
-- ====================

/-- Client messages -/
inductive LSQClientMessage where
  | MsgAcquire (point : Option (Nat × ByteArray))  -- None = tip
  | MsgQuery (queryBytes : ByteArray)
  | MsgReAcquire (point : Option (Nat × ByteArray))
  | MsgRelease
  | MsgDone

/-- Decode a client message from CBOR -/
partial def decodeLSQClientMessage (bs : ByteArray) : Option LSQClientMessage := do
  let r1 ← decodeArrayHeader bs
  let r2 ← decodeUInt r1.remaining
  match r2.value with
  | 0 => -- MsgAcquire: [0, point] where point is [slot, hash]
    -- Parse the optional point from remaining CBOR
    let point := match decodeArrayHeader r2.remaining with
      | some r3 =>
        if r3.value == 2 then  -- [slot, hash]
          match decodeUInt r3.remaining with
          | some slotR =>
            let slot := slotR.value
            let hash := slotR.remaining.extract 0 (min 32 slotR.remaining.size)
            some (slot, hash)
          | none => none
        else none
      | none => none
    some (.MsgAcquire point)
  | 3 => -- MsgQuery: [3, queryBytes]
    some (.MsgQuery r2.remaining)
  | 5 => -- MsgReAcquire: [5, point]
    some (.MsgReAcquire none)
  | 6 => -- MsgRelease: [6]
    some .MsgRelease
  | 7 => -- MsgDone: [7]
    some .MsgDone
  | 8 => -- MsgAcquire Nothing: [8] — acquire at current tip (no point)
    some (.MsgAcquire none)
  | _ => none

-- ====================
-- = Response Encoding =
-- ====================

/-- Encode MsgAcquired: [1] -/
def encodeMsgAcquired : ByteArray :=
  encodeArrayHeader 1 ++ encodeUInt 1

/-- Encode MsgFailure: [2, reason] -/
def encodeMsgFailure (reason : Nat) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt 2 ++ encodeUInt reason

/-- Encode MsgResult: [4, result] -/
def encodeMsgResult (resultBytes : ByteArray) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt 4 ++ resultBytes

-- ====================
-- = Query Handlers   =
-- ====================

/-- Handle GetLedgerTip query. Returns [slot, blockHash] -/
def handleGetLedgerTip (state : LedgerState) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt state.lastSlot ++ encodeBytes state.lastBlockHash

/-- Handle GetCurrentPParams query -/
def handleGetCurrentPParams (state : LedgerState) : ByteArray :=
  encodeConwayPParams state.protocolParams

/-- Handle GetEpochNo query -/
def handleGetEpochNo (state : LedgerState) : ByteArray :=
  let epoch := epochForSlot state state.lastSlot
  encodeUInt epoch

/-- Handle GetFilteredUTxO query -/
def handleGetFilteredUTxO (state : LedgerState) (queryPayload : ByteArray) : ByteArray :=
  match decodeAddressSet queryPayload with
  | some addrs =>
    let filtered := filterByAddresses state.utxo addrs
    encodeFilteredUTxOResult filtered
  | none =>
    -- Return empty map on decode failure
    encodeMapHeader 0

/-- Handle GetUTxOByTxIn query -/
def handleGetUTxOByTxIn (state : LedgerState) (queryPayload : ByteArray) : ByteArray :=
  match decodeTxInSet queryPayload with
  | some txIns =>
    let filtered := filterByTxIns state.utxo txIns
    encodeFilteredUTxOResult filtered
  | none =>
    encodeMapHeader 0

/-- Handle GetStakePools query — returns tag258 set of pool key hashes -/
def handleGetStakePools (state : LedgerState) : ByteArray :=
  -- Return pool IDs from epoch boundary stake distribution if available
  match state.epochBoundary with
  | some eb =>
    let poolIds := eb.stakeDistribution.map (·.1)
    let items := poolIds.foldl (fun acc pid => acc ++ encodeBytes pid) ByteArray.empty
    encodeTagged 258 (encodeArrayHeader poolIds.length ++ items)
  | none =>
    -- Empty set: tag258([])
    encodeTagged 258 (encodeArrayHeader 0)

/-- Handle GetStakePoolParams query — returns map from pool ID to pool params -/
def handleGetStakePoolParams (_state : LedgerState) (_queryPayload : ByteArray) : ByteArray :=
  -- Return empty map for now (we don't track full pool params)
  encodeMapHeader 0

/-- Handle GetStakeDistribution query -/
def handleGetStakeDistribution (state : LedgerState) : ByteArray :=
  match state.epochBoundary with
  | some eb =>
    -- Map from poolId → (stake, totalStake)
    let entries := eb.stakeDistribution
    let m := encodeMapHeader entries.length
    entries.foldl (fun acc (poolId, stake) =>
      acc ++ encodeBytes poolId
          ++ encodeArrayHeader 2 ++ encodeUInt stake ++ encodeUInt eb.totalStake
    ) m
  | none =>
    encodeMapHeader 0

/-- Dispatch a decoded Shelley-era query to the appropriate handler -/
def dispatchQuery (state : LedgerState) (query : ShelleyQuery)
    (queryPayload : ByteArray) : ByteArray :=
  match query with
  | .GetLedgerTip => handleGetLedgerTip state
  | .GetCurrentPParams => handleGetCurrentPParams state
  | .GetEpochNo => handleGetEpochNo state
  | .GetUTxOByAddress => handleGetFilteredUTxO state queryPayload
  | .GetUTxOByTxIn => handleGetUTxOByTxIn state queryPayload
  | .GetStakePools => handleGetStakePools state
  | .GetStakePoolParams => handleGetStakePoolParams state queryPayload
  | .GetDRepState => encodeMapHeader 0  -- empty DRep state map
  | .GetAccountState => encodeArrayHeader 2 ++ encodeUInt state.treasury ++ encodeUInt state.reserves
  | .GetStakeDistribution => handleGetStakeDistribution state
  | .Unknown _ => encodeMapHeader 0  -- default: empty map (most queries expect map or set)

-- ====================
-- = Frame Handler    =
-- ====================

/-- State of the LocalStateQuery protocol -/
inductive LSQState where
  | Idle
  | Acquired
  deriving BEq

/-- Handle one LocalStateQuery frame. Returns (continue, newState). -/
def handleStateQueryFrame (sock : Socket) (payload : ByteArray)
    (ledgerStateRef : Std.Mutex LedgerState) (lsqState : LSQState)
    (networkMagic : Nat := 764824073) (quiet : Bool := false)
    : IO (Bool × LSQState) := do
  let log (msg : String) : IO Unit := if !quiet then IO.eprintln msg else pure ()
  -- Debug: dump first 32 bytes of payload
  let hexBytes := payload.toList.take 32 |>.map fun b =>
    let hi := b.toNat / 16
    let lo := b.toNat % 16
    let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
    String.mk [toHex hi, toHex lo]
  log s!"[lsq] Received {payload.size} bytes: {String.intercalate " " hexBytes}"
  match decodeLSQClientMessage payload with
  | none =>
    log "[lsq] Failed to decode message"
    return (false, lsqState)
  | some .MsgDone =>
    log "[lsq] MsgDone"
    return (false, .Idle)
  | some (.MsgAcquire _) => do
    log "[lsq] MsgAcquire"
    let response := encodeMsgAcquired
    match ← sendN2CPayload sock .LocalStateQuery .Responder response with
    | .error _ => return (false, .Idle)
    | .ok () => return (true, .Acquired)
  | some (.MsgReAcquire _) => do
    let response := encodeMsgAcquired
    match ← sendN2CPayload sock .LocalStateQuery .Responder response with
    | .error _ => return (false, .Idle)
    | .ok () => return (true, .Acquired)
  | some .MsgRelease =>
    return (true, .Idle)
  | some (.MsgQuery queryBytes) => do
    log s!"[lsq] MsgQuery ({queryBytes.size} bytes)"
    let state ← ledgerStateRef.atomically (fun ref => ref.get)
    -- Parse two-layer query: outer Query wrapper + inner HardFork query
    let result ← match decodeTopLevelQuery queryBytes with
    | some (.BlockQuery hfBytes) =>
      match decodeHardForkQuery hfBytes with
      | some (.QueryHardFork 1) => do
        log "[lsq] → QueryHardFork GetCurrentEra"
        pure (encodeUInt conwayEraDepth)
      | some (.QueryHardFork 0) => do
        log "[lsq] → QueryHardFork GetInterpreter"
        let hist := if networkMagic == 1 then encodePreprodEraHistory
                    else encodeMainnetEraHistory
        let hexResp := hist.toList.take 48 |>.map fun b =>
          let hi := b.toNat / 16
          let lo := b.toNat % 16
          let toHex := fun n => if n < 10 then Char.ofNat (48 + n) else Char.ofNat (87 + n)
          String.mk [toHex hi, toHex lo]
        log s!"[lsq] Era history ({hist.size} bytes): {String.intercalate " " hexResp}"
        pure hist
      | some (.QueryIfCurrent eraDepth innerQueryBytes) => do
        log s!"[lsq] → QueryIfCurrent (era depth {eraDepth})"
        match decodeShelleyQuery innerQueryBytes with
        | some (query, queryPayload) => do
          log s!"[lsq]   → Shelley query: {repr query}"
          -- HardForkNodeToClientEnabled: Right (success) = 1-element list [result]
          let result := dispatchQuery state query queryPayload
          pure (encodeArrayHeader 1 ++ result)
        | none => do
          log "[lsq]   → Unknown Shelley query"
          pure (encodeArrayHeader 1 ++ encodeArrayHeader 0)
      | _ => do
        log "[lsq] → Unknown HardFork query"
        pure (encodeArrayHeader 0)
    | some .GetSystemStart => do
      log "[lsq] → GetSystemStart"
      -- SystemStart → UTCTime via ToCBOR: [year, dayOfYear, timeOfDayPico]
      let (year, day, pico) :=
        if networkMagic == 1 then
          -- Preprod: 2022-06-01T00:00:00Z → year=2022, dayOfYear=152, pico=0
          (2022, 152, 0)
        else if networkMagic == 2 then
          -- Preview: 2023-11-26T00:00:00Z → year=2023, day=330, pico=0
          (2023, 330, 0)
        else
          -- Mainnet: 2017-09-23T21:44:51Z → year=2017, day=266, pico=78291*10^12
          (2017, 266, 78291000000000000)
      pure (encodeArrayHeader 3
        ++ encodeUInt year
        ++ encodeUInt day
        ++ encodeUInt pico)
    | some .GetChainBlockNo => do
      log "[lsq] → GetChainBlockNo"
      -- WithOrigin BlockNo uses Generic-derived Serialise:
      -- Origin → [0], At n → [1, n]
      pure (encodeArrayHeader 2 ++ encodeUInt 1 ++ encodeUInt state.lastBlockNo)
    | some .GetChainPoint => do
      log "[lsq] → GetChainPoint"
      -- Point: BlockPoint slot hash → [slot, hash]; GenesisPoint → []
      pure (encodeArrayHeader 2 ++ encodeUInt state.lastSlot ++ encodeBytes state.lastBlockHash)
    | none => do
      log "[lsq] → Failed to decode query"
      pure (encodeArrayHeader 0)
    let response := encodeMsgResult result
    match ← sendN2CPayload sock .LocalStateQuery .Responder response with
    | .error _ => return (false, .Idle)
    | .ok () => return (true, .Acquired)

end Dion.Network.N2C.LocalStateQuery
