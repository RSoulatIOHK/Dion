import Cleanode.Network.Cbor
import Cleanode.Ledger.State

/-!
# Conway Protocol Parameters CBOR Encoding

Encodes `ProtocolParamsState` as a CBOR array (list) matching the Conway ledger's
`EncCBOR (ConwayPParams Identity era)` — a 31-element positional list.

## Field Order (from ConwayPParams record)
 0: minFeeA, 1: minFeeB, 2: maxBlockBodySize, 3: maxTxSize,
 4: maxBlockHeaderSize, 5: stakeKeyDeposit, 6: poolDeposit,
 7: eMax, 8: nOpt, 9: a0, 10: rho, 11: tau,
12: protocolVersion, 13: minPoolCost, 14: coinsPerUTxOByte,
15: costModels, 16: prices, 17: maxTxExUnits, 18: maxBlockExUnits,
19: maxValueSize, 20: collateralPercentage, 21: maxCollateralInputs,
22: poolVotingThresholds, 23: dRepVotingThresholds,
24: committeeMinSize, 25: committeeMaxTermLength, 26: govActionLifetime,
27: govActionDeposit, 28: dRepDeposit, 29: dRepActivity,
30: minFeeRefScriptCostPerByte

## References
- cardano-ledger: Cardano.Ledger.Conway.PParams (ConwayPParams record)
- cardano-ledger: Cardano.Ledger.Conway.PParams (EncCBOR instance)
-/

namespace Cleanode.Network.N2C.PParamsCodec

open Cleanode.Network.Cbor
open Cleanode.Ledger.State

-- ====================
-- = Helpers          =
-- ====================

/-- Encode a rational number as CBOR tagged rational: tag(30, [num, denom]) -/
def encodeRational (num denom : Nat) : ByteArray :=
  let inner := encodeArrayHeader 2 ++ encodeUInt num ++ encodeUInt denom
  encodeTagged 30 inner

/-- Encode execution units as CBOR array: [mem, steps] -/
def encodeExUnits (mem steps : Nat) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt mem ++ encodeUInt steps

/-- Encode protocol version as CBOR array: [major, minor] -/
def encodeProtocolVersion (major minor : Nat) : ByteArray :=
  encodeArrayHeader 2 ++ encodeUInt major ++ encodeUInt minor

-- ====================
-- = PParams Encoding =
-- ====================

/-- Encode Conway protocol parameters as a 31-element CBOR array.
    For fields not tracked in ProtocolParamsState, uses mainnet defaults. -/
def encodeConwayPParams (params : ProtocolParamsState) : ByteArray :=
  encodeArrayHeader 31
  -- 0: minFeeA (Coin)
  ++ encodeUInt params.feeParams.minFeeA
  -- 1: minFeeB (Coin)
  ++ encodeUInt params.feeParams.minFeeB
  -- 2: maxBlockBodySize (Word32)
  ++ encodeUInt params.maxBlockSize
  -- 3: maxTxSize (Word32)
  ++ encodeUInt params.maxTxSize
  -- 4: maxBlockHeaderSize (Word16)
  ++ encodeUInt params.maxBlockHeaderSize
  -- 5: stakeKeyDeposit (Coin)
  ++ encodeUInt params.stakeKeyDeposit
  -- 6: poolDeposit (Coin)
  ++ encodeUInt params.poolDeposit
  -- 7: eMax (EpochInterval)
  ++ encodeUInt 18
  -- 8: nOpt (Word16)
  ++ encodeUInt 500
  -- 9: a0 (NonNegativeInterval → rational)
  ++ encodeRational 3 10
  -- 10: rho (UnitInterval → rational)
  ++ encodeRational 3 1000
  -- 11: tau (UnitInterval → rational)
  ++ encodeRational 2 10
  -- 12: protocolVersion [major, minor]
  ++ encodeProtocolVersion 10 0  -- Conway era = protocol version 10.0
  -- 13: minPoolCost (Coin)
  ++ encodeUInt params.minPoolCost
  -- 14: coinsPerUTxOByte (CoinPerByte → Coin → uint)
  ++ encodeUInt 4310
  -- 15: costModels (CostModels → map)
  ++ encodeMapHeader 0  -- empty for now
  -- 16: prices [priceMemory, priceSteps]
  ++ (encodeArrayHeader 2
    ++ encodeRational 577 10000
    ++ encodeRational 721 10000000)
  -- 17: maxTxExUnits [mem, steps]
  ++ encodeExUnits 14000000 10000000000
  -- 18: maxBlockExUnits [mem, steps]
  ++ encodeExUnits 62000000 20000000000
  -- 19: maxValueSize (Word32)
  ++ encodeUInt 5000
  -- 20: collateralPercentage (Word16)
  ++ encodeUInt 150
  -- 21: maxCollateralInputs (Word16)
  ++ encodeUInt 3
  -- 22: poolVotingThresholds (5 rationals)
  ++ (encodeArrayHeader 5
    ++ encodeRational 51 100   -- motionNoConfidence
    ++ encodeRational 51 100   -- committeeNormal
    ++ encodeRational 51 100   -- committeeNoConfidence
    ++ encodeRational 65 100   -- hardForkInitiation
    ++ encodeRational 51 100)  -- ppSecurityGroup
  -- 23: dRepVotingThresholds (10 rationals)
  ++ (encodeArrayHeader 10
    ++ encodeRational 67 100   -- motionNoConfidence
    ++ encodeRational 67 100   -- committeeNormal
    ++ encodeRational 60 100   -- committeeNoConfidence
    ++ encodeRational 67 100   -- updateToConstitution
    ++ encodeRational 60 100   -- hardForkInitiation
    ++ encodeRational 67 100   -- ppNetworkGroup
    ++ encodeRational 67 100   -- ppEconomicGroup
    ++ encodeRational 67 100   -- ppTechnicalGroup
    ++ encodeRational 67 100   -- ppGovGroup
    ++ encodeRational 51 100)  -- treasuryWithdrawal
  -- 24: committeeMinSize (Word16)
  ++ encodeUInt 7
  -- 25: committeeMaxTermLength (EpochInterval)
  ++ encodeUInt 146
  -- 26: govActionLifetime (EpochInterval)
  ++ encodeUInt 6
  -- 27: govActionDeposit (Coin)
  ++ encodeUInt 100000000000
  -- 28: dRepDeposit (Coin)
  ++ encodeUInt 500000000
  -- 29: dRepActivity (EpochInterval)
  ++ encodeUInt 20
  -- 30: minFeeRefScriptCostPerByte (NonNegativeInterval → rational)
  ++ encodeRational 15 1

end Cleanode.Network.N2C.PParamsCodec
