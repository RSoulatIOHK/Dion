import Dion.Network.Cbor
import Dion.Network.ConwayBlock

/-!
# Era-Specific Transaction Structures

Defines transaction structures for each Cardano era with their specific fields:
- Mary: Multi-asset support (mint/burn)
- Alonzo: Plutus script support (script data hash, collateral)
- Babbage: Reference inputs, inline datums, reference scripts
- Conway: Governance actions (voting, proposals)

All eras share the common TransactionBody/TxInput/TxOutput types from ConwayBlock
since the parser already handles both map and array formats. This module adds
era-specific metadata and type aliases.

## References
- CIP-0002: Shelley transaction format
- CIP-0005: Mary multi-asset
- CIP-0032: Alonzo Plutus
- CIP-0040: Babbage inline datums
- CIP-1694: Conway governance
-/

namespace Dion.Network.EraTx

open Dion.Network.Cbor
open Dion.Network.ConwayBlock

-- ====================
-- = Multi-Asset (Mary) =
-- ====================

/-- Mary-era multi-asset value -/
structure MultiAssetValue where
  lovelace : Nat
  assets : List NativeAsset    -- Policy-scoped assets

instance : Repr MultiAssetValue where
  reprPrec v _ := s!"MultiAssetValue(lovelace={v.lovelace}, assets={v.assets.length})"

/-- Mary-era transaction metadata -/
structure MaryTxMeta where
  mint : List NativeAsset      -- Newly minted/burned tokens

instance : Repr MaryTxMeta where
  reprPrec m _ := s!"MaryTxMeta(mint={m.mint.length})"

-- ====================
-- = Plutus (Alonzo)  =
-- ====================

/-- Plutus script version -/
inductive PlutusVersion where
  | V1    -- Alonzo
  | V2    -- Babbage
  | V3    -- Conway
  deriving Repr, BEq

/-- Plutus script (versioned bytecode) -/
structure PlutusScript where
  version : PlutusVersion
  bytes : ByteArray            -- Serialized Plutus Core

instance : Repr PlutusScript where
  reprPrec s _ := s!"PlutusScript({repr s.version}, {s.bytes.size}B)"

/-- Alonzo-era transaction metadata -/
structure AlonzoTxMeta where
  scriptDataHash : Option ByteArray     -- Hash of script-related data
  collateralInputs : List TxInput       -- Collateral for script failure
  requiredSigners : List ByteArray      -- Required signer key hashes

instance : Repr AlonzoTxMeta where
  reprPrec m _ := s!"AlonzoTxMeta(collateral={m.collateralInputs.length}, signers={m.requiredSigners.length})"

-- ====================
-- = Babbage          =
-- ====================

/-- Datum option for outputs -/
inductive DatumOption where
  | DatumHash (hash : ByteArray)        -- Reference by hash
  | InlineDatum (data_ : ByteArray)     -- Inline datum (Babbage+)

instance : Repr DatumOption where
  reprPrec
    | .DatumHash h, _ => s!"DatumHash({h.size}B)"
    | .InlineDatum d, _ => s!"InlineDatum({d.size}B)"

/-- Script reference for outputs -/
structure ScriptRef where
  scriptBytes : ByteArray

instance : Repr ScriptRef where
  reprPrec s _ := s!"ScriptRef({s.scriptBytes.size}B)"

/-- Babbage-era extended output -/
structure BabbageTxOutput where
  base : TxOutput
  datumOption : Option DatumOption
  scriptRef : Option ScriptRef

instance : Repr BabbageTxOutput where
  reprPrec o _ := s!"BabbageTxOutput({repr o.base})"

/-- Babbage-era transaction metadata -/
structure BabbageTxMeta where
  alonzo : AlonzoTxMeta
  referenceInputs : List TxInput        -- Read-only inputs (no spending)
  collateralReturn : Option TxOutput    -- Return excess collateral
  totalCollateral : Option Nat          -- Total collateral amount

instance : Repr BabbageTxMeta where
  reprPrec m _ := s!"BabbageTxMeta(refInputs={m.referenceInputs.length})"

-- ====================
-- = Conway (Governance) =
-- ====================

/-- Governance action type -/
inductive GovActionType where
  | ParameterChange
  | HardForkInitiation
  | TreasuryWithdrawals
  | NoConfidence
  | UpdateCommittee
  | NewConstitution
  | InfoAction
  deriving Repr, BEq

/-- Governance action proposal -/
structure ProposalProcedure where
  deposit : Nat
  rewardAccount : ByteArray
  govAction : GovActionType
  anchor : Option ByteArray    -- Metadata anchor URL hash

instance : Repr ProposalProcedure where
  reprPrec p _ := s!"ProposalProcedure({repr p.govAction}, deposit={p.deposit})"

/-- Vote on a governance action -/
inductive Vote where
  | Yes | No | Abstain
  deriving Repr, BEq

/-- Voting procedure -/
structure VotingProcedure where
  voter : ByteArray             -- Voter credential
  govActionId : ByteArray       -- Which proposal
  vote : Vote
  anchor : Option ByteArray

instance : Repr VotingProcedure where
  reprPrec v _ := s!"VotingProcedure({repr v.vote})"

/-- Conway-era transaction metadata -/
structure ConwayTxMeta where
  babbage : BabbageTxMeta
  votingProcedures : List VotingProcedure
  proposalProcedures : List ProposalProcedure
  treasuryAmount : Option Nat
  donation : Option Nat

instance : Repr ConwayTxMeta where
  reprPrec m _ := s!"ConwayTxMeta(votes={m.votingProcedures.length}, proposals={m.proposalProcedures.length})"

-- ====================
-- = Era Tag          =
-- ====================

/-- Cardano era identifier -/
inductive CardanoEra where
  | Byron | Shelley | Allegra | Mary | Alonzo | Babbage | Conway
  deriving Repr, BEq

/-- Map era number to era type -/
def eraFromNumber : Nat → Option CardanoEra
  | 0 => some .Byron
  | 1 => some .Shelley
  | 2 => some .Allegra
  | 3 => some .Mary
  | 4 => some .Alonzo
  | 5 => some .Babbage
  | 6 => some .Conway
  | _ => none

/-- Get era number from era type -/
def eraToNumber : CardanoEra → Nat
  | .Byron   => 0
  | .Shelley => 1
  | .Allegra => 2
  | .Mary    => 3
  | .Alonzo  => 4
  | .Babbage => 5
  | .Conway  => 6

/-- Check if era supports multi-asset -/
def eraHasMultiAsset : CardanoEra → Bool
  | .Mary | .Alonzo | .Babbage | .Conway => true
  | _ => false

/-- Check if era supports Plutus scripts -/
def eraHasPlutus : CardanoEra → Bool
  | .Alonzo | .Babbage | .Conway => true
  | _ => false

/-- Check if era supports governance -/
def eraHasGovernance : CardanoEra → Bool
  | .Conway => true
  | _ => false

end Dion.Network.EraTx
