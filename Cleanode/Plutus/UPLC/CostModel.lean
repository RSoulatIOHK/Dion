import Dion.Plutus.UPLC.Term
import Dion.Plutus.UPLC.CEK

/-!
# Plutus Cost Model

Defines execution cost parameters for PlutusV1, V2, and V3.
Each builtin has a cost function based on argument sizes.

## Cost Functions
- Constant: `c`
- Linear in one arg: `a * size + b`
- Linear in two args: `a * size1 + b * size2 + c`
- Quadratic: `a * size^2 + b * size + c`

The ~200 parameters for PlutusV3 are protocol parameters and can be
updated via governance. Default values match mainnet Conway era.

## References
- Plutus Cost Model Parameters (CIP-0055)
- cardano-ledger: Alonzo cost model encoding
-/

namespace Dion.Plutus.UPLC.CostModel

open Dion.Plutus.UPLC
open Dion.Plutus.UPLC.CEK

-- ====================
-- = Cost Functions   =
-- ====================

/-- A cost function that computes execution cost from argument sizes -/
inductive CostFunction where
  | Constant (cost : Nat)
  | LinearInX (intercept slope : Nat)
  | LinearInY (intercept slope : Nat)
  | LinearInXY (intercept slopeX slopeY : Nat)
  | AddedSizes (intercept slope : Nat)
  | MultipliedSizes (intercept slope : Nat)
  | MinSize (intercept slope : Nat)
  | MaxSize (intercept slope : Nat)
  | ConstAboveDiagonal (belowCost : Nat) (aboveCost : Nat)
  deriving Repr

/-- Evaluate a cost function given argument sizes -/
def CostFunction.eval (f : CostFunction) (sizeX sizeY : Nat) : Nat :=
  match f with
  | .Constant c => c
  | .LinearInX i s => i + s * sizeX
  | .LinearInY i s => i + s * sizeY
  | .LinearInXY i sx sy => i + sx * sizeX + sy * sizeY
  | .AddedSizes i s => i + s * (sizeX + sizeY)
  | .MultipliedSizes i s => i + s * (sizeX * sizeY)
  | .MinSize i s => i + s * min sizeX sizeY
  | .MaxSize i s => i + s * max sizeX sizeY
  | .ConstAboveDiagonal below above => if sizeX > sizeY then above else below

-- ====================
-- = Builtin Costs    =
-- ====================

/-- Memory and CPU cost for a single builtin -/
structure BuiltinCost where
  cpuCost : CostFunction
  memCost : CostFunction
  deriving Repr

/-- Default PlutusV3 cost model (approximation of mainnet values) -/
def defaultBuiltinCost : BuiltinFun → BuiltinCost
  -- Integer arithmetic
  | .AddInteger => { cpuCost := .MaxSize 205665 812, memCost := .MaxSize 1 1 }
  | .SubtractInteger => { cpuCost := .MaxSize 205665 812, memCost := .MaxSize 1 1 }
  | .MultiplyInteger => { cpuCost := .MultipliedSizes 69522 11687, memCost := .AddedSizes 0 1 }
  | .DivideInteger => { cpuCost := .ConstAboveDiagonal 196500 453240, memCost := .LinearInX 0 1 }
  | .QuotientInteger => { cpuCost := .ConstAboveDiagonal 196500 453240, memCost := .LinearInX 0 1 }
  | .RemainderInteger => { cpuCost := .ConstAboveDiagonal 196500 453240, memCost := .LinearInX 0 1 }
  | .ModInteger => { cpuCost := .ConstAboveDiagonal 196500 453240, memCost := .LinearInX 0 1 }
  | .EqualsInteger => { cpuCost := .MinSize 208512 421, memCost := .Constant 1 }
  | .LessThanInteger => { cpuCost := .MinSize 208896 511, memCost := .Constant 1 }
  | .LessThanEqualsInteger => { cpuCost := .MinSize 204924 473, memCost := .Constant 1 }

  -- ByteString operations
  | .AppendByteString => { cpuCost := .AddedSizes 1000 571, memCost := .AddedSizes 0 1 }
  | .ConsByteString => { cpuCost := .LinearInY 221973 4, memCost := .AddedSizes 0 1 }
  | .SliceByteString => { cpuCost := .LinearInY 265318 0, memCost := .LinearInY 4 0 }
  | .LengthOfByteString => { cpuCost := .Constant 1000, memCost := .Constant 10 }
  | .IndexByteString => { cpuCost := .Constant 57667, memCost := .Constant 4 }
  | .EqualsByteString => { cpuCost := .MinSize 245000 216, memCost := .Constant 1 }
  | .LessThanByteString => { cpuCost := .MinSize 197145 156, memCost := .Constant 1 }

  -- Crypto
  | .Sha2_256 => { cpuCost := .LinearInX 806990 30482, memCost := .Constant 4 }
  | .Sha3_256 => { cpuCost := .LinearInX 1927926 82523, memCost := .Constant 4 }
  | .Blake2b_256 => { cpuCost := .LinearInX 117366 10475, memCost := .Constant 4 }
  | .VerifyEd25519Signature => { cpuCost := .LinearInY 53384111 14333, memCost := .Constant 10 }

  -- String
  | .AppendString => { cpuCost := .AddedSizes 1000 571, memCost := .AddedSizes 4 1 }
  | .EqualsString => { cpuCost := .MinSize 187000 1000, memCost := .Constant 1 }
  | .EncodeUtf8 => { cpuCost := .LinearInX 1000 28662, memCost := .LinearInX 4 2 }
  | .DecodeUtf8 => { cpuCost := .LinearInX 497525 14068, memCost := .LinearInX 4 2 }

  -- Control
  | .IfThenElse => { cpuCost := .Constant 80556, memCost := .Constant 1 }

  -- Pair
  | .FstPair => { cpuCost := .Constant 80436, memCost := .Constant 32 }
  | .SndPair => { cpuCost := .Constant 85931, memCost := .Constant 32 }

  -- List
  | .ChooseList => { cpuCost := .Constant 175354, memCost := .Constant 32 }
  | .MkCons => { cpuCost := .Constant 65493, memCost := .Constant 32 }
  | .HeadList => { cpuCost := .Constant 43249, memCost := .Constant 32 }
  | .TailList => { cpuCost := .Constant 41182, memCost := .Constant 32 }
  | .NullList => { cpuCost := .Constant 60091, memCost := .Constant 32 }

  -- Data
  | .ChooseData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .ConstrData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .MapData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .ListData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .IData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .BData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .UnConstrData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .UnMapData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .UnListData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .UnIData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .UnBData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .EqualsData => { cpuCost := .MinSize 150000 10000, memCost := .Constant 1 }
  | .SerialiseData => { cpuCost := .LinearInX 1000 2 , memCost := .LinearInX 2 2 }
  | .MkPairData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .MkNilData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .MkNilPairData => { cpuCost := .Constant 150000, memCost := .Constant 32 }
  | .Trace => { cpuCost := .Constant 150000, memCost := .Constant 32 }

  -- Default for less common builtins
  | _ => { cpuCost := .Constant 200000, memCost := .Constant 32 }

/-- Compute the ExBudget cost for a builtin application -/
def builtinExBudget (fun_ : BuiltinFun) (argSizes : List Nat) : ExBudget :=
  let cost := defaultBuiltinCost fun_
  let sizeX := argSizes.head?.getD 0
  let sizeY := argSizes.tail.head?.getD 0
  { mem := cost.memCost.eval sizeX sizeY,
    steps := cost.cpuCost.eval sizeX sizeY }

-- ====================
-- = Machine Costs    =
-- ====================

/-- Default step costs for CEK machine operations -/
def cekStartupCost : ExBudget := { mem := 100, steps := 100 }
def cekVarCost : ExBudget := { mem := 100, steps := 23000 }
def cekConstCost : ExBudget := { mem := 100, steps := 23000 }
def cekLamCost : ExBudget := { mem := 100, steps := 23000 }
def cekDelayCost : ExBudget := { mem := 100, steps := 23000 }
def cekForceCost : ExBudget := { mem := 100, steps := 23000 }
def cekApplyCost : ExBudget := { mem := 100, steps := 23000 }
def cekBuiltinCost : ExBudget := { mem := 100, steps := 23000 }
def cekConstrCost : ExBudget := { mem := 100, steps := 23000 }
def cekCaseCost : ExBudget := { mem := 100, steps := 23000 }

end Dion.Plutus.UPLC.CostModel
