import Cleanode.Plutus.ScriptContext

/-!
# Untyped Plutus Lambda Calculus (UPLC) AST

The UPLC term language is the final compilation target for Plutus scripts.
This is what gets serialized in the Flat format and evaluated by the CEK machine.

## Term Constructors (PlutusV3)
- Var: de Bruijn indexed variable
- LamAbs: lambda abstraction (single parameter)
- Apply: function application
- Force: force a delayed computation
- Delay: delay a computation
- Constant: literal value
- Builtin: built-in function reference
- Error: unconditional failure
- Constr: constructor application (PlutusV3+)
- Case: case/match on constructors (PlutusV3+)

## References
- Plutus Core Specification
- CIP-0085: Plutus Core v1.1.0
-/

namespace Cleanode.Plutus.UPLC

open Cleanode.Plutus.ScriptContext

-- ====================
-- = Builtin Enum     =
-- ====================

/-- All PlutusV3 builtin functions.
    Order matters — the Flat encoding uses the enum index. -/
inductive BuiltinFun where
  -- Integer arithmetic (0-7)
  | AddInteger            -- 0
  | SubtractInteger       -- 1
  | MultiplyInteger       -- 2
  | DivideInteger         -- 3
  | QuotientInteger       -- 4
  | RemainderInteger      -- 5
  | ModInteger            -- 6
  | EqualsInteger         -- 7
  -- Integer comparison (8-9)
  | LessThanInteger       -- 8
  | LessThanEqualsInteger -- 9
  -- ByteString operations (10-16)
  | AppendByteString      -- 10
  | ConsByteString        -- 11
  | SliceByteString       -- 12
  | LengthOfByteString    -- 13
  | IndexByteString       -- 14
  | EqualsByteString      -- 15
  | LessThanByteString    -- 16
  -- Crypto (17-20)
  | Sha2_256              -- 17
  | Sha3_256              -- 18
  | Blake2b_256           -- 19
  | VerifyEd25519Signature -- 20
  -- String operations (21-23)
  | AppendString          -- 21
  | EqualsString          -- 22
  | EncodeUtf8            -- 23
  -- String/bytes conversion (24)
  | DecodeUtf8            -- 24
  -- Control (25)
  | IfThenElse            -- 25
  -- Pair operations (26-27)
  | FstPair               -- 26
  | SndPair               -- 27
  -- List operations (28-33)
  | ChooseList            -- 28
  | MkCons                -- 29
  | HeadList              -- 30
  | TailList              -- 31
  | NullList              -- 32
  | ChooseData            -- 33
  -- Data constructors (34-38)
  | ConstrData            -- 34
  | MapData               -- 35
  | ListData              -- 36
  | IData                 -- 37
  | BData                 -- 38
  -- Data destructors (39-43)
  | UnConstrData          -- 39
  | UnMapData             -- 40
  | UnListData            -- 41
  | UnIData               -- 42
  | UnBData               -- 43
  -- Data comparison (44)
  | EqualsData            -- 44
  -- Serialization (45)
  | SerialiseData         -- 45
  -- Pair constructor (46)
  | MkPairData            -- 46
  -- List constructor (47)
  | MkNilData             -- 47
  | MkNilPairData         -- 48
  -- Trace (49)
  | Trace                 -- 49
  -- V2 additions (50-52)
  | VerifyEcdsaSecp256k1Signature -- 50
  | VerifySchnorrSecp256k1Signature -- 51
  | SerialiseData2        -- 52 (placeholder)
  -- Integer/ByteString conversion (53-54)
  | IntegerToByteString   -- 53
  | ByteStringToInteger   -- 54
  -- BLS12-381 (55-71)
  | Bls12_381_G1_add      -- 55
  | Bls12_381_G1_neg      -- 56
  | Bls12_381_G1_scalarMul -- 57
  | Bls12_381_G1_equal    -- 58
  | Bls12_381_G1_hashToGroup -- 59
  | Bls12_381_G1_compress -- 60
  | Bls12_381_G1_uncompress -- 61
  | Bls12_381_G2_add      -- 62
  | Bls12_381_G2_neg      -- 63
  | Bls12_381_G2_scalarMul -- 64
  | Bls12_381_G2_equal    -- 65
  | Bls12_381_G2_hashToGroup -- 66
  | Bls12_381_G2_compress -- 67
  | Bls12_381_G2_uncompress -- 68
  | Bls12_381_millerLoop  -- 69
  | Bls12_381_mulMlResult -- 70
  | Bls12_381_finalVerify -- 71
  deriving BEq, Repr

/-- Number of arguments each builtin expects (before saturation) -/
def BuiltinFun.arity : BuiltinFun → Nat
  | .AddInteger | .SubtractInteger | .MultiplyInteger
  | .DivideInteger | .QuotientInteger | .RemainderInteger | .ModInteger => 2
  | .EqualsInteger | .LessThanInteger | .LessThanEqualsInteger => 2
  | .AppendByteString | .ConsByteString => 2
  | .SliceByteString => 3
  | .LengthOfByteString => 1
  | .IndexByteString => 2
  | .EqualsByteString | .LessThanByteString => 2
  | .Sha2_256 | .Sha3_256 | .Blake2b_256 => 1
  | .VerifyEd25519Signature => 3
  | .AppendString | .EqualsString => 2
  | .EncodeUtf8 | .DecodeUtf8 => 1
  | .IfThenElse => 3
  | .FstPair | .SndPair => 1
  | .ChooseList => 3
  | .MkCons => 2
  | .HeadList | .TailList | .NullList => 1
  | .ChooseData => 6
  | .ConstrData => 2
  | .MapData | .ListData | .IData | .BData => 1
  | .UnConstrData | .UnMapData | .UnListData | .UnIData | .UnBData => 1
  | .EqualsData => 2
  | .SerialiseData => 1
  | .MkPairData => 2
  | .MkNilData | .MkNilPairData => 1
  | .Trace => 2
  | .VerifyEcdsaSecp256k1Signature | .VerifySchnorrSecp256k1Signature => 3
  | .SerialiseData2 => 1
  | .IntegerToByteString => 3
  | .ByteStringToInteger => 2
  | .Bls12_381_G1_add | .Bls12_381_G2_add => 2
  | .Bls12_381_G1_neg | .Bls12_381_G2_neg => 1
  | .Bls12_381_G1_scalarMul | .Bls12_381_G2_scalarMul => 2
  | .Bls12_381_G1_equal | .Bls12_381_G2_equal => 2
  | .Bls12_381_G1_hashToGroup | .Bls12_381_G2_hashToGroup => 2
  | .Bls12_381_G1_compress | .Bls12_381_G2_compress => 1
  | .Bls12_381_G1_uncompress | .Bls12_381_G2_uncompress => 1
  | .Bls12_381_millerLoop => 2
  | .Bls12_381_mulMlResult => 2
  | .Bls12_381_finalVerify => 2

/-- Decode a builtin function from its Flat index -/
def BuiltinFun.fromIndex : Nat → Option BuiltinFun
  | 0 => some .AddInteger | 1 => some .SubtractInteger
  | 2 => some .MultiplyInteger | 3 => some .DivideInteger
  | 4 => some .QuotientInteger | 5 => some .RemainderInteger
  | 6 => some .ModInteger | 7 => some .EqualsInteger
  | 8 => some .LessThanInteger | 9 => some .LessThanEqualsInteger
  | 10 => some .AppendByteString | 11 => some .ConsByteString
  | 12 => some .SliceByteString | 13 => some .LengthOfByteString
  | 14 => some .IndexByteString | 15 => some .EqualsByteString
  | 16 => some .LessThanByteString
  | 17 => some .Sha2_256 | 18 => some .Sha3_256
  | 19 => some .Blake2b_256 | 20 => some .VerifyEd25519Signature
  | 21 => some .AppendString | 22 => some .EqualsString
  | 23 => some .EncodeUtf8 | 24 => some .DecodeUtf8
  | 25 => some .IfThenElse
  | 26 => some .FstPair | 27 => some .SndPair
  | 28 => some .ChooseList | 29 => some .MkCons
  | 30 => some .HeadList | 31 => some .TailList
  | 32 => some .NullList | 33 => some .ChooseData
  | 34 => some .ConstrData | 35 => some .MapData
  | 36 => some .ListData | 37 => some .IData | 38 => some .BData
  | 39 => some .UnConstrData | 40 => some .UnMapData
  | 41 => some .UnListData | 42 => some .UnIData | 43 => some .UnBData
  | 44 => some .EqualsData | 45 => some .SerialiseData
  | 46 => some .MkPairData | 47 => some .MkNilData | 48 => some .MkNilPairData
  | 49 => some .Trace
  | 50 => some .VerifyEcdsaSecp256k1Signature
  | 51 => some .VerifySchnorrSecp256k1Signature
  | 53 => some .IntegerToByteString | 54 => some .ByteStringToInteger
  | 55 => some .Bls12_381_G1_add | 56 => some .Bls12_381_G1_neg
  | 57 => some .Bls12_381_G1_scalarMul | 58 => some .Bls12_381_G1_equal
  | 59 => some .Bls12_381_G1_hashToGroup
  | 60 => some .Bls12_381_G1_compress | 61 => some .Bls12_381_G1_uncompress
  | 62 => some .Bls12_381_G2_add | 63 => some .Bls12_381_G2_neg
  | 64 => some .Bls12_381_G2_scalarMul | 65 => some .Bls12_381_G2_equal
  | 66 => some .Bls12_381_G2_hashToGroup
  | 67 => some .Bls12_381_G2_compress | 68 => some .Bls12_381_G2_uncompress
  | 69 => some .Bls12_381_millerLoop | 70 => some .Bls12_381_mulMlResult
  | 71 => some .Bls12_381_finalVerify
  | _ => none

-- ====================
-- = Constants        =
-- ====================

/-- Type tags for polymorphic constants (used in Flat encoding) -/
inductive ConstType where
  | Integer | ByteString | String | Unit | Bool | Data
  | List (inner : ConstType)
  | Pair (fst snd : ConstType)
  | Apply (fun_ arg : ConstType)
  deriving BEq, Repr

/-- UPLC constant values -/
inductive Constant : Type where
  | Integer (value : Int)
  | ByteString (bytes : ByteArray)
  | Str (value : _root_.String)
  | Unit
  | Bool (value : _root_.Bool)
  | Data (value : PlutusData)
  | List (typ : ConstType) (items : _root_.List Constant)
  | Pair (fst snd : Constant)

instance : BEq Constant where
  beq a b := match a, b with
    | .Integer x, .Integer y => x == y
    | .ByteString x, .ByteString y => x == y
    | .Str x, .Str y => x == y
    | .Unit, .Unit => true
    | .Bool x, .Bool y => x == y
    | .Data x, .Data y => x == y
    | _, _ => false

partial def reprConstant : Constant → _root_.String
  | .Integer v => s!"(con integer {v})"
  | .ByteString bs => s!"(con bytestring #{bs.size})"
  | .Str s => s!"(con string \"{s}\")"
  | .Unit => "(con unit ())"
  | .Bool b => s!"(con bool {b})"
  | .Data d => s!"(con data {PlutusData.repr d})"
  | .List _ items => s!"(con list [{items.length}])"
  | .Pair a b => s!"(con pair ({reprConstant a}, {reprConstant b}))"

instance : Repr Constant where
  reprPrec c _ := reprConstant c

-- ====================
-- = UPLC Terms       =
-- ====================

/-- UPLC term — the core AST for Plutus script evaluation -/
inductive Term : Type where
  | Var (idx : Nat)                          -- de Bruijn index (0-based)
  | LamAbs (body : Term)                    -- λ. body
  | Apply (fun_ arg : Term)                 -- f x
  | Force (term : Term)                      -- force t
  | Delay (term : Term)                      -- delay t
  | Constant (value : Constant)             -- literal
  | Builtin (fun_ : BuiltinFun)            -- builtin reference
  | Error                                    -- ⊥
  | Constr (tag : Nat) (args : List Term)   -- constructor (PlutusV3)
  | Case (scrut : Term) (cases : List Term) -- case/match (PlutusV3)

partial def Term.repr : Term → String
  | .Var idx => s!"(var {idx})"
  | .LamAbs body => s!"(lam {body.repr})"
  | .Apply f x => s!"[{f.repr} {x.repr}]"
  | .Force t => s!"(force {t.repr})"
  | .Delay t => s!"(delay {t.repr})"
  | .Constant c => reprConstant c
  | .Builtin b => s!"(builtin {Repr.reprPrec b 0})"
  | .Error => "(error)"
  | .Constr tag args =>
    let as_ := args.map Term.repr |>.intersperse " " |> String.join
    s!"(constr {tag} {as_})"
  | .Case scrut cases =>
    let cs := cases.map Term.repr |>.intersperse " " |> String.join
    s!"(case {scrut.repr} {cs})"

instance : Repr Term where
  reprPrec t _ := t.repr

-- ====================
-- = Program          =
-- ====================

/-- A UPLC program with version info -/
structure Program where
  versionMajor : Nat
  versionMinor : Nat
  versionPatch : Nat
  term : Term

instance : Repr Program where
  reprPrec p _ := s!"(program {p.versionMajor}.{p.versionMinor}.{p.versionPatch} {p.term.repr})"

end Cleanode.Plutus.UPLC
