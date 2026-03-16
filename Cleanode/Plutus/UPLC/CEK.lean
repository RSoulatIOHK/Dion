import Cleanode.Plutus.UPLC.Term
import Cleanode.Plutus.ScriptContext

/-!
# CEK Machine for UPLC Evaluation

The CEK (Control-Environment-Kontinuation) machine evaluates UPLC terms.
This is a small-step abstract machine that drives Plutus script execution.

## Machine States
- **Compute**: evaluating a term in an environment
- **Return**: returning a value to a continuation
- **Done**: final value produced
- **Error**: evaluation failed (script returns false, budget exhausted, etc.)

## Budget Enforcement
Every step costs ExUnits (memory + CPU steps). The machine tracks remaining
budget and errors on exhaustion.

## References
- Plutus Core Specification: CEK Machine
- plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Evaluation/Machine/Cek.hs
-/

namespace Cleanode.Plutus.UPLC.CEK

open Cleanode.Plutus.UPLC
open Cleanode.Plutus.ScriptContext

-- ====================
-- = Execution Budget =
-- ====================

/-- Execution budget (memory + CPU steps) -/
structure ExBudget where
  mem : Nat
  steps : Nat
  deriving Repr, BEq

def ExBudget.subtract (a b : ExBudget) : Option ExBudget :=
  if a.mem >= b.mem && a.steps >= b.steps then
    some { mem := a.mem - b.mem, steps := a.steps - b.steps }
  else none

-- ====================
-- = CEK Values       =
-- ====================

/-- A value in the CEK machine -/
inductive CekValue where
  | VConst (c : Constant)
  | VDelay (term : Term) (env : Array CekValue)
  | VLamAbs (body : Term) (env : Array CekValue)
  | VConstr (tag : Nat) (fields : List CekValue)
  | VBuiltin (fun_ : BuiltinFun) (arity : Nat) (args : List CekValue)

/-- Environment: list of values (de Bruijn indices index into this) -/
abbrev Env := Array CekValue

partial def CekValue.repr : CekValue → String
  | .VConst c => s!"VConst({reprConstant c})"
  | .VDelay _ _ => "VDelay(...)"
  | .VLamAbs _ _ => "VLamAbs(...)"
  | .VConstr tag fields => s!"VConstr({tag}, [{fields.length}])"
  | .VBuiltin f arity args => s!"VBuiltin({Repr.reprPrec f 0}, {arity}, [{args.length}])"

instance : Repr CekValue where
  reprPrec v _ := CekValue.repr v

-- ====================
-- = Continuations    =
-- ====================

/-- Continuation frames for the CEK machine -/
inductive Frame where
  | FrameApplyFun (fun_ : CekValue)             -- □ applied to function, waiting for arg
  | FrameApplyArg (env : Env) (arg : Term)      -- function □, waiting for function to eval
  | FrameForce                                    -- force □
  | FrameConstr (tag : Nat) (env : Env)
      (done : List CekValue) (todo : List Term)  -- constr building fields
  | FrameCases (env : Env) (cases : List Term)   -- case □ [branches]

abbrev Cont := List Frame

-- ====================
-- = Machine State    =
-- ====================

/-- CEK machine state -/
inductive CekState where
  | Computing (env : Env) (term : Term) (cont : Cont)
  | Returning (value : CekValue) (cont : Cont)
  | Done (value : CekValue)
  | Error (msg : String)

-- ====================
-- = PlutusData CBOR  =
-- ====================

/-- Encode a CBOR unsigned integer header (major type 0) -/
private def cborUInt (n : Nat) : ByteArray :=
  if n < 24 then ByteArray.mk #[n.toUInt8]
  else if n < 256 then ByteArray.mk #[24, n.toUInt8]
  else if n < 65536 then ByteArray.mk #[25, (n / 256).toUInt8, (n % 256).toUInt8]
  else if n < 4294967296 then
    ByteArray.mk #[26, (n / 16777216 % 256).toUInt8, (n / 65536 % 256).toUInt8,
                    (n / 256 % 256).toUInt8, (n % 256).toUInt8]
  else
    ByteArray.mk #[27, (n / (1 <<< 56) % 256).toUInt8, (n / (1 <<< 48) % 256).toUInt8,
                    (n / (1 <<< 40) % 256).toUInt8, (n / (1 <<< 32) % 256).toUInt8,
                    (n / 16777216 % 256).toUInt8, (n / 65536 % 256).toUInt8,
                    (n / 256 % 256).toUInt8, (n % 256).toUInt8]

/-- Encode a CBOR header with major type and additional info -/
private def cborHeader (majorType : UInt8) (n : Nat) : ByteArray :=
  let base := majorType <<< 5
  if n < 24 then ByteArray.mk #[base ||| n.toUInt8]
  else if n < 256 then ByteArray.mk #[base ||| 24, n.toUInt8]
  else if n < 65536 then ByteArray.mk #[base ||| 25, (n / 256).toUInt8, (n % 256).toUInt8]
  else if n < 4294967296 then
    ByteArray.mk #[base ||| 26, (n / 16777216 % 256).toUInt8, (n / 65536 % 256).toUInt8,
                    (n / 256 % 256).toUInt8, (n % 256).toUInt8]
  else
    ByteArray.mk #[base ||| 27, (n / (1 <<< 56) % 256).toUInt8, (n / (1 <<< 48) % 256).toUInt8,
                    (n / (1 <<< 40) % 256).toUInt8, (n / (1 <<< 32) % 256).toUInt8,
                    (n / 16777216 % 256).toUInt8, (n / 65536 % 256).toUInt8,
                    (n / 256 % 256).toUInt8, (n % 256).toUInt8]

/-- CBOR-encode PlutusData per the Cardano specification.
    - Constr tag 0-6: CBOR tag 121+tag, fields as array
    - Constr tag 7+: CBOR tag 102, [tag, fields]
    - Map: CBOR major type 5
    - List: CBOR major type 4
    - Integer: CBOR major type 0 (positive) or 1 (negative)
    - ByteString: CBOR major type 2 -/
partial def encodePlutusDataCbor (d : PlutusData) : ByteArray :=
  match d with
  | .Constr tag fields =>
    let encodedFields := cborHeader 4 fields.length ++
      fields.foldl (fun acc f => acc ++ encodePlutusDataCbor f) ByteArray.empty
    if tag <= 6 then
      -- Tags 0-6: use CBOR tags 121-127
      cborHeader 6 (121 + tag) ++ encodedFields
    else if tag <= 127 then
      -- Tags 7-127: use CBOR tags 1280+tag-7
      cborHeader 6 (1280 + tag - 7) ++ encodedFields
    else
      -- Tags 128+: use CBOR tag 102 with [tag, fields]
      cborHeader 6 102 ++ cborHeader 4 2 ++ cborUInt tag ++ encodedFields
  | .Map entries =>
    let header := cborHeader 5 entries.length
    entries.foldl (fun acc (k, v) => acc ++ encodePlutusDataCbor k ++ encodePlutusDataCbor v) header
  | .List items =>
    let header := cborHeader 4 items.length
    items.foldl (fun acc item => acc ++ encodePlutusDataCbor item) header
  | .Integer value =>
    if value >= 0 then cborUInt value.toNat
    else
      -- Negative: CBOR major type 1, value = -1 - n
      let n := (-value - 1).toNat
      cborHeader 1 n
  | .ByteString bytes =>
    cborHeader 2 bytes.size ++ bytes

-- ====================
-- = Builtin Apply    =
-- ====================

/-- Apply a fully saturated builtin to its arguments.
    Returns the result value or an error message. -/
def applyBuiltin (fun_ : BuiltinFun) (args : List CekValue) : Except String CekValue := do
  match fun_, args with
  -- Integer arithmetic
  | .AddInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    return .VConst (.Integer (a + b))
  | .SubtractInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    return .VConst (.Integer (a - b))
  | .MultiplyInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    return .VConst (.Integer (a * b))
  | .DivideInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    if b == 0 then throw "division by zero"
    else return .VConst (.Integer (a / b))
  | .QuotientInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    if b == 0 then throw "division by zero"
    else return .VConst (.Integer (a / b))  -- Lean Int.div is truncated
  | .RemainderInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    if b == 0 then throw "division by zero"
    else return .VConst (.Integer (a % b))
  | .ModInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    if b == 0 then throw "division by zero"
    else return .VConst (.Integer (a % b))  -- TODO: distinguish mod vs rem
  | .EqualsInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    return .VConst (.Bool (a == b))
  | .LessThanInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    return .VConst (.Bool (a < b))
  | .LessThanEqualsInteger, [.VConst (.Integer a), .VConst (.Integer b)] =>
    return .VConst (.Bool (a ≤ b))

  -- ByteString operations
  | .AppendByteString, [.VConst (.ByteString a), .VConst (.ByteString b)] =>
    return .VConst (.ByteString (a ++ b))
  | .ConsByteString, [.VConst (.Integer n), .VConst (.ByteString bs)] =>
    let byte := (n.toNat % 256).toUInt8
    return .VConst (.ByteString (ByteArray.mk #[byte] ++ bs))
  | .SliceByteString, [.VConst (.Integer start), .VConst (.Integer len), .VConst (.ByteString bs)] =>
    let s := max 0 start.toNat
    let l := max 0 len.toNat
    let result := bs.extract s (s + l)
    return .VConst (.ByteString result)
  | .LengthOfByteString, [.VConst (.ByteString bs)] =>
    return .VConst (.Integer (Int.ofNat bs.size))
  | .IndexByteString, [.VConst (.ByteString bs), .VConst (.Integer idx)] =>
    let i := idx.toNat
    if i >= bs.size then throw "index out of bounds"
    else return .VConst (.Integer (Int.ofNat bs[i]!.toNat))
  | .EqualsByteString, [.VConst (.ByteString a), .VConst (.ByteString b)] =>
    return .VConst (.Bool (a == b))
  | .LessThanByteString, [.VConst (.ByteString a), .VConst (.ByteString b)] =>
    -- Lexicographic comparison
    let result := a.toList.zip b.toList |>.foldl (init := Ordering.eq) fun acc (x, y) =>
      match acc with
      | .eq => compare x y
      | other => other
    let isLess := match result with
      | .lt => true
      | .eq => a.size < b.size
      | .gt => false
    return .VConst (.Bool isLess)

  -- Bool / Control
  | .IfThenElse, [.VConst (.Bool cond), thenVal, elseVal] =>
    return if cond then thenVal else elseVal

  -- List operations
  | .MkCons, [hd, .VConst (.List typ items)] =>
    match hd with
    | .VConst c => return .VConst (.List typ (c :: items))
    | _ => throw "MkCons: head must be a constant"
  | .HeadList, [.VConst (.List _ items)] =>
    match items with
    | [] => throw "HeadList: empty list"
    | h :: _ => return .VConst h
  | .TailList, [.VConst (.List typ items)] =>
    match items with
    | [] => throw "TailList: empty list"
    | _ :: t => return .VConst (.List typ t)
  | .NullList, [.VConst (.List _ items)] =>
    return .VConst (.Bool items.isEmpty)
  | .ChooseList, [.VConst (.List _ items), nilCase, consCase] =>
    return if items.isEmpty then nilCase else consCase

  -- Pair operations
  | .FstPair, [.VConst (.Pair a _)] => return .VConst a
  | .SndPair, [.VConst (.Pair _ b)] => return .VConst b
  | .MkPairData, [.VConst (.Data a), .VConst (.Data b)] =>
    return .VConst (.Pair (.Data a) (.Data b))

  -- Data constructors
  | .ConstrData, [.VConst (.Integer tag), .VConst (.List _ items)] =>
    let dataItems := items.filterMap fun c =>
      match c with
      | .Data d => some d
      | _ => none
    return .VConst (.Data (.Constr tag.toNat dataItems))
  | .MapData, [.VConst (.List _ items)] =>
    let pairs := items.filterMap fun c =>
      match c with
      | .Pair (.Data k) (.Data v) => some (k, v)
      | _ => none
    return .VConst (.Data (.Map pairs))
  | .ListData, [.VConst (.List _ items)] =>
    let dataItems := items.filterMap fun c =>
      match c with
      | .Data d => some d
      | _ => none
    return .VConst (.Data (.List dataItems))
  | .IData, [.VConst (.Integer v)] =>
    return .VConst (.Data (.Integer v))
  | .BData, [.VConst (.ByteString bs)] =>
    return .VConst (.Data (.ByteString bs))

  -- Data destructors
  | .UnConstrData, [.VConst (.Data (.Constr tag fields))] =>
    return .VConst (.Pair (.Integer (Int.ofNat tag)) (.List .Data (fields.map (.Data ·))))
  | .UnMapData, [.VConst (.Data (.Map entries))] =>
    return .VConst (.List (.Pair .Data .Data) (entries.map fun (k, v) => .Pair (.Data k) (.Data v)))
  | .UnListData, [.VConst (.Data (.List items))] =>
    return .VConst (.List .Data (items.map (.Data ·)))
  | .UnIData, [.VConst (.Data (.Integer v))] =>
    return .VConst (.Integer v)
  | .UnBData, [.VConst (.Data (.ByteString bs))] =>
    return .VConst (.ByteString bs)

  -- Data comparison
  | .EqualsData, [.VConst (.Data a), .VConst (.Data b)] =>
    return .VConst (.Bool (a == b))

  -- Data chooser
  | .ChooseData, [.VConst (.Data d), constrCase, mapCase, listCase, intCase, bsCase] =>
    return match d with
    | .Constr _ _ => constrCase
    | .Map _ => mapCase
    | .List _ => listCase
    | .Integer _ => intCase
    | .ByteString _ => bsCase

  -- String operations
  | .AppendString, [.VConst (.Str a), .VConst (.Str b)] =>
    return .VConst (.Str (a ++ b))
  | .EqualsString, [.VConst (.Str a), .VConst (.Str b)] =>
    return .VConst (.Bool (a == b))
  | .EncodeUtf8, [.VConst (.Str s)] =>
    return .VConst (.ByteString s.toUTF8)
  | .DecodeUtf8, [.VConst (.ByteString bs)] =>
    return .VConst (.Str (String.fromUTF8! bs))

  -- Nil constructors
  | .MkNilData, [.VConst .Unit] =>
    return .VConst (.List .Data [])
  | .MkNilPairData, [.VConst .Unit] =>
    return .VConst (.List (.Pair .Data .Data) [])

  -- Trace
  | .Trace, [.VConst (.Str _msg), val] =>
    -- In production, would log the message
    return val

  -- Serialization
  | .SerialiseData, [.VConst (.Data d)] =>
    return .VConst (.ByteString (encodePlutusDataCbor d))

  -- Integer/ByteString conversion
  | .IntegerToByteString, [.VConst (.Bool _endian), .VConst (.Integer _width), .VConst (.Integer n)] =>
    -- Simplified: encode as big-endian bytes
    let mut bytes : List UInt8 := []
    let mut v := n.toNat
    if v == 0 then bytes := [0]
    else
      while v > 0 do
        bytes := (v % 256).toUInt8 :: bytes
        v := v / 256
    return .VConst (.ByteString (ByteArray.mk bytes.toArray))
  | .ByteStringToInteger, [.VConst (.Bool _endian), .VConst (.ByteString bs)] =>
    let n := bs.foldl (fun acc b => acc * 256 + b.toNat) 0
    return .VConst (.Integer (Int.ofNat n))

  -- Crypto (hash functions — would need FFI in production)
  | .Blake2b_256, [.VConst (.ByteString _bs)] =>
    -- Placeholder — in production, call FFI blake2b_256
    return .VConst (.ByteString (ByteArray.mk (Array.replicate 32 0)))
  | .Sha2_256, [.VConst (.ByteString _bs)] =>
    return .VConst (.ByteString (ByteArray.mk (Array.replicate 32 0)))
  | .Sha3_256, [.VConst (.ByteString _bs)] =>
    return .VConst (.ByteString (ByteArray.mk (Array.replicate 32 0)))
  | .VerifyEd25519Signature, [.VConst (.ByteString _vk), .VConst (.ByteString _msg), .VConst (.ByteString _sig)] =>
    -- Placeholder — needs FFI
    return .VConst (.Bool false)

  | _, _ => throw s!"builtin type mismatch or unimplemented: {repr fun_}"

-- ====================
-- = Machine Steps    =
-- ====================

/-- Cost of a single CEK step -/
def stepCost : ExBudget := { mem := 100, steps := 23000 }

/-- Perform one step of the CEK machine.
    Returns (new state, remaining budget). -/
def step (state : CekState) (budget : ExBudget) : CekState × ExBudget :=
  match budget.subtract stepCost with
  | none => (.Error "budget exhausted", { mem := 0, steps := 0 })
  | some budget' =>
    match state with
    | .Done _ | .Error _ => (state, budget')
    | .Computing env term cont =>
      match term with
      | .Var idx =>
        if h : idx < env.size then
          (.Returning env[idx] cont, budget')
        else (.Error s!"variable index {idx} out of range (env size {env.size})", budget')
      | .LamAbs body =>
        (.Returning (.VLamAbs body env) cont, budget')
      | .Delay body =>
        (.Returning (.VDelay body env) cont, budget')
      | .Constant c =>
        (.Returning (.VConst c) cont, budget')
      | .Builtin b =>
        (.Returning (.VBuiltin b b.arity []) cont, budget')
      | .Error => (.Error "error term evaluated", budget')
      | .Apply f x =>
        (.Computing env f (.FrameApplyArg env x :: cont), budget')
      | .Force t =>
        (.Computing env t (.FrameForce :: cont), budget')
      | .Constr tag args =>
        match args with
        | [] => (.Returning (.VConstr tag []) cont, budget')
        | a :: rest =>
          (.Computing env a (.FrameConstr tag env [] rest :: cont), budget')
      | .Case scrut cases =>
        (.Computing env scrut (.FrameCases env cases :: cont), budget')

    | .Returning value cont =>
      match cont with
      | [] => (.Done value, budget')
      | frame :: rest =>
        match frame with
        | .FrameApplyArg env arg =>
          (.Computing env arg (.FrameApplyFun value :: rest), budget')
        | .FrameApplyFun fun_ =>
          match fun_ with
          | .VLamAbs body env =>
            (.Computing (env.push value) body rest, budget')
          | .VBuiltin b arity args =>
            let newArgs := args ++ [value]
            if newArgs.length >= arity then
              match applyBuiltin b newArgs with
              | .ok result => (.Returning result rest, budget')
              | .error msg => (.Error msg, budget')
            else
              (.Returning (.VBuiltin b arity newArgs) rest, budget')
          | _ => (.Error "applying non-function", budget')
        | .FrameForce =>
          match value with
          | .VDelay body env => (.Computing env body rest, budget')
          | .VBuiltin b arity args =>
            -- Force on a builtin reduces its "type argument" (polymorphic builtins)
            -- For simplicity, treat force as a no-op on builtins
            (.Returning (.VBuiltin b arity args) rest, budget')
          | _ => (.Error "forcing non-delay", budget')
        | .FrameConstr tag env done todo =>
          match todo with
          | [] =>
            (.Returning (.VConstr tag (done ++ [value])) rest, budget')
          | next :: remaining =>
            (.Computing env next (.FrameConstr tag env (done ++ [value]) remaining :: rest), budget')
        | .FrameCases env cases =>
          match value with
          | .VConstr tag fields =>
            if h : tag < cases.length then
              let branch := cases[tag]
              -- Apply the branch to each field
              let applied := fields.foldl (fun acc field => .Apply acc (.Constant (valueToConstant field))) branch
              (.Computing env applied rest, budget')
            else (.Error s!"case: constructor tag {tag} out of range", budget')
          | _ => (.Error "case: scrutinee not a constructor", budget')
where
  /-- Convert a CekValue back to a Constant (for case application) -/
  valueToConstant : CekValue → Constant
    | .VConst c => c
    | .VConstr tag fields => .Data (.Constr tag (fields.map fun v =>
        match valueToConstant v with
        | .Data d => d
        | .Integer n => .Integer n
        | .ByteString bs => .ByteString bs
        | _ => .Integer 0))
    | _ => .Unit

-- ====================
-- = Evaluation       =
-- ====================

/-- Run the CEK machine to completion (with fuel limit). -/
partial def evaluate (term : Term) (budget : ExBudget) : Except String (CekValue × ExBudget) :=
  let rec go (state : CekState) (budget : ExBudget) (fuel : Nat) : Except String (CekValue × ExBudget) :=
    if fuel == 0 then .error "evaluation fuel exhausted"
    else
      match state with
      | .Done value => .ok (value, budget)
      | .Error msg => .error msg
      | _ =>
        let (state', budget') := step state budget
        go state' budget' (fuel - 1)
  go (.Computing #[] term []) budget 10000000

/-- Evaluate a UPLC Program -/
def evaluateProgram (prog : Program) (budget : ExBudget) : Except String (CekValue × ExBudget) :=
  evaluate prog.term budget

/-- Check if a script evaluation result indicates success (returns True) -/
def isScriptSuccess (result : CekValue) : Bool :=
  match result with
  | .VConst (.Bool true) => true
  | .VConstr 0 _ => true  -- Constr 0 [] is also considered "success" in some contexts
  | _ => false

end Cleanode.Plutus.UPLC.CEK
