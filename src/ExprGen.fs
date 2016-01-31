module Fovel.Gen.Expr
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fovel

module Intrinsics =

  let (|Fn|_|) name (x: Microsoft.FSharp.Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue) = if x.FullName = name then Some() else None
  let (|CoreOperator|_|) = (|Fn|_|) << sprintf "Microsoft.FSharp.Core.Operators.( %s )"
  
  let (|InfixOperator|_|) = function
  | CoreOperator "+" -> Some InfixOpKind.Plus
  | CoreOperator "-" -> Some InfixOpKind.Minus
  | CoreOperator "*" -> Some InfixOpKind.Mul
  | CoreOperator "/" -> Some InfixOpKind.Div
  | CoreOperator ">" -> Some InfixOpKind.Greater
  | CoreOperator ">=" -> Some InfixOpKind.GreaterOrEqual
  | CoreOperator "<" -> Some InfixOpKind.Less
  | CoreOperator "<=" -> Some InfixOpKind.LessOrEqual
  | CoreOperator "=" -> Some InfixOpKind.Equal
  | CoreOperator "<>" -> Some InfixOpKind.NotEqual
  | _ -> None

  let (|Pipe|_|) = (|CoreOperator|_|) "|>"
  let (|BackPipe|_|) = (|CoreOperator|_|) "<|"

  let (|TupleGet|_|) = function
  | Fn "Microsoft.FSharp.Core.fst" -> Some 0
  | Fn "Microsoft.FSharp.Core.snd" -> Some 1
  | _ -> None

  let ofSeq defs (fn: FSharpMemberOrFunctionOrValue) = 
    let fn = fn.FullName
    let isFn (fnName, i) = if fn = fnName then Some i else None
    defs |> Seq.choose isFn |> Seq.tryHead

module Errors =
  let malformedDecisionTree = sprintf "Cannot parse decision tree %A"
  let decisionTreeBindingsNumberMismatch = sprintf "Mismatch between the number of symbols %d and bindings %d in decition tree branch."
  let undefinedCaseField = sprintf "The program attempts to retrieve the case field %s.%s, but that field is not defined on that union."
  let unsupportedExpression = sprintf "Expression is not supported: %A"
  let memberMethodsNotSupported = sprintf "Member methods are not supported."

let rec parseDecisionTree parseExpr (branches: (FSharpMemberOrFunctionOrValue list * FSharpExpr) list ) (expr: FSharpExpr) =
  let r = parseDecisionTree parseExpr branches
  match expr with
  | BasicPatterns.IfThenElse (test, thn, els) -> E.Conditional <!! (parseExpr test, r thn, r els)

  | BasicPatterns.DecisionTreeSuccess (idx, bindings) when idx >= 0 && idx < branches.Length ->
    let branchSymbols, branchExpr = branches.[idx]
    let bindings = bindings |> Seq.map parseExpr |> List.ofSeq

    if bindings.Length <> branchSymbols.Length then 
      Result.fail (Errors.decisionTreeBindingsNumberMismatch branchSymbols.Length bindings.Length)
    else
      let branchExpr = parseExpr branchExpr
      let combineLet nextExpr (binding, symbol) = E.Let <!! (Result.retn symbol, binding, nextExpr)
        
      Seq.zip bindings branchSymbols 
      |> Seq.fold combineLet branchExpr

  | _ -> Result.fail (Errors.malformedDecisionTree expr)  


let rec exprToFovel intrinsic expr : Result<_,_> =
  let r = exprToFovel intrinsic
  let rl = Seq.map r >> Result.sequence
  let retn = Result.retn
  let (|Intrinsic|_|) e = intrinsic e
  match expr with
  | BasicPatterns.Call (None, Intrinsic i, _, _, args) -> E.Intrinsic <! ( retn i, rl args )

  | BasicPatterns.Call (None, Intrinsics.InfixOperator op, _, _, [arg1; arg2]) -> E.InfixOp <!! (r arg1, retn op, r arg2)
  | BasicPatterns.Call (None, Intrinsics.Pipe, _, _, [arg; fn]) -> E.Call <! (r fn, Result.sequence [r arg])
  | BasicPatterns.Call (None, Intrinsics.BackPipe, _, _, [fn; arg]) -> E.Call <! (r fn, Result.sequence [r arg])
  | BasicPatterns.Call (None, Intrinsics.TupleGet idx, _, _, [tupl]) -> E.TupleGet <!! (retn tupl.Type, retn idx, r tupl)
  | BasicPatterns.Call (None, fn, _, _, []) -> retn (E.SymRef fn)
  | BasicPatterns.Call (None, fn, _, _, args) -> E.Call <! (retn (E.SymRef fn), rl args)

  | BasicPatterns.NewTuple (typ, exprs) -> E.NewTuple <! (retn typ, rl exprs)
  | BasicPatterns.TupleGet (typ, idx, tupleExpr) -> E.TupleGet <!! (retn typ, retn idx, r tupleExpr)
  
  | BasicPatterns.Const (c, typ) -> retn <| E.Const( c, typ )
  | BasicPatterns.Value v -> retn (E.SymRef v)
  | BasicPatterns.Let ((sym,expr), inExpr) -> E.Let <!! (retn sym, r expr, r inExpr)
  | BasicPatterns.Lambda (sym, expr) -> E.Function <! (retn sym, r expr)
  | BasicPatterns.Application (fn, _, args) -> E.Call <! (r fn, rl args)

  | BasicPatterns.IfThenElse (test, thn, els) -> E.Conditional <!! (r test, r thn, r els)
  | BasicPatterns.DecisionTree (rootExpr, branches) -> parseDecisionTree r branches rootExpr

  | BasicPatterns.NewUnionCase (typ, case, exprs) -> E.UnionCase <!! (retn typ, retn case.Name, rl exprs)
  | BasicPatterns.UnionCaseTest (expr, typ, case) -> E.UnionCaseTest <!! (r expr, retn typ, retn case.Name)
  | BasicPatterns.UnionCaseGet (expr, typ, case, field) -> E.UnionCaseGet <!!! (r expr, retn typ, retn case.Name, retn field.Name)

  | BasicPatterns.NewRecord (typ, fields) -> E.NewRecord <! (retn typ, rl fields)
  | BasicPatterns.FSharpFieldGet (Some record, recordType, field) -> E.RecordFieldGet <!! (retn recordType, r record, retn field.Name)

  // Unsupported patterns
  | BasicPatterns.Call (Some _, _, _, _, _) -> Result.fail (Errors.memberMethodsNotSupported)

  | e -> Result.fail (Errors.unsupportedExpression e)