module Fovel.Gen.Binding
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fovel
open Expr

let isBadBinding (fn: FSharpMemberOrFunctionOrValue) = 
  fn.IsMember && fn.IsCompilerGenerated && (fn.EnclosingEntity.IsFSharpUnion || fn.EnclosingEntity.IsFSharpRecord)

let argToFovel (arg: FSharpMemberOrFunctionOrValue) = arg, arg.FullType
let argsToFovel<'a> = List.map (List.map argToFovel)

let bindingToFovel = function
  | Some (fn, _), _ when isBadBinding fn -> None
  | Some (fn, args), body -> Some { Fn = Some (fn, argsToFovel args); EnclosingType = None; Expr = exprToFovel body }
  | None, body -> Some { Fn = None; EnclosingType = None; Expr = exprToFovel body }

let isUnsupportedBinding { Binding.Expr = expr } = isUnsupportedExpr expr

let programToFovel fsProgram : Program<_,_> =
  fsProgram 
  |> Seq.choose bindingToFovel 
  //|> Seq.filter (not << isUnsupportedBinding)
  |> Seq.toList