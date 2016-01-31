module Fovel.Gen.Binding
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fovel
open Expr

let isMember (fn: FSharpMemberOrFunctionOrValue) = fn.IsMember
let isCompilerGenerated (fn: FSharpMemberOrFunctionOrValue) = fn.IsMember
let isAlgebraicType (t: FSharpEntity) = t.IsFSharpUnion || t.IsFSharpRecord

let skipBinding (fn: FSharpMemberOrFunctionOrValue) =  isMember fn && isCompilerGenerated fn && isAlgebraicType fn.EnclosingEntity

let argToFovel (arg: FSharpMemberOrFunctionOrValue) = arg, arg.FullType
let argsToFovel<'a> = List.map (List.map argToFovel)

let bindingToFovel parseExpr = function
  | Some (fn, _), _ when skipBinding fn -> None
  | Some (fn, _), _ when isMember fn -> Some <| Result.fail (Error.MemberMethodsNotSupported fn)

  | Some (fn, args), body -> 
      parseExpr body 
      |> Result.map (fun expr -> { Fn = Some (fn, argsToFovel args); EnclosingType = None; Expr = expr })
      |> Some

  | None, body -> 
    parseExpr body
    |> Result.map (fun expr -> { Fn = None; EnclosingType = None; Expr = expr })
    |> Some

let isUnsupportedBinding { Binding.Expr = expr } = isUnsupportedExpr expr

let programToFovel parseExpr fsProgram : Result<Program<_,_,_>,_> =
  fsProgram 
  |> Seq.choose (bindingToFovel parseExpr)
  |> Result.sequence
  |> Result.map (List.filter (not << isUnsupportedBinding))

let excludeIntrinsicDefinitions parseIntrinsic = 
  let isIntrinsicBinding = function
    | { Fn = Some (var, _) } -> parseIntrinsic var |> Option.isSome
    | _ -> false
  List.filter (not << isIntrinsicBinding)