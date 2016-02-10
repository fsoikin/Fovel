module Fovel.Gen.Binding
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fovel
open Expr

let isInstanceMember (fn: FSharpMemberOrFunctionOrValue) = fn.IsInstanceMember
let isImplicitConstructor (fn: FSharpMemberOrFunctionOrValue) = fn.IsImplicitConstructor
let isCompilerGenerated (fn: FSharpMemberOrFunctionOrValue) = fn.IsCompilerGenerated

let skipBinding fn = isInstanceMember fn || isCompilerGenerated fn || isImplicitConstructor fn

let argToFovel (arg: FSharpMemberOrFunctionOrValue) = arg, arg.FullType
let argsToFovel<'a> = List.map (List.map argToFovel)

let bindingToFovel parseExpr = function
  | Some (fn, _), _ when skipBinding fn -> None
  | Some (fn, _), _ when isInstanceMember fn -> Some <| Result.fail (Error.InstanceMethodsNotSupported fn)

  | Some (fn, args), body -> 
      parseExpr body 
      |> Result.map (fun expr -> { Binding.Fn = Some (fn, argsToFovel args); EnclosingType = None; Expr = expr })
      |> Some

  | None, body -> 
    parseExpr body
    |> Result.map (fun expr -> { Binding.Fn = None; EnclosingType = None; Expr = expr })
    |> Some

let programToFovel parseExpr fsProgram : Result<Program<_,_,_>,_> =
  fsProgram 
  |> Seq.choose (bindingToFovel parseExpr)
  |> Result.sequence

let excludeIntrinsicDefinitions parseIntrinsic = 
  let isIntrinsicBinding = function
    | { Fn = Some (var, _) } -> parseIntrinsic var |> Option.isSome
    | _ -> false
  List.filter (not << isIntrinsicBinding)

let mapExpr f binding = {
  Binding.Fn = binding.Fn
  EnclosingType = binding.EnclosingType
  Expr = f binding.Expr }