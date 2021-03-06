﻿module Fovel.Gen.Binding
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fovel
open Expr

let isInstanceMember (fn: FSharpMemberOrFunctionOrValue) = fn.IsInstanceMember
let isImplicitConstructor (fn: FSharpMemberOrFunctionOrValue) = fn.IsImplicitConstructor

let skipBinding fn = isInstanceMember fn || isImplicitConstructor fn

let argToFovel (arg: FSharpMemberOrFunctionOrValue) = arg, arg.FullType
let argsToFovel<'a> = List.collect (List.map argToFovel)

let bindingToFovel parseExpr = function
  | Some (fn, _), _ when skipBinding fn -> None

  | Some (fn, args), body -> 
      parseExpr body 
      |> Result.map (fun expr -> { Binding.Fn = Some (fn, argsToFovel args); Expr = expr })
      |> Some

  | None, body -> 
    parseExpr body
    |> Result.map (fun expr -> { Binding.Fn = None; Expr = expr })
    |> Some

let programToFovel parseExpr fsProgram : Result<Program<_,_,_>,_> =
  fsProgram 
  |> Seq.choose (bindingToFovel parseExpr)
  |> Result.sequence

let excludeIntrinsicDefinitions parseIntrinsic = 
  let isIntrinsicBinding = function
    | { Binding.Fn = Some (var, _) } -> parseIntrinsic var |> Option.isSome
    | _ -> false
  List.filter (not << isIntrinsicBinding)

let inline mapExpr f binding = {
  Binding.Fn = binding.Fn
  Expr = f binding.Expr }

let inline referencedExternalSymbols b = b |> Binding.expr |> Expr.referencedExternalSymbols