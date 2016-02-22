module Fovel.Transformation
open Fovel
open Fovel.Gen
open Microsoft.FSharp.Compiler.SourceCodeServices

let private fnDefinition = function
  | { Binding.Fn = Some (fn,parms); Expr = expr } when parms <> [] -> Some (fn, parms |> List.map fst, expr)
  | _ -> None

let private fstt (x,_,_) = x

let rec private replaceGenericParams parms args expr =
  let map = Seq.zip parms args |> Seq.toList
  let sameType (a: FSharpType) (p: FSharpGenericParameter) = a.IsGenericParameter && a.GenericParameter = p
  let getArg typ = map |> Seq.tryFind (fst >> sameType typ) |> Option.map snd |> Option.orElse typ
  Expr.mapType getArg expr

let private inlineExpression replaceNestedCalls args typeArgs (fn,parms,body) =
  let args = args |> List.map replaceNestedCalls
  let typeParams = FSharp.typeParameters fn
  let paramBindings = List.zip parms args
  let body = body |> replaceNestedCalls |> replaceGenericParams typeParams typeArgs
  E.Let (paramBindings, body)

let inlineFunctions program =
  let allInlineBindings = program |> List.choose fnDefinition |> List.filter (fstt >> FSharp.isInline)
  let findBinding fn = allInlineBindings |> List.tryFind (fstt >> (=) fn)

  let rec replaceInlineFnRefs = function
    | E.Call (E.SymRef fn, typeArgs, args) as e -> 
        findBinding fn
        |> Option.map (inlineExpression replaceInlineFnRefs args typeArgs)
        |> Option.orElse e

//    | E.SymRef fn as e when not (FSharp.isGeneric fn) ->
//        findBinding fn
//        |> Option.map (fun (_,parms,body) -> E.fun)
//        |> Option.orElse e

    | e -> Expr.cata replaceInlineFnRefs id id id e

  program |> List.map (Binding.mapExpr replaceInlineFnRefs)


let excludeInlineDefinitions program = 
  let isInlineFnDef binding = fnDefinition binding |> Option.map (fstt >> FSharp.isInline) |> Option.orElse false
  program |> List.filter (not << isInlineFnDef)


let applyAll program = 
  program
  |> inlineFunctions
  |> excludeInlineDefinitions