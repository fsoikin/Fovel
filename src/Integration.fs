[<AutoOpen>]
module Fovel.Integration
open Fovel
open Fovel.Gen
open Microsoft.FSharp.Compiler.SourceCodeServices

type Config<'Intrinsic> = 
  {
    ParseIntrinsic: FSharpMemberOrFunctionOrValue -> 'Intrinsic option
    GenerateIntrinsicCode: 'Intrinsic -> Code list -> Code
    ReplaceFunctions: FSharpMemberOrFunctionOrValue seq -> FSharpMemberOrFunctionOrValue -> FSharpMemberOrFunctionOrValue
    FSharpPrelude: Code option
    ShovelPrelude: Code option
  }

module Config =
  let withCoreLib config = {
    ParseIntrinsic = CoreLib.parseIntrinsic config.ParseIntrinsic
    GenerateIntrinsicCode = CoreLib.intrinsicCode config.GenerateIntrinsicCode
    ReplaceFunctions = fun all -> (CoreLib.replaceSymbols all) >> (config.ReplaceFunctions all)
    FSharpPrelude = Some (CoreLib.prelude() + (config.FSharpPrelude |> Option.orElse ""))
    ShovelPrelude = config.ShovelPrelude }

  let WithoutCoreLib = { 
    ParseIntrinsic = fun _ -> None
    GenerateIntrinsicCode = fun () _ -> ""
    ReplaceFunctions = fun _ -> id
    FSharpPrelude = None
    ShovelPrelude = None }

  let Default = WithoutCoreLib |> withCoreLib


let fsharpProgramToFovel parseIntrinsic program =
  program
  |> Binding.programToFovel (Expr.exprToFovel parseIntrinsic) 
  |*> Binding.excludeIntrinsicDefinitions parseIntrinsic

let eraseFSharpEntities program =
  program |> Symbol.genSymbols |> Type.genTypes |*> CodeGen.assignTypeNames

let generateShovelCode generateIntrinsicCode prelude = 
  let prependPrelude = match prelude with Some code -> (+) code | None -> id
  CodeGen.programCode (CodeGen.exprCode generateIntrinsicCode) >> prependPrelude

let replaceFunctions replaceCall program = 
  let functionOfBinding = function { Binding.Fn = Some(fn,_) } -> Some fn | _ -> None
  let allFunctions = program |> List.choose functionOfBinding |> List.toSeq
  let replace fn = if FSharp.isFunction fn then replaceCall allFunctions fn else fn
  program |> List.map (Binding.mapExpr <| Expr.mapSymbol replace)

let fsharpSourcesToShovel config sources =
  let prelude = 
    config.FSharpPrelude 
    |> Option.map (fun code -> ["_prelude.fs", code]) 
    |> Option.orElse []

  FSCompiler.parseProgram (prelude @ sources)
  >>= fsharpProgramToFovel config.ParseIntrinsic
  |*> replaceFunctions config.ReplaceFunctions
  >>= Validation.allValidations
  >>= eraseFSharpEntities
  |*> generateShovelCode config.GenerateIntrinsicCode config.ShovelPrelude