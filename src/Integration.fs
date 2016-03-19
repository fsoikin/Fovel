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
  }

module Config =
  let withCoreLib config = {
    ParseIntrinsic = CoreLib.parseIntrinsic config.ParseIntrinsic
    GenerateIntrinsicCode = CoreLib.intrinsicCode config.GenerateIntrinsicCode
    ReplaceFunctions = fun all -> (CoreLib.replaceSymbols all) >> (config.ReplaceFunctions all)
    FSharpPrelude = Some (CoreLib.prelude() + (config.FSharpPrelude |> Option.orElse "")) }

  let WithoutCoreLib = { 
    ParseIntrinsic = fun _ -> None
    GenerateIntrinsicCode = fun () _ -> ""
    ReplaceFunctions = fun _ -> id
    FSharpPrelude = None }

  let Default = WithoutCoreLib |> withCoreLib


let fsharpProgramToFovel parseIntrinsic program =
  program
  |> Binding.programToFovel (Expr.exprToFovel parseIntrinsic) 
  |*> Binding.excludeIntrinsicDefinitions parseIntrinsic

let eraseFSharpEntities program =
  program |> Symbol.genSymbols |> Type.genTypes

let replaceFunctions replaceCall program = 
  let functionOfBinding = function { Binding.Fn = Some(fn,_) } -> Some fn | _ -> None
  let allFunctions = program |> List.choose functionOfBinding |> List.toSeq
  let replace fn = if FSharp.isFunction fn then replaceCall allFunctions fn else fn
  program |> List.map (Binding.mapExpr <| Expr.mapSymbol replace)

let prependPrelude config sources = 
  let prelude =
    config.FSharpPrelude 
    |> Option.map (fun code -> ["_prelude.fs", code]) 
    |> Option.orElse []

  prelude @ sources


let fsharpSourcesToShovel config sources =
  sources
  |> prependPrelude config
  |> FSCompiler.parseProgram
  >>= fsharpProgramToFovel config.ParseIntrinsic
  |*> replaceFunctions config.ReplaceFunctions
  |*> Transformation.applyAll
  >>= Validation.applyAll
  |*> eraseFSharpEntities
  |*> Optimization.applyAll
  |*> CodeGen.programCode (CodeGen.exprCode config.GenerateIntrinsicCode)
  |*> PrettyPrint.print
  |*> String.concat ""