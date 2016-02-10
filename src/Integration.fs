[<AutoOpen>]
module Fovel.Integration
open Fovel
open Fovel.Gen
open Microsoft.FSharp.Compiler.SourceCodeServices

type Config<'Intrinsic> = 
  {
    ParseIntrinsic: FSharpMemberOrFunctionOrValue -> 'Intrinsic option
    GenerateIntrinsicCode: 'Intrinsic -> Code list -> Code
    ReplaceSymbols: (FSharpMemberOrFunctionOrValue -> FSharpMemberOrFunctionOrValue) option
  }

module Config =
  let withCoreLib config = {
    ParseIntrinsic = CoreLib.parseIntrinsic config.ParseIntrinsic
    GenerateIntrinsicCode = CoreLib.intrinsicCode config.GenerateIntrinsicCode
    ReplaceSymbols = config.ReplaceSymbols }

  let WithoutCoreLib = { 
    ParseIntrinsic = fun _ -> None
    GenerateIntrinsicCode = fun () _ -> ""
    ReplaceSymbols = None }

  let Default = WithoutCoreLib |> withCoreLib


let fsharpProgramToFovel parseIntrinsic program =
  program
  |> Binding.programToFovel (Expr.exprToFovel parseIntrinsic) 
  |*> Binding.excludeIntrinsicDefinitions parseIntrinsic

let eraseFSharpEntities program =
  program |> Symbol.genSymbols |> Type.genTypes |*> CodeGen.assignTypeNames

let generateShovelCode generateIntrinsicCode = CodeGen.programCode (CodeGen.exprCode generateIntrinsicCode)

let replaceSymbols replaceCall program =
  match replaceCall with
  | Some f -> program |> List.map (Binding.mapExpr (Expr.mapSymbol f))
  | None -> program

let fsharpSourcesToShovel config sources =
  FSCompiler.parseProgram sources
  >>= fsharpProgramToFovel config.ParseIntrinsic
  |*> replaceSymbols config.ReplaceSymbols
  >>= eraseFSharpEntities
  |*> generateShovelCode config.GenerateIntrinsicCode