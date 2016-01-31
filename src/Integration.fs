[<AutoOpen>]
module Fovel.Integration
open Fovel
open Fovel.Gen

let fsharpProgramToFovel parseIntrinsic program =
  program
  |> Binding.programToFovel (Expr.exprToFovel parseIntrinsic) 
  |*> Binding.excludeIntrinsicDefinitions parseIntrinsic

let eraseFSharpEntities program =
  program |> Symbol.genSymbols |> Type.genTypes |*> CodeGen.assignTypeNames

let generateShovelCode generateIntrinsicCode = CodeGen.programCode (CodeGen.exprCode generateIntrinsicCode)

let fsharpSourcesToShovel parseIntrinsic generateIntrinsicCode sources =
  FSCompiler.parseProgram sources
  >>= fsharpProgramToFovel parseIntrinsic
  >>= eraseFSharpEntities
  ||*> generateShovelCode generateIntrinsicCode