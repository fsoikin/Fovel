#load "Scripts/load-references-debug.fsx"

module Resources =
  let CoreLib() = System.IO.File.ReadAllText """d:\work\Fovel\src\CoreLib.FSharp.fs"""

#load "Common.fs"
#load "Error.fs"
#load "Expr.fs"
#load "Binding.fs"
#load "ExprGen.fs"
#load "BindingGen.fs"
#load "Type.fs"
#load "TypeGen.fs"
#load "SymbolGen.fs"
#load "FSCompiler.fs"
#load "CodeGen.fs"
#load "PrettyPrint.fs"
#load "CoreLib.FSharp.fs"
#load "CoreLib.fs"
#load "Transformation.fs"
#load "Validation.fs"
#load "Optimization.fs"
#load "Integration.fs"

open Fovel
open Fovel.Gen
open Microsoft.FSharp.Compiler.SourceCodeServices


let src = """
      module X
      type T() =
        static member X = "abc"

      type U() =
        static member X = "xyz"

      let inline getX< ^t when ^t: (static member X: string)> = (^t: (static member X: string)())
      
      let a = getX<T> 
      let b = getX<U>
      """

let srcs = [ "a.fs", src]

let e = 
  fsharpSourcesToShovel Config.WithoutCoreLib srcs 
  |> Result.mapError Error.formatAll

//let ee =
//  srcs
//  |> FSCompiler.parseProgram
//  >>= fsharpProgramToFovel (fun _ -> None:unit option)
//  |*> Transformation.applyAll
//  >>= Validation.applyAll
//  |*> eraseFSharpEntities
//  |*> Optimization.applyAll