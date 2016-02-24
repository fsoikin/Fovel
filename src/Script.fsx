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
#load "CoreLib.FSharp.fs"
#load "CoreLib.fs"
#load "Transformation.fs"
#load "Validation.fs"
#load "Integration.fs"

open Fovel
open Fovel.Gen
open Microsoft.FSharp.Compiler.SourceCodeServices

let ints = """
  module XX
  //let a = [1..3]
  """

let src = """
      module X
      type U = U of int with static member X = "abc"

      let inline f< ^t when ^t: (static member X: string)> () = (^t: (static member X: string)())
      let y = f<U>()
      """

let srcs = [ "ints.fs", ints; "a.fs", src]


let e = 
  fsharpSourcesToShovel Config.Default srcs 
  |> Result.mapError Error.formatAll

let ee =
  srcs 
  |> FSCompiler.parseProgram
  >>= fsharpProgramToFovel Config.WithoutCoreLib.ParseIntrinsic

let (OK ({Fn=Some(fn,_)}::_)) = ee
let (OK ([_;_;{Expr=E.Call(_,[t],_)}])) = ee

t.TypeDefinition.MembersFunctionsAndValues.[0].IsEffectivelySameAs fn
  
let eee = ee |*> Transformation.inlineFunctions
