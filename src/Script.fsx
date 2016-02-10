#load "Scripts/load-references-debug.fsx"
#load "Error.fs"
#load "Common.fs"
#load "Expr.fs"
#load "Binding.fs"
#load "ExprGen.fs"
#load "BindingGen.fs"
#load "Type.fs"
#load "TypeGen.fs"
#load "SymbolGen.fs"
#load "FSCompiler.fs"
#load "CodeGen.fs"
#load "CoreLib.fs"
#load "Integration.fs"

open Fovel
open Fovel.Gen
open Microsoft.FSharp.Compiler.SourceCodeServices

let ints = """
  module XX
  let a = 0
  """

let src = """
      module X

      let a = [|1 .. 3|]
      let x = 5+8
      let b = a.[x]
      let c = Array.length a
      """

let srcs = [ "ints.fs", ints; "a.fs", src]

let e = 
  fsharpSourcesToShovel Config.Default srcs 
  |> Result.mapError Error.formatAll
