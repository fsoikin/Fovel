﻿#load "Scripts/load-references-debug.fsx"

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

      type T<'a> = T of int
      let inline f<'a> () : T<'a> = T 0
      let inline t<'a> = f<'a> ()
      let x = t<string>
      let y = t<bool>
      """

let srcs = [ "ints.fs", ints; "a.fs", src]


let e = 
  fsharpSourcesToShovel Config.WithoutCoreLib srcs 
  |> Result.mapError Error.formatAll

//let ee =
//  srcs
//  |> FSCompiler.parseProgram
//  >>= fsharpProgramToFovel Config.WithoutCoreLib.ParseIntrinsic
//  |*> Transformation.inlineFunctions