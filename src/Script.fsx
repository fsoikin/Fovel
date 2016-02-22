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
//      type U = U of int | W of string with static member X: string = "abc"
//
//      let inline f< ^t when ^t: (static member X: string)> () = (^t: (static member X: string)())
//      let y = f<U>
//      let z = y()
      let inline f x = [x]
      let inline ap f x = f x
      let z = ap f 10 
      """

let srcs = [ "ints.fs", ints; "a.fs", src]


let e = 
  fsharpSourcesToShovel Config.Default srcs 
  |> Result.mapError Error.formatAll

//let ee =
//  srcs 
//  |> FSCompiler.parseProgram
//  >>= fsharpProgramToFovel Config.WithoutCoreLib.ParseIntrinsic
//  
//let eee = ee |*> Transformation.inlineFunctions
