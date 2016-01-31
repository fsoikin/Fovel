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
#load "Integration.fs"

open Fovel
open Fovel.Gen
open Microsoft.FSharp.Compiler.SourceCodeServices

let ints = """
  [<AutoOpen>]
  module Intrinsics

  type IntrinsicAttribute() = 
    inherit System.Attribute()
  """

let src = """
      module X
      type C() =
        static member f() = 5
        static member f(s: string) = 6
        static member f(x: 'a) = "whatever"

      let x = C.f()
      let y = C.f "abc"
      let z = C.f 5 """

let srcs = [ "ints.fs", ints; "a.fs", src]
let parseIntrinsic (i: FSharpMemberOrFunctionOrValue) = 
  if i.Attributes |> Seq.exists (fun a -> a.AttributeType.TryFullName = Some "Intrinsics.IntrinsicAttribute") then Some 1
  else None

let intrinsicCode i args = string i

//FSCompiler.parseProgram srcs >>= fsharpProgramToFovel parseIntrinsic >>= eraseFSharpEntities

let e = 
  fsharpSourcesToShovel parseIntrinsic intrinsicCode srcs 
  |> Result.mapError Error.formatAll