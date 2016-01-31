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

let ints = """
  module Intrinsics

    let udp (name: string) a  = failwith "x"
    let udp2 (name: string) a b = failwith "x"
    let udp3 (name: string) a b c = failwith "x"
  """

let src = """
    module M
      type A() =
        member x.f() = ()
"""

let srcs = [
  //"ints.fs", ints
  "a.fs", src]

let parseIntrinsic = 
  Expr.Intrinsics.ofSeq [
    "Intrinsics.udp", 1
    "Intrinsics.udp2", 2
    "Intrinsics.udp3", 3 ]

let intrinsicCode i args = 
  match args with 
  | name::rest when (name:string).StartsWith("'") && name.EndsWith("'") -> sprintf "@%s( %s )" (name.Substring(1, name.Length-2)) (rest |> String.concat ", ")
  | _ -> "@fail"

let e = 
  fsharpSourcesToShovel parseIntrinsic intrinsicCode srcs 
  |> Result.mapError Error.formatAll