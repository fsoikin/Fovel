#load "Scripts/load-references-debug.fsx"
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

open Fovel
open Fovel.FSCompiler
open Fovel.Expr
open Fovel.Gen
open FSharpx.Collections
open Microsoft.FSharp.Compiler.SourceCodeServices

let ints = """
  module Intrinsics

    let udp (name: string) a  = failwith "x"
    let udp2 (name: string) a b = failwith "x"
    let udp3 (name: string) a b c = failwith "x"
  """

let src = """
module Whatevs.Some

let x = 5
let f x = x+2
let z = Intrinsics.udp3 "some" x (f 5) "err"
let g = (fun s -> s-2) 7
"""

let srcs = ["ints.fs", ints; "a.fs", src]

let parseIntrinsic = 
  Expr.Intrinsics.ofSeq [
    "Intrinsics.udp", 1
    "Intrinsics.udp2", 2
    "Intrinsics.udp3", 3 ]

let intrinsicCode i args = 
  match args with 
  | name::rest when (name:string).StartsWith("'") && name.EndsWith("'") -> sprintf "@%s( %s )" (name.Substring(1, name.Length-2)) (rest |> String.concat ", ")
  | _ -> "@fail"

let e = parseProgram srcs |> Binding.programToFovel (Expr.exprToFovel parseIntrinsic)
let e1, typs = e |> Symbol.genSymbols |> Type.genTypes |> CodeGen.assignTypeNames
let ec = CodeGen.programCode (CodeGen.exprCode intrinsicCode) e1 typs

//List.collect Binding.allTypes e |> Seq.distinct |> Seq.toList |> List.map (fun t -> t.ToString())