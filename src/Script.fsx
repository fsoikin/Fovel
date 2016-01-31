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

  type C() = 
    member x.f() = "a"

  let rec f x y = 
    let z = x+6
    let y = y-6
    z * (g y)
  and g x = f x (x+1)
  let h = g 7 + f 5 8
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
  parseProgram srcs 
  >>= Binding.programToFovel (Expr.exprToFovel parseIntrinsic) 
  |*> Binding.excludeIntrinsicDefinitions parseIntrinsic
  |*> Symbol.genSymbols >>= Type.genTypes |*> CodeGen.assignTypeNames
  ||*> CodeGen.programCode (CodeGen.exprCode intrinsicCode)
  |> Result.mapError Error.formatAll

let x: E<_,_,int option> = Call (Call (SymRef "f",[Const (1,"int#0")]),[Const (2,"int#0")])
CodeGen.exprCode intrinsicCode (Call (SymRef "f",[Const (1, "")]) )

//List.collect Binding.allTypes e |> Seq.distinct |> Seq.toList |> List.map (fun t -> t.ToString())