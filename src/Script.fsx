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

let src = """
type R = { X: int; Y: string }
let r = { X = 5; Y = "a" }
let x = { r with Y = "c" }

type U = A of int | B of string * R
let u = A 5
let x = B ( x.Y, x )
"""

//parseProgram src |> Seq.iter( fun (b, e) -> printfn "%A = %A" b (fst b.Value).IsMember )

let e = parseProgram src |> Binding.programToFovel
let e1, typs = e |> Symbol.genSymbols |> Type.genTypes |> CodeGen.assignTypeNames

let ec = CodeGen.programCode e1 typs

//List.collect Binding.allTypes e |> Seq.distinct |> Seq.toList |> List.map (fun t -> t.ToString())