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
      
      let f1 (x: int -> string -> unit) = x 5 "abc"
      let f2 (x: int * string -> string -> unit) = x (5, "abc") "xyz"
      let f3 (x: int * string -> string -> unit*int -> float -> unit) = x (5, "abc") "xyz" ((),9) 3.4
      let h (a,b) c (d,e) f = ()
      h (1,2) 3 (4,5) 6
      f3 h
      """

let srcs = [ "a.fs", src]

let e = 
  fsharpSourcesToShovel Config.WithoutCoreLib srcs 
  |> Result.mapError Error.formatAll

let (OK ( 
          ( Some (_, [[p]]), BasicPatterns.Application (BasicPatterns.Value f, a, b) ) 
          :: 
          ( Some (_, [[p1]]), BasicPatterns.Application (BasicPatterns.Value f1, a1, b1) ) 
          ::
          ( Some (_, [[p2]]), BasicPatterns.Application (BasicPatterns.Value f2, a2, b2) ) 
          ::
          _
    ) ) =
  srcs
  |> FSCompiler.parseProgram

p2.CurriedParameterGroups |> Seq.map (fun g -> g.Count)

let ee =
  srcs
  |> FSCompiler.parseProgram
//  >>= fsharpProgramToFovel (fun _ -> None:unit option)
//  |*> Transformation.applyAll
//  >>= Validation.applyAll
//  |*> eraseFSharpEntities
//  |*> Optimization.applyAll

let x = [1;2;3;4;5;6;7;8]
let y = [2;1;2;1]

Seq.scan (fun rem y -> List.take rem) 0 [1;2;3]