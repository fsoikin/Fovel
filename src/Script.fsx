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

      type ObjectHandle = private ObjectHandle of Value: string

      module AboutObject =
        let inline typeOf< ^obj when ^obj: (static member AboutType: string)> = (^obj: (static member AboutType: string) ())

      type Candidate = Candidate of handle: ObjectHandle with static member Handle (Candidate h) = h; static member AboutType = "Candidate"

      let t = AboutObject.typeOf<Candidate>
      """

let srcs = [ "ints.fs", ints; "a.fs", src]


let e = 
  fsharpSourcesToShovel Config.Default srcs 
  |> Result.mapError Error.formatAll

let fnDefinition = function
  | { Binding.Fn = Some (fn,parms); Expr = expr } when parms <> [] -> Some (fn, parms |> List.map fst, expr)
  | _ -> None

let fstt (x,_,_) = x

let ee =
  srcs
  |> FSCompiler.parseProgram
  >>= fsharpProgramToFovel Config.WithoutCoreLib.ParseIntrinsic
  |*> (List.choose fnDefinition)// >> List.filter (fstt >> FSharp.isInline))

  |*> Transformation.inlineFunctions

let (OK t) = ee |*> List.item 3 |*> (fun {Expr=Call (SymRef fn,_,_)} -> fn)

FSharp.isInline t

let (OK (_::_::_::_::_::_::_::_::{Expr=E.UnionCase (t,_,_)}::_)) = ee

t.TypeDefinition.IsFSharpUnion
  
let eee = ee |*> Transformation.inlineFunctions
