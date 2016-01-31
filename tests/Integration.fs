module Fovel.Tests.Integration
open Xunit
open Fovel
open Fovel.Gen
open FsUnit.Xunit

let compileSources srcs parseIntrinsic intrinsicCode = 
  FSCompiler.parseProgram srcs 
  |> Binding.programToFovel (Expr.exprToFovel parseIntrinsic) 
  |> Binding.excludeIntrinsicDefinitions parseIntrinsic
  |> Symbol.genSymbols |> Type.genTypes |> CodeGen.assignTypeNames
  ||> CodeGen.programCode (CodeGen.exprCode intrinsicCode)

let compileSource src = compileSources ["file.fs", src]

let noIntrinsics _ = None
let emptyIntrCode _ _ = ""

let split lines = (lines:string).Split('\n') |> Seq.map (fun s -> s.Trim()) |> Seq.filter ((<>) "") |> Seq.toList

let compileCompare fsharpSource shovelSource = 
  (compileSource fsharpSource noIntrinsics emptyIntrCode) |> split 
  |> should equal (split shovelSource)

let [<Fact>] ``Basic`` () = 
  compileCompare
    """
      let x = 5
      let f x = x+1
      let g a b = a b
      let y = f 5
      let c = g f 8 """
    """
      var x = 5
      var f = fn(x__1) (x__1) + (1)
      var g = fn(a, b) (a)(b)
      var y = (f)(5)
      var c = (g)(f, 8)"""

let [<Fact>] ``Operators as functions`` () = 
  compileCompare
    """
      let f = (+)
      let x = f 1 2
      let y = (+) 5
      let z = y 6 """
    """
      var f = fn(x) fn(y) (x) + (y)
      var x__1 = ((f)(1))(2)
      var y__1 = { var x__2 = (5) fn(y__2) (x__2) + (y__2) }
      var z = (y__1)(6)"""

let [<Fact>] ``Conditional`` () = 
  compileCompare
    """
      let x = 5
      let y = 7
      let z = if x > y then x+y else y-x """
    """
      var x = 5
      var y = 7
      var z = if ((x) > (y)) ((x) + (y)) else ((y) - (x)) """

let [<Fact>] ``Complex functions`` () = 
  compileCompare
    """
      let f x y = 
        let z = x+6
        let y = y-6
        z * y
      let g = f 5
      let h = g 7 + f 5 8 """
    """
      var f = fn(x, y) { var z = ((x) + (6)) { var y__1 = ((y) - (6)) (z) * (y__1) } }
      var g = { var x__1 = (5) fn(y__2) (f)(x__1, y__2) }
      var h = (g)(7) """

let [<Fact>] ``Recursive functions`` () = 
  compileCompare
    """
      let rec f x y = 
        let z = x+6
        let y = y-6
        z * (g y)
      and g x = f x (x+1)
      let h = g 7 + f 5 8 """
    """
      var f = fn(x, y) { var z = ((x) + (6)) { var y__1 = ((y) - (6)) (z) * ((g)(y__1)) } }
      var g = fn(x__1) (f)(x__1, (x__1) + (1))
      var h = ((g)(7)) + ((f)(5, 8)) """