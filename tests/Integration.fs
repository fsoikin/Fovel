module Fovel.Tests.Integration
open Xunit
open Fovel
open Fovel.Gen
open FsUnit.Xunit

let compileSources srcs config = 
  match
    fsharpSourcesToShovel config srcs 
    |> Result.mapError Error.formatAll with 
  | OK r -> r
  | Error err -> failwith <| String.concat "\n" err

let compileSource src = compileSources ["file.fs", src]

let split lines = (lines:string).Split('\n') |> Seq.map (fun s -> s.Trim()) |> Seq.filter ((<>) "") |> String.concat "\n"

let compileCompare fsharpSource shovelSource = 
  (compileSource fsharpSource Config.WithoutCoreLib) |> split 
  |> should equal (split shovelSource)

let getErrors = function | OK _ -> [] | Error errs -> errs

let [<Fact>] ``Basic`` () = 
  compileCompare
    """
      module X
      let x = 5
      let f x = x+1
      let g a b = a b
      let y = f 5
      let c = g f 8 """
    """
      var x = 5
      var f = fn(x__1) {x__1} + {1}
      var g = fn(a, b) {a}(b)
      var y = {f}(5)
      var c = {g}(f, 8)"""

let [<Fact>] ``Operators as functions`` () = 
  compileCompare
    """
      module X
      let f = (+)
      let x = f 1 2
      let y = (+) 5
      let z = y 6 """
    """
      var f = fn(x) fn(y) {x} + {y}
      var x__1 = {{f}(1)}(2)
      var y__1 = { var x__2 = {5}
                   fn(y__2) {x__2} + {y__2} }
      var z = {y__1}(6) """

let [<Fact>] ``Conditional`` () = 
  compileCompare
    """
      module X
      let x = 5
      let y = 7
      let z = if x > y then x+y else y-x """
    """
      var x = 5
      var y = 7
      var z = if {{x} > {y}} {{x} + {y}} else {{y} - {x}} """

let [<Fact>] ``Complex functions`` () = 
  compileCompare
    """
      module X
      let f x y = 
        let z = x+6
        let y = y-6
        z * y
      let g = f 5
      let h = g 7 + f 5 8 """
    """
      var f = fn(x, y) { var z = {{x} + {6}}
                       { var y__1 = {{y} - {6}}
                         {z} * {y__1} } }
      var g = { var x__1 = {5}
                fn(y__2) {f}(x__1, y__2) }
      var h = {g}(7) """

let [<Fact>] ``Recursive functions`` () = 
  compileCompare
    """
      module X
      let rec f x y = 
        let z = x+6
        let y = y-6
        z * (g y)
      and g x = f x (x+1)
      let h = g 7 + f 5 8 """
    """
      var f = fn(x, y) { var z = {{x} + {6}}
                       { var y__1 = {{y} - {6}}
                         {z} * {{g}(y__1)} } }
      var g = fn(x__1) {f}(x__1, {x__1} + {1})
      var h = {{g}(7)} + {{f}(5, 8)} """

let [<Fact>] ``Static methods and overloads`` () = 
  compileCompare
    """
      module X
      type C() =
        static member f() = 5
        static member f(s: string) = 6
        static member f(x: 'a) = "whatever"

      let x = C.f()
      let y = C.f "abc"
      let z = C.f 5 """
    """
      var f = fn(unitVar0) 5
      var f__1 = fn(s) 6
      var f__2 = fn(x) 'whatever'
      var x__1 = {f}(null)
      var y = {f__1}('abc')
      var z = {f__2}(5) """

let [<Fact>] ``Instance methods are not supported`` () =
  let config = { 
    ParseIntrinsic = fun f -> if f.FullName.EndsWith("failwith") then Some() else None
    GenerateIntrinsicCode = fun _ _ -> "0"
    ReplaceFunctions = fun _ -> id
    FSharpPrelude = None
    ShovelPrelude = None }
  fsharpSourcesToShovel config
    ["a.fs","""
      module M
      type [<AllowNullLiteral>] A() =
        member x.f() = () 
        
      let a: A = failwith "boo"
      let x = a.f() """ ]
  |> Result.mapError Error.formatAll
  |> getErrors
  |> should equal [ "Instance methods are not supported: M.A.f"]

  
let [<Fact>] ``Single-case unions are erased`` () = 
  compileCompare
    """
      module X
      type U = U of int

      let x = U 0
      let y = U 1
      let (U z) = x
      let (U w) = y
      let p = z + w """
    """
      var x = 0
      var y = 1
      var patternInput_7 = x
      var z = patternInput_7
      var patternInput_8_1 = y
      var w = patternInput_8_1
      var p = {z} + {w} """

  
let [<Fact>] ``Unions`` () = 
  compileCompare
    """
      module X
      type U = U of int | W of ss: string | Z of a: int * b: bool

      let x = U 0
      let y = W "1"
      let z = Z (5, true) """
    """
      var __unioncase = defstruct( array( 'make', 'test' ) )
      var __t = make( defstruct( array( 'U' ) ), 
        make( defstruct( array( 'U','W','Z' ) ), 
          {
            var def = defstruct( array( 'Item' ) )
            makestruct( __unioncase, 
              fn (Item) make( def, Item ), 
              fn (x) isStructInstance( x, def ) ) },
          {
            var def = defstruct( array( 'ss' ) )
            makestruct( __unioncase, 
              fn (ss) make( def, ss ), 
              fn (x) isStructInstance( x, def ) ) },
          {
            var def = defstruct( array( 'a', 'b' ) )
            makestruct( __unioncase, 
              fn (a, b) make( def, a, b ), 
              fn (x) isStructInstance( x, def ) ) } ) )

      var x = __t.U.U.make( 0 )
      var y = __t.U.W.make( '1' )
      var z = __t.U.Z.make( 5, true ) """


let [<Fact>] ``Inlining`` () = 
  compileCompare
    """
      module X
      let inline f x = x+5
      let inline g x y = x+y
      let inline ap f x = f x
      let y = f 6 
      let z = ap f 10 
      let w = ap (g 5) 10 """
    """
      var y = { var x = {6}
                {x} + {5} }

      var z = { var f = {fn(x) {x} + {5}}
                var x__1 = {10}
                {f}(x__1) }

      var w = { var f = {{ var x__2 = {5}
                           fn(y__1) { var x__3 = {x__2}
                                      var y__2 = {y__1}
                                      {x__3} + {y__2} } }}
                var x__1 = {10}
                {f}(x__1) }"""


let [<Fact>] ``Statically resolved type constraints`` () = 
  compileCompare
    """
      module X

      type T() =
        static member X = "abc"
        static member Y i = i+5
        static member Z x = x

      type H() =
        static member X = "xyz"
        static member Y i = i-8

      let inline getX< ^t when ^t: (static member X: string)> () = (^t: (static member X: string)())
      let inline y< ^t when ^t: (static member Y: int -> int)> x = (^t: (static member Y: int -> int) x) 
      let inline z< ^a, ^t when ^t: (static member Z: ^a -> ^a)> x = (^t: (static member Z: ^a -> ^a) x)
      
      let a = getX<T>()
      let b = y<T> 5
      let c = z<string, T> "123" 
      
      let p = getX<H>()
      let q = y<H> 10 """

    """
      var get_X = fn(unitVar0) 'abc'
      var Y = fn(i) {i} + {5}
      var Z = fn(x) x

      var get_X__1 = fn(unitVar0__1) 'xyz'
      var Y__1 = fn(i__1) {i__1} - {8}

      var a = { var unitVar0__1 = {null}
                {get_X}() }

      var b = { var x__1 = {5}
                {Y}(x__1) }

      var c = { var x__2 = {'123'}
                {Z}(x__2) } 
                
      var p = { var unitVar0__2 = {null}
                {get_X__1}() }

      var q = { var x__1 = {10}
                {Y__1}(x__1) }"""
