module Fovel.Tests.CodeGenEndToEnd
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

let patternInputRegex = System.Text.RegularExpressions.Regex("""(?'p'patternInput_\d+)_\d+""")
let replacePatternInputs s = patternInputRegex.Replace(s, System.Text.RegularExpressions.MatchEvaluator( fun m -> m.Groups.["p"].Value ) )

let split lines = (lines:string).Split('\n') |> Seq.map (fun s -> s.Trim()) |> Seq.filter ((<>) "") |> Seq.map replacePatternInputs |> String.concat "\n"

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
      var x__1 = {{{f}}(1)}(2)
      var y__1 = { var x__2 = {5}
                   fn(y__2) {x__2} + {y__2} }
      var z = {{y__1}}(6) """

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
      var h = {{g}}(7) """

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

  
let [<Fact>] ``Single-case single-datum unions are erased`` () = 
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
      var patternInput_7_1 = x
      var z = patternInput_7_1
      var patternInput_8_2 = y
      var w = patternInput_8_2
      var p = {z} + {w} """

let [<Fact>] ``Single-case unions with multiple data are not erased`` () = 
  compileCompare
    """
      module X
      type U = U of int*string

      let x = U (0, "abc")
      let y = U (1, "xyz")
      let (U (a,b)) = x
      let (U (c,d)) = y
      let p = a + c """
    """
      var __t_U = make( defstruct( array( 'U' ) ),
        defstruct( array( 'Item1', 'Item2' ) )
      )

      var x = make( __t_U.U, 0, 'abc' )
      var y = make( __t_U.U, 1, 'xyz' )
      var patternInput_7 = x
      var b = {patternInput_7}.Item2
      var a = {patternInput_7}.Item1
      var patternInput_8_1 = y
      var d = {patternInput_8_1}.Item2
      var c = {patternInput_8_1}.Item1
      var p = {a} + {c}
      """

      
let [<Fact>] ``Single-case unions without data are not erased`` () = 
  compileCompare
    """
      module X
      type U = U

      let x = U
      let y = U
      let p = x = y """
    """
      var __t_U = make( defstruct( array( 'U' ) ),
        defstruct( array(  ) )
      )

      var x = make( __t_U.U )
      var y = make( __t_U.U )
      var p = {x} == {y}
      """
  
let [<Fact>] ``Unions`` () = 
  compileCompare
    """
      module X
      type U = U of int | W of ss: string | Z of a: int * b: bool

      let x = U 0
      let y = W "1"
      let z = Z (5, true)
      
      let a = match x with | U i -> i | W j -> 5 | Z (k,b) -> k """
    """
      var __t_U = make( defstruct( array( 'U','W','Z' ) ),
        defstruct( array( 'Item' ) ),
        defstruct( array( 'ss' ) ),
        defstruct( array( 'a', 'b' ) )
      )

      var x = make( __t_U.U, 0 )
      var y = make( __t_U.W, '1' )
      var z = make( __t_U.Z, 5, true )
      var a = if {isStructInstance( x, __t_U.W )} {{
        { var j = {{x}.ss}
          5 } }} else {if {isStructInstance( x, __t_U.Z )} {{
            { var k = {{x}.a}
            { var b = {{x}.b}
              k } } }} else {{
                { var i = {{x}.Item}
                  i } }}} """

let [<Fact>] ``Records`` () = 
  compileCompare
    """
      module X
      type U = X of int | Y
      type R = { A: string; B: int; C: U }

      let r1 = { A = "abc"; B = 5; C = Y }
      let r2 = { r1 with C = X 5 }
      
      let ({ A = a; B = i }) = r2
      let m = match r1 with | { C = Y } -> 1 | { C = X x } -> x """
    """
      var __t_R = defstruct( array( 'A', 'B', 'C' ) )
      var __t_U = make( defstruct( array( 'X','Y' ) ),
        defstruct( array( 'Item' ) ),
        defstruct( array(  ) )
      )

      var r1 = make( __t_R, 'abc', 5, make( __t_U.Y ) )
      var r2 = { var C = {make( __t_U.X, 5 )}
                 make( __t_R, {r1}.A, {r1}.B, C ) }

      var patternInput_9 = r2
      var i = {patternInput_9}.B
      var a = {patternInput_9}.A

      var m = if {isStructInstance( {r1}.C, __t_U.X )} {{
        { var x = {{{r1}.C}.Item}
          x } }} else {{
                        1 }} """

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

      var a = { var unitVar0__2 = {null}
                {get_X}(null) }

      var b = { var x__1 = {5}
                {Y}(x__1) }

      var c = { var x__2 = {'123'}
                {Z}(x__2) } 
                
      var p = { var unitVar0__2 = {null}
                {get_X__1}(null) }

      var q = { var x__1 = {10}
                {Y__1}(x__1) }"""

let [<Fact>] ``Statically resolved type constraints - parameterless`` () = 
  compileCompare
    """
      module X

      type T() =
        static member X = "abc"

      type U() =
        static member X = "xyz"

      let inline getX< ^t when ^t: (static member X: string)> = (^t: (static member X: string)())
      
      let a = getX<T> 
      let b = getX<U> """
    """
      var get_X = fn(unitVar0) 'abc'
      var get_X__1 = fn(unitVar0__1) 'xyz'
    
      var a = {
        {get_X}(null) }

      var b = {
        {get_X__1}(null) } """


let [<Fact>] ``Generic values`` () = 
  compileCompare
    """
      module X

      type T<'a> = T of string | U of int

      let t<'a> () : T<'a> = T "abc"
      let u<'a> = t<'a>()
      
      let a = u<string>
      let b = u<int> """
    """
      var __t_T_1 = make( defstruct( array( 'T','U' ) ),
        defstruct( array( 'Item' ) ),
        defstruct( array( 'Item' ) )
      )

      var t = fn(unitVar0) make( __t_T_1.T, 'abc' )
      var u = {t}(null)
      var a = {u}
      var b = {u} """


let [<Fact>] ``Type alias`` () = 
  compileCompare
    """
      module X

      type A<'a> = X of int | Y of 'a
      type 'a B = A<'a>
      
      let f (b: B<_>) = match b with X i -> i | Y _ -> 0 """

    """ var __t_A_1 = make( defstruct( array( 'X','Y' ) ),
          defstruct( array( 'Item' ) ),
          defstruct( array( 'Item' ) )
        )

        var f = fn(b) if {isStructInstance( b, __t_A_1.Y )} {{
          0 }} else {{
            { var i = {{b}.Item}
              i } }}
      """

let [<Fact>] ``List`` () = 
  compileCompare
    """
      module X

      let f = function | x::xs -> x | _ -> 0 """

    """ var __t_List_1 = make( defstruct( array( 'op_Nil','op_ColonColon' ) ),
          defstruct( array(  ) ),
          defstruct( array( 'Head', 'Tail' ) )
        )

        var f = fn(_arg1) if {isStructInstance( _arg1, __t_List_1.op_ColonColon )} {{ var xs = {{_arg1}.Tail}
                                                                                    { var x = {{_arg1}.Head}
                                                                                      x } }} else {0}
      """

let [<Fact>] ``Multiline strings`` () = 
  compileCompare
    ("""
      module X

      let s = """ + "\"\"\"" + """
        some text
        multiline
        """ + "\"\"\"" + """
    """)
      """ var s = '
            some text
            multiline
            ' """