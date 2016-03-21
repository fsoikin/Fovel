module Fovel.Tests.Intrinsics
open Xunit
open Fovel

let [<Fact>] ``Basic`` () = 
  let parseIntrinsic (fn: FSharp.fn) = if fn.LogicalName = "singleton" then Some() else None
  let intrinsicCode () args = "foo " + (String.concat "," args) + " bar"
  let config = 
    { Config.WithoutCoreLib with
        ParseIntrinsic = parseIntrinsic
        GenerateIntrinsicCode = intrinsicCode }

  compileCompareWithConfig config ""
    """
      module X
      let x = Seq.singleton 5 
    """
    """
      var x = { var _0 = 5 foo _0 bar } """


let [<Fact>] ``Code inside intrinsic definition isn't validated`` () = 
  let parseIntrinsic (fn: FSharp.fn) = if fn.LogicalName = "the_intrinsic" then Some() else None
  let intrinsicCode () _ = "foo"
  let config = 
    { Config.WithoutCoreLib with
        ParseIntrinsic = parseIntrinsic
        GenerateIntrinsicCode = intrinsicCode }

  compileCompareWithConfig config ""
    """
      module X
      let the_intrinsic x y : int = failwith "boo!"
      let x = the_intrinsic 5 6
    """
    """
      var x = { var _0 = 5 var _1 = 6 foo } """

let [<Fact>] ``Used as values`` () = 
  let parseIntrinsic (fn: FSharp.fn) = if fn.LogicalName.StartsWith "intr" then Some() else None
  let intrinsicCode () args = "foo( " + (String.concat ", " args) + " )"
  let config = 
    { Config.WithoutCoreLib with
        ParseIntrinsic = parseIntrinsic
        GenerateIntrinsicCode = intrinsicCode }

  compileCompareWithConfig config ""
    (*
      Usually the F# compiler will create an in-place lambda expression where a function is used as a value.
      For example, this:
        let f x = x+5
        let g = f
      Will be parsed as "g = fun x -> f x"

      But in _some_ cases, it will generate a genuine SymRef (i.e. "g = f") instead.
      I'm not quite sure what these "some cases" are, but I found at least one, and it is below.
    *)
    """
      module X
      let inline ap f x y = f x y
      let intr1 (x: string) (y: int) : unit = failwith ""
      let intr2 (x: string, y: int) (z: bool) : unit = failwith ""
      ap intr1 "a" 0
      ap intr2 ("b", 5) true
    """
    """
      {
	      var f = {fn(_0, _1) foo( _0, _1 )}
	      var x = {'a'}
	      var y = {0}
	      {{f}(x)}(y)}
      {
	      var f = {fn(_0, _1) foo( _0[0], _0[1], _1 )}
	      var x = {array( 'b', 5 )}
	      var y = {true}
	      {{f}(x)}(y)} """
