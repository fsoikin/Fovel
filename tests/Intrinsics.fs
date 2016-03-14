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

  compileCompareWithConfig config
    """
      module X
      let x = Seq.singleton 5 
    """
    """
      var x = foo 5 bar """


let [<Fact>] ``Code inside intrinsic definition isn't validated`` () = 
  let parseIntrinsic (fn: FSharp.fn) = if fn.LogicalName = "the_intrinsic" then Some() else None
  let intrinsicCode () _ = "foo"
  let config = 
    { Config.WithoutCoreLib with
        ParseIntrinsic = parseIntrinsic
        GenerateIntrinsicCode = intrinsicCode }

  compileCompareWithConfig config
    """
      module X
      let the_intrinsic x y : int = failwith "boo!"
      let x = the_intrinsic 5 6
    """
    """
      var x = foo """