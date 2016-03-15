module Fovel.Tests.ExprGen
open Xunit
open Fovel
open Swensen.Unquote

let flattenExpr e = 
  e
  |> Expr.mapSymbol (fun (f: FSharp.fn) -> f.LogicalName)
  |> Expr.mapType FSharp.typeName

let parseExprs code =
  ["file.fs", code]
  |> FSCompiler.parseProgram
  >>= fsharpProgramToFovel (fun _ -> None)
  |*> List.map Binding.expr
  |*> List.map flattenExpr

let compareExprs code expectedAst =
  parseExprs code =! OK expectedAst


let [<Fact>] ``Box`` () = 
  compareExprs
    """
      module M
      box 5 """

    [ Call
        ( SymRef "box", ["Microsoft.FSharp.Core.int"],
          [Const (5,"Microsoft.FSharp.Core.int")] )
    ]


let [<Fact>] ``Null`` () = 
  compareExprs
    """
      module M
      "abc" = null """

    [ InfixOp
        ( Const ("abc","Microsoft.FSharp.Core.string"),
          Equal,
          Const (null,"Microsoft.FSharp.Core.string") )
    ]


let [<Fact>] ``Sequence`` () = 
  compareExprs
    """
      module M
      let f x = ()
      let x = f "abc"; 5 """

    [ Const (null,"Microsoft.FSharp.Core.unit")
      Sequence
        [ Call (SymRef "f", ["Microsoft.FSharp.Core.string"], [Const ("abc","Microsoft.FSharp.Core.string")])
          Const (5,"Microsoft.FSharp.Core.int") ] ]

