namespace Fovel.Tests
open Xunit
open FsCheck
open Fovel
open FsUnit.Xunit
open FSharpx.State

[<AutoOpen>]
module Common =
  let mapIsDual mapTo mapFrom input = 
    (input |> mapTo string |> mapFrom int) = input

module Expr =
  open Expr
  let [<Fact>] ``mapType should be its own dual`` () = Check.Quick (mapIsDual mapType mapType)
  let [<Fact>] ``mapSymbol should be its own dual`` () = Check.Quick (mapIsDual mapSymbol mapSymbol)

  let additiveProp getAll exprs = 
    let a = exprs |> List.collect getAll |> List.distinct
    let b = E.NewTuple (0, exprs) |> getAll |> List.distinct
    a = b

  let [<Fact>] ``allTypes should be additive`` () = Check.Quick (additiveProp allTypes)
  let [<Fact>] ``allSymbols should be additive`` () = Check.Quick (additiveProp allSymbols)

module Binding =
  open Binding
  let [<Fact>] ``mapType should be its own dual`` () = Check.Quick (mapIsDual mapType mapType)
  let [<Fact>] ``mapSymbol should be its own dual`` () = Check.Quick (mapIsDual mapSymbol mapSymbol)
