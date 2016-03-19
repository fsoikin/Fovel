module Fovel.Optimization
open Fovel
open Fovel.Gen

let rec eraseSingleCaseUnions = function
  | E.UnionCase (SingleCaseUnion _, _, [field]) -> field
  | E.UnionCase (SingleCaseUnion _, _, _) -> failwith "Creating a single-case union with a number of fields not equal to one."

  | E.UnionCaseGet (union, SingleCaseUnion _, _, _) -> union
  | E.UnionCaseTest (_, SingleCaseUnion _, _) -> E.Const (true, NotImportant)

  | e -> e |> Expr.cata eraseSingleCaseUnions id id id

let rec collapseChainedLet = function
  | E.Let (bindings1, (E.Let (bindings2, body))) ->
    E.Let (bindings1 @ bindings2, body) |> collapseChainedLet

  | e -> e |> Expr.cata collapseChainedLet id id id

let rec collapseTrivialLet = function
  | E.Let (bindings, body) ->
    let trivials = bindings |> List.choose (fun (sym, e) -> match e with | E.SymRef s -> Some (sym, s) | _ -> None)
    let nonTrivials = bindings |> List.choose (fun (sym, e) -> match e with | E.SymRef _ -> None | _ -> Some (sym, collapseTrivialLet e))

    if trivials = [] then E.Let( nonTrivials, body |> Expr.cata collapseTrivialLet id id id )
    else
      let map = Map.ofSeq trivials
      let getSym s = printfn "%A" s; map.TryFind s |> Option.orElse s
      let body = body |> Expr.mapSymbol getSym |> collapseTrivialLet
      E.Let (nonTrivials, body)

  | e -> e |> Expr.cata collapseTrivialLet id id id

let aplyAllToExpr e = e |> eraseSingleCaseUnions |> collapseTrivialLet |> collapseChainedLet
let applyAllToBinding binding = Binding.mapExpr aplyAllToExpr binding

let applyAll (program: Program<_,_,_>) : Program<_,_,_> = program |> List.map applyAllToBinding