namespace Fovel

type Binding<'Type, 'Symbol, 'Intrinsic> = { 
  (* This pair defines the function (or value) being bound plus list of arguments.
     The arguments come in groups of tuples, hence the doubly nested list type.
     For example, a binding like "let x y z = ..." will have Fn = (x, [[y];[z]]),
     but a binding like "let x (y,z) = ..." will have Fn = (x, [[y;z]]),
     and a binding like "let x (y,z) w (t,u) = ..." will have Fn = (x, [[y;z];[w];[t;u]]).
     F# does this in order to keep names of tupled arguments.
  *)
  Fn: ('Symbol * ('Symbol * 'Type) list list) option; 

  /// Body of the function or value
  Expr: E<'Type, 'Symbol, 'Intrinsic> }

type Program<'Type, 'Symbol, 'Intrinsic> = Binding<'Type, 'Symbol, 'Intrinsic> list

module Binding =

  let expr { Expr = e } = e

  let mapType f b = 
    let mapSym (sym, typ) = sym, f typ
    let mapLeftPart (sym, args) = sym, (args |> List.map (List.map mapSym))
    { Binding.Fn = b.Fn |> Option.map mapLeftPart
      Expr = b.Expr |> Expr.mapType f }

  let mapSymbol f b =
    let mapSym (sym, typ) = f sym, typ
    let mapLeftPart (sym, args) = f sym, (args |> List.map (List.map mapSym))
    { Binding.Fn = b.Fn |> Option.map mapLeftPart
      Expr = b.Expr |> Expr.mapSymbol f }

  let mapIntrinsic f b =
    { Binding.Fn = b.Fn
      Expr = b.Expr |> Expr.mapIntrinsic f }

  let allSymbols { Binding.Fn = fn; Expr = e } =
    let leftPart = 
      match fn with
      | Some (sym, args) -> sym :: (args |> List.collect (List.map fst))
      | None -> []
    leftPart @ (Expr.allSymbols e)

  let allTypes b =
    let typesFromLeftPart = function 
      | Some (_, args) -> List.collect (List.map snd) args
      | None -> []

    (typesFromLeftPart b.Fn) @ (Expr.allTypes b.Expr)
