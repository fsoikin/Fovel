namespace Fovel

type Binding<'Type, 'Symbol, 'Intrinsic> = { 
  /// Function with optional list of parameters
  Fn: ('Symbol * ('Symbol * 'Type) list) option; 

  /// Body of the function or value
  Expr: E<'Type, 'Symbol, 'Intrinsic> }

type Program<'Type, 'Symbol, 'Intrinsic> = Binding<'Type, 'Symbol, 'Intrinsic> list

module Binding =

  let expr { Expr = e } = e

  let mapType f b = 
    let mapSym (sym, typ) = sym, f typ
    let mapLeftPart (sym, args) = sym, List.map mapSym args
    { Binding.Fn = b.Fn |> Option.map mapLeftPart
      Expr = b.Expr |> Expr.mapType f }

  let mapSymbol f b =
    let mapSym (sym, typ) = f sym, typ
    let mapLeftPart (sym, args) = f sym, List.map mapSym args
    { Binding.Fn = b.Fn |> Option.map mapLeftPart
      Expr = b.Expr |> Expr.mapSymbol f }

  let mapIntrinsic f b =
    { Binding.Fn = b.Fn
      Expr = b.Expr |> Expr.mapIntrinsic f }

  let allSymbols { Binding.Fn = fn; Expr = e } =
    let leftPart = 
      match fn with
      | Some (sym, args) -> sym :: (List.map fst args)
      | None -> []
    leftPart @ (Expr.allSymbols e)

  let allTypes b =
    let typesFromLeftPart = function 
      | Some (_, args) -> List.map snd args
      | None -> []

    (typesFromLeftPart b.Fn) @ (Expr.allTypes b.Expr)
