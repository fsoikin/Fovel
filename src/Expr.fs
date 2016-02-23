namespace Fovel

type InfixOpKind = 
  | Plus | Minus | Mul | Div 
  | Greater | GreaterOrEqual | Less | LessOrEqual
  | Equal | NotEqual
  
type E<'Type, 'Symbol, 'Intrinsic> =
  | Intrinsic of fn: 'Intrinsic * args: E<'Type, 'Symbol, 'Intrinsic> list
  | NewTuple of tupleType: 'Type * items: E<'Type, 'Symbol, 'Intrinsic> list
  | TupleGet of tupleType: 'Type * index: int * tuple: E<'Type, 'Symbol, 'Intrinsic>
  | UnionCase of unionType: 'Type * case: Identifier * fields: E<'Type, 'Symbol, 'Intrinsic> list
  | UnionCaseTest of union: E<'Type, 'Symbol, 'Intrinsic> * unionType: 'Type * case: Identifier
  | UnionCaseGet of union: E<'Type, 'Symbol, 'Intrinsic> * unionType: 'Type * case: Identifier * field: Identifier
  | NewRecord of recordType: 'Type * fields: E<'Type, 'Symbol, 'Intrinsic> list
  | RecordFieldGet of recordType: 'Type * record: E<'Type, 'Symbol, 'Intrinsic> * field: Identifier
  | NewArray of elementType: 'Type * elements: E<'Type, 'Symbol, 'Intrinsic> list
  | ArrayElement of array: E<'Type, 'Symbol, 'Intrinsic> * idx: E<'Type, 'Symbol, 'Intrinsic>
  | Function of parameters: 'Symbol list * body: E<'Type, 'Symbol, 'Intrinsic>
  | Call of func: E<'Type, 'Symbol, 'Intrinsic> * typeArgs: 'Type list * args: E<'Type, 'Symbol, 'Intrinsic> list
  | TraitCall of types: 'Type list * membr: Identifier * args: E<'Type, 'Symbol, 'Intrinsic> list
  | InfixOp of leftArg: E<'Type, 'Symbol, 'Intrinsic> * op: InfixOpKind * rightArg: E<'Type, 'Symbol, 'Intrinsic>
  | SymRef of 'Symbol
  | Const of obj * 'Type
  | Let of ('Symbol * E<'Type, 'Symbol, 'Intrinsic>) list * body: E<'Type, 'Symbol, 'Intrinsic>
  | Sequence of E<'Type, 'Symbol, 'Intrinsic> list
  | Conditional of test: E<'Type, 'Symbol, 'Intrinsic> * then': E<'Type, 'Symbol, 'Intrinsic> * else': E<'Type, 'Symbol, 'Intrinsic>

module Expr =
  let private mapLetBindings fSym fExpr = List.map (fun (sym, expr) -> fSym sym, fExpr expr)

  let rec cata fExpr fSym fTyp fIntrinsic e = 
    let fl = List.map fExpr
    match e with
    | E.ArrayElement (arr, idx) -> E.ArrayElement (fExpr arr, fExpr idx)
    | E.Call (fn, typeArgs, args) -> E.Call( fExpr fn, List.map fTyp typeArgs, fl args )
    | E.TraitCall (typs, membr, args) -> E.TraitCall ( List.map fTyp typs, membr, List.map fExpr args )
    | E.InfixOp (a, op, b) -> E.InfixOp( fExpr a, op, fExpr b )
    | E.Sequence es -> E.Sequence (fl es)
    | E.Conditional (test, thn, els) -> E.Conditional( fExpr test, fExpr thn, fExpr els )
    | E.NewTuple (t, items) -> E.NewTuple( fTyp t, fl items )
    | E.TupleGet (t, ix, tup) -> E.TupleGet( fTyp t, ix, fExpr tup )
    | E.UnionCase (t, case, fields) -> E.UnionCase( fTyp t, case, fl fields )
    | E.UnionCaseTest (u, t, case) -> E.UnionCaseTest( fExpr u, fTyp t, case )
    | E.UnionCaseGet (u, t, case, field) -> E.UnionCaseGet( fExpr u, fTyp t, case, field )
    | E.NewRecord (t, fields) -> E.NewRecord( fTyp t, fl fields )
    | E.RecordFieldGet (typ, record, field) -> E.RecordFieldGet (fTyp typ, fExpr record, field)
    | E.NewArray (typ, els) -> E.NewArray (fTyp typ, fl els)
    | E.Intrinsic (i, args) -> E.Intrinsic(fIntrinsic i, fl args)
    | E.Const (o, t) -> E.Const( o, fTyp t )
    | E.Function (ps, body) -> E.Function( ps |> List.map fSym, fExpr body )
    | E.SymRef s -> E.SymRef (fSym s)
    | E.Let (bindings, body) -> E.Let( bindings |> mapLetBindings fSym fExpr, fExpr body )

  let rec mapType f e = cata (mapType f) id f id e
  let rec mapSymbol f e = cata (mapSymbol f) f id id e
  let rec mapIntrinsic f e = cata (mapIntrinsic f) id id f e

  let rec allTypes expr = 
    let r = allTypes
    let rl = List.collect r
    match expr with
    | E.Intrinsic (_, es) | E.Sequence es -> rl es
    | E.NewTuple (t, items) | E.UnionCase (t, _, items) | E.NewRecord (t, items) | E.NewArray (t, items) ->  t :: (rl items)
    | E.UnionCaseTest (e, t, _) | E.UnionCaseGet (e, t, _, _) | E.TupleGet (t, _, e) | E.RecordFieldGet (t, e, _) -> t :: (r e)
    | E.ArrayElement (arr, idx) -> rl [arr; idx]
    | E.Function (_, body) -> r body
    | E.Call (fn, typeArgs, args) -> (r fn) @ (rl args) @ typeArgs
    | E.TraitCall (typs, _, args) -> typs @ (rl args)
    | E.InfixOp (e1, _, e2) -> (r e1) @ (r e2)
    | E.Let (bindings, body) -> body :: (bindings |> List.map snd) |> rl
    | E.SymRef _ -> []
    | E.Const (_, t) -> [t]
    | E.Conditional (test, thn, els) -> rl [test; thn; els]

  let allSymbols expr = 
    let rec rl es = Seq.collect r es
    and r = function
    | E.Intrinsic (_, es) | E.Sequence es -> rl es
    | E.NewTuple (_, items) | E.UnionCase (_, _, items) | E.NewRecord (_, items) | E.NewArray (_, items) ->  rl items
    | E.UnionCaseTest (e, _, _) | E.UnionCaseGet (e, _, _, _) | E.TupleGet (_, _, e ) | E.RecordFieldGet (_, e, _) -> r e
    | E.Function (ss, e) -> Seq.append ss (r e)
    | E.ArrayElement (arr,idx) -> rl [arr; idx]
    | E.Call (e1, _, e2) -> Seq.append (r e1) (rl e2)
    | E.TraitCall (_, _, args) -> rl args
    | E.InfixOp (e1, _, e2)  -> Seq.append (r e1) (r e2)
    | E.Let (bindings, body) -> Seq.concat [ bindings |> Seq.map fst; r body; bindings |> Seq.collect (snd >> r) ]
    | E.SymRef sym -> Seq.singleton sym
    | E.Const _ -> Seq.empty
    | E.Conditional (test, thn, els) -> Seq.concat [r test; r thn; r els]

    r expr |> Seq.toList