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
  | Function of parameter: 'Symbol * body: E<'Type, 'Symbol, 'Intrinsic>
  | Call of func: E<'Type, 'Symbol, 'Intrinsic> * args: E<'Type, 'Symbol, 'Intrinsic> list
  | InfixOp of leftArg: E<'Type, 'Symbol, 'Intrinsic> * op: InfixOpKind * rightArg: E<'Type, 'Symbol, 'Intrinsic>
  | SymRef of 'Symbol
  | Const of obj * 'Type
  | Let of ('Symbol * E<'Type, 'Symbol, 'Intrinsic>) list * body: E<'Type, 'Symbol, 'Intrinsic>
  | Sequence of E<'Type, 'Symbol, 'Intrinsic> list
  | Conditional of test: E<'Type, 'Symbol, 'Intrinsic> * then': E<'Type, 'Symbol, 'Intrinsic> * else': E<'Type, 'Symbol, 'Intrinsic>

module Expr =
  let private mapLetBindings fSym fExpr = List.map (fun (sym, expr) -> fSym sym, fExpr expr)

  let rec mapType f e = 
    let r = mapType f
    let rl = List.map r
    match e with
    | E.Intrinsic (i, args) -> E.Intrinsic(i, rl args)
    | E.NewTuple (t, items) -> E.NewTuple( f t, rl items )
    | E.TupleGet (t, ix, tup) -> E.TupleGet( f t, ix, r tup )
    | E.UnionCase (t, case, fields) -> E.UnionCase( f t, case, rl fields )
    | E.UnionCaseTest (u, t, case) -> E.UnionCaseTest( r u, f t, case )
    | E.UnionCaseGet (u, t, case, field) -> E.UnionCaseGet( r u, f t, case, field )
    | E.NewRecord (t, fields) -> E.NewRecord( f t, rl fields )
    | E.RecordFieldGet (typ, record, field) -> E.RecordFieldGet (f typ, r record, field)
    | E.NewArray (typ, els) -> E.NewArray (f typ, rl els)
    | E.ArrayElement (arr, idx) -> E.ArrayElement (r arr, r idx)
    | E.Function (p, body) -> E.Function( p, r body )
    | E.Call (fn, args) -> E.Call( r fn, rl args )
    | E.InfixOp (a, op, b) -> E.InfixOp( r a, op, r b )
    | E.SymRef s -> E.SymRef s
    | E.Const (o, t) -> E.Const( o, f t )
    | E.Let (bindings, body) -> E.Let( bindings |> mapLetBindings id r, r body )
    | E.Sequence es -> E.Sequence (rl es)
    | E.Conditional (test, thn, els) -> E.Conditional( r test, r thn, r els )

  let rec mapSymbol f e = 
    let r = mapSymbol f
    let rl = List.map r
    match e with
    | E.Intrinsic (i, args) -> E.Intrinsic(i, rl args)
    | E.NewTuple (t, items) -> E.NewTuple( t, rl items )
    | E.TupleGet (t, ix, tup) -> E.TupleGet( t, ix, r tup )
    | E.UnionCase (t, case, fields) -> E.UnionCase( t, case, rl fields )
    | E.UnionCaseTest (u, t, case) -> E.UnionCaseTest( r u, t, case )
    | E.UnionCaseGet (u, t, case, field) -> E.UnionCaseGet( r u, t, case, field )
    | E.NewRecord (t, fields) -> E.NewRecord( t, rl fields )
    | E.RecordFieldGet (typ, record, field) -> E.RecordFieldGet (typ, r record, field)
    | E.NewArray (typ, els) -> E.NewArray (typ, rl els)
    | E.ArrayElement (arr, idx) -> E.ArrayElement (r arr, r idx)
    | E.Function (p, body) -> E.Function( f p, r body )
    | E.Call (fn, args) -> E.Call( r fn, rl args )
    | E.InfixOp (a, op, b) -> E.InfixOp( r a, op, r b )
    | E.SymRef s -> E.SymRef (f s)
    | E.Const (o, t) -> E.Const( o, t )
    | E.Let (bindings, body) -> E.Let( bindings |> mapLetBindings f r, r body )
    | E.Sequence es -> E.Sequence (rl es)
    | E.Conditional (test, thn, els) -> E.Conditional( r test, r thn, r els )

  let rec mapIntrinsic f e = 
    let r = mapIntrinsic f
    let rl = List.map r
    match e with
    | E.Intrinsic (i, args) -> E.Intrinsic(f i, rl args)
    | E.NewTuple (t, items) -> E.NewTuple( t, rl items )
    | E.TupleGet (t, ix, tup) -> E.TupleGet( t, ix, r tup )
    | E.UnionCase (t, case, fields) -> E.UnionCase( t, case, rl fields )
    | E.UnionCaseTest (u, t, case) -> E.UnionCaseTest( r u, t, case )
    | E.UnionCaseGet (u, t, case, field) -> E.UnionCaseGet( r u, t, case, field )
    | E.NewRecord (t, fields) -> E.NewRecord( t, rl fields )
    | E.RecordFieldGet (typ, record, field) -> E.RecordFieldGet (typ, r record, field)
    | E.NewArray (typ, els) -> E.NewArray (typ, rl els)
    | E.ArrayElement (arr, idx) -> E.ArrayElement (r arr, r idx)
    | E.Function (p, body) -> E.Function( p, r body )
    | E.Call (fn, args) -> E.Call( r fn, rl args )
    | E.InfixOp (a, op, b) -> E.InfixOp( r a, op, r b )
    | E.SymRef s -> E.SymRef s
    | E.Const (o, t) -> E.Const( o, t )
    | E.Let (bindings, body) -> E.Let( bindings |> mapLetBindings id r, r body )
    | E.Sequence es -> E.Sequence (rl es)
    | E.Conditional (test, thn, els) -> E.Conditional( r test, r thn, r els )

  let rec allTypes expr = 
    let r = allTypes
    let rl = List.collect r
    match expr with
    | E.Intrinsic (_, es) | E.Sequence es -> rl es
    | E.NewTuple (t, items) | E.UnionCase (t, _, items) | E.NewRecord (t, items) | E.NewArray (t, items) ->  t :: (rl items)
    | E.UnionCaseTest (e, t, _) | E.UnionCaseGet (e, t, _, _) | E.TupleGet (t, _, e) | E.RecordFieldGet (t, e, _) -> t :: (r e)
    | E.ArrayElement (arr, idx) -> rl [arr; idx]
    | E.Function (_, body) -> r body
    | E.Call (fn, args) -> (r fn) @ (rl args)
    | E.InfixOp (e1, _, e2) -> (r e1) @ (r e2)
    | E.Let (bindings, body) -> body :: (bindings |> List.map snd) |> rl
    | E.SymRef _ -> []
    | E.Const (_, t) -> [t]
    | E.Conditional (test, thn, els) -> rl [test; thn; els]

  let rec allSymbols expr = 
    let r = allSymbols
    let rl = List.collect r
    match expr with
    | E.Intrinsic (_, es) | E.Sequence es -> rl es
    | E.NewTuple (_, items) | E.UnionCase (_, _, items) | E.NewRecord (_, items) | E.NewArray (_, items) ->  rl items
    | E.UnionCaseTest (e, _, _) | E.UnionCaseGet (e, _, _, _) | E.TupleGet (_, _, e ) | E.RecordFieldGet (_, e, _) -> r e
    | E.Function (s, e) -> s :: (r e)
    | E.ArrayElement (arr,idx) -> rl [arr; idx]
    | E.Call (e1, e2) -> (r e1) @ (rl e2)
    | E.InfixOp (e1, _, e2)  -> (r e1) @ (r e2)
    | E.Let (bindings, body) -> (bindings |> List.map fst) @ ( body :: (bindings |> List.map snd) |> rl )
    | E.SymRef sym -> [sym]
    | E.Const _ -> []
    | E.Conditional (test, thn, els) -> rl [test; thn; els]