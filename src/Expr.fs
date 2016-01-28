namespace Fovel

type InfixOpKind = 
  | Plus | Minus | Mul | Div 
  | Greater | GreaterOrEqual | Less | LessOrEqual
  | Equal | NotEqual
  
type E<'Type, 'Symbol> =
  | NewTuple of tupleType: 'Type * items: E<'Type, 'Symbol> list
  | TupleGet of tupleType: 'Type * index: int * tuple: E<'Type, 'Symbol>
  | UnionCase of unionType: 'Type * case: Identifier * fields: E<'Type, 'Symbol> list
  | UnionCaseTest of union: E<'Type, 'Symbol> * unionType: 'Type * case: Identifier
  | UnionCaseGet of union: E<'Type, 'Symbol> * unionType: 'Type * case: Identifier * field: Identifier
  | NewRecord of recordType: 'Type * fields: E<'Type, 'Symbol> list
  | RecordFieldGet of recordType: 'Type * record: E<'Type, 'Symbol> * field: Identifier
  | Function of parameter: 'Symbol * body: E<'Type, 'Symbol>
  | Call of func: E<'Type, 'Symbol> * args: E<'Type, 'Symbol> list
  | InfixOp of leftArg: E<'Type, 'Symbol> * op: InfixOpKind * rightArg: E<'Type, 'Symbol>
  | SymRef of 'Symbol
  | Const of obj * 'Type
  | Let of var: 'Symbol * varValue: E<'Type, 'Symbol> * body: E<'Type, 'Symbol>
  | Conditional of test: E<'Type, 'Symbol> * then': E<'Type, 'Symbol> * else': E<'Type, 'Symbol>
  | Unsupported of error: string

module Expr =
  let rec mapType f e = 
    let r = mapType f
    let rl = List.map r
    match e with
    | NewTuple (t, items) -> NewTuple( f t, rl items )
    | TupleGet (t, ix, tup) -> TupleGet( f t, ix, r tup )
    | UnionCase (t, case, fields) -> UnionCase( f t, case, rl fields )
    | UnionCaseTest (u, t, case) -> UnionCaseTest( r u, f t, case )
    | UnionCaseGet (u, t, case, field) -> UnionCaseGet( r u, f t, case, field )
    | NewRecord (t, fields) -> NewRecord( f t, rl fields )
    | RecordFieldGet (typ, record, field) -> RecordFieldGet (f typ, r record, field)
    | Function (p, body) -> Function( p, r body )
    | Call (fn, args) -> Call( r fn, rl args )
    | InfixOp (a, op, b) -> InfixOp( r a, op, r b )
    | SymRef s -> SymRef s
    | Const (o, t) -> Const( o, f t )
    | Let (var, value, body) -> Let( var, r value, r body )
    | Conditional (test, thn, els) -> Conditional( r test, r thn, r els )
    | Unsupported err -> Unsupported err

  let rec mapSymbol f e = 
    let r = mapSymbol f
    let rl = List.map r
    match e with
    | NewTuple (t, items) -> NewTuple( t, rl items )
    | TupleGet (t, ix, tup) -> TupleGet( t, ix, r tup )
    | UnionCase (t, case, fields) -> UnionCase( t, case, rl fields )
    | UnionCaseTest (u, t, case) -> UnionCaseTest( r u, t, case )
    | UnionCaseGet (u, t, case, field) -> UnionCaseGet( r u, t, case, field )
    | NewRecord (t, fields) -> NewRecord( t, rl fields )
    | RecordFieldGet (typ, record, field) -> RecordFieldGet (typ, r record, field)
    | Function (p, body) -> Function( f p, r body )
    | Call (fn, args) -> Call( r fn, rl args )
    | InfixOp (a, op, b) -> InfixOp( r a, op, r b )
    | SymRef s -> SymRef (f s)
    | Const (o, t) -> Const( o, t )
    | Let (var, value, body) -> Let( f var, r value, r body )
    | Conditional (test, thn, els) -> Conditional( r test, r thn, r els )
    | Unsupported err -> Unsupported err

  let rec isUnsupportedExpr = function
    | E.Call (fn, args) -> isUnsupportedExpr fn || Seq.exists isUnsupportedExpr args
    | E.Conditional (tst, thn, els) -> Seq.exists isUnsupportedExpr [tst;thn;els]
    | E.Function (_, body) -> isUnsupportedExpr body
    | E.InfixOp (a, _, b) -> isUnsupportedExpr a || isUnsupportedExpr b
    | E.Let (_, letExpr, inExpr) -> isUnsupportedExpr letExpr || isUnsupportedExpr inExpr
    | E.NewTuple (_, es) -> Seq.exists isUnsupportedExpr es
    | E.TupleGet (_, _, tupl) -> isUnsupportedExpr tupl
    | E.UnionCase (_, _, fields) -> Seq.exists isUnsupportedExpr fields
    | E.UnionCaseGet (union, _, _, _) -> isUnsupportedExpr union
    | E.UnionCaseTest (union, _, _) -> isUnsupportedExpr union
    | E.NewRecord (_, fields) -> Seq.exists isUnsupportedExpr fields
    | E.RecordFieldGet (_, r, _) -> isUnsupportedExpr r
    | E.Const _ | E.SymRef _ -> false
    | E.Unsupported _ -> true

  let rec allTypes expr = 
    let r = allTypes
    let rl = List.collect r
    match expr with
    | E.NewTuple (t, items) | E.UnionCase (t, _, items) | E.NewRecord (t, items) ->  t :: (rl items)
    | E.UnionCaseTest (e, t, _) | E.UnionCaseGet (e, t, _, _) | E.TupleGet (t, _, e) | E.RecordFieldGet (t, e, _) -> t :: (r e)
    | E.Function (_, body) -> r body
    | E.Call (fn, args) -> (r fn) @ (rl args)
    | E.InfixOp (e1, _, e2) | E.Let (_, e1, e2) -> (r e1) @ (r e2)
    | E.SymRef _ -> []
    | E.Const (_, t) -> [t]
    | E.Conditional (test, thn, els) -> rl [test; thn; els]
    | E.Unsupported _ -> []

  let rec allSymbols expr = 
    let r = allSymbols
    let rl = List.collect r
    match expr with
    | E.NewTuple (_, items) | E.UnionCase (_, _, items) | E.NewRecord (_, items) ->  rl items
    | E.UnionCaseTest (e, _, _) | E.UnionCaseGet (e, _, _, _) | E.TupleGet (_, _, e ) | E.Function (_, e) | E.RecordFieldGet (_, e, _) -> r e
    | E.Call (e1, e2) -> (r e1) @ (rl e2)
    | E.InfixOp (e1, _, e2) | E.Let (_, e1, e2) -> (r e1) @ (r e2)
    | E.SymRef sym -> [sym]
    | E.Const (_, _) -> []
    | E.Conditional (test, thn, els) -> rl [test; thn; els]
    | E.Unsupported _ -> []