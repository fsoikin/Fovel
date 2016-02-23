module Fovel.Gen.CodeGen
open Fovel

type NamedType = NamedType of name: string * typ: Type

let assignTypeNames program =
  let indexedName name idx = if idx = 0 then name else sprintf "%s__%d" name idx
  let typeNames = 
    program |> Seq.collect Binding.allTypes |> Seq.distinct
    |> Common.uniqueIndexedNames id Type.name indexedName
  let namedType typ = NamedType (typeNames.[typ], typ)

  let program = program |> List.map (Binding.mapType namedType)
  program

let codeGlobals = """
  var __unioncase = defstruct( array( 'make', 'test' ) )
"""

let (|SingleCaseUnion|_|) u = match u with Union (_, [_]) -> Some() | _ -> None

let unionCaseCode case =
  let fieldsQuoted = case.Fields |> Seq.map (sprintf "'%s'") |> String.concat ", "
  let fields = case.Fields |> Seq.map sanitizeId |> String.concat ", "
  let comma = if fields = "" then "" else ", "
  sprintf """{
    var def = defstruct( array( %s ) )
    makestruct( __unioncase, 
      fn (%s) make( def%s%s ), 
      fn (x) isStructInstance( x, def ) ) }"""
  <| fieldsQuoted <| fields <| comma <| fields

let typeCode = function
  | NotImportant -> None
  | SingleCaseUnion -> None // Erase single-case unions

  | Union(_, cases) -> 
    sprintf "make( defstruct( array( '%s' ) ), \n%s )"
    <| (cases |> Seq.map (fun c -> c.CaseId) |> String.concat "','")
    <| (cases |> Seq.map unionCaseCode |> String.concat ",\n")
    |> Some

  | Record(_, []) -> Some "fn() hash()" // Empty structs shouldn't really happen, so we don't have to make this code nice

  | Record(_, fields) -> 
    let fieldsQuoted = fields |> String.concat "', '"
    let fields = fields |> String.concat ", "
    Some <| sprintf "{ var def = defstruct( array( '%s' ) )  fn( %s ) make( def, %s ) }" fieldsQuoted fields fields

let typesCode types =
  let gen (NamedType (name, typ)) = typeCode typ |> Option.map(fun c -> name, c)
  let codes = types |> Seq.choose gen |> Seq.toList
  let names = codes |> Seq.map (fst >> sprintf "'%s'") |> String.concat ", "
  let defs = codes |> Seq.map snd |> String.concat ",\n\n"
  if names = "" 
    then "" 
    else sprintf "make( defstruct( array( %s ) ), \n\n%s )" names defs

let infixOpCode = function
  | InfixOpKind.Plus -> "+"
  | InfixOpKind.Minus -> "-"
  | InfixOpKind.Mul -> "*"
  | InfixOpKind.Div -> "/"
  | InfixOpKind.Greater -> ">"
  | InfixOpKind.GreaterOrEqual -> ">="
  | InfixOpKind.Less -> "<"
  | InfixOpKind.LessOrEqual -> "<="
  | InfixOpKind.Equal -> "=="
  | InfixOpKind.NotEqual -> "!="

let constCode (c: obj) = 
  match c with
  | null -> "null"
  | :? int -> string c
  | :? bool as b -> if b then "true" else "false"
  | :? string as s -> sprintf "'%s'" s
  | :? float as f -> sprintf "%f" f
  | _ -> failwithf "Const of type %s not supported" (c.GetType().Name)

let rec exprCode intrinsicCode expr = 
  let r = exprCode intrinsicCode
  let rl = Seq.map r >> String.concat ", "
  match expr with
  | E.Intrinsic (i, args) -> intrinsicCode i (args |> List.map r)

  | E.NewTuple(_, items) -> sprintf "array( %s )" <| rl items
  | E.TupleGet(_, index, tuple) -> sprintf "{%s}[%d]" <| r tuple <| index

  // Single-case unions are erased:
  | E.UnionCase(NamedType (_,SingleCaseUnion), _, [value]) -> r value
  | E.UnionCaseTest(_, NamedType (_,SingleCaseUnion), _) -> "true"
  | E.UnionCaseGet(union, NamedType (_,SingleCaseUnion), _, _) -> r union

  | E.UnionCase(NamedType (unionType,_), case, fields) -> sprintf "__t.%s.%s.make( %s )" unionType case (rl fields)
  | E.UnionCaseTest(union, NamedType (unionType,_), case) -> sprintf "__t.%s.%s.test( %s )" unionType case (r union)
  | E.UnionCaseGet(union, _, _, field) -> sprintf "{%s}.%s" (r union) field

  | E.NewRecord(NamedType (recordType,_), fields) -> sprintf "__t.%s( %s )" recordType (rl fields)
  | E.RecordFieldGet(_, record, field) -> sprintf "{%s}.%s" (r record) field

  | E.NewArray (_, els) -> sprintf "array( %s )" (rl els)
  | E.ArrayElement (arr, idx) -> sprintf "{%s}.%s" (r arr) (r idx)

  | E.Function(parameters, body) -> sprintf "fn(%s) %s" (String.concat ", " parameters) (r body)
  | E.InfixOp(leftArg, op, rightArg) -> sprintf "{%s} %s {%s}" (r leftArg) (infixOpCode op) (r rightArg)
  | E.SymRef sym -> sym
  | E.Const(c, _) -> constCode c
  
  | E.Let(bindings, body) -> 
    let formatBinding (sym, expr) = sprintf "var %s = {%s}" sym (r expr)
    let bindings = bindings |> Seq.map formatBinding |> String.concat "\n"
    sprintf "{ %s\n%s }" bindings (r body)

  | E.Sequence es -> es |> Seq.map r |> String.concat "\n"

  | E.Conditional(test, then', else') -> sprintf "if {%s} {%s} else {%s}" (r test) (r then') (r else')
  | E.Call(func, _, args) -> sprintf "{%s}(%s)" (r func) (rl args)
  | E.TraitCall _ -> sprintf "panic( 'Unresolved static generic constraint' )"

let bindingCode exprCode { Binding.Fn = fn; Expr = expr } = 
  let expr = exprCode expr

  match fn with
  | None -> expr
  | Some (fn, []) -> sprintf "var %s = %s" fn expr
  | Some (fn, args) -> 
    let args = args |> Seq.map fst |> String.concat ", "
    sprintf "var %s = fn(%s) %s" fn args expr

let programCode exprCode program = 
  let types = program |> List.collect Binding.allTypes |> List.distinct |> typesCode
  let types = if types = "" then "" else sprintf "%s\nvar __t = %s" codeGlobals types
  let code = program |> Seq.map (bindingCode exprCode) |> String.concat "\n"
  types + "\n" + code