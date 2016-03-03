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

let (|SingleCaseUnion|_|) u = match u with Union (_, [{ Fields=[_] }]) -> Some() | _ -> None

let typeName (NamedType (name,_)) = sprintf "__t_%s" name

let unionCaseType case =
  case.Fields |> Seq.map (sprintf "'%s'") |> String.concat ", "
  |> sprintf "defstruct( array( %s ) )"

let unionType cases =
  sprintf "make( defstruct( array( '%s' ) ), \n\t%s\n )"
  <| (cases |> Seq.map (fun c -> c.CaseId) |> String.concat "','")
  <| (cases |> Seq.map unionCaseType |> String.concat ",\n\t")

let refUnionCase typ case = sprintf "%s.%s" (typeName typ) case

let createUnionCase typ case fields =
  let fields = fields |> String.concat ", "
  let comma = if fields = "" then "" else ", "
  sprintf "make( %s%s%s )" (refUnionCase typ case) comma fields

let testUnionCase expr typ case =
  sprintf "isStructInstance( %s, %s )" expr (refUnionCase typ case)

let recordType fields =
  fields |> String.concat "', '"
  |> sprintf "defstruct( array( '%s' ) )"

let createRecord typ fields =
  let fields = fields |> String.concat ", "
  let comma = if fields = "" then "" else ", "
  sprintf "make( %s%s%s )" (typeName typ) comma fields

let typeCode = function
  | NotImportant -> None
  | SingleCaseUnion -> None // Erase single-case unions
  | Record(_, []) -> None // Empty structs shouldn't really happen, so we don't have to make this code nice
  | Union(_, cases) -> Some (unionType cases)
  | Record(_, fields) -> Some (recordType fields)

let typesCode types =
  let gen (NamedType (_, typ) as t) = typeCode typ |> Option.map (sprintf "var %s = %s\n" (typeName t))
  types |> Seq.choose gen |> String.concat ""

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
  let rl = Seq.map r
  let rlc = rl >> String.concat ", "
  match expr with
  | E.Intrinsic (i, args) -> intrinsicCode i (args |> List.map r)

  | E.NewTuple(_, items) -> sprintf "array( %s )" <| rlc items
  | E.TupleGet(_, index, tuple) -> sprintf "{%s}[%d]" <| r tuple <| index

  // Single-case unions are erased:
  | E.UnionCase(NamedType (_,SingleCaseUnion), _, [value]) -> r value
  | E.UnionCaseTest(_, NamedType (_,SingleCaseUnion), _) -> "true"
  | E.UnionCaseGet(union, NamedType (_,SingleCaseUnion), _, _) -> r union

  | E.UnionCase(unionType, case, fields) -> createUnionCase unionType case (rl fields)
  | E.UnionCaseTest(union, unionType, case) -> testUnionCase (r union) unionType case
  | E.UnionCaseGet(union, _, _, field) -> sprintf "{%s}.%s" (r union) field

  | E.NewRecord(recordType, fields) -> createRecord recordType (rl fields)
  | E.RecordFieldGet(_, record, field) -> sprintf "{%s}.%s" (r record) field

  | E.NewArray (_, els) -> sprintf "array( %s )" (rlc els)
  | E.ArrayElement (arr, idx) -> sprintf "{%s}[%s]" (r arr) (r idx)

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
  | E.TraitCall _ -> sprintf "panic( 'Unresolved static generic constraint' )"

  // A call without arguments in F# means "generic value", as in "let v<'a> = f<'a>()"
  | E.Call(func, _, []) -> sprintf "{%s}" (r func)

  | E.Call(func, _, args) -> sprintf "{%s}(%s)" (r func) (rlc args)


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
  let code = program |> Seq.map (bindingCode exprCode) |> String.concat "\n"
  types + code