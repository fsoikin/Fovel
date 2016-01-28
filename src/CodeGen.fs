module Fovel.Gen.CodeGen
open Fovel

let assignTypeNames program =
  let typeNames = 
    program |> Seq.collect Binding.allTypes |> Seq.distinct
    |> Common.uniqueIndexedNames Type.name (sprintf "%s#%d")
  let getName typ = typeNames.[typ]

  let program = program |> List.map (Binding.mapType getName)
  let nameTypes = typeNames |> Seq.map (fun (typ,name) -> name, typ) |> Map.ofSeq

  program, nameTypes

let codeGlobals = """
  var __unioncase = defstruct( array( 'make', 'test' ) )
"""

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
  | Array | Bool | Tuple _ | Unit | String 
  | Int | Float | Function | GenericParameter _ -> 
    None
  
  | Unsupported _ -> 
    None

  | Union(_, cases) -> 
    sprintf "make( defstruct( array( '%s' ) ), \n%s )"
    <| (cases |> Seq.map (fun c -> c.CaseId) |> String.concat "','")
    <| (cases |> Seq.map unionCaseCode |> String.concat ",\n")
    |> Some

  | Record(_, []) -> Some "fn() hash()" // Empty structs shouldn't really happen, so we don't have to make this code nice

  | Record(_, fields) -> 
    let fieldsQuoted = fields |> String.concat "', '"
    let fields = fields |> String.concat ", "
    Some <| sprintf "{ var def = defstruct( '%s' )  fn( %s ) make( def, %s ) }" fieldsQuoted fields fields

let typesCode types =
  let gen (name, typ) = typeCode typ |> Option.map(fun c -> name, c)
  let codes = types |> Seq.choose gen |> Seq.toList
  let names = codes |> Seq.map (fst >> sprintf "'%s'") |> String.concat ", "
  let defs = codes |> Seq.map snd |> String.concat ",\n\n"
  sprintf "make( defstruct( array( %s ) ), \n\n%s )" names defs

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
  | :? int | :? bool -> string c
  | :? string as s -> sprintf "'%s'" s
  | :? float as f -> sprintf "%f" f
  | _ -> failwithf "Const of type %s not supported" (c.GetType().Name)

let rec exprListCode exprs = exprs |> Seq.map (exprCode >> sprintf "(%s)") |> String.concat ", "

and exprCode = function
  | E.NewTuple(_, items) -> sprintf "array( %s )" <| exprListCode items
  | E.TupleGet(_, index, tuple) -> sprintf "(%s)[%d]" <| exprCode tuple <| index

  | E.UnionCase(unionType, case, fields) -> sprintf "__t['%s']['%s'].make( %s )" unionType case (exprListCode fields)
  | E.UnionCaseTest(union, unionType, case) -> sprintf "__t['%s']['%s'].test( %s )" unionType case (exprCode union)
  | E.UnionCaseGet(union, _, _, field) -> sprintf "(%s)['%s']" (exprCode union) field

  | E.NewRecord(recordType, fields) -> sprintf "__t['%s']( %s )" recordType (exprListCode fields)
  | E.RecordFieldGet(_, record, field) -> sprintf "(%s)['%s']" (exprCode record) field

  | E.Function(parameter, body) -> sprintf "fn(%s) %s" parameter (exprCode body)
  | E.InfixOp(leftArg, op, rightArg) -> sprintf "(%s) %s (%s)" (exprCode leftArg) (infixOpCode op) (exprCode rightArg)
  | E.SymRef sym -> sym
  | E.Const(c, _) -> constCode c
  | E.Let(var, varValue, body) -> sprintf "{ var %s = (%s) %s }" var (exprCode varValue) (exprCode body)
  | E.Conditional(test, then', else') -> sprintf "if (%s) (%s) else (%s)" (exprCode test) (exprCode then') (exprCode else')
  | E.Call(func, args) -> sprintf "(%s)(%s)" (exprCode func) (exprListCode args)
  | E.Unsupported(error) -> failwith error

let bindingCode { Binding.Fn = fn; Expr = expr } = 
  let expr = exprCode expr

  match fn with
  | None -> expr
  | Some (fn, []) -> sprintf "var %s = %s" fn expr
  | Some (fn, args) -> 
    let args = args |> Seq.collect id |> Seq.map fst |> String.concat ", "
    sprintf "var %s = fn(%s) %s" fn args expr

let programCode program types = 
  let types = types |> Map.toSeq |> typesCode 
  let code = program |> Seq.map bindingCode |> String.concat "\n"
  sprintf "var __t = %s\n\n%s" types code