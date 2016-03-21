module Fovel.Gen.CodeGen
open Fovel

type NamedType = NamedType of name: string * typ: Type

type ProgramText =
  | Text of Code
  | NewLine of contents: ProgramText
  | Indent of content: ProgramText 
  | Sequence of ProgramText list

let curlyWrap content = Sequence <| (Text "{") :: (content @ [Text "}"])
let roundWrap content = Sequence <| (Text "(") :: (content @ [Text ")"])
let text fmt = Text << (sprintf fmt)
let sepBy sep xs = [for x in xs do yield Text sep; yield x] |> List.skip 1

let assignTypeNames program =
  let indexedName name idx = if idx = 0 then name else sprintf "%s__%d" name idx
  let typeNames = 
    program |> Seq.collect Binding.allTypes |> Seq.distinct
    |> Common.uniqueIndexedNames id Type.name indexedName
  let namedType typ = NamedType (typeNames.[typ], typ)

  let program = program |> List.map (Binding.mapType namedType)
  program

let typeName (NamedType (name,_)) = sprintf "__t_%s" name

let structType fields =
  fields |> Seq.map (sprintf "'%s'") |> String.concat ", "
  |> sprintf "defstruct( array( %s ) )"
  |> Text

let unionType cases =
  let id { CaseId = i } = i
  let fields { Fields = f } = f
  Sequence
    [ text "make( defstruct( array( '%s' ) )," (cases |> Seq.map id |> String.concat "','")
      cases |> List.map (NewLine << structType << fields) |> sepBy "," |> Sequence |> Indent
      Text ")" ]

let refUnionCase typ case = sprintf "%s.%s" (typeName typ) case

let createStruct structDef fields : ProgramText =
  if fields = [] 
    then text "make( %s )" structDef
    else Sequence [text "make( %s, " structDef; Sequence (sepBy ", " fields); Text " )"]

let createUnionCase typ case = createStruct (refUnionCase typ case)
let createRecord typ = createStruct (typeName typ)

let testUnionCase expr typ case =
  Sequence [Text "isStructInstance( "; expr; text ", %s )" (refUnionCase typ case) ]

let typeCode = function
  | NotImportant -> None
  | SingleCaseUnion _ -> None // Erase single-case unions
  | Record(_, []) -> None // Empty structs shouldn't really happen, so we don't have to make this code nice
  | Union(_, cases) -> Some (unionType cases)
  | Record(_, fields) -> Some (structType fields)

let typesCode types =
  let gen (NamedType (_, typ) as t) = typeCode typ |> Option.map (fun code -> NewLine (Sequence [text "var %s = " (typeName t); code ]))
  types |> List.choose gen

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

let rec exprCode intrinsicCode expr : ProgramText = 
  let r = exprCode intrinsicCode
  let rl = List.map r
  let rlc xs = rl xs |> sepBy ", "
  match expr with
  | E.Intrinsic (i, args) -> 
    let argIdxs = args |> List.mapi (fun i _ -> sprintf "_%d" i)
    let argBindings = 
      Seq.zip (rl args) argIdxs 
      |> Seq.map (fun (a,i) -> Sequence [text " var %s = " i; a]) 
      |> Seq.toList
    curlyWrap (argBindings @ [text " %s " (intrinsicCode i argIdxs)])

  | E.IntrinsicAsValue (fn, curriedArgGroups) -> 
    // Parameters - will have the form "_0, _1, _2 ... _n" where each "_n" stands for one curried group - either a tuple or one parameter
    let parms = curriedArgGroups |> Seq.mapi (fun idx _ -> sprintf "_%d" idx) |> String.concat ", "

    // Arguments - will have the form "_0, _1[0], _1[1], _2, ..." where "_n" stands for one individual argument and "_n[0], _n[1]" stands
    // for destructuring of the n-th tuple.
    let arg idx tupleSize =
      if tupleSize = 1 then [sprintf "_%d" idx]
      else List.init tupleSize (fun item -> sprintf "_%d[%d]" idx item)
    let args = curriedArgGroups |> Seq.mapi arg |> Seq.collect id |> Seq.toList

    Sequence [ text "fn(%s) " parms; Text (intrinsicCode fn args) ]

  | E.NewTuple(_, items) -> Sequence [ Text "array( "; Sequence (rlc items); Text " )" ]
  | E.TupleGet(_, index, tuple) -> Sequence [ curlyWrap [r tuple]; text "[%d]" index ] 

  // Single-case unions are erased:
  | E.UnionCase(NamedType (_,SingleCaseUnion _), _, [value]) -> r value
  | E.UnionCaseTest(_, NamedType (_,SingleCaseUnion _), _) -> Text "true"
  | E.UnionCaseGet(union, NamedType (_,SingleCaseUnion _), _, _) -> r union

  | E.UnionCase(unionType, case, fields) -> createUnionCase unionType case (rl fields)
  | E.UnionCaseTest(union, unionType, case) -> testUnionCase (r union) unionType case
  | E.UnionCaseGet(union, _, _, field) -> Sequence [ curlyWrap[r union]; Text "."; Text field ]

  | E.NewRecord(recordType, fields) -> createRecord recordType (rl fields)
  | E.RecordFieldGet(_, record, field) -> Sequence [ curlyWrap[r record]; Text "."; Text field ]

  | E.NewArray (_, els) -> Sequence [ Text "array( "; Sequence (rlc els); Text " )" ]
  | E.ArrayElement (arr, idx) -> Sequence [ curlyWrap [r arr]; Sequence [ Text "["; r idx; Text "]" ] ]

  | E.Function(parameters, body) -> Sequence [ text "fn(%s) " (String.concat ", " parameters); Indent (r body) ]
  | E.InfixOp(leftArg, op, rightArg) -> Sequence [ curlyWrap [r leftArg]; text " %s " (infixOpCode op); curlyWrap [r rightArg] ]
  | E.SymRef sym -> Text sym
  | E.Const(c, _) -> Text (constCode c)
  
  | E.Let(bindings, body) -> 
    let formatBinding (sym, expr) = NewLine( Sequence [ text "var %s = " sym; curlyWrap [r expr] ] )
    let bindings = bindings |> List.map formatBinding
    bindings @ [r body |> NewLine] |> curlyWrap |> Indent

  | E.Sequence es -> es |> List.map (NewLine << r) |> curlyWrap |> Indent

  | E.Conditional(test, then', else') -> 
    let test = curlyWrap [r test]
    let then' = curlyWrap [r then']
    let else' = Sequence [Text " else "; curlyWrap [r else']]
    Sequence [ Text "if "; test; Text " "; then'; else' ]

  | E.TraitCall _ -> Text "panic( 'Unresolved static generic constraint' )"

  // A call without arguments in F# means "generic value", as in "let v<'a> = f<'a>()"
  | E.Call(func, _, []) -> curlyWrap [r func]

  | E.Call(func, _, args) -> Sequence [ curlyWrap [r func]; roundWrap (rlc args) ]


let bindingCode exprCode { Binding.Fn = fn; Expr = expr } : ProgramText = 
  let expr = exprCode expr

  match fn with
  | None -> expr
  | Some (fn, []) -> Sequence [ Text (sprintf "var %s = " fn); Indent expr ]
  | Some (fn, args) -> 
    let args = args |> Seq.map fst |> String.concat ", "
    Sequence [ Text (sprintf "var %s = fn(%s) " fn args); Indent expr ]

let programCode exprCode program : ProgramText = 
  let program = assignTypeNames program
  let types = program |> List.collect Binding.allTypes |> List.distinct |> typesCode
  let code = program |> List.map (NewLine << bindingCode exprCode)
  Sequence (types @ code)