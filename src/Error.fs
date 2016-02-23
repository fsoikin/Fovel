namespace Fovel
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler

type Error =
  | FSharpError of err: FSharpErrorInfo

  | MalformedDecisionTree of tree: FSharpExpr
  | DecisionTreeBindingsNumberMismatch of symbols: int * bindings: int * tree: FSharpExpr
  | InstanceMethodsNotSupported of fn: FSharpMemberOrFunctionOrValue
  | CannotReferenceExternalSymbol of s: FSharpMemberOrFunctionOrValue

  | UnsupportedExpression of expr: FSharpExpr
  | UnsupportedType of typ: FSharpType

  | UnresolvedTraitCall of types: FSharpType list * memberName: Identifier


[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module Error =
  let private formatFSharpSeverity = function
    | FSharpErrorSeverity.Error -> "Error"
    | FSharpErrorSeverity.Warning -> "Warning"

  let private formatExpr (e: FSharpExpr) = e.ToString() // TODO
  let rec private formatType (t: FSharpType) = 
    if t.IsAbbreviation && t.AbbreviatedType <> t then formatType t.AbbreviatedType
    else if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then formatType t.TypeDefinition.AbbreviatedType
    else if t.HasTypeDefinition then match t.TypeDefinition.TryFullName with | Some name -> name | None -> t.ToString()
    else t.ToString()

  let format = function
    | Error.FSharpError e -> sprintf "FSharp %s: %s" (formatFSharpSeverity e.Severity) e.Message
    | Error.MalformedDecisionTree t -> sprintf "Malformed decision tree: %s" (formatExpr t)
    | Error.DecisionTreeBindingsNumberMismatch (symbols, bindings, tree) -> sprintf "Decision tree has %d symbols, but %d bindings: %s" symbols bindings (formatExpr tree)
    | Error.InstanceMethodsNotSupported fn -> sprintf "Instance methods are not supported: %s.%s" fn.EnclosingEntity.FullName fn.LogicalName
    | Error.CannotReferenceExternalSymbol s -> sprintf "Symbol '%s' cannot be turned into Shovel code, because it comes from an external assembly. Either include source code for the symbol in the compilation batch or handle it as an intrinsic." (FSharp.fnFullName s)
    | Error.UnsupportedExpression e -> sprintf "Unsupported expression: %s" (formatExpr e)
    | Error.UnsupportedType t -> sprintf "Unsupported type: %s" (formatType t)
    | Error.UnresolvedTraitCall (types, memberName) -> sprintf "Cannot resolve static constraint: member '%s' not found on any of types: %s" memberName (types |> Seq.map FSharp.typeName |> String.concat ", ")

  let formatAll = List.map format