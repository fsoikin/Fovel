namespace Fovel
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler

type Error =
  | FSharpError of err: FSharpErrorInfo

  | MalformedDecisionTree of tree: FSharpExpr
  | DecisionTreeBindingsNumberMismatch of symbols: int * bindings: int * tree: FSharpExpr
  | InstanceMethodsNotSupported of pos: Range.range * fn: FSharpMemberOrFunctionOrValue
  | CannotReferenceExternalSymbol of s: FSharpMemberOrFunctionOrValue

  | UnsupportedExpression of expr: FSharpExpr
  | UnsupportedType of typ: FSharpType

  | UnresolvedTraitCall of pos: Range.range * types: FSharpType list * memberName: Identifier


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

  let private fmtRange (r: Range.range) = sprintf "%s (%d, %d)" r.FileName r.StartLine r.StartColumn

  let format = function
    | Error.FSharpError e -> 
      sprintf "FSharp %s: %s (%d, %d): %s" (formatFSharpSeverity e.Severity) e.FileName e.StartLineAlternate e.StartColumn e.Message
    | Error.MalformedDecisionTree t -> 
      sprintf "%s: Malformed decision tree: %s" (fmtRange t.Range) (formatExpr t) 
    | Error.DecisionTreeBindingsNumberMismatch (symbols, bindings, tree) -> 
      sprintf "%s Decision tree has %d symbols, but %d bindings: %s" (fmtRange tree.Range) symbols bindings (formatExpr tree)
    | Error.InstanceMethodsNotSupported (r, fn) -> 
      sprintf "%s: Instance methods are not supported: %s.%s" (fmtRange r) fn.EnclosingEntity.FullName fn.LogicalName
    | Error.CannotReferenceExternalSymbol s -> 
      sprintf "Symbol '%s' cannot be turned into Shovel code, because it comes from an external assembly. Either include source code for the symbol in the compilation batch or handle it as an intrinsic." (FSharp.fnFullName s)
    | Error.UnsupportedExpression e -> 
      sprintf "%s: Unsupported expression: %s" (fmtRange e.Range) (formatExpr e)
    | Error.UnsupportedType t -> 
      sprintf "Unsupported type: %s" (formatType t)
    | Error.UnresolvedTraitCall (r, types, memberName) -> 
      sprintf "%s: Cannot resolve static constraint: member '%s' not found on any of types: %s" 
        (fmtRange r) 
        memberName 
        (types |> Seq.map FSharp.typeName |> String.concat ", ")

  let formatAll = List.map format