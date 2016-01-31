﻿namespace Fovel
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler

type Error =
  | FSharpError of err: FSharpErrorInfo

  | MalformedDecisionTree of tree: FSharpExpr
  | DecisionTreeBindingsNumberMismatch of symbols: int * bindings: int * tree: FSharpExpr
  | MemberMethodsNotSupported of fn: FSharpMemberOrFunctionOrValue

  | UnsupportedExpression of expr: FSharpExpr
  | UnsupportedType of typ: FSharpType


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
    | Error.MemberMethodsNotSupported fn -> sprintf "Member methods/functions are not supported: %s.%s" fn.EnclosingEntity.FullName fn.LogicalName
    | Error.UnsupportedExpression e -> sprintf "Unsupported expression: %s" (formatExpr e)
    | Error.UnsupportedType t -> sprintf "Unsupported type: %s" (formatType t)

  let formatAll = List.map format