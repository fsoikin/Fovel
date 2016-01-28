module Fovel.Gen.Type
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Fovel
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library.Tables

module Errors =
  let unsupportedType = sprintf "Type %A is not supported."

let rec private unwrapAbbreviation (t: FSharpType) =
  if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then unwrapAbbreviation t.TypeDefinition.AbbreviatedType
  else t

let fsharpTypeName t =
  match unwrapAbbreviation t with
  | t when t.HasTypeDefinition -> t.TypeDefinition.LogicalName
  | _ -> "?"

let private unionCase (c: FSharpUnionCase) =
  let items = c.UnionCaseFields |> Seq.map (fun f -> f.Name) |> Seq.toList
  { CaseId = c.Name; Fields = items }

let private typeFromEntity (e: FSharpEntity) =
  match e with
  | _ when e.IsFSharpUnion -> 
    let cases = e.UnionCases |> Seq.map unionCase |> Seq.toList
    Some <| Type.Union (e.LogicalName, cases)

  | _ when e.IsFSharpRecord ->
    let fields = e.FSharpFields |> Seq.map (fun f -> f.Name) |>  Seq.toList
    Some <| Type.Record (e.LogicalName, fields)

  | _ ->
    let name = e.TryFullName |> Option.orElse (fun () -> Some e.CompiledName) |> Option.get
    match name with
    | "System.Int32" -> Some Type.Int
    | "System.Double" | "System.Float" -> Some Type.Float
    | "System.String" -> Some Type.String
    | "System.Boolean" -> Some Type.Bool
    | "Microsoft.FSharp.Core.Unit" -> Some Type.Unit
    | _ -> None

  | _ -> None

let fsharpTypeToFovelType (t: FSharpType) = 
  let t = unwrapAbbreviation t
  match t with
  | _ when t.IsGenericParameter -> Type.GenericParameter t.GenericParameter.Name
  | _ when t.IsTupleType -> Type.Tuple (t.GenericArguments.Count)
  | _ when t.IsFunctionType && t.GenericArguments.Count = 2 -> Type.Function
  | _ when t.HasTypeDefinition && t.TypeDefinition.IsArrayType && t.TypeDefinition.GenericParameters.Count = 1 -> Type.Array
  | _ when t.HasTypeDefinition ->
    match typeFromEntity t.TypeDefinition with
    | Some t -> t
    | None -> Type.Unsupported (Errors.unsupportedType t)

  | _ -> Type.Unsupported (Errors.unsupportedType t)

let genTypes p = p |> List.map (Binding.mapType fsharpTypeToFovelType)