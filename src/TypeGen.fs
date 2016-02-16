module Fovel.Gen.Type
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Fovel

let rec private unwrapAbbreviation (t: FSharpType) =
  if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then unwrapAbbreviation t.TypeDefinition.AbbreviatedType
  else t

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
    let name = e.TryFullName |> Option.orElse e.CompiledName
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
  | _ when t.IsGenericParameter -> Result.retn <| Type.GenericParameter t.GenericParameter.Name
  | _ when t.IsTupleType -> Result.retn <| Type.Tuple (t.GenericArguments.Count)
  | _ when t.IsFunctionType && t.GenericArguments.Count = 2 -> Result.retn <| Type.Function
  | _ when t.HasTypeDefinition && t.TypeDefinition.IsArrayType && t.TypeDefinition.GenericParameters.Count = 1 -> Result.retn <| Type.Array
  | _ when t.HasTypeDefinition ->
    match typeFromEntity t.TypeDefinition with
    | Some t -> Result.retn <| t
    | None -> Result.fail (Error.UnsupportedType t)

  | _ -> Result.fail (Error.UnsupportedType t)

let genTypes program = 
  program 
  |> Seq.collect Binding.allTypes
  |> Result.seqMap (fun t -> Result.tuple (Result.retn t) (fsharpTypeToFovelType t))
  |> Result.map (fun types ->
    let getType t = types |> Seq.find (fst >> (=) t) |> snd // TODO: turns this into map as soon as FSC #518 is fixed
    program |> List.map (Binding.mapType getType)
  )