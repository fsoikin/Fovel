module Fovel.Gen.Type
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Fovel

let rec private unwrapAbbreviation (t: FSharpType) =
  if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then unwrapAbbreviation t.TypeDefinition.AbbreviatedType
  else t

let private unionCase (c: FSharpUnionCase) =
  let items = c.UnionCaseFields |> Seq.map (fun f -> f.Name) |> Seq.toList
  { CaseId = sanitizeId c.Name; Fields = items |> List.map sanitizeId }

let typeDefinition (t: FSharpType) = if t.HasTypeDefinition then Some t.TypeDefinition else None

let rec fsharpTypeToFovelType (t: FSharpType) = 
  match t.IsAbbreviation, typeDefinition t with
  | true,_ -> fsharpTypeToFovelType t.AbbreviatedType
  | _, Some e when e.IsFSharpUnion -> 
    if e.UnionCases.Count = 1 && e.UnionCases.[0].UnionCaseFields.Count = 1 
      then Type.SingleCaseUnion e.LogicalName
      else Type.Union (e.LogicalName, e.UnionCases |> Seq.map unionCase |> Seq.toList)
  | _, Some e when e.IsFSharpRecord -> Type.Record (e.LogicalName, e.FSharpFields |> Seq.map (fun f -> sanitizeId f.Name) |>  Seq.toList)
  | _ -> NotImportant

let genTypes program = 
  let shovelTypes =
    program 
    |> Seq.collect Binding.allTypes
    // TODO: Seq.distinct
    |> Seq.map (fun t -> t, fsharpTypeToFovelType t)
    |> List.ofSeq

  let getType t = shovelTypes |> List.find (fst >> (=) t) |> snd // TODO: turns this into map as soon as FSC #518 is fixed
  program |> List.map (Binding.mapType getType)