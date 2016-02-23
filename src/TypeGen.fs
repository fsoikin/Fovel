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

let fsharpTypeToFovelType (t: FSharpType) = 
  if not t.HasTypeDefinition then
    NotImportant
  else
    match t.TypeDefinition with
    | e when e.IsFSharpUnion -> Type.Union (e.LogicalName, e.UnionCases |> Seq.map unionCase |> Seq.toList)
    | e when e.IsFSharpRecord -> Type.Record (e.LogicalName, e.FSharpFields |> Seq.map (fun f -> f.Name) |>  Seq.toList)
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