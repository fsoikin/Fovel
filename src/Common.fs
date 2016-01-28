namespace Fovel
open FSharpx.Collections

type Identifier = string

[<AutoOpen>]
module Common =

  let sanitizeId id = id

  let uniqueIndexedNames getName indexedName items =
    items 
    |> Seq.groupBy getName 
    |> Seq.map snd 
    |> Seq.collect( 
        Seq.mapi (fun idx x -> x, indexedName (getName x) idx) ) 
    |> PersistentHashMap.ofSeq