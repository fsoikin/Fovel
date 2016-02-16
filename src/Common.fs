namespace Fovel
open FSharpx.Collections

type Identifier = string
type Code = string

module Option =
  let orElse x = function None -> x | Some y -> y

module FSharp =
  open Microsoft.FSharp.Compiler.SourceCodeServices
  let fnFullName (fn: FSharpMemberOrFunctionOrValue) = fn.FullName
  let isFunction (fn: FSharpMemberOrFunctionOrValue) = not (fn.CurriedParameterGroups |> Seq.isEmpty)

[<AutoOpen>]
module Common =

  let idRegex = System.Text.RegularExpressions.Regex("[^a-zA-Z_0-9]")
  let sanitizeId id = idRegex.Replace( id, "_" )

  let uniqueIndexedNames getKey getName indexedName items =
    items 
    |> Seq.groupBy getName 
    |> Seq.map snd
    |> Seq.collect( 
        Seq.mapi (fun idx x -> getKey x, indexedName (getName x) idx) ) 
    |> PersistentHashMap.ofSeq

type Result<'t, 'err> = OK of 't | Error of 'err list

module Result =
  let inline bind f = function | OK t -> f t | Error e -> Error e
  let inline bindError f = function | OK t -> OK t | Error e -> f e
  let inline retn x = OK x
  let inline fail e = Error [e]
  let inline map f = bind (OK << f)
  let inline mapError f = bindError (Error << f)

  let inline ap f r = f |> bind (fun f -> bind f r)
  let inline map2 f a b =
    match a, b with
    | OK x, OK y -> OK (f x y)
    | Error x, Error y -> Error (x @ y)
    | Error x, _ | _, Error x -> Error x

  let inline tuple a b = map2 (fun x y -> x, y) a b
  let inline tuple3 a b c = tuple a b |> tuple c |> map (fun (c, (a, b)) -> a, b, c)
  let inline tuple4 a b c d = tuple (tuple a b) (tuple c d) |> map (fun ((a, b), (c, d)) -> a, b, c, d)

  let sequence rs = Seq.foldBack (map2 List.cons) rs (retn [])
  let seqMap f rs = rs |> Seq.map f |> sequence

[<AutoOpen>]
module ResultAuto =
  open Result

  let inline (>>=) r f = bind f r
  let inline (|*>) r f = map f r
  let inline (||*>) r f = map ((<||) f) r

  let inline (<*>) f r = ap f r
  let inline (<!>) f r = ap (retn (retn << f)) r

  let inline (<!) f (a,b) = f <!> (tuple a b)
  let inline (<!!) f (a,b,c) = f <!> (tuple3 a b c)
  let inline (<!!!) f (a,b,c,d) = f <!> (tuple4 a b c d)