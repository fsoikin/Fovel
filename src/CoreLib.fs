module Fovel.CoreLib
open Fovel
open Fovel.Gen
open Fovel.Gen.Expr

let private (|CoreFunction|_|) name fn = Expr.Intrinsics.(|Fn|_|) <| sprintf "Microsoft.FSharp.Core.%s" name <| fn

module SeqIntrinsic =

  type Intrinsic = Range | RangeStep | ToArray | ToList | Seq

  let private (|SeqFn|_|) name = Expr.Intrinsics.(|Fn|_|) <| sprintf "Microsoft.FSharp.Collections.Seq.%s" name

  let (|Parse|_|) = function
  | CoreFunction "Operators.( .. )" -> Some Range
  | CoreFunction "Operators.( .. .. )" -> Some RangeStep
  | CoreFunction "Operators.seq" -> Some Seq
  | SeqFn "toArray" -> Some ToArray
  | SeqFn "toList" -> Some ToArray
  | _ -> None

  let code = function
  | Range, [from;to'] -> sprintf "__core.range( %s, %s )" from to'
  | RangeStep, [from;step;to'] -> sprintf "__core.rangeStep( %s, %s, %s )" from step to'
  | Seq, [s] -> sprintf "__core.id( %s )" s
  | ToArray, [arg] -> arg
  | ToList, [arg] -> failwith "boo"
  | intr, args -> sprintf "panic('Intrinsic %A applied with incorrect number of arguments %d')" intr (List.length args)

module ArrayIntrinsic =

  type Intrinsic = Length

  let private (|ArrFn|_|) name = Expr.Intrinsics.(|Fn|_|) <| sprintf "Microsoft.FSharp.Collections.Array.%s" name

  let (|Parse|_|) = function
  | ArrFn "length" -> Some Length
  | _ -> None

  let code = function
  | Length, [a] -> sprintf "length( %s )" a
  | intr, args -> sprintf "panic('Intrinsic %A applied with incorrect number of arguments %d')" intr (List.length args)

type CoreLibIntrinsic<'CustomIntrinsic> =
  | Seq of SeqIntrinsic.Intrinsic
  | Array of ArrayIntrinsic.Intrinsic
  | FailWith
  | Custom of 'CustomIntrinsic

let parseIntrinsic (|ParseCustomIntrinsic|_|) = function
  | ParseCustomIntrinsic custom -> Some (Custom custom)
  | SeqIntrinsic.Parse seq -> Some (Seq seq)
  | ArrayIntrinsic.Parse arr -> Some (Array arr)
  | CoreFunction "Operators.failwith" -> Some FailWith
  | _ -> None

let intrinsicCode customIntrinsicCode intr args =
  match intr, args with
  | Custom c, _ -> customIntrinsicCode c args
  | Seq seq, args -> SeqIntrinsic.code (seq, args)
  | Array arr, args -> ArrayIntrinsic.code (arr, args)
  | FailWith, args -> sprintf "panic( %s )" (defaultArg (List.tryHead args) "")

let wrapInstrinsics (parseCustomIntrinsic: 'a -> 'b option, customIntrinsicCode: 'b -> string list -> string) = 
  (parseIntrinsic parseCustomIntrinsic, intrinsicCode customIntrinsicCode)

let [<Literal>] ShovelCode = """
  var __core = {

    var appendRange = fn(arr, from, to, step) {
      if from == to arr
      else {
        push(arr, from)
        if from > to appendRange(arr, from-step, to, step) 
        else appendRange(arr, from+step, to, step)
      }
    }

    var fns = hash(
      'id', fn x x,
      'rangeStep', fn(from, to, step) appendRange(array(), from, to, step),
      'range', fn(from, to) __core.rangeStep(from, to, 1)
    )

    hashToStruct( defstruct(keys(fns)), fns )
  }
"""