
open FsCheck

let sorted (keys: string list) (permutations: int list list) =
  let keys = keys |> List.filter ((<>) null) |> List.distinct
  let permutations = if permutations = [] then [[]] else permutations

  let insertInOrder order =
    let order = Seq.append order (Seq.initInfinite id)
    printfn "%A" order
    let dict = System.Collections.Generic.Dictionary<_,_>()
    Seq.zip keys order |> Seq.sortBy snd |> Seq.iter (fun (k,_) -> printfn "%s" k; dict.Add(k, k))
    dict |> Seq.map (fun p -> p.Key) |> Seq.toList

  let insertedInAllOrders = permutations |> List.map insertInOrder
  printfn "%A" insertedInAllOrders
  insertedInAllOrders |> List.distinct |> List.length = 1

Check.Quick sorted

sorted ["a";"b";"c"] [[1;2;3];[3;2;1];[5;4;2];[1;-1;5]]

let d = System.Collections.Generic.Dictionary<_,_>()
d.Add("a", "a")
d.Add("", "")
d.Keys |> Seq.toList