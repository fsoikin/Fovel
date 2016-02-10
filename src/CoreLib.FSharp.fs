module Fovel.Core

type ShovelNameAttribute(name: string) = inherit System.Attribute()

let private external() = failwith "This function shouldn't get called, because it is compiled as a CorLib intrinsic."

let inline flip f a b = f b a

module List =
  let inline Cons x xs = x::xs

module Seq =
  let nth (idx: int) (seq: 't seq) : 't = external()
  
  let fold (folder: 'State -> 'T -> 'State) (initialState: 'State) (seq: 'T seq) = 
    let len = Seq.length seq
    let rec iter (st: 'State) (idx: int) = if idx < len then iter (folder st (nth idx seq)) (idx+1) else st
    iter initialState 0
  
  let foldBack (folder: 'T -> 'State -> 'State) (seq: 'T seq) (initialState: 'State) = 
    let rec iter (st: 'State) (idx: int) = if idx >= 0 then iter (folder (nth idx seq) st) (idx-1) else st
    iter initialState (Seq.length seq - 1)

  [<ShovelName("__core_Seq_toList")>]
  let inline toList seq = foldBack List.Cons seq