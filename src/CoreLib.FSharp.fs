module Fovel.Core

let inline flip f a b = f b a
let inline id x = x
let inline fst (x,_) = x
let inline snd (_,x) = x
let inline (>>) f g x = g (f x)
let inline (<<) f g x = f (g x)

module List =
  let inline Cons x xs = x::xs

  let rec fold (folder: 'State -> 'T -> 'State) (initialState: 'State) = function
    | [] -> initialState
    | x::xs -> fold folder (folder initialState x) xs

  let inline map f = fold (fun xs x -> (f x)::xs) []


module Array =
  let create<'a> (size: int) : 'a array = failwith ""

type Seq<'t> = Empty | Item of (unit -> 't * Seq<'t>)

module Seq =
  let rangeStep start step end' = 
    let rec iter i = if i = end' then Empty else Item <| fun() -> (i, iter (i+step))
    iter start

  let inline range start end' = rangeStep start 1 end'

  let initInfinite f =
    let rec iter i = Item <| fun () -> (f i, iter (i+1))
    iter 0

  let rec fold (folder: 'State -> 'T -> 'State) (initialState: 'State) = function
    | Empty -> initialState
    | Item next -> 
      let item, next = next()
      fold folder (folder initialState item) next

  let rec foldBack (folder: 'T -> 'State -> 'State) seq (initialState: 'State) = 
    let rec r = function
      | Empty -> fun() -> initialState
      | Item next -> 
        let item, next = next()
        fun() -> folder item <| r next ()
    r seq ()

  let rec take n = function
    | Empty -> Empty
    | Item next -> Item <| fun() ->
      let item, next = next()
      item, take (n-1) next

  let rec skip n = function 
    | Empty -> Empty 
    | Item next -> 
      if n = 0 then Item next 
      else (next() |> snd |> skip (n-1))

  let inline iter (f: 't -> unit) seq = fold (fun () t -> f t) () seq

  let inline iteri (f: 't -> int -> unit) seq = fold (fun idx t -> f t idx; idx+1) 0 seq

  let inline init count f = initInfinite f |> take count

  let inline tryHead seq = match seq with | Empty -> None | Item next -> next() |> fst |> Some

  let inline tryNth idx = skip idx >> tryHead

  let inline length seq = fold (fun c _ -> c+1) 0 seq
  
  let inline toList seq = foldBack List.Cons seq

  let inline toArray seq = 
    let a = Array.create (length seq)
    iteri (fun x idx -> a.[idx] <- x) seq