module FovelCore

let inline flip f a b = f b a
let inline id x = x
let inline fst (x,_) = x
let inline snd (_,x) = x
let inline (>>) f g x = g (f x)
let inline (<<) f g x = f (g x)
let inline (|>) x f = f x
let inline (<|) f x = f x
let inline ignore _ = ()

module Option =
  let map f = function | None -> None | Some x -> Some (f x)
  let bind f = function | None -> None | Some x -> f x

module List =
  let inline Cons x xs = x::xs

  let rec fold (folder: 'State -> 'T -> 'State) (initialState: 'State) = function
    | [] -> initialState
    | x::xs -> fold folder (folder initialState x) xs

  let inline map f = fold (fun xs x -> (f x)::xs) []

module Array =
  let create<'a> (size: int) : 'a array = failwith ""

type Seq<'t> = Sequence of (unit -> SeqStep<'t>)
and SeqStep<'t> = Empty | Cons of 't * Seq<'t>
type 't seq = Seq<'t>

module Seq =
  let inline empty<'t> : 't seq = Sequence( fun() -> Empty )
  let inline cons x xs = Sequence( fun() -> Cons (x, xs) )
  let inline singleton x = cons x empty
  let defer (s: unit -> Seq<_>) = Sequence( fun() -> 
    let (Sequence step) = s() 
    step() )

  let inline tryStep (Sequence step) = 
    match step() with 
    | Empty -> None
    | Cons (x, xs) -> Some (x, xs)

  let rangeStep start step end' = 
    let rec iter i () = if i = end' then empty else cons i (defer <| iter (i+step))
    iter start ()

  let inline range start end' = rangeStep start 1 end'

  let initInfinite f =
    let rec iter i = cons (f i) (iter (i+1))
    iter 0

  let rec fold (folder: 'State -> 'T -> 'State) (initialState: 'State) (Sequence step) = 
    match step() with
    | Empty -> initialState
    | Cons (x, xs) -> fold folder (folder initialState x) xs

  let rec foldBack (folder: 'T -> 'State -> 'State) seq (initialState: 'State) = 
    let rec r (Sequence step) = 
      match step() with
      | Empty -> fun() -> initialState
      | Cons (x, xs) -> fun() -> folder x (r xs ())
    r seq ()

  let rec take n (Sequence step) = 
    match step() with
    | Cons (x, xs) when n > 0 -> cons x (defer (fun () -> take (n-1) xs))
    | _ -> empty

  let rec skip n (Sequence step) = 
    match step() with
    | Cons (_, xs) when n > 0 -> defer (fun () -> skip (n-1) xs)
    | s -> Sequence (fun () -> s)

  let inline iter (f: 't -> unit) seq = fold (fun () t -> f t) () seq

  let inline iteri (f: 't -> int -> unit) seq = fold (fun idx t -> f t idx; idx+1) 0 seq |> ignore

  let inline init count f = initInfinite f |> take count

  let inline tryHead (Sequence step) = match step() with | Empty -> None | Cons (x, _) -> x

  let inline tryNth idx = skip idx >> tryHead

  let inline length seq = fold (fun c _ -> c+1) 0 seq
  
  let inline toList seq = foldBack List.Cons seq

  let inline toArray seq = 
    let a = Array.create (length seq)
    iteri (fun x idx -> a.[idx] <- x) seq
    a

  let rec chooseFirst (f: 'a -> 'b option) (seq: 'a seq) : ('b * 'a seq) option =
    match tryStep seq with
    | None -> None
    | Some (x, xs) ->
      match f x with
      | Some x -> Some (x, xs)
      | None -> chooseFirst f xs

  let tryPick f seq = chooseFirst f seq |> Option.map fst

  let rec choose (f: 'a -> 'b option) (seq: Seq<'a>) : Seq<'b> =
    Sequence <| fun() ->
      match chooseFirst f seq with
      | Some (x, xs) -> Cons (x, choose f xs)
      | None -> Empty

  let rec filter (f: 'a -> bool) (seq: 'a seq) : 'a seq = 
    let pred x = if f x then Some x else None
    Sequence <| fun() ->
      match chooseFirst pred seq with
      | Some (x, xs) -> Cons (x, filter f xs)
      | None -> Empty

  let rec map (f: 'a -> 'b) (seq: 'a seq) : 'b seq = 
    Sequence <| fun() ->
      match tryStep seq with
      | Some (x, xs) -> Cons (f x, map f xs)
      | None -> Empty

  let rec append a b = 
    Sequence <| fun() ->
      match tryStep a with
      | Some (x, xs) -> Cons (x, append xs b)
      | None -> 
        match tryStep b with
        | Some (x, xs) -> Cons (x, xs)
        | None -> Empty