module Fovel.Gen.PrettyPrint
open Fovel
open CodeGen

let rec toStringSeq indent = function
  | Text code -> Seq.singleton code
  | Sequence xs -> Seq.collect (toStringSeq indent) xs
  
  | NewLine content 
  | NewLine (NewLine content) -> 
    seq {
      yield "\n"; 
      yield! Seq.replicate indent "\t"
      yield! toStringSeq indent content }

  | Indent content
  | Indent (Indent content)
  | Indent (Indent (Indent content)) -> toStringSeq (indent+1) content

let print = toStringSeq 0