module Fovel.Validation
open Fovel
open Fovel.Gen

let noReferencesToExternalSymbols program =
  let ensureBinding = 
    Binding.referencedExternalSymbols 
    >> Result.seqMap (Result.fail << Error.CannotReferenceExternalSymbol)
  program |> Result.seqMap ensureBinding

//let resolvedTraitCalls program =
//  let rec traitCall = 
//    Binding.referencedExternalSymbols 
//    >> Result.seqMap (Result.fail << Error.CannotReferenceExternalSymbol)
//  program |> Result.seqMap ensureBinding

let applyAll program =
  noReferencesToExternalSymbols program
  |*> (fun _ -> program)