module Fovel.Validation
open Fovel
open Fovel.Gen

let noReferencesToExternalSymbols program =
  let ensureBinding = 
    Binding.referencedExternalSymbols 
    >> Result.seqMap (Result.fail << Error.CannotReferenceExternalSymbol)
  program |> Result.seqMap ensureBinding

let allValidations program =
  noReferencesToExternalSymbols program
  |*> (fun _ -> program)