module Fovel.Gen.Symbol
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fovel

let genSymbols program = 
  let name (s: FSharpMemberOrFunctionOrValue) = sanitizeId s.CompiledName
  let indexedName name idx = if idx > 0 then sprintf "%s__%d" name idx else name
  let names = 
    program 
    |> List.collect Binding.allSymbols 
    |> Seq.distinct
    |> Common.uniqueIndexedNames name indexedName
  let getName sym = names.[sym]

  program |> List.map (Binding.mapSymbol getName)