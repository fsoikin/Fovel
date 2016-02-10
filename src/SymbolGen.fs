module Fovel.Gen.Symbol
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fovel

let genSymbols program = 
  let key (s: FSharpMemberOrFunctionOrValue) = s.DeclarationLocation, s.LogicalName
  let keyName s = key s, sanitizeId s.CompiledName
  let indexedName name idx = if idx > 0 then sprintf "%s__%d" name idx else name
  let names = 
    program 
    |> List.collect Binding.allSymbols 
    |> Seq.map keyName
    |> Seq.distinctBy fst
    |> Common.uniqueIndexedNames fst snd indexedName
  let getName sym = names.[key sym]

  program |> List.map (Binding.mapSymbol getName)