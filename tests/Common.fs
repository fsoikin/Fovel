[<AutoOpen>]
module Fovel.Tests.Utils
open Xunit
open Fovel
open Fovel.Gen
open FsUnit.Xunit
open Swensen.Unquote

let compileSources srcs config = 
  match
    fsharpSourcesToShovel config srcs 
    |> Result.mapError Error.formatAll with 
  | OK r -> r
  | Error err -> failwith <| String.concat "\n" err

let compileSource src = compileSources ["file.fs", src]

let patternInputRegex = System.Text.RegularExpressions.Regex("""(?'p'patternInput_\d+)_\d+""")
let replacePatternInputs s = patternInputRegex.Replace(s, System.Text.RegularExpressions.MatchEvaluator( fun m -> m.Groups.["p"].Value ) )

let normalizeLines lines = 
  (lines:string).Split('\n') 
  |> Seq.map (fun s -> s.Trim()) 
  |> Seq.filter ((<>) "") 
  |> Seq.map replacePatternInputs 
  |> String.concat "\n"
  
let coreLibCompiled = compileSources [] Config.Default |> normalizeLines

let compileCompareWithConfig config shovelPrefix fsharpSource shovelSource = 
  let expected = normalizeLines shovelSource
  let actual = normalizeLines (compileSource fsharpSource config)
  test <@ actual.StartsWith shovelPrefix @>
  (actual.Substring shovelPrefix.Length).Trim() =! expected

let compileCompare = compileCompareWithConfig Config.WithoutCoreLib ""
let compileCompareWithCoreLib fsharpSource shovelSource = compileCompareWithConfig Config.Default coreLibCompiled fsharpSource shovelSource

let getErrors = function | OK _ -> [] | Error errs -> errs