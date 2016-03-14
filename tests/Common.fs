[<AutoOpen>]
module Fovel.Tests.Utils
open Xunit
open Fovel
open Fovel.Gen
open FsUnit.Xunit

let compileSources srcs config = 
  match
    fsharpSourcesToShovel config srcs 
    |> Result.mapError Error.formatAll with 
  | OK r -> r
  | Error err -> failwith <| String.concat "\n" err

let compileSource src = compileSources ["file.fs", src]

let patternInputRegex = System.Text.RegularExpressions.Regex("""(?'p'patternInput_\d+)_\d+""")
let replacePatternInputs s = patternInputRegex.Replace(s, System.Text.RegularExpressions.MatchEvaluator( fun m -> m.Groups.["p"].Value ) )

let split lines = (lines:string).Split('\n') |> Seq.map (fun s -> s.Trim()) |> Seq.filter ((<>) "") |> Seq.map replacePatternInputs |> String.concat "\n"

let compileCompareWithConfig config fsharpSource shovelSource = 
  (compileSource fsharpSource config) |> split 
  |> should equal (split shovelSource)

let compileCompare = compileCompareWithConfig Config.WithoutCoreLib

let getErrors = function | OK _ -> [] | Error errs -> errs