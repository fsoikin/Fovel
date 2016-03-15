#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Testing.XUnit2

let config = getBuildParamOrDefault "Config" "Debug"

Target "Build" <| fun _ ->
  ["src/Fovel.fsproj"]
  |> MSBuild "" "Build" ["Configuration", config]
  |> Log "Build: "

Target "BuildTests" <| fun _ ->
  ["tests/Fovel.Tests.fsproj"]
  |> MSBuild "" "Build" ["Configuration", config]
  |> Log "Build tests: "

Target "Clean" <| fun _ ->
  !! "**/*.fsproj"
  |> MSBuild "" "Clean" ["Configuration", config]
  |> Log "Build tests: "

Target "RunTests" <| fun _ ->
  [sprintf "tests/bin/%s/Fovel.Tests.dll" config]
  |> xUnit2 (fun p -> { p with MaxThreads = MaxThreads 1 })

"Build" ==> "BuildTests" ==> "RunTests"

RunTargetOrDefault "Build"