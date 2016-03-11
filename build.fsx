#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake

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
  |> Fake.Testing.XUnit2.xUnit2 id

"Build" ==> "BuildTests" ==> "RunTests"

RunTargetOrDefault "Build"