module Fovel.FSCompiler
open System.IO
open System
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.SourceCodeServices

let makeFileSystem (getFile: string -> byte[] option) (log: string -> obj -> unit) = 
  let time = DateTime.Now
  let ni() = failwith "not implemented"
  { new IFileSystem with
    member x.AssemblyLoad n = log "AssemblyLoad" n; ni()
    member x.AssemblyLoadFrom n = log "AssemblyLoadFrom" n; ni()
    member x.FileDelete n = log "FileDelete" n; ni()
    member x.FileStreamCreateShim n = log "FileStreamCreateShim" n; ni()
             
    member x.FileStreamReadShim fileName = 
      log "FileStreamReadShim" fileName
      match getFile fileName with
      | Some contents -> new MemoryStream( contents ) :> Stream
      | None -> null
             
    member x.FileStreamWriteExistingShim n = log "FileStreamWriteExistingShim" n; ni()
    member x.GetFullPathShim fileName = log "GetFullPathShim" fileName; fileName
    member x.GetLastWriteTimeShim n = log "GetLastWriteTimeShim" n; time
    member x.GetTempPathShim() = log "GetTempPathShim" ""; @"c:\work\fstemp"
    member x.IsInvalidPathShim n = log "IsInvalidPathShim" n; false
    member x.IsPathRootedShim n = log "IsPathRootedShim" n; false

    member x.ReadAllBytesShim(fileName: string): byte [] = 
      log "ReadAllBytesShim" fileName
      match getFile fileName with
      | Some contents -> contents
      | None -> 
        try
          File.ReadAllBytes fileName
        with _ -> null
             
    member x.SafeExists fileName = 
      log "SafeExists" fileName
      (getFile fileName |> Option.isSome) || (File.Exists fileName)
  }

let parseProgram source =
  let sourceFileName = "x.fsx"
  let checker = FSharpChecker.Create(keepAssemblyContents=true)
  let opts = {
    FSharpProjectOptions.IsIncompleteTypeCheckEnvironment = false
    ProjectFileName = "project.fsproj"
    ProjectFileNames = [|sourceFileName|]
    OtherOptions = [|"--noframework"; "--out:x.dll"; "--target:library"|]
    ReferencedProjects = [||]
    UseScriptResolutionRules = false
    LoadTime = DateTime.Now
    UnresolvedReferences = None }

  let rec flattenDecls = 
    function
    | FSharpImplementationFileDeclaration.Entity (_, subs) -> subs |> List.map flattenDecls |> List.concat
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (v, vs, expr) -> [(Some (v, vs), expr)]
    | FSharpImplementationFileDeclaration.InitAction expr -> [(None, expr)]

  let sourceBytes = System.Text.Encoding.UTF8.GetBytes( source: string )

  let output = new StringWriter()
  let getFile name = if System.String.Compare( name, sourceFileName, true ) = 0 then Some sourceBytes else None
  Shim.FileSystem <- makeFileSystem getFile (fun _ _ -> ()) //(fun s o -> output.WriteLine( sprintf "%s %A" s o ))

  let res = checker.ParseAndCheckProject( opts ) |> Async.RunSynchronously

  res.AssemblyContents.ImplementationFiles 
  |> Seq.collect (fun f -> f.Declarations |> Seq.map flattenDecls)
  |> Seq.concat 
  |> List.ofSeq
