namespace FsAutoComplete

open FSharp.Compiler.AbstractIL.Internal.Library
open System

type VolatileFile =
  { Touched: DateTime
    Lines: string []
    Version: int option}

open System.IO

type FileSystem (actualFs: IFileSystem, tryFindFile: SourceFilePath -> VolatileFile option) =
    let normalize = Utils.normalizePath
    let getFile = normalize >> tryFindFile

    let getContent (filename: string) =
        filename
        |> getFile
        |> Option.map (fun file ->
             System.Text.Encoding.UTF8.GetBytes (String.Join ("\n", file.Lines)))

    interface IFileSystem with
        member __.FileStreamReadShim fileName =
            getContent fileName
            |> Option.map (fun bytes -> new MemoryStream (bytes) :> Stream)
            |> Option.getOrElseFun (fun _ -> actualFs.FileStreamReadShim fileName)

        member __.ReadAllBytesShim fileName =
            getContent fileName
            |> Option.getOrElseFun (fun _ -> actualFs.ReadAllBytesShim fileName)

        member __.GetLastWriteTimeShim fileName =
            getFile fileName
            |> Option.map (fun x -> x.Touched)
            |> Option.getOrElseFun (fun _ -> actualFs.GetLastWriteTimeShim fileName)

        member __.GetTempPathShim() = actualFs.GetTempPathShim()
        member __.FileStreamCreateShim file = file |> normalize |> actualFs.FileStreamCreateShim
        member __.FileStreamWriteExistingShim file = file |> normalize |> actualFs.FileStreamWriteExistingShim
        member __.GetFullPathShim file = file |> normalize |> actualFs.GetFullPathShim
        member __.IsInvalidPathShim file = file |> normalize |> actualFs.IsInvalidPathShim
        member __.IsPathRootedShim file = file |> normalize |> actualFs.IsPathRootedShim
        member __.SafeExists file = file |> normalize |> actualFs.SafeExists
        member __.FileDelete file = file |> normalize |> actualFs.FileDelete
        member __.AssemblyLoadFrom file = file |> normalize |> actualFs.AssemblyLoadFrom
        member __.AssemblyLoad assemblyName = actualFs.AssemblyLoad assemblyName
        member __.IsStableFileHeuristic fileName =
            let directory = Path.GetDirectoryName(fileName)
            directory.Contains("Reference Assemblies/") ||
            directory.Contains("Reference Assemblies\\") ||
            directory.Contains("packages/") ||
            directory.Contains("packages\\") ||
            directory.Contains("lib/mono/")
