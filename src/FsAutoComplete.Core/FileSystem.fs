namespace FsAutoComplete

open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System

type VolatileFile =
  {
    Touched: DateTime
    Lines: string []
  }

open System.IO

type FileSystem (actualFs: IFileSystem, getFiles: unit -> Map<string, VolatileFile>) =
    let getFile (filename: string) =
       let filename = Utils.normalizePath filename
       let files = getFiles ()
       Map.tryFind filename files

    let getContent (filename: string) =
        let filename = Utils.normalizePath filename
        match getFile filename with
        | Some d ->
           let bytes = System.Text.Encoding.UTF8.GetBytes (String.Join ("\n", d.Lines))
           Some bytes
        | _ -> None

    let getOrElse f o =
        match o with
        | Some v -> v
        | _      -> f()

    interface IFileSystem with
        member x.FileStreamReadShim fileName =
            getContent fileName
            |> Option.map (fun bytes -> new MemoryStream (bytes) :> Stream)
            |> getOrElse (fun () -> actualFs.FileStreamReadShim fileName)

        member x.ReadAllBytesShim fileName =
            getContent fileName
            |> getOrElse (fun () -> actualFs.ReadAllBytesShim fileName)

        member x.GetLastWriteTimeShim fileName =
            match getFile fileName with
                | Some f -> f.Touched
                | _      -> actualFs.GetLastWriteTimeShim fileName

        member __.GetTempPathShim() = actualFs.GetTempPathShim()
        member __.FileStreamCreateShim fileName = actualFs.FileStreamCreateShim (Utils.normalizePath fileName)
        member __.FileStreamWriteExistingShim fileName = actualFs.FileStreamWriteExistingShim (Utils.normalizePath fileName)
        member __.GetFullPathShim fileName = actualFs.GetFullPathShim (Utils.normalizePath fileName)
        member __.IsInvalidPathShim fileName = actualFs.IsInvalidPathShim (Utils.normalizePath fileName)
        member __.IsPathRootedShim fileName = actualFs.IsPathRootedShim (Utils.normalizePath fileName)
        member __.SafeExists fileName = actualFs.SafeExists (Utils.normalizePath fileName)
        member __.FileDelete fileName = actualFs.FileDelete (Utils.normalizePath fileName)
        member __.AssemblyLoadFrom fileName = actualFs.AssemblyLoadFrom (Utils.normalizePath fileName)
        member __.AssemblyLoad(assemblyName) = actualFs.AssemblyLoad assemblyName
