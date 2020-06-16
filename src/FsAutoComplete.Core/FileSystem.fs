namespace FsAutoComplete

open FSharp.Compiler.AbstractIL.Internal.Library
open System
open FsAutoComplete.Logging

type VolatileFile =
  { Touched: DateTime
    Lines: string []
    Version: int option}

open System.IO

type FileSystem (actualFs: IFileSystem, tryFindFile: SourceFilePath -> VolatileFile option) =
    let getContent (filename: string) =
         filename
         |> tryFindFile
         |> Option.map (fun file ->
              System.Text.Encoding.UTF8.GetBytes (String.Join ("\n", file.Lines)))

    let fsLogger = LogProvider.getLoggerByName "FileSystem"
    /// translation of the BCL's Windows logic for Path.IsPathRooted.
    ///
    /// either the first char is '/', or the first char is a drive identifier followed by ':'
    let isWindowsStyleRootedPath (p: string) =
        let isAlpha (c: char) =
            (c >= 'A' && c <= 'Z')
            || (c >= 'a' && c <= 'z')
        (p.Length >= 1 && p.[0] = '/')
        || (p.Length >= 2 && isAlpha p.[0] && p.[1] = ':')

    /// translation of the BCL's Unix logic for Path.IsRooted.
    ///
    /// if the first character is '/' then the path is rooted
    let isUnixStyleRootedPath (p: string) =
        p.Length > 0 && p.[0] = '/'

    interface IFileSystem with
        member _.IsPathRootedShim (p: string) =
          let r =
            isWindowsStyleRootedPath p
            || isUnixStyleRootedPath p
          fsLogger.debug (Log.setMessage "Is {path} rooted? {result}" >> Log.addContext "path" p >> Log.addContext "result" r)
          r

        member _.GetFullPathShim (f: string) =
          let expanded =
            Path.FilePathToUri f
            |> Path.FileUriToLocalPath
          fsLogger.debug (Log.setMessage "{path} expanded to {expanded}" >> Log.addContext "path" f >> Log.addContext "expanded" expanded)
          expanded

              // delegate all others
        member _.ReadAllBytesShim (f) =
          getContent f
          |> Option.defaultWith (fun _ -> actualFs.ReadAllBytesShim f)

        member _.FileStreamReadShim (f) =
          getContent f
          |> Option.map (fun bytes -> new MemoryStream(bytes) :> Stream)
          |> Option.defaultWith (fun _ -> actualFs.FileStreamReadShim f)

        member _.GetLastWriteTimeShim (f) =
          tryFindFile f
          |> Option.map (fun f -> f.Touched)
          |> Option.defaultWith (fun _ -> actualFs.GetLastWriteTimeShim f)

        member _.FileStreamCreateShim (f) = actualFs.FileStreamCreateShim f
        member _.FileStreamWriteExistingShim (f) = actualFs.FileStreamWriteExistingShim f
        member _.IsInvalidPathShim (f) = actualFs.IsInvalidPathShim f
        member _.GetTempPathShim () = actualFs.GetTempPathShim()
        member _.SafeExists (f) = actualFs.SafeExists f
        member _.FileDelete (f) = actualFs.FileDelete f
        member _.AssemblyLoadFrom (f) = actualFs.AssemblyLoadFrom f
        member _.AssemblyLoad (f) = actualFs.AssemblyLoad f
        member _.IsStableFileHeuristic (f) = actualFs.IsStableFileHeuristic f
