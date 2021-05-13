namespace FsAutoComplete

open FSharp.Compiler.EditorServices
open FSharp.Compiler.IO
open System
open FsAutoComplete.Logging
open FSharp.UMX
open FSharp.Compiler.Text
open System.Runtime.CompilerServices

type VolatileFile =
  { Touched: DateTime
    Created: DateTime
    Lines: ISourceText
    Version: int option }

open System.IO
open System.Collections.Generic
#nowarn "57"

[<Extension>]
type SourceTextExtensions =
  [<Extension>]
  static member GetText(t: ISourceText, m: FSharp.Compiler.Text.Range): Result<string, string> =
    let allFileRange = Range.mkRange m.FileName Position.pos0 (t.GetLastFilePosition())
    if not (Range.rangeContainsRange allFileRange m)
    then Error $"%A{m} is outside of the bounds of the file"
    else
      if m.StartLine = m.EndLine then // slice of a single line, just do that
        let lineText = t.GetLineString (m.StartLine - 1)
        lineText.Substring(m.StartColumn, m.EndColumn - m.StartColumn) |> Ok
      else
        // multiline, use a builder
        let builder = new System.Text.StringBuilder()
        // slice of the first line
        let firstLine = t.GetLineString (m.StartLine - 1)
        builder.Append (firstLine.Substring(m.StartColumn)) |> ignore<System.Text.StringBuilder>
        // whole intermediate lines
        for line in (m.StartLine + 1)..(m.EndLine - 1) do
          builder.AppendLine (t.GetLineString(line - 1)) |> ignore<System.Text.StringBuilder>
        // final part, potential slice
        let lastLine = t.GetLineString (m.EndLine - 1)
        builder.Append (lastLine.Substring(0, m.EndColumn)) |> ignore<System.Text.StringBuilder>
        Ok (builder.ToString())

  [<Extension>]
  static member inline Lines(t: ISourceText) =
    Array.init (t.GetLineCount()) t.GetLineString

  [<Extension>]
  /// a safe alternative to GetLastCharacterPosition, which returns untagged indexes. this version
  /// returns a FCS Pos to prevent confusion about line index offsets
  static member GetLastFilePosition(t: ISourceText): FSharp.Compiler.Text.Position =
    let endLine, endChar = t.GetLastCharacterPosition()
    Position.mkPos endLine endChar

type FileSystem (actualFs: IFileSystem, tryFindFile: string<LocalPath> -> VolatileFile option, tryRemoveFile: string<LocalPath> -> unit) =
    let getContent (filename: string<LocalPath>) =
         filename
         |> tryFindFile
         |> Option.map (fun file ->
            let bytes = file.Lines.ToString() |> System.Text.Encoding.UTF8.GetBytes
            let byteArrayMemory = if bytes.Length = 0 then ByteArrayMemory([||], 0, 0) else ByteArrayMemory(bytes, 0, bytes.Length)
            byteArrayMemory :> ByteMemory
          )

    let fsLogger = LogProvider.getLoggerByName "FileSystem"
    /// translation of the BCL's Windows logic for Path.IsPathRooted.
    ///
    /// either the first char is '/', or the first char is a drive identifier followed by ':'
    static let isWindowsStyleRootedPath (p: string) =
        let isAlpha (c: char) =
            (c >= 'A' && c <= 'Z')
            || (c >= 'a' && c <= 'z')
        (p.Length >= 1 && p.[0] = '/')
        || (p.Length >= 2 && isAlpha p.[0] && p.[1] = ':')

    /// translation of the BCL's Unix logic for Path.IsRooted.
    ///
    /// if the first character is '/' then the path is rooted
    static let isUnixStyleRootedPath (p: string) =
        p.Length > 0 && p.[0] = '/'

    static let unixInvalidPathChars = [| Unchecked.defaultof<char> |]
    static let windowsInvalidPathChars = [| '|'; Unchecked.defaultof<char>; yield! Array.init 30 (fun i -> char (i + 1)) |]

    static let hasIllegalPathChars (path: string) =
      for c in path do
        if Array.contains c unixInvalidPathChars || Array.contains c windowsInvalidPathChars then raise(IllegalFileNameChar(path, c))

    interface IFileSystem with
        (* for these few members we have to be incredibly careful to root/extend paths in an OS-agnostic way,
           as they handle paths for windows and unix file systems regardless of your host OS.
           Therefore, you cannot use the BCL's Path.IsPathRooted/Path.GetFullPath members *)

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

        member this.NormalizePathShim (path: string) =
            try
                let ifs = this :> IFileSystem
                if ifs.IsPathRootedShim path then
                    ifs.GetFullPathShim path
                else
                    path
            with _ -> path

        member this.GetDirectoryNameShim(s:string) =
            hasIllegalPathChars s
            if s = "" then "."
            else
              match Path.GetDirectoryName(s) with
              | null -> if (this :> IFileSystem).IsPathRootedShim(s) then s else "."
              | res -> if res = "" then "." else res

        (* These next members all make use of the VolatileFile concept, and so need to check that before delegating to the original FS implementation *)

        (* Note that in addition to this behavior, we _also_ do not normalize the file paths anymore for any other members of this interface,
           because these members are always used by the compiler with paths returned from `GetFullPathShim`, which has done the normalization *)
        member _.OpenFileForReadShim(filePath: string, ?useMemoryMappedFile: bool, ?shouldShadowCopy: bool) =
          filePath
          |> Utils.normalizePath
          |> getContent
          |> Option.defaultWith (fun _ -> actualFs.OpenFileForReadShim(filePath, ?useMemoryMappedFile = useMemoryMappedFile, ?shouldShadowCopy = shouldShadowCopy))

        member _.OpenFileForWriteShim (filePath: string, ?fileMode: FileMode, ?fileAccess: FileAccess, ?fileShare: FileShare): Stream =
          filePath
          |> Utils.normalizePath
          |> getContent
          |> Option.map (fun bytes -> bytes.AsStream())
          |> Option.defaultWith (fun _ -> actualFs.OpenFileForWriteShim(filePath, ?fileMode = fileMode, ?fileAccess = fileAccess, ?fileShare = fileShare))

        member _.GetLastWriteTimeShim (f) =
          f
          |> Utils.normalizePath
          |> tryFindFile
          |> Option.map (fun f -> f.Touched)
          |> Option.defaultWith (fun _ -> actualFs.GetLastWriteTimeShim f)

        member _.GetCreationTimeShim(path: string): DateTime =
          path
          |> Utils.normalizePath
          |> tryFindFile
          |> Option.map (fun f -> f.Created)
          |> Option.defaultWith (fun _ -> actualFs.GetCreationTimeShim path)

        member this.GetFullFilePathInDirectoryShim dir fileName: string =
          let p = if (this :> IFileSystem).IsPathRootedShim(fileName) then fileName else Path.Combine(dir, fileName)
          try (this :> IFileSystem).GetFullPathShim(p) with
          | :? ArgumentException
          | :? ArgumentNullException
          | :? NotSupportedException
          | :? PathTooLongException
          | :? System.Security.SecurityException -> p

        member _.FileExistsShim (path) =
          path
          |> Utils.normalizePath
          |> tryFindFile
          |> Option.map (fun _ -> true)
          |> Option.defaultWith (fun _ -> actualFs.FileExistsShim path)

        member _.FileDeleteShim (path) =
          path
          |> Utils.normalizePath
          |> tryRemoveFile

          actualFs.FileDeleteShim path

        (* The rest of these are very simple passthroughs *)
        member _.AssemblyLoader = actualFs.AssemblyLoader
        member _.GetTempPathShim () = actualFs.GetTempPathShim()
        member _.EnumerateFilesShim (path, pattern) = actualFs.EnumerateFilesShim(path, pattern)
        member _.EnumerateDirectoriesShim(path) = actualFs.EnumerateDirectoriesShim(path)
        member _.DirectoryCreateShim(path) = actualFs.DirectoryCreateShim(path)
        member _.DirectoryExistsShim(path) = actualFs.DirectoryExistsShim(path)
        member _.DirectoryDeleteShim(path) = actualFs.DirectoryDeleteShim(path)
        member _.IsStableFileHeuristic (f) = actualFs.IsStableFileHeuristic f
        member _.CopyShim(src, dest, overwrite) = actualFs.CopyShim(src, dest, overwrite)
        member _.IsInvalidPathShim (path) = actualFs.IsInvalidPathShim(path)
