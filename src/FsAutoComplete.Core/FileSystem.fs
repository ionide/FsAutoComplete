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
    Lines: ISourceText
    Version: int option }

open System.IO


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

type FileSystem (actualFs: IFileSystem, tryFindFile: string<LocalPath> -> VolatileFile option) =
    let getContent (filename: string<LocalPath>) =
         filename
         |> tryFindFile
         |> Option.map (fun file -> file.Lines.ToString() |> System.Text.Encoding.UTF8.GetBytes)

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
        (* for these two members we have to be incredibly careful to root/extend paths in an OS-agnostic way,
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

        (* These next members all make use of the VolatileFile concept, and so need to check that before delegating to the original FS implementation *)

        (* Note that in addition to this behavior, we _also_ do not normalize the file paths anymore for any other members of this interfact,
           because these members are always used by the compiler with paths returned from `GetFullPathShim`, which has done the normalization *)

        member _.ReadAllBytesShim (f) =
          f
          |> Utils.normalizePath
          |> getContent
          |> Option.defaultWith (fun _ -> actualFs.ReadAllBytesShim f)

        member _.FileStreamReadShim (f) =
          f
          |> Utils.normalizePath
          |> getContent
          |> Option.map (fun bytes -> new MemoryStream(bytes) :> Stream)
          |> Option.defaultWith (fun _ -> actualFs.FileStreamReadShim f)

        member _.GetLastWriteTimeShim (f) =
          f
          |> Utils.normalizePath
          |> tryFindFile
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
