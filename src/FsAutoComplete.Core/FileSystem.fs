namespace FsAutoComplete

open FSharp.Compiler.CodeAnalysis
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
open FSharp.Compiler.IO


[<Extension>]
type SourceTextExtensions =
  [<Extension>]
  static member GetText(t: ISourceText, m: FSharp.Compiler.Text.Range) : Result<string, string> =
    let allFileRange =
      Range.mkRange m.FileName Position.pos0 (t.GetLastFilePosition())

    if not (Range.rangeContainsRange allFileRange m) then
      Error $"%A{m} is outside of the bounds of the file"
    else if m.StartLine = m.EndLine then // slice of a single line, just do that
      let lineText = t.GetLineString(m.StartLine - 1)

      lineText.Substring(m.StartColumn, m.EndColumn - m.StartColumn)
      |> Ok
    else
      // multiline, use a builder
      let builder = new System.Text.StringBuilder()
      // slice of the first line
      let firstLine = t.GetLineString(m.StartLine - 1)

      builder.Append(firstLine.Substring(m.StartColumn))
      |> ignore<System.Text.StringBuilder>
      // whole intermediate lines
      for line in (m.StartLine + 1) .. (m.EndLine - 1) do
        builder.AppendLine(t.GetLineString(line - 1))
        |> ignore<System.Text.StringBuilder>
      // final part, potential slice
      let lastLine = t.GetLineString(m.EndLine - 1)

      builder.Append(lastLine.Substring(0, m.EndColumn))
      |> ignore<System.Text.StringBuilder>

      Ok(builder.ToString())

  [<Extension>]
  static member inline Lines(t: ISourceText) =
    Array.init (t.GetLineCount()) t.GetLineString

  [<Extension>]
  /// a safe alternative to GetLastCharacterPosition, which returns untagged indexes. this version
  /// returns a FCS Pos to prevent confusion about line index offsets
  static member GetLastFilePosition(t: ISourceText) : Position =
    let endLine, endChar = t.GetLastCharacterPosition()
    Position.mkPos endLine endChar

type FileSystem(actualFs: IFileSystem, tryFindFile: string<LocalPath> -> VolatileFile option) =
  let fsLogger = LogProvider.getLoggerByName "FileSystem"

  let getContent (filename: string<LocalPath>) =
    fsLogger.debug (
        Log.setMessage "Getting content of `{path}`"
        >> Log.addContext "path" filename
      )
    filename
    |> tryFindFile
    |> Option.map (fun file ->
      file.Lines.ToString()
      |> System.Text.Encoding.UTF8.GetBytes)

  /// translation of the BCL's Windows logic for Path.IsPathRooted.
  ///
  /// either the first char is '/', or the first char is a drive identifier followed by ':'
  let isWindowsStyleRootedPath (p: string) =
    let isAlpha (c: char) =
      (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

    (p.Length >= 1 && p.[0] = '/')
    || (p.Length >= 2 && isAlpha p.[0] && p.[1] = ':')

  /// translation of the BCL's Unix logic for Path.IsRooted.
  ///
  /// if the first character is '/' then the path is rooted
  let isUnixStyleRootedPath (p: string) = p.Length > 0 && p.[0] = '/'

  interface IFileSystem with
    (* for these two members we have to be incredibly careful to root/extend paths in an OS-agnostic way,
    as they handle paths for windows and unix file systems regardless of your host OS.
    Therefore, you cannot use the BCL's Path.IsPathRooted/Path.GetFullPath members *)

    member _.IsPathRootedShim(p: string) =
      let r =
        isWindowsStyleRootedPath p
        || isUnixStyleRootedPath p

      fsLogger.debug (
        Log.setMessage "Is {path} rooted? {result}"
        >> Log.addContext "path" p
        >> Log.addContext "result" r
      )

      r

    member _.GetFullPathShim(f: string) =
      let expanded =
        Path.FilePathToUri f |> Path.FileUriToLocalPath

      fsLogger.debug (
        Log.setMessage "{path} expanded to {expanded}"
        >> Log.addContext "path" f
        >> Log.addContext "expanded" expanded
      )

      expanded

    member _.GetLastWriteTimeShim(f: string) =
      f
      |> Utils.normalizePath
      |> tryFindFile
      |> Option.map (fun f -> f.Touched)
      |> Option.defaultValue DateTime.MinValue

    member _.NormalizePathShim(f: string) = f |> Utils.normalizePath |> UMX.untag

    member _.IsInvalidPathShim(f) = actualFs.IsInvalidPathShim f
    member _.GetTempPathShim() = actualFs.GetTempPathShim()
    member _.IsStableFileHeuristic(f) = actualFs.IsStableFileHeuristic f
    member _.CopyShim(src, dest, o) = actualFs.CopyShim(src, dest, o)
    member _.DirectoryCreateShim p = actualFs.DirectoryCreateShim p
    member _.DirectoryDeleteShim p = actualFs.DirectoryDeleteShim p
    member _.DirectoryExistsShim p = actualFs.DirectoryExistsShim p
    member _.EnumerateDirectoriesShim p = actualFs.EnumerateDirectoriesShim p
    member _.EnumerateFilesShim(p, pat) = actualFs.EnumerateFilesShim(p, pat)
    member _.FileDeleteShim f = actualFs.FileDeleteShim f
    member _.FileExistsShim f = actualFs.FileExistsShim f
    member _.GetCreationTimeShim p = actualFs.GetCreationTimeShim p
    member _.GetDirectoryNameShim p = actualFs.GetDirectoryNameShim p

    member _.GetFullFilePathInDirectoryShim dir f =
      actualFs.GetFullFilePathInDirectoryShim dir f

    member _.OpenFileForReadShim(filePath: string, useMemoryMappedFile, shouldShadowCopy) =
      filePath
      |> Utils.normalizePath
      |> getContent
      |> Option.map (fun bytes -> new MemoryStream(bytes) :> Stream)
      |> Option.defaultWith (fun _ ->
        actualFs.OpenFileForReadShim(
          filePath,
          ?useMemoryMappedFile = useMemoryMappedFile,
          ?shouldShadowCopy = shouldShadowCopy
        ))

    member _.OpenFileForWriteShim(filePath: string, fileMode, fileAccess, fileShare) =
      actualFs.OpenFileForWriteShim(filePath, ?fileMode = fileMode, ?fileAccess = fileAccess, ?fileShare = fileShare)

    member _.AssemblyLoader = actualFs.AssemblyLoader
