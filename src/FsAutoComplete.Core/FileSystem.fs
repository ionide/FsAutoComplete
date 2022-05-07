namespace FsAutoComplete

open FSharp.Compiler.CodeAnalysis
open System
open FsAutoComplete.Logging
open FSharp.UMX
open FSharp.Compiler.Text
open System.Runtime.CompilerServices
open FsToolkit.ErrorHandling

open System.IO
open FSharp.Compiler.IO

[<AutoOpen>]
module PositionExtensions =
  type FSharp.Compiler.Text.Position with
    member x.LinesToBeginning() =
      if x.Line <= 0 then
        Seq.empty
      else
        seq {
          for i = x.Line - 1 downto 0 do
            yield Position.mkPos i 0
        }

    member x.IncLine() = Position.mkPos (x.Line + 1) x.Column
    member x.DecLine() = Position.mkPos (x.Line - 1) x.Column

  let inline (|Pos|) (p: FSharp.Compiler.Text.Position) = p.Line, p.Column

/// A copy of the StringText type from F#.Compiler.Text, which is private.
/// Adds a UOM-typed filename to make range manipulation easier, as well as
/// safer traversals
[<Sealed>]
type NamedText(fileName: string<LocalPath>, str: string) =

  let getLines (str: string) =
    use reader = new StringReader(str)

    [| let mutable line = reader.ReadLine()

       while not (isNull line) do
         yield line
         line <- reader.ReadLine()

       if str.EndsWith("\n", StringComparison.Ordinal) then
         // last trailing space not returned
         // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
         yield String.Empty |]

  let getLines =
    // This requires allocating and getting all the lines.
    // However, likely whoever is calling it is using a different implementation of ISourceText
    // So, it's ok that we do this for now.
    lazy getLines str

  let lastCharPos =
    lazy
      (let lines = getLines.Value

       if lines.Length > 0 then
         (lines.Length, lines.[lines.Length - 1].Length)
       else
         (0, 0))

  let safeLastCharPos =
    lazy
      (let (endLine, endChar) = lastCharPos.Value
       Position.mkPos endLine endChar)

  member _.String = str

  override _.GetHashCode() = str.GetHashCode()

  override _.Equals(obj: obj) =
    match obj with
    | :? NamedText as other -> other.String.Equals(str)
    | :? string as other -> other.Equals(str)
    | _ -> false

  override _.ToString() = str

  /// The local absolute path of the file whose contents this NamedText represents
  member x.FileName = fileName

  /// The unwrapped local abolute path of the file whose contents this NamedText represents.
  /// Should only be used when interoping with the Compiler/Serialization
  member x.RawFileName = UMX.untag fileName

  /// Cached representation of the final position in this file
  member x.LastFilePosition = safeLastCharPos.Value

  /// Cached representation of the entire contents of the file, for inclusion checks

  member x.TotalRange =
    Range.mkRange (UMX.untag fileName) Position.pos0 x.LastFilePosition

  /// Provides safe access to a substring of the file via FCS-provided Range
  member x.GetText(m: FSharp.Compiler.Text.Range) : Result<string, string> =
    if not (Range.rangeContainsRange x.TotalRange m) then
      Error $"%A{m} is outside of the bounds of the file"
    else if m.StartLine = m.EndLine then // slice of a single line, just do that
      let lineText = (x :> ISourceText).GetLineString(m.StartLine - 1)

      lineText.Substring(m.StartColumn, m.EndColumn - m.StartColumn)
      |> Ok
    else
      // multiline, use a builder
      let builder = new System.Text.StringBuilder()
      // slice of the first line
      let firstLine = (x :> ISourceText).GetLineString(m.StartLine - 1)

      builder.Append(firstLine.Substring(m.StartColumn))
      |> ignore<System.Text.StringBuilder>
      // whole intermediate lines
      for line in (m.StartLine + 1) .. (m.EndLine - 1) do
        builder.AppendLine((x :> ISourceText).GetLineString(line - 1))
        |> ignore<System.Text.StringBuilder>
      // final part, potential slice
      let lastLine = (x :> ISourceText).GetLineString(m.EndLine - 1)

      builder.Append(lastLine.Substring(0, m.EndColumn))
      |> ignore<System.Text.StringBuilder>

      Ok(builder.ToString())

  member private x.GetLineUnsafe(pos: FSharp.Compiler.Text.Position) =
    (x :> ISourceText).GetLineString(pos.Line - 1)

  /// Provides safe access to a line of the file via FCS-provided Position
  member x.GetLine(pos: FSharp.Compiler.Text.Position) : string option =
    if pos.Line > getLines.Value.Length then
      None
    else
      Some(x.GetLineUnsafe pos)

  member x.GetLineLength(pos: FSharp.Compiler.Text.Position) =
    if pos.Line > getLines.Value.Length then
      None
    else
      Some (x.GetLineUnsafe pos).Length

  member x.GetCharUnsafe(pos: FSharp.Compiler.Text.Position) : char = x.GetLine(pos).Value[pos.Column - 1]

  /// <summary>Provides safe access to a character of the file via FCS-provided Position.
  /// Also available in indexer form: <code lang="fsharp">x[pos]</code></summary>
  member x.TryGetChar(pos: FSharp.Compiler.Text.Position) : char option =
    option {
      do! Option.guard (Range.rangeContainsPos (x.TotalRange) pos)
      let lineText = x.GetLineUnsafe(pos)

      if pos.Column = 0 then
        return! None
      else
        let lineIndex = pos.Column - 1

        if lineText.Length <= lineIndex then
          return! None
        else
          return lineText[lineIndex]
    }

  member x.NextLine(pos: FSharp.Compiler.Text.Position) =
    if pos.Line < getLines.Value.Length then
      Position.mkPos (pos.Line + 1) 0 |> Some
    else
      None

  /// Provides safe incrementing of a position in the file via FCS-provided Position
  member x.NextPos(pos: FSharp.Compiler.Text.Position) : FSharp.Compiler.Text.Position option =
    option {
      let! currentLine = x.GetLine pos

      if pos.Column - 1 = currentLine.Length then
        if getLines.Value.Length > pos.Line then
          // advance to the beginning of the next line
          return Position.mkPos (pos.Line + 1) 0
        else
          return! None
      else
        return Position.mkPos pos.Line (pos.Column + 1)
    }

  /// Provides safe incrementing of positions in a file while returning the character at the new position.
  /// Intended use is for traversal loops.
  member x.TryGetNextChar(pos: FSharp.Compiler.Text.Position) : (FSharp.Compiler.Text.Position * char) option =
    option {
      let! np = x.NextPos pos
      return np, x.GetCharUnsafe np
    }

  /// Provides safe decrementing of a position in the file via FCS-provided Position
  member x.PrevPos(pos: FSharp.Compiler.Text.Position) : FSharp.Compiler.Text.Position option =
    option {
      if pos.Column <> 0 then
        return Position.mkPos pos.Line (pos.Column - 1)
      else if pos.Line = 0 then
        return! None
      else if getLines.Value.Length > pos.Line - 2 then
        let prevLine = (x :> ISourceText).GetLineString(pos.Line - 2)
        // retreat to the end of the previous line
        return Position.mkPos (pos.Line - 1) (prevLine.Length - 1)
      else
        return! None
    }

  /// Provides safe decrementing of positions in a file while returning the character at the new position.
  /// Intended use is for traversal loops.
  member x.TryGetPrevChar(pos: FSharp.Compiler.Text.Position) : (FSharp.Compiler.Text.Position * char) option =
    option {
      let! np = x.PrevPos pos
      return np, x.GetCharUnsafe np
    }

  /// Safe access to the contents of a file by Range
  member x.Item
    with get (m: FSharp.Compiler.Text.Range) = x.GetText(m)

  /// Safe access to the char in a file by Position
  member x.Item
    with get (pos: FSharp.Compiler.Text.Position) = x.TryGetChar(pos)

  member private x.Walk
    (
      start: FSharp.Compiler.Text.Position,
      (posChange: FSharp.Compiler.Text.Position -> FSharp.Compiler.Text.Position option),
      terminal,
      condition
    ) =
    /// if the condition is never met, return None

    let firstPos = Position.pos0
    let finalPos = x.LastFilePosition

    let rec loop (pos: FSharp.Compiler.Text.Position) : FSharp.Compiler.Text.Position option =
      option {
        let! charAt = x[pos]
        do! Option.guard (firstPos <> pos && finalPos <> pos)
        do! Option.guard (not (terminal charAt))

        if condition charAt then
          return pos
        else
          let! nextPos = posChange pos
          return! loop nextPos
      }

    loop start

  member x.WalkForward(start, terminal, condition) =
    x.Walk(start, x.NextPos, terminal, condition)

  member x.WalkBackwards(start, terminal, condition) =
    x.Walk(start, x.PrevPos, terminal, condition)


  /// Provides line-by-line access to the underlying text.
  /// This can lead to unsafe access patterns, consider using one of the range or position-based
  /// accessors instead
  member x.Lines = getLines.Value

  interface ISourceText with

    member _.Item
      with get index = str.[index]

    member _.GetLastCharacterPosition() = lastCharPos.Value

    member _.GetLineString(lineIndex) = getLines.Value.[lineIndex]

    member _.GetLineCount() = getLines.Value.Length

    member _.GetSubTextString(start, length) = str.Substring(start, length)

    member _.SubTextEquals(target, startIndex) =
      if startIndex < 0 || startIndex >= str.Length then
        invalidArg "startIndex" "Out of range."

      if String.IsNullOrEmpty(target) then
        invalidArg "target" "Is null or empty."

      let lastIndex = startIndex + target.Length

      if lastIndex <= startIndex || lastIndex >= str.Length then
        invalidArg "target" "Too big."

      str.IndexOf(target, startIndex, target.Length)
      <> -1

    member _.Length = str.Length

    member this.ContentEquals(sourceText) =
      match sourceText with
      | :? NamedText as sourceText when sourceText = this || sourceText.String = str -> true
      | _ -> false

    member _.CopyTo(sourceIndex, destination, destinationIndex, count) =
      str.CopyTo(sourceIndex, destination, destinationIndex, count)

type VolatileFile =
  { Touched: DateTime
    Lines: NamedText
    Version: int option }

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
      let expanded = Path.FilePathToUri f |> Path.FileUriToLocalPath

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
