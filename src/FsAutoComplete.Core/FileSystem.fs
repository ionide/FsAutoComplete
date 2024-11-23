namespace FsAutoComplete

open System
open FsAutoComplete.Logging
open FSharp.UMX
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling

open System.IO
open FSharp.Compiler.IO
open System.Threading.Tasks
open IcedTasks


module File =
  let getLastWriteTimeOrDefaultNow (path: string<LocalPath>) =
    let path = UMX.untag path

    if File.Exists path then
      File.GetLastWriteTimeUtc path
    else
      DateTime.UtcNow

  /// Buffer size for reading from the stream.
  /// 81,920 bytes (80KB) is below the Large Object Heap threshold (85,000 bytes)
  /// and is a good size for performance. Dotnet uses this for their defaults.
  [<Literal>]
  let bufferSize = 81920

  let openFileStreamForReadingAsync (path: string<LocalPath>) =
    new FileStream(
      (UMX.untag path),
      FileMode.Open,
      FileAccess.Read,
      FileShare.Read,
      bufferSize = bufferSize,
      useAsync = true
    )

[<AutoOpen>]
module PositionExtensions =
  type FSharp.Compiler.Text.Position with

    /// Excluding current line
    member x.LinesToBeginning() =
      if x.Line <= 1 then
        Seq.empty
      else
        seq {
          for i = x.Line - 1 downto 1 do
            yield Position.mkPos i 0
        }

    member x.IncLine() = Position.mkPos (x.Line + 1) x.Column
    member x.DecLine() = Position.mkPos (x.Line - 1) x.Column
    member x.IncColumn() = Position.mkPos x.Line (x.Column + 1)
    member x.IncColumn n = Position.mkPos x.Line (x.Column + n)

    member inline p.WithColumn(col) = Position.mkPos p.Line col

  let inline (|Pos|) (p: FSharp.Compiler.Text.Position) = p.Line, p.Column

[<AutoOpen>]
module RangeExtensions =
  type FSharp.Compiler.Text.Range with

    member x.WithFileName(fileName: string) = Range.mkRange fileName x.Start x.End

    /// the checker gives us back wacky ranges sometimes, so what we're going to do is check if the text of the triggering
    /// symbol use is in each of the ranges we plan to rename, and if we're looking at a range that is _longer_ than our rename range,
    /// do some splicing to find just the range we need to replace.
    /// TODO: figure out where the caps are coming from in the compilation, maybe something wrong in the
    member x.NormalizeDriveLetterCasing() =
      if System.Char.IsUpper(x.FileName[0]) then
        // we've got a case where the compiler is reading things from the file system that we'd rather it not -
        // if we're adjusting the range's filename, we need to construct a whole new range or else indexing won't work
        let fileName =
          string (System.Char.ToLowerInvariant x.FileName[0]) + (x.FileName.Substring(1))

        let newRange = Range.mkRange fileName x.Start x.End
        newRange
      else
        x

    /// utility method to get the tagged filename for use in our state storage
    /// TODO: should we enforce this/use the Path members for normalization?
    member x.TaggedFileName: string<LocalPath> = Utils.normalizePath x.FileName

    member inline r.With(start, fin) = Range.mkRange r.FileName start fin
    member inline r.WithStart(start) = Range.mkRange r.FileName start r.End
    member inline r.WithEnd(fin) = Range.mkRange r.FileName r.Start fin

    member inline range.ToRoslynTextSpan(sourceText: Microsoft.CodeAnalysis.Text.SourceText) =

      let startPosition =
        sourceText.Lines.[max 0 (range.StartLine - 1)].Start + range.StartColumn

      let endPosition =
        sourceText.Lines.[min (range.EndLine - 1) (sourceText.Lines.Count - 1)].Start
        + range.EndColumn

      Microsoft.CodeAnalysis.Text.TextSpan(startPosition, endPosition - startPosition)

/// A SourceText with operations commonly used in FsAutocomplete
type IFSACSourceText =
  abstract member String: string
  /// The local absolute path of the file whose contents this IFSACSourceText represents
  abstract member FileName: string<LocalPath>
  /// The unwrapped local absolute path of the file whose contents this IFSACSourceText represents.
  /// Should only be used when interoperating with the Compiler/Serialization
  abstract member RawFileName: string
  /// Representation of the final position in this file
  abstract member LastFilePosition: Position
  /// Representation of the entire contents of the file, for inclusion checks
  abstract member TotalRange: Range
  /// Provides line-by-line access to the underlying text.
  /// This can lead to unsafe access patterns, consider using one of the range or position-based
  /// accessors instead
  abstract member Lines: string array
  /// Provides safe access to a substring of the file via FCS-provided Range
  abstract member GetText: range: Range -> Result<string, string>
  /// Provides safe access to a line of the file via FCS-provided Position
  abstract member GetLine: position: Position -> option<string>
  /// Provide safe access to the length of a line of the file via FCS-provided Position
  abstract member GetLineLength: position: Position -> option<uint32>
  abstract member GetCharUnsafe: position: Position -> char
  /// <summary>Provides safe access to a character of the file via FCS-provided Position.
  /// Also available in indexer form: <code lang="fsharp">x[pos]</code></summary>
  abstract member TryGetChar: position: Position -> option<char>
  /// Provides safe incrementing of a lien in the file via FCS-provided Position
  abstract member NextLine: position: Position -> option<Position>
  /// Provides safe incrementing of a position in the file via FCS-provided Position
  abstract member NextPos: position: Position -> option<Position>
  /// Provides safe incrementing of positions in a file while returning the character at the new position.
  /// Intended use is for traversal loops.
  abstract member TryGetNextChar: position: Position -> option<Position * char>
  /// Provides safe decrementing of a position in the file via FCS-provided Position
  abstract member PrevPos: position: Position -> option<Position>
  /// Provides safe decrementing of positions in a file while returning the character at the new position.
  /// Intended use is for traversal loops.
  abstract member TryGetPrevChar: position: Position -> option<Position * char>
  /// create a new IFSACSourceText for this file with the given text inserted at the given range.
  abstract member ModifyText: range: Range * text: string -> IFSACSourceText
  /// Safe access to the char in a file by Position
  abstract Item: index: Position -> option<char> with get
  /// Safe access to the contents of a file by Range
  abstract Item: index: Range -> Result<string, string> with get

  abstract member WalkForward:
    position: Position * terminal: (char -> bool) * condition: (char -> bool) -> option<Position>

  abstract member WalkBackwards:
    position: Position * terminal: (char -> bool) * condition: (char -> bool) -> option<Position>

  inherit ISourceText
  inherit ISourceTextNew

module RoslynSourceText =
  open Microsoft.CodeAnalysis.Text

  /// Ported from Roslyn.Utilities
  [<RequireQualifiedAccess>]
  module Hash =
    /// (From Roslyn) This is how VB Anonymous Types combine hash values for fields.
    let combine (newKey: int) (currentKey: int) = (currentKey * (int 0xA5555529)) + newKey

    let combineValues (values: seq<'T>) =
      (0, values) ||> Seq.fold (fun hash value -> combine (value.GetHashCode()) hash)

  type RoslynSourceTextFile(fileName: string<LocalPath>, sourceText: SourceText) =

    let walk
      (
        x: IFSACSourceText,
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


    let totalLinesLength () = sourceText.Lines |> Seq.length


    override _.ToString() = sourceText.ToString()

    override _.Equals(x) = sourceText.Equals(x)

    override _.GetHashCode() =
      let checksum = sourceText.GetChecksum()

      let contentsHash =
        if not checksum.IsDefault then
          Hash.combineValues checksum
        else
          0

      let encodingHash =
        if not (isNull sourceText.Encoding) then
          sourceText.Encoding.GetHashCode()
        else
          0

      sourceText.ChecksumAlgorithm.GetHashCode()
      |> Hash.combine encodingHash
      |> Hash.combine contentsHash
      |> Hash.combine sourceText.Length

    interface IFSACSourceText with

      member x.Item
        with get (index: Range): Result<string, string> = (x :> IFSACSourceText).GetText(index)

      member x.Item
        with get (index: Position): char option = (x :> IFSACSourceText).TryGetChar(index)

      member x.WalkBackwards(start: Position, terminal: char -> bool, condition: char -> bool) : Position option =
        walk (x, start, (x :> IFSACSourceText).PrevPos, terminal, condition)

      member x.WalkForward(start: Position, terminal: char -> bool, condition: char -> bool) : Position option =
        walk (x, start, (x :> IFSACSourceText).NextPos, terminal, condition)

      member x.String: string = sourceText.ToString()
      member x.FileName: string<LocalPath> = fileName
      member x.RawFileName: string = UMX.untag fileName

      member x.LastFilePosition: Position =
        let endLine, endChar = (x :> ISourceText).GetLastCharacterPosition()
        Position.mkPos endLine endChar

      member x.TotalRange: Range =
        (Range.mkRange (UMX.untag fileName) Position.pos0 ((x :> IFSACSourceText).LastFilePosition))

      member x.Lines: string array =
        sourceText.Lines |> Seq.toArray |> Array.map (fun l -> l.ToString())

      member this.GetText(range: Range) : Result<string, string> =
        range.ToRoslynTextSpan(sourceText) |> sourceText.GetSubText |> string |> Ok

      member x.GetLine(pos: Position) : string option =
        if pos.Line < 1 || pos.Line > totalLinesLength () then
          None
        else
          Some((x :> ISourceText).GetLineString(pos.Line - 1))

      member x.GetLineLength(pos: Position) : uint32 option =
        if pos.Line > totalLinesLength () then
          None
        else
          Some(uint32 ((x :> ISourceText).GetLineString(pos.Line - 1).Length))

      member x.GetCharUnsafe(pos: Position) : char = (x :> IFSACSourceText).GetLine(pos).Value[pos.Column - 1]

      member x.TryGetChar(pos: Position) : char option =
        option {
          do! Option.guard (Range.rangeContainsPos ((x :> IFSACSourceText).TotalRange) pos)

          if pos.Column = 0 then
            return! None
          else
            let lineIndex = pos.Column - 1
            let! lineText = (x :> IFSACSourceText).GetLine(pos)

            if lineText.Length <= lineIndex then
              return! None
            else
              return lineText[lineIndex]
        }

      member this.NextLine(pos: Position) : Position option =
        if pos.Line < totalLinesLength () then
          Position.mkPos (pos.Line + 1) 0 |> Some
        else
          None

      member x.NextPos(pos: Position) : Position option =
        option {
          let! currentLine = (x :> IFSACSourceText).GetLine pos

          if pos.Column - 1 = currentLine.Length then
            if totalLinesLength () > pos.Line then
              // advance to the beginning of the next line
              return Position.mkPos (pos.Line + 1) 0
            else
              return! None
          else
            return Position.mkPos pos.Line (pos.Column + 1)
        }

      member x.TryGetNextChar(pos: Position) : (Position * char) option =
        option {
          let! np = (x :> IFSACSourceText).NextPos pos
          return np, (x :> IFSACSourceText).GetCharUnsafe np
        }

      member x.PrevPos(pos: Position) : Position option =
        option {
          if pos.Column <> 0 then
            return Position.mkPos pos.Line (pos.Column - 1)
          else if pos.Line <= 1 then
            return! None
          else if totalLinesLength () > pos.Line - 2 then
            let prevLine = (x :> ISourceText).GetLineString(pos.Line - 2)
            // retreat to the end of the previous line
            return Position.mkPos (pos.Line - 1) (prevLine.Length - 1)
          else
            return! None
        }

      member x.TryGetPrevChar(pos: FSharp.Compiler.Text.Position) : (FSharp.Compiler.Text.Position * char) option =
        option {
          let! np = (x :> IFSACSourceText).PrevPos pos
          let! prevLineLength = (x :> IFSACSourceText).GetLineLength(np)

          if np.Column < 1 || prevLineLength < uint32 np.Column then
            return! (x :> IFSACSourceText).TryGetPrevChar(np)
          else
            return np, (x :> IFSACSourceText).GetCharUnsafe np
        }

      member x.ModifyText(range: Range, text: string) : IFSACSourceText =
        let span = range.ToRoslynTextSpan(sourceText)
        let change = TextChange(span, text)
        RoslynSourceTextFile(fileName, sourceText.WithChanges(change))


    interface ISourceText with

      member _.Item
        with get index = sourceText.[index]

      member _.GetLineString(lineIndex) = sourceText.Lines.[lineIndex].ToString()

      member _.GetLineCount() = sourceText.Lines.Count

      member _.GetLastCharacterPosition() =
        if sourceText.Lines.Count > 0 then
          (sourceText.Lines.Count, sourceText.Lines.[sourceText.Lines.Count - 1].Span.Length)
        else
          (0, 0)

      member _.GetSubTextString(start, length) = sourceText.GetSubText(TextSpan(start, length)).ToString()

      member _.GetSubTextFromRange(range: FSharp.Compiler.Text.Range) =
        range.ToRoslynTextSpan(sourceText) |> sourceText.GetSubText |> string

      member _.SubTextEquals(target, startIndex) =
        if startIndex < 0 || startIndex >= sourceText.Length then
          invalidArg "startIndex" "Out of range."

        if String.IsNullOrEmpty(target) then
          invalidArg "target" "Is null or empty."

        let lastIndex = startIndex + target.Length

        if lastIndex <= startIndex || lastIndex >= sourceText.Length then
          invalidArg "target" "Too big."

        let mutable finished = false
        let mutable didEqual = true
        let mutable i = 0

        while not finished && i < target.Length do
          if target.[i] <> sourceText.[startIndex + i] then
            didEqual <- false
            finished <- true // bail out early
          else
            i <- i + 1

        didEqual

      member _.ContentEquals(sourceText) =
        match sourceText with
        | :? SourceText as sourceText -> sourceText.ContentEquals(sourceText)
        | _ -> false

      member _.Length = sourceText.Length

      member _.CopyTo(sourceIndex, destination, destinationIndex, count) =
        sourceText.CopyTo(sourceIndex, destination, destinationIndex, count)

    interface ISourceTextNew with
      member this.GetChecksum() = sourceText.GetChecksum()

type ISourceTextFactory =
  abstract member Create: fileName: string<LocalPath> * text: string -> IFSACSourceText
  abstract member Create: fileName: string<LocalPath> * stream: Stream -> CancellableValueTask<IFSACSourceText>

module SourceTextFactory =


  let readFile (fileName: string<LocalPath>) (sourceTextFactory: ISourceTextFactory) =
    cancellableValueTask {
      let file = UMX.untag fileName

      // use large object heap hits or threadpool hits? Which is worse? Choose your foot gun.

      if FileInfo(file).Length >= File.bufferSize then
        // Roslyn SourceText doesn't actually support async streaming reads but avoids the large object heap hit
        // so we have to block a thread.
        use s = File.openFileStreamForReadingAsync fileName
        let! source = sourceTextFactory.Create(fileName, s)
        return source
      else
        // otherwise it'll be under the LOH threshold and the current thread isn't blocked
        let! text = fun ct -> File.ReadAllTextAsync(file, ct)
        let source = sourceTextFactory.Create(fileName, text)
        return source
    }

type RoslynSourceTextFactory() =
  interface ISourceTextFactory with
    member this.Create(fileName: string<LocalPath>, text: string) : IFSACSourceText =
      // This uses a TextReader because the TextReader overload https://github.com/dotnet/roslyn/blob/6df76ec8b109c9460f7abccc3a310c7cdbd2975e/src/Compilers/Core/Portable/Text/SourceText.cs#L120-L139
      // attempts to use the LargeText implementation for large strings. While the string is already allocated, if using CONSERVE_MEMORY, it should be cleaned up and compacted eventually.
      use t = new StringReader(text)
      RoslynSourceText.RoslynSourceTextFile(fileName, (Microsoft.CodeAnalysis.Text.SourceText.From(t, text.Length)))

    member this.Create(fileName: string<LocalPath>, stream: Stream) : CancellableValueTask<IFSACSourceText> =
      fun ct ->
        ct.ThrowIfCancellationRequested()
        // Maybe one day we'll have an async version for streams: https://github.com/dotnet/roslyn/issues/61489
        RoslynSourceText.RoslynSourceTextFile(fileName, (Microsoft.CodeAnalysis.Text.SourceText.From(stream)))
        :> IFSACSourceText
        |> ValueTask.FromResult


type VolatileFile =
  { LastTouched: DateTime
    Source: IFSACSourceText
    Version: int }

  member this.FileName = this.Source.FileName

  /// <summary>Updates the Source value</summary>
  member this.SetSource(source) = { this with Source = source }

  /// <summary>Updates the Touched value</summary>
  member this.SetLastTouched touched = { this with LastTouched = touched }

  /// <summary>Updates the Touched value attempting to use the file on disk's GetLastWriteTimeUtc otherwise uses DateTime.UtcNow. </summary>
  member this.UpdateTouched() =
    let dt = File.getLastWriteTimeOrDefaultNow this.Source.FileName
    this.SetLastTouched dt


  /// <summary>Helper method to create a VolatileFile</summary>
  static member Create(source: IFSACSourceText, version: int, ?touched: DateTime) =
    let touched =
      match touched with
      | Some t -> t
      | None -> File.getLastWriteTimeOrDefaultNow source.FileName

    { Source = source
      Version = version
      LastTouched = touched }

type FileSystem(actualFs: IFileSystem, tryFindFile: string<LocalPath> -> VolatileFile option) =
  let fsLogger = LogProvider.getLoggerByName "FileSystem"

  let getContent (filename: string<LocalPath>) =

    filename
    |> tryFindFile
    |> Option.map (fun file ->
      fsLogger.debug (
        Log.setMessage "Getting content of `{path}` - {hash}"
        >> Log.addContext "path" filename
        >> Log.addContext "hash" (file.Source.GetHashCode())
      )

      file.Source.ToString() |> System.Text.Encoding.UTF8.GetBytes)

  /// translation of the BCL's Windows logic for Path.IsPathRooted.
  ///
  /// either the first char is '/', or the first char is a drive identifier followed by ':'
  let isWindowsStyleRootedPath (p: string) =
    let isAlpha (c: char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

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
      let r = isWindowsStyleRootedPath p || isUnixStyleRootedPath p

      // fsLogger.debug (
      //   Log.setMessage "Is {path} rooted? {result}"
      //   >> Log.addContext "path" p
      //   >> Log.addContext "result" r
      // )

      r

    member _.GetFullPathShim(f: string) =
      let expanded = Path.FilePathToUri f |> Path.FileUriToLocalPath

      // fsLogger.debug (
      //   Log.setMessage "{path} expanded to {expanded}"
      //   >> Log.addContext "path" f
      //   >> Log.addContext "expanded" expanded
      // )

      expanded

    member _.GetLastWriteTimeShim(filename: string) =
      let result =
        filename
        |> Utils.normalizePath
        |> tryFindFile
        |> Option.map (fun f -> f.LastTouched)
        |> Option.defaultWith (fun () -> actualFs.GetLastWriteTimeShim filename)

      // fsLogger.debug (
      //   Log.setMessage "GetLastWriteTimeShim of `{path}` - {date} "
      //   >> Log.addContext "path" filename
      //   >> Log.addContext "date" result
      // )

      result

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

    member _.GetFullFilePathInDirectoryShim dir f = actualFs.GetFullFilePathInDirectoryShim dir f

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

    member _.ChangeExtensionShim(path: string, extension: string) : string = Path.ChangeExtension(path, extension)

module Symbol =
  open FSharp.Compiler.Symbols

  /// Declaration, Implementation, Signature
  let getDeclarationLocations (symbol: FSharpSymbol) =
    [| symbol.DeclarationLocation
       symbol.ImplementationLocation
       symbol.SignatureLocation |]
    |> Array.choose id
    |> Array.distinct
    |> Array.map (fun r -> r.NormalizeDriveLetterCasing())

  /// `true` if `range` is inside at least one `declLocation`
  ///
  /// inside instead of equal: `declLocation` for Active Pattern Case is complete Active Pattern
  ///   (`Even` -> declLoc: `|Even|Odd|`)
  let isDeclaration (declLocations: Range[]) (range: Range) =
    declLocations |> Array.exists (fun l -> Range.rangeContainsRange l range)

  /// For multiple `isDeclaration` calls:
  /// caches declaration locations (-> `getDeclarationLocations`) for multiple `isDeclaration` checks of same symbol
  let getIsDeclaration (symbol: FSharpSymbol) =
    let declLocs = getDeclarationLocations symbol
    isDeclaration declLocs

  /// returns `(declarations, usages)`
  let partitionIntoDeclarationsAndUsages (symbol: FSharpSymbol) (ranges: Range[]) =
    let isDeclaration = getIsDeclaration symbol
    ranges |> Array.partition isDeclaration

module Tokenizer =
  /// Extracts identifier by either looking at backticks or splitting at last `.`.
  /// Removes leading paren too (from operator with Module name: `MyModule.(+++`)
  ///
  /// Note: doesn't handle operators containing `.`,
  ///       but does handle strange Active Patterns (like with linebreak)
  ///
  ///
  /// based on: `dotnet/fsharp` `Tokenizer.fixupSpan`
  let private tryFixupRangeBySplittingAtDot
    (range: Range, text: IFSACSourceText, includeBackticks: bool)
    : Range voption =
    match text[range] with
    | Error _ -> ValueNone
    | Ok rangeText when rangeText.EndsWith("``", StringComparison.Ordinal) ->
      // find matching opening backticks

      // backticks cannot contain linebreaks -- even for Active Pattern:
      // `(``|Even|Odd|``)` is ok, but ` (``|Even|\n    Odd|``) is not

      let pre = rangeText.AsSpan(0, rangeText.Length - 2 (*backticks*) )

      match pre.LastIndexOf("``") with
      | -1 ->
        // invalid identifier -> should not happen
        range |> ValueSome
      | i when includeBackticks ->
        let startCol = range.EndColumn - 2 (*backticks*) - (pre.Length - i)
        range.WithStart(range.End.WithColumn(startCol)) |> ValueSome
      | i ->
        let startCol =
          range.EndColumn - 2 (*backticks*) - (pre.Length - i - 2 (*backticks*) )

        let endCol = range.EndColumn - 2 (*backticks*)

        range.With(range.Start.WithColumn(startCol), range.End.WithColumn(endCol))
        |> ValueSome
    | Ok rangeText ->
      // split at `.`
      // identifier (after `.`) might contain linebreak -> multiple lines
      // Note: Active Pattern cannot contain `.` -> split at `.` should be always valid because we handled backticks above
      //    (`(|``Hello.world``|Odd|)` is not valid (neither is a type name with `.`: `type ``Hello.World`` = ...`))
      match rangeText.LastIndexOf '.' with
      | -1 -> range |> ValueSome
      | i ->
        // there might be a `(` after `.`:
        // `MyModule.(+++` (Note: closing paren in not part of FSharpSymbolUse.Range)
        // and there might be additional newlines and spaces afterwards
        let ident = rangeText.AsSpan(i + 1 (*.*) )
        let trimmedIdent = ident.TrimStart('(').TrimStart("\n\r ")
        let inFrontOfIdent = ident.Length - trimmedIdent.Length

        let pre = rangeText.AsSpan(0, i + 1 (*.*) + inFrontOfIdent)
        // extract lines and columns
        let nLines = pre.CountLines()
        let lastLine = pre.LastLine()
        let startLine = range.StartLine + (nLines - 1)

        let startCol =
          match nLines with
          | 1 -> range.StartColumn + lastLine.Length
          | _ -> lastLine.Length

        range.WithStart(Position.mkPos startLine startCol) |> ValueSome

  /// Cleans `FSharpSymbolUse.Range` (and similar) to only contain main (= last) identifier
  /// * Removes leading Namespace, Module, Type: `System.String.IsNullOrEmpty` -> `IsNullOrEmpty`
  /// * Removes leftover open paren: `Microsoft.FSharp.Core.Operators.(+` -> `+`
  /// * keeps backticks based on `includeBackticks`
  ///   -> full identifier range with backticks, just identifier name (~`symbolNameCore`) without backticks
  ///
  /// returns `None` iff `range` isn't inside `text` -> `range` & `text` for different states
  let tryFixupRange
    (symbolNameCore: string, range: Range, text: IFSACSourceText, includeBackticks: bool)
    : Range voption =
    // first: try match symbolNameCore in last line
    // usually identifier cannot contain linebreak -> is in last line of range
    // Exception: Active Pattern can span multiple lines: `(|Even|Odd|)` -> `(|Even|\n  Odd|)` is valid too

    /// Range in last line with actual content (-> without indentation)
    let contentRangeInLastLine (range: range, lastLineText: string) =
      if range.StartLine = range.EndLine then
        range
      else
        let text = lastLineText.AsSpan(0, range.EndColumn)
        // remove leading indentation
        let l = text.TrimStart(' ').Length
        let startCol = (range.EndColumn - l)
        range.WithStart(range.End.WithColumn(startCol))

    match text.GetLine range.End with
    | None -> ValueNone
    | Some line ->
      let contentRange = contentRangeInLastLine (range, line)
      assert (contentRange.StartLine = contentRange.EndLine)

      let content =
        line.AsSpan(contentRange.StartColumn, contentRange.EndColumn - contentRange.StartColumn)

      match content.LastIndexOf symbolNameCore with
      | -1 ->
        // cases this can happens:
        // * Active Pattern with linebreak: `(|Even|\n  Odd|)`
        //   -> spans multiple lines
        // * Active Pattern with backticks in case: `(|``Even``|Odd|)`
        //   -> symbolNameCore doesn't match content

        // fall back to split at `.`

        // differences between `tryFixupRangeBySplittingAtDot` and current function (in other match clause)
        // * `tryFixupRangeBySplittingAtDot`:
        //    * handles strange forms of Active Patterns (like linebreak)
        //    * handles empty symbolName of Active Patterns Case (in decl)
        //    * (allocates new string)
        // * current function:
        //    * handles operators containing `.`
        //    * (uses Span)

        tryFixupRangeBySplittingAtDot (range, text, includeBackticks)
      // Extra Pattern: `| -1 | _ when symbolNameCore = "" -> ...` is incorrect -> `when` clause applies to both...
      | _ when symbolNameCore = "" ->
        // happens for:
        // * Active Pattern case inside Active Pattern declaration
        //   ```fsharp
        //   let (|Even|Odd|) v =
        //     if v % 2 = 0 then Even else Odd
        //                       ^^^^
        //   ```
        //   -> `FSharpSymbolUse.Symbol.DisplayName` on marked position is empty
        tryFixupRangeBySplittingAtDot (range, text, includeBackticks)
      | i ->
        let startCol = contentRange.StartColumn + i
        let endCol = startCol + symbolNameCore.Length

        if
          includeBackticks
          &&
          // detect possible backticks around [startCol:endCol]
          (contentRange.StartColumn <= startCol - 2 (*backticks*)
           && endCol + 2 (*backticks*) <= contentRange.EndColumn
           && (let maybeBackticks = content.Slice(i - 2, 2 + symbolNameCore.Length + 2)
               maybeBackticks.StartsWith("``") && maybeBackticks.EndsWith("``")))
        then
          contentRange.With(
            contentRange.Start.WithColumn(startCol - 2 (*backticks*) ),
            contentRange.End.WithColumn(endCol + 2 (*backticks*) )
          )
          |> ValueSome
        else
          contentRange.With(contentRange.Start.WithColumn(startCol), contentRange.End.WithColumn(endCol))
          |> ValueSome
