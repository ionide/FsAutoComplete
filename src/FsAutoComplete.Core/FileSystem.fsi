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
open IcedTasks

module File =
  val getLastWriteTimeOrDefaultNow: path: string<LocalPath> -> DateTime
  val openFileStreamForReadingAsync: path: string<LocalPath> -> FileStream

[<AutoOpen>]
module PositionExtensions =
  type FSharp.Compiler.Text.Position with

    /// Excluding current line
    member LinesToBeginning: unit -> seq<pos>
    member IncLine: unit -> pos
    member DecLine: unit -> pos
    member IncColumn: unit -> pos
    member IncColumn: n: int -> pos
    member inline WithColumn: col: int -> pos

  val inline (|Pos|): p: FSharp.Compiler.Text.Position -> int * int

[<AutoOpen>]
module RangeExtensions =
  type FSharp.Compiler.Text.Range with

    member WithFileName: fileName: string -> range
    /// the checker gives us back wacky ranges sometimes, so what we're going to do is check if the text of the triggering
    /// symbol use is in each of the ranges we plan to rename, and if we're looking at a range that is _longer_ than our rename range,
    /// do some splicing to find just the range we need to replace.
    /// TODO: figure out where the caps are coming from in the compilation, maybe something wrong in the
    member NormalizeDriveLetterCasing: unit -> range
    /// utility method to get the tagged filename for use in our state storage
    /// TODO: should we enforce this/use the Path members for normalization?
    member TaggedFileName: string<LocalPath>
    member inline With: start: pos * fin: pos -> range
    member inline WithStart: start: pos -> range
    member inline WithEnd: fin: pos -> range

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

type ISourceTextFactory =
  abstract member Create: fileName: string<LocalPath> * text: string -> IFSACSourceText
  abstract member Create: fileName: string<LocalPath> * stream: Stream -> CancellableValueTask<IFSACSourceText>


module SourceTextFactory =
  val readFile:
    fileName: string<LocalPath> -> sourceTextFactory: ISourceTextFactory -> CancellableValueTask<IFSACSourceText>

type RoslynSourceTextFactory =
  new: unit -> RoslynSourceTextFactory
  interface ISourceTextFactory

type VolatileFile =
  { LastTouched: DateTime
    Source: IFSACSourceText
    Version: int }

  member FileName: string<LocalPath>
  /// <summary>Updates the Lines value</summary>
  member SetSource: source: IFSACSourceText -> VolatileFile
  /// <summary>Updates the Touched value</summary>
  member SetLastTouched: touched: DateTime -> VolatileFile
  /// <summary>Updates the Touched value attempting to use the file on disk's GetLastWriteTimeUtc otherwise uses DateTime.UtcNow. </summary>
  member UpdateTouched: unit -> VolatileFile
  /// <summary>Helper method to create a VolatileFile</summary>
  static member Create: source: IFSACSourceText * version: int * ?touched: DateTime -> VolatileFile


type FileSystem =
  new: actualFs: IFileSystem * tryFindFile: (string<LocalPath> -> VolatileFile option) -> FileSystem
  interface IFileSystem

module Symbol =
  open FSharp.Compiler.Symbols
  /// Declaration, Implementation, Signature
  val getDeclarationLocations: symbol: FSharpSymbol -> range array
  /// `true` if `range` is inside at least one `declLocation`
  ///
  /// inside instead of equal: `declLocation` for Active Pattern Case is complete Active Pattern
  ///   (`Even` -> declLoc: `|Even|Odd|`)
  val isDeclaration: declLocations: Range[] -> range: Range -> bool
  /// For multiple `isDeclaration` calls:
  /// caches declaration locations (-> `getDeclarationLocations`) for multiple `isDeclaration` checks of same symbol
  val getIsDeclaration: symbol: FSharpSymbol -> (Range -> bool)
  /// returns `(declarations, usages)`
  val partitionIntoDeclarationsAndUsages: symbol: FSharpSymbol -> ranges: Range[] -> Range array * Range array

module Tokenizer =
  /// Cleans `FSharpSymbolUse.Range` (and similar) to only contain main (= last) identifier
  /// * Removes leading Namespace, Module, Type: `System.String.IsNullOrEmpty` -> `IsNullOrEmpty`
  /// * Removes leftover open paren: `Microsoft.FSharp.Core.Operators.(+` -> `+`
  /// * keeps backticks based on `includeBackticks`
  ///   -> full identifier range with backticks, just identifier name (~`symbolNameCore`) without backticks
  ///
  /// returns `None` iff `range` isn't inside `text` -> `range` & `text` for different states
  val tryFixupRange:
    symbolNameCore: string * range: Range * text: IFSACSourceText * includeBackticks: bool -> Range voption
