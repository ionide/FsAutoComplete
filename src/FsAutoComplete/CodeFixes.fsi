namespace FsAutoComplete.CodeFix

open FsAutoComplete
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Logging
open FSharp.UMX
open FSharp.Compiler.Text
open FsAutoComplete.FCSPatches
open FSharp.Compiler.CodeAnalysis.ProjectSnapshot

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
type FcsPos = FSharp.Compiler.Text.Position


module Types =
  type IsEnabled = unit -> bool
  type GetRangeText = string<LocalPath> -> Ionide.LanguageServerProtocol.Types.Range -> Async<ResultOrString<string>>
  type GetFileLines = string<LocalPath> -> Async<ResultOrString<IFSACSourceText>>
  type GetLineText = IFSACSourceText -> Ionide.LanguageServerProtocol.Types.Range -> Async<Result<string, string>>

  type GetParseResultsForFile =
    string<LocalPath>
      -> FSharp.Compiler.Text.Position
      -> Async<ResultOrString<ParseAndCheckResults * string * IFSACSourceText>>

  type GetLanguageVersion = string<LocalPath> -> Async<LanguageVersionShim>

  type GetProjectOptionsForFile = string<LocalPath> -> Async<ResultOrString<CompilerProjectOption>>

  [<RequireQualifiedAccess>]
  [<Struct>]
  type FixKind =
    | Fix
    | Refactor
    | Rewrite

  type Fix =
    { Edits: TextEdit[]
      File: TextDocumentIdentifier
      Title: string
      SourceDiagnostic: Diagnostic option
      Kind: FixKind }

  type CodeFix = CodeActionParams -> Async<Result<Fix list, string>>

  type CodeAction with

    static member OfFix:
      getFileVersion: (string<LocalPath> -> Async<int option>) ->
      clientCapabilities: ClientCapabilities ->
      fix: Fix ->
        Async<CodeAction>

    static member OfDiagnostic:
      fileUri: TextDocumentIdentifier ->
      fileVersion: int32 option ->
      title: string ->
      diagnostic: Diagnostic option ->
      edits: TextEdit array ->
      fixKind: FixKind ->
      clientCapabilities: ClientCapabilities ->
        CodeAction

module SourceText =
  /// Note: this fails when `sourceText` is empty string (`""`)
  /// -> No lines
  ///    Use `WithEmptyHandling.isFirstLine` to handle empty string
  val isFirstLine: lineIndex: int -> sourceText: ISourceText -> bool
  /// Note: this fails when `sourceText` is empty string (`""`)
  /// -> No lines
  ///    Use `WithEmptyHandling.isLastLine` to handle empty string
  val isLastLine: lineIndex: int -> sourceText: ISourceText -> bool

  /// SourceText treats empty string as no source:
  /// ```fsharp
  /// let text = SourceText.ofString ""
  /// assert(text.ToString() = "")
  /// assert(text.GetLastCharacterPosition() = (0, 0))  // Note: first line is `1`
  /// assert(text.GetLineCount() = 0) // Note: `(SourceText.ofString "\n").GetLineCount()` is `2`
  /// assert(text.GetLineString 0 )  // System.IndexOutOfRangeException: Index was outside the bounds of the array.
  /// ```
  /// -> Functions in here treat empty string as empty single line
  ///
  /// Note: There's always at least empty single line
  ///       -> source MUST at least be empty (cannot not exist)
  module WithEmptyHandling =
    val getLineCount: sourceText: ISourceText -> uint32
    val getLineString: lineIndex: uint32 -> sourceText: ISourceText -> string
    val isFirstLine: lineIndex: uint32 -> sourceText: ISourceText -> bool
    val isLastLine: lineIndex: uint32 -> sourceText: ISourceText -> bool
    /// Returns position after last character in specified line.
    /// Same as line length.
    ///
    /// Example:
    /// ```fsharp
    /// let text = SourceText.ofString "let a = 2\nlet foo = 42\na + foo\n"
    ///
    /// assert(afterLastCharacterPosition 0 text = 9)
    /// assert(afterLastCharacterPosition 1 text = 12)
    /// assert(afterLastCharacterPosition 2 text = 7)
    /// assert(afterLastCharacterPosition 2 text = 0)
    /// ```
    val afterLastCharacterPosition: lineIndex: uint32 -> sourceText: ISourceText -> uint32

/// helpers for iterating along text lines
module Navigation =
  val findPosForCharacter: lines: string[] -> pos: uint32 -> FcsPos

  val inc:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
      Ionide.LanguageServerProtocol.Types.Position option

  val dec:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
      Ionide.LanguageServerProtocol.Types.Position option

  val decMany:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
    count: uint32 ->
      Ionide.LanguageServerProtocol.Types.Position option

  val incMany:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
    count: uint32 ->
      Ionide.LanguageServerProtocol.Types.Position option

  val walkBackUntilConditionWithTerminal:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
    condition: (char -> bool) ->
    terminal: (char -> bool) ->
      Ionide.LanguageServerProtocol.Types.Position option

  val walkForwardUntilConditionWithTerminal:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
    condition: (char -> bool) ->
    terminal: (char -> bool) ->
      Ionide.LanguageServerProtocol.Types.Position option

  val walkBackUntilCondition:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
    condition: (char -> bool) ->
      Ionide.LanguageServerProtocol.Types.Position option

  val walkForwardUntilCondition:
    lines: IFSACSourceText ->
    pos: Ionide.LanguageServerProtocol.Types.Position ->
    condition: (char -> bool) ->
      Ionide.LanguageServerProtocol.Types.Position option

  /// Tries to detect the last cursor position in line before `currentLine` (0-based).
  ///
  /// Returns `None` iff there's no prev line -> `currentLine` is first line
  val tryEndOfPrevLine:
    lines: IFSACSourceText -> currentLine: uint32 -> Ionide.LanguageServerProtocol.Types.Position option

  /// Tries to detect the first cursor position in line after `currentLine` (0-based).
  ///
  /// Returns `None` iff there's no next line -> `currentLine` is last line
  val tryStartOfNextLine:
    lines: IFSACSourceText -> currentLine: uint32 -> Ionide.LanguageServerProtocol.Types.Position option

  /// Gets the range to delete the complete line `lineIndex` (0-based).
  /// Deleting the line includes a linebreak if possible
  /// -> range starts either at end of previous line (-> includes leading linebreak)
  ///    or start of next line (-> includes trailing linebreak)
  ///
  /// Special case: there's just one line
  /// -> delete text of (single) line
  val rangeToDeleteFullLine: lineIndex: uint32 -> lines: IFSACSourceText -> Ionide.LanguageServerProtocol.Types.Range

module Run =
  open Types

  val ifEnabled:
    enabled: (unit -> bool) ->
    codeFix: (CodeActionParams -> Async<Result<Fix list, string>>) ->
      (CodeActionParams -> Async<Result<Fix list, string>>)

  val ifDiagnosticByMessage:
    checkMessage: string ->
    handler: (Diagnostic -> CodeActionParams -> Async<Result<Fix list, string>>) ->
      (CodeActionParams -> Async<Result<Fix list, string>>)

  val ifDiagnosticByCheckMessage:
    checkMessageFunc: (string -> bool) list ->
    handler: (Diagnostic -> CodeActionParams -> Async<Result<Fix list, string>>) ->
      (CodeActionParams -> Async<Result<Fix list, string>>)

  val ifDiagnosticByType:
    diagnosticType: string ->
    handler: (Diagnostic -> CodeActionParams -> Async<Result<Fix list, string>>) ->
      (CodeActionParams -> Async<Result<Fix list, string>>)

  val ifDiagnosticByCode:
    codes: Set<string> ->
    handler: (Diagnostic -> CodeActionParams -> Async<Result<Fix list, string>>) ->
      (CodeActionParams -> Async<Result<Fix list, string>>)

  val ifImplementationFileBackedBySignature:
    getProjectOptionsForFile: GetProjectOptionsForFile ->
    codeFix: CodeFix ->
    codeActionParams: CodeActionParams ->
      Async<Result<Fix list, string>>
