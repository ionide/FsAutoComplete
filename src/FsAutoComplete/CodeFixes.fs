/// This module contains the logic for codefixes that FSAC surfaces, as well as conversion logic between
/// compiler diagnostics and LSP diagnostics/code actions
namespace FsAutoComplete.CodeFix

open FsAutoComplete
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Logging
open FSharp.UMX
open FsToolkit.ErrorHandling
open FSharp.Compiler.Text

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
type FcsPos = FSharp.Compiler.Text.Position

module LspTypes = Ionide.LanguageServerProtocol.Types

module Types =
  type IsEnabled = unit -> bool

  type GetRangeText = string<LocalPath> -> LspTypes.Range -> ResultOrString<string>
  type GetFileLines = string<LocalPath> -> ResultOrString<NamedText>
  type GetLineText = NamedText -> LspTypes.Range -> Result<string, string>

  type GetParseResultsForFile =
    string<LocalPath>
      -> FSharp.Compiler.Text.Position
      -> Async<ResultOrString<ParseAndCheckResults * string * NamedText>>

  type GetProjectOptionsForFile = string<LocalPath> -> ResultOrString<FSharp.Compiler.CodeAnalysis.FSharpProjectOptions>

  [<RequireQualifiedAccess>]
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
    static member OfFix getFileVersion clientCapabilities (fix: Fix) =
      let filePath = fix.File.GetFilePath() |> Utils.normalizePath
      let fileVersion = getFileVersion filePath

      CodeAction.OfDiagnostic fix.File fileVersion fix.Title fix.SourceDiagnostic fix.Edits fix.Kind clientCapabilities

    static member OfDiagnostic
      (fileUri)
      (fileVersion)
      title
      (diagnostic)
      (edits)
      fixKind
      clientCapabilities
      : CodeAction =

      let edit =
        { TextDocument =
            { Uri = fileUri.Uri
              Version = fileVersion }
          Edits = edits }

      let workspaceEdit = WorkspaceEdit.Create([| edit |], clientCapabilities)

      { CodeAction.Title = title
        Kind =
          Some(
            match fixKind with
            | FixKind.Fix -> "quickfix"
            | FixKind.Refactor -> "refactor"
            | FixKind.Rewrite -> "refactor.rewrite"
          )
        Diagnostics = diagnostic |> Option.map Array.singleton
        IsPreferred = None
        Disabled = None
        Edit = Some workspaceEdit
        Command = None
        Data = None }

module SourceText =
  let inline private assertLineIndex lineIndex (sourceText: ISourceText) =
    assert
      (0 <= lineIndex
       && lineIndex < sourceText.GetLineCount())

  /// Note: this fails when `sourceText` is empty string (`""`)
  /// -> No lines
  ///    Use `WithEmptyHandling.isFirstLine` to handle empty string
  let isFirstLine lineIndex (sourceText: ISourceText) =
    assertLineIndex lineIndex sourceText
    lineIndex = 0

  /// Note: this fails when `sourceText` is empty string (`""`)
  /// -> No lines
  ///    Use `WithEmptyHandling.isLastLine` to handle empty string
  let isLastLine lineIndex (sourceText: ISourceText) =
    assertLineIndex lineIndex sourceText
    lineIndex = sourceText.GetLineCount() - 1

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
    let getLineCount (sourceText: ISourceText) =
      match sourceText.GetLineCount() with
      | 0 -> 1
      | c -> c
    // or
    // max 1 (sourceText.GetLineCount())

    let inline private assertLineIndex lineIndex sourceText =
      assert
        (0 <= lineIndex
         && lineIndex < getLineCount sourceText)

    let getLineString lineIndex (sourceText: ISourceText) =
      assertLineIndex lineIndex sourceText

      if lineIndex = 0 && sourceText.GetLineCount() = 0 then
        ""
      else
        sourceText.GetLineString lineIndex

    let isFirstLine lineIndex (sourceText: ISourceText) =
      assertLineIndex lineIndex sourceText
      // No need to check for inside `getLineCount`: there's always at least one line (might be empty)
      lineIndex = 0

    let isLastLine lineIndex (sourceText: ISourceText) =
      assertLineIndex lineIndex sourceText
      lineIndex = (getLineCount sourceText) - 1

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
    let afterLastCharacterPosition lineIndex (sourceText: ISourceText) =
      assertLineIndex lineIndex sourceText
      let line = sourceText |> getLineString lineIndex
      line.Length

/// helpers for iterating along text lines
module Navigation =

  let findPosForCharacter (lines: string[]) (pos: int) =
    let mutable lineNumber = 0
    let mutable runningLength = 0
    let mutable found = false

    let mutable fcsPos = Unchecked.defaultof<FcsPos>

    while not found do
      let line = lines.[lineNumber]
      let lineLength = line.Length

      if pos <= runningLength + lineLength then
        let column = pos - runningLength
        found <- true
        fcsPos <- FSharp.Compiler.Text.Position.mkPos lineNumber column
      else
        lineNumber <- lineNumber + 1
        runningLength <- runningLength + lineLength

    fcsPos

  let inc (lines: NamedText) (pos: LspTypes.Position) : LspTypes.Position option =
    lines.NextPos(protocolPosToPos pos)
    |> Option.map fcsPosToLsp

  let dec (lines: NamedText) (pos: LspTypes.Position) : LspTypes.Position option =
    lines.PrevPos(protocolPosToPos pos)
    |> Option.map fcsPosToLsp

  let rec decMany lines pos count =
    option {
      let mutable pos = pos
      let mutable count = count

      while count > 0 do
        let! nextPos = dec lines pos
        pos <- nextPos
        count <- count - 1

      return pos
    }

  let rec incMany lines pos count =
    option {
      let mutable pos = pos
      let mutable count = count

      while count > 0 do
        let! nextPos = inc lines pos
        pos <- nextPos
        count <- count - 1

      return pos
    }

  let walkBackUntilConditionWithTerminal (lines: NamedText) pos condition terminal =
    let fcsStartPos = protocolPosToPos pos

    lines.WalkBackwards(fcsStartPos, terminal, condition)
    |> Option.map fcsPosToLsp

  let walkForwardUntilConditionWithTerminal (lines: NamedText) pos condition terminal =
    let fcsStartPos = protocolPosToPos pos

    lines.WalkForward(fcsStartPos, terminal, condition)
    |> Option.map fcsPosToLsp

  let walkBackUntilCondition lines pos condition =
    walkBackUntilConditionWithTerminal lines pos condition (fun _ -> false)

  let walkForwardUntilCondition lines pos condition =
    walkForwardUntilConditionWithTerminal lines pos condition (fun _ -> false)

  /// Tries to detect the last cursor position in line before `currentLine` (0-based).
  ///
  /// Returns `None` iff there's no prev line -> `currentLine` is first line
  let tryEndOfPrevLine (lines: ISourceText) currentLine =
    if SourceText.WithEmptyHandling.isFirstLine currentLine lines then
      None
    else
      let prevLine = currentLine - 1

      { Line = prevLine
        Character =
          lines
          |> SourceText.WithEmptyHandling.afterLastCharacterPosition prevLine }
      |> Some

  /// Tries to detect the first cursor position in line after `currentLine` (0-based).
  ///
  /// Returns `None` iff there's no next line -> `currentLine` is last line
  let tryStartOfNextLine (lines: ISourceText) currentLine =
    if SourceText.WithEmptyHandling.isLastLine currentLine lines then
      None
    else
      let nextLine = currentLine + 1
      { Line = nextLine; Character = 0 } |> Some

  /// Gets the range to delete the complete line `lineIndex` (0-based).
  /// Deleting the line includes a linebreak if possible
  /// -> range starts either at end of previous line (-> includes leading linebreak)
  ///    or start of next line (-> includes trailing linebreak)
  ///
  /// Special case: there's just one line
  /// -> delete text of (single) line
  let rangeToDeleteFullLine lineIndex (lines: ISourceText) =
    match tryEndOfPrevLine lines lineIndex with
    | Some start ->
      // delete leading linebreak
      { Start = start
        End =
          { Line = lineIndex
            Character =
              lines
              |> SourceText.WithEmptyHandling.afterLastCharacterPosition lineIndex } }
    | None ->
      match tryStartOfNextLine lines lineIndex with
      | Some fin ->
        // delete trailing linebreak
        { Start = { Line = lineIndex; Character = 0 }
          End = fin }
      | None ->
        // single line
        // -> just delete all text in line
        { Start = { Line = lineIndex; Character = 0 }
          End =
            { Line = lineIndex
              Character =
                lines
                |> SourceText.WithEmptyHandling.afterLastCharacterPosition lineIndex } }



module Run =
  open Types

  let ifEnabled enabled codeFix : CodeFix =
    fun codeActionParams ->
      if enabled () then
        codeFix codeActionParams
      else
        AsyncResult.retn []

  let private runDiagnostics pred handler : CodeFix =
    fun codeActionParams ->
      codeActionParams.Context.Diagnostics
      |> Array.choose (fun d -> if pred d then Some d else None)
      |> Array.toList
      |> List.traverseAsyncResultM (fun d -> handler d codeActionParams)
      |> AsyncResult.map List.concat


  let ifDiagnosticByMessage (checkMessage: string) handler : CodeFix =
    runDiagnostics (fun d -> d.Message.Contains checkMessage) handler

  let ifDiagnosticByType (diagnosticType: string) handler : CodeFix =
    runDiagnostics (fun d -> d.Source.Contains diagnosticType) handler

  let ifDiagnosticByCode codes handler : CodeFix =
    runDiagnostics (fun d -> d.Code.IsSome && Set.contains d.Code.Value codes) handler
