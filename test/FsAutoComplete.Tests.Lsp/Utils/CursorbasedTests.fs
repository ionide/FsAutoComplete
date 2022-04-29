module Utils.CursorbasedTests
open Expecto
open Expecto.Diff
open Ionide.LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open Utils.Utils
open Utils.Server
open Utils.TextEdit
open Ionide.ProjInfo.Logging

/// Checks for CodeFixes, CodeActions
///
/// Prefixes:
/// * `check`: Check to use inside a `testCaseAsync`. Not a Test itself!
/// * `test`: Returns Expecto Test. Usually combines multiple tests (like: test all positions).
module CodeFix =
  let private logger = LogProvider.getLoggerByName "CursorbasedTests.CodeFix"

  let private diagnosticsIn (range: Range) (diags: Diagnostic[]) =
    diags
    |> Array.filter (fun diag -> range |> Range.overlapsStrictly diag.Range)

  /// Note: Return should be just ONE `CodeAction` (for Applicable) or ZERO `CodeAction` (for Not Applicable).
  ///       But actual return type is an array of `CodeAction`s:
  ///       * Easier to successive filter CodeActions down with simple pipe and `Array.filter`
  ///       * Returning `CodeAction option` would mean different filters for `check` (exactly one fix) and `checkNotApplicable` (exactly zero fix).
  ///         Both error with multiple matching fixes!
  type ChooseFix = CodeAction[] -> CodeAction[]
  type ExpectedResult =
    | NotApplicable
    | Applicable
    | After of string

  let checkFixAt
    (doc: Document, diagnostics: Diagnostic[])
    (beforeWithoutCursor: string, cursorRange: Range)
    (validateDiagnostics: Diagnostic[] -> unit)
    (chooseFix: ChooseFix)
    (expected: ExpectedResult)
    = async {
      // filter to only diags matching the cursor range
      let diags = diagnostics |> diagnosticsIn cursorRange
      validateDiagnostics diags
      // get code fixes from server
      let! res = doc |> Document.codeActionAt diags cursorRange
      let allCodeActions =
        match res, expected with
        | None, (Applicable | After _) ->
            // error here instead of later to return error noting it was `None` instead of empty CodeAction array
            Expect.isSome res "No CodeAction returned (`None`)"
            failwith "unreachable"
        | None, NotApplicable ->
            [||]
        | Some res, _ ->
            match res with
            | TextDocumentCodeActionResult.Commands cs -> failtestf "Expected CodeActions, but got commands: %A" cs
            | TextDocumentCodeActionResult.CodeActions cas -> cas

      // select code action to use
      let codeActions = chooseFix allCodeActions

      let getCodeAction =
        // validate codeAction (exactly one)
        // split test into two to output all available code actions when no matching
        function
        | [||] -> failtestf "No matching CodeAction. Available code actions were: %A" allCodeActions
        | _ ->
          Expect.hasLength codeActions 1 "Should be exactly ONE applicable code action"
          codeActions |> Array.head

      match expected with
      | NotApplicable ->
        // Expect.isEmpty codeActions "There should be no applicable code action" // doesn't show `actual` when not empty
        if  not (codeActions |> Array.isEmpty) then
          failtestf "There should be no applicable code action, but was %A" codeActions
      | Applicable ->
          codeActions
          |> getCodeAction
          |> ignore
          //ENHANCEMENT?: apply edits to check valid?
      | After expected ->
          let codeAction = codeActions |> getCodeAction

          /// Error message is appended by selected `codeAction`
          let inline failCodeFixTest (msg: string) =
            let msg =
              if System.String.IsNullOrWhiteSpace msg || System.Char.IsPunctuation(msg, msg.Length-1) then
                msg
              else
                msg + "."
            failtest $"{msg} CodeAction was: %A{codeAction}"

          // only text edits supported
          if codeAction.Command |> Option.isSome then
            failCodeFixTest "Code action contains commands. Commands aren't supported in this test!"

          let edits =
            codeAction.Edit
            |> Option.defaultWith (fun _ -> failCodeFixTest "Code action doesn't contain any edits")
            |> WorkspaceEdit.tryExtractTextEditsInSingleFile doc.VersionedTextDocumentIdentifier
            |> Result.valueOr failCodeFixTest

          // apply fix
          let actual =
            beforeWithoutCursor
            |> TextEdits.apply edits
            |> Result.valueOr failCodeFixTest

          Expecto.Diff.equals actual expected "Incorrect text after applying the chosen code action"
    }

  let private checkFix
    (server: CachedServer)
    (beforeWithCursor: string)
    (validateDiagnostics: Diagnostic[] -> unit)
    (chooseFix: ChooseFix)
    (expected: ExpectedResult)
    = async {
      let (range, text) =
        beforeWithCursor
        |> Text.trimTripleQuotation
        |> Cursor.assertExtractRange
      // load text file
      let! (doc, diags) = server |> Server.createUntitledDocument text
      use doc = doc // ensure doc gets closed (disposed) after test

      do! checkFixAt (doc, diags) (text, range) validateDiagnostics chooseFix expected
    }

  /// Checks a CodeFix (CodeAction) for validity.
  ///
  /// * Extracts cursor position (`$0`) or range (between two `$0`) from `beforeWithCursor`
  /// * Opens untitled Doc with source `beforeWithCursor` (with cursor removed)
  ///   * Note: untitled Document acts as Script file!
  ///   * Note: untitled Documents doesn't exist on disk!
  /// * Waits for Diagnostics in that doc
  /// * Filters Diags down to diags matching cursor position/range
  /// * Then validates diags with `validateDiagnostics`
  ///   * Note: Validates filtered diags (-> only diags at cursor pos); not all diags in doc!
  /// * Gets CodeFixes (CodeActions) from LSP server (`textDocument/codeAction`) for cursor range
  ///   * Request includes filtered diags
  /// * Selects CodeFix from returned CodeFixes with `chooseFix`
  ///   * Note: `chooseFix` should return a single CodeFix. No CodeFix or multiple CodeFixes count as Failure!
  ///     * Use `checkNotApplicable` when there shouldn't be a CodeFix
  ///   * Note: Though `chooseFix` should return one CodeFix, the function actually returns an array of CodeFixes.
  ///           Reasons:
  ///           * Easier to filter down CodeFixes (`CodeFix.ofKind "..." >> CodeFix.withTitle "..."`)
  ///           * Better error messages: Can differentiate between no CodeFixes and too many CodeFixes
  /// * Validates selected CodeFix:
  /// * Applies selected CodeFix to source (`beforeWithCursor` with cursor removed)
  /// * Compares result with `expected`
  ///
  /// Note:
  /// `beforeWithCursor` as well as `expected` get trimmed with `Text.trimTripleQuotation`: Leading empty line and indentation gets removed.
  ///
  /// Note:
  /// `beforeWithCursor` and `expected` MUST use `\n` for linebreaks -- using `\r` (either alone or as `\r\n`) results in test failure!
  /// Linebreaks from edits in selected CodeFix are all transformed to just `\n`
  /// -> CodeFix can use `\r` and `\r\n`
  /// If you want to validate Line Endings of CodeFix, add a validation step to your `chooseFix`
  let check
    server
    beforeWithCursor
    validateDiagnostics
    chooseFix
    expected
    =
    checkFix
      server
      beforeWithCursor
      validateDiagnostics
      chooseFix
      (After (expected |> Text.trimTripleQuotation))

  /// Note: Doesn't apply Fix! Just checks its existence!
  let checkApplicable
    server
    beforeWithCursor
    validateDiagnostics
    chooseFix
    =
    checkFix
      server
      beforeWithCursor
      validateDiagnostics
      chooseFix
      Applicable

  let checkNotApplicable
    server
    beforeWithCursor
    validateDiagnostics
    chooseFix
    =
    checkFix
      server
      beforeWithCursor
      validateDiagnostics
      chooseFix
      NotApplicable

  let matching cond (fixes: CodeAction array) =
    fixes
    |> Array.filter cond
  let withTitle title = matching (fun f -> f.Title = title)
  let ofKind kind = matching (fun f -> f.Kind = Some kind)

  /// Bundled tests in Expecto test
  module private Test =
    /// One `testCaseAsync` for each cursorRange.
    /// All test cases use same document (`ServerTests.documentTestList`) with source `beforeWithoutCursor`.
    ///
    /// Test names:
    /// * `name` is name of outer test list.
    /// * Each test case: `Cursor {i} at {pos or range}`
    ///
    /// Note: Sharing a common `Document` is just barely faster than using a new `Document` for each test (at least for simple source in `beforeWithoutCursor`).
    let checkFixAll
      (name: string)
      (server: CachedServer)
      (beforeWithoutCursor: string)
      (cursorRanges: Range seq)
      (validateDiagnostics: Diagnostic[] -> unit)
      (chooseFix: ChooseFix)
      (expected: ExpectedResult)
      =
      Expect.isNonEmpty cursorRanges "No Range(s) specified"
      ServerTests.documentTestList name server (Server.createUntitledDocument beforeWithoutCursor) (fun doc -> [
        for (i, range) in cursorRanges |> Seq.indexed do
          let pos =
            if range |> Range.isPosition then
              range.Start.DebuggerDisplay
            else
              $"{range.Start.DebuggerDisplay}..{range.End.DebuggerDisplay}"
          testCaseAsync $"Cursor {i} at {pos}" (async {
            let! (doc, diags) = doc
            do! checkFixAt (doc, diags) (beforeWithoutCursor, range) validateDiagnostics chooseFix expected
          })
      ])

    /// One test for each Cursor.
    ///
    /// Note: Tests single positions -> each `$0` gets checked.
    ///       -> Every test is for single-position range (`Start=End`)!
    let checkAllPositions
      (name: string)
      (server: CachedServer)
      (beforeWithCursors: string)
      (validateDiagnostics: Diagnostic[] -> unit)
      (chooseFix: ChooseFix)
      (expected: ExpectedResult)
      =
      let (beforeWithoutCursor, poss) = beforeWithCursors |> Text.trimTripleQuotation |> Cursors.extract
      let ranges = poss |> List.map (fun p -> { Start = p; End = p })
      checkFixAll name server beforeWithoutCursor ranges validateDiagnostics chooseFix expected

  let testAllPositions
    name
    server
    beforeWithCursors
    validateDiagnostics
    chooseFix
    expected
    =
    Test.checkAllPositions
      name
      server
      beforeWithCursors
      validateDiagnostics
      chooseFix
      (After (expected |> Text.trimTripleQuotation))
  let testApplicableAllPositions
    name
    server
    beforeWithCursors
    validateDiagnostics
    chooseFix
    =
    Test.checkAllPositions
      name
      server
      beforeWithCursors
      validateDiagnostics
      chooseFix
      Applicable
  let testNotApplicableAllPositions
    name
    server
    beforeWithCursors
    validateDiagnostics
    chooseFix
    =
    Test.checkAllPositions
      name
      server
      beforeWithCursors
      validateDiagnostics
      chooseFix
      NotApplicable
