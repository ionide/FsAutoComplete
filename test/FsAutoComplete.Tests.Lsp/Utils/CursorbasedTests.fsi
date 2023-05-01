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

    val checkFixAt:
        doc: Document * diagnostics: Diagnostic[] ->
            beforeWithoutCursor: string * cursorRange: Range ->
                validateDiagnostics: (Diagnostic[] -> unit) ->
                chooseFix: ChooseFix ->
                expected: ExpectedResult ->
                    Async<unit>

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
    val check:
        server: CachedServer ->
        beforeWithCursor: string ->
        validateDiagnostics: (Diagnostic array -> unit) ->
        chooseFix: ChooseFix ->
        expected: string ->
            Async<unit>

    /// Note: Doesn't apply Fix! Just checks its existence!
    val checkApplicable:
        server: CachedServer ->
        beforeWithCursor: string ->
        validateDiagnostics: (Diagnostic array -> unit) ->
        chooseFix: ChooseFix ->
            Async<unit>

    val checkNotApplicable:
        server: CachedServer ->
        beforeWithCursor: string ->
        validateDiagnostics: (Diagnostic array -> unit) ->
        chooseFix: ChooseFix ->
            Async<unit>

    val matching: cond: (CodeAction -> bool) -> fixes: CodeAction array -> CodeAction array
    val withTitle: title: string -> (CodeAction array -> CodeAction array)
    val ofKind: kind: string -> (CodeAction array -> CodeAction array)

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
        val checkFixAll:
            name: string ->
            server: CachedServer ->
            beforeWithoutCursor: string ->
            cursorRanges: Range seq ->
            validateDiagnostics: (Diagnostic[] -> unit) ->
            chooseFix: ChooseFix ->
            expected: ExpectedResult ->
                Test

        /// One test for each Cursor.
        ///
        /// Note: Tests single positions -> each `$0` gets checked.
        ///       -> Every test is for single-position range (`Start=End`)!
        val checkAllPositions:
            name: string ->
            server: CachedServer ->
            beforeWithCursors: string ->
            validateDiagnostics: (Diagnostic[] -> unit) ->
            chooseFix: ChooseFix ->
            expected: (unit -> ExpectedResult) ->
                Test

    val testAllPositions:
        name: string ->
        server: CachedServer ->
        beforeWithCursors: string ->
        validateDiagnostics: (Diagnostic array -> unit) ->
        chooseFix: ChooseFix ->
        expected: string ->
            Test

    val testApplicableAllPositions:
        name: string ->
        server: CachedServer ->
        beforeWithCursors: string ->
        validateDiagnostics: (Diagnostic array -> unit) ->
        chooseFix: ChooseFix ->
            Test

    val testNotApplicableAllPositions:
        name: string ->
        server: CachedServer ->
        beforeWithCursors: string ->
        validateDiagnostics: (Diagnostic array -> unit) ->
        chooseFix: ChooseFix ->
            Test
