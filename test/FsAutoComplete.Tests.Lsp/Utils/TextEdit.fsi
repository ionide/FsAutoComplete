module Utils.TextEdit

open Ionide.LanguageServerProtocol.Types
open Utils.Utils
open Expecto
open FsToolkit.ErrorHandling

/// Functions to extract Cursor or Range from a given string.
/// Cursor is marked in string with `$0` (`Cursor.Marker`)
///
/// Note: Only `\n` is supported. Neither `\r\n` nor `\r` produce correct results.
module Cursor =
  /// 0-based
  val inline private pos: line: uint32 -> column: uint32 -> Position

  /// Cursor Marker in text.
  /// Single marker: Position
  /// Two markers: Range
  [<Literal>]
  val Marker: string = "$0"

  /// Returns Cursor Position BEFORE index
  ///
  /// Index might be `text.Length` (-> cursor AFTER last character).
  /// All other out of text range indices throw exception.
  val beforeIndex: i: uint32 -> text: string -> Position
  /// Returns index of first `$0` (`Cursor.Marker`) and the updated input text without the cursor marker.
  ///
  /// Note: Cursor Position is BEFORE index.
  /// Note: Index might be `text.Length` (-> Cursor AFTER last char in text)
  val tryExtractIndex: text: string -> (uint32 * string) option
  /// `tryExtractIndex`, but fails when there's no cursor
  val assertExtractIndex: (string -> uint32 * string)
  /// Extracts first cursor marked with any of `markers`. Remaining cursors aren't touched
  val tryExtractPositionMarkedWithAnyOf: markers: string[] -> text: string -> ((string * Position) * string) option
  /// Returns Position of first `$0` (`Cursor.Marker`) and the updated input text without the cursor marker.
  /// Only the first `$0` is processed.
  ///
  /// Note: Cursor Position is BETWEEN characters and might be outside of text range (cursor AFTER last character)
  val tryExtractPosition: (string -> (Position * string) option)
  /// `tryExtractPosition`, but fails when there's no cursor
  val assertExtractPosition: (string -> Position * string)
  /// Returns Range between the first two `$0` (`Cursor.Marker`) and the updated text without the two cursor markers.
  ///
  /// If there's only one cursor marker, the range covers exactly that position (`Start = End`)
  val tryExtractRange: text: string -> (Range * string) option
  /// `tryExtractRange`, but fails when there's no cursor.
  val assertExtractRange: (string -> Range * string)
  /// Position is between characters, while index is on character.
  /// For Insert & Remove: character indices
  ///
  /// Returned index is AFTER cursor:
  /// * `Column=0`: before first char; `Index=0`: on first char
  /// * `Column=1`: after first char, before 2nd char; `Index=1`: on 2nd char
  /// * `Column=max`: after last char; `Index=max`: AFTER last char in line (-> `\n` or end of string)
  val tryIndexOf: pos: Position -> text: string -> Result<uint32, string>
  /// `tryIndexOf`, but fails when position is invalid
  val assertIndexOf: pos: Position -> (string -> uint32)
  /// Calculates cursors position after all edits are applied.
  ///
  /// When cursor inside a changed area:
  /// * deleted: cursor moves to start of deletion:
  ///   ```fsharp
  ///   let foo = 42 $|+ $013 $|+ 123
  ///   ```
  ///   -> delete inside `$|`
  ///   ```fsharp
  ///   let foo = 42 $0+ 123
  ///   ```
  /// * inserted: cursor stays at start of insert
  ///   ```fsharp
  ///   let foo = 42 $0+ 123
  ///   ```
  ///   -> insert at cursor pos
  ///   ```fsharp
  ///   let foo = 42 $0+ 13 + 123
  ///   ```
  /// * changes: cursors moved to start of replacement
  ///   ```fsharp
  ///   let foo = 42 $|+ $013 $|+ 123
  ///   ```
  ///   -> replace inside `$|`
  ///   ```fsharp
  ///   let foo = 42 $0- 7 + 123
  ///   ```
  ///   -> like deletion
  ///   * Implementation detail:
  ///     Replacement is considered: First delete (-> move cursor to front), then insert (-> cursor stays)
  ///
  /// Note: `edits` must be sorted by range!
  val afterEdits: edits: TextEdit[] -> pos: Position -> Position

module Cursors =
  /// For each cursor (`$0`) in text: return text with just that one cursor
  ///
  /// Note: doesn't trim input!
  val iter: textWithCursors: string -> string list
  /// Returns all cursor (`$0`) positions and the text without any cursors.
  ///
  /// Unlike `iter` this extracts positions instead of reducing to texts with one cursor
  val extract: textWithCursors: string -> string * Position list
  /// Like `extract`, but instead of just extracting Cursors marked with `Cursor.Marker` (`$0`),
  /// this here extract all specified markers.
  val extractWith: markers: string[] -> text: string -> string * (string * Position) list
  /// Like `extractWith`, but additional groups cursor positions by marker
  val extractGroupedWith: markers: string[] -> text: string -> string * Map<string, Position list>

module Text =
  val remove: range: Range -> text: string -> Result<string, string>
  val insert: pos: Position -> insert: string -> text: string -> Result<string, string>
  val replace: range: Range -> replacement: string -> text: string -> Result<string, string>

module TextEdit =
  val apply: edit: TextEdit -> (string -> Result<string, string>)
  val deletes: edit: TextEdit -> bool
  val inserts: edit: TextEdit -> bool
  val replaces: edit: TextEdit -> bool
  val doesNothing: edit: TextEdit -> bool
  /// Checks passed `edit` for errors:
  /// * Positive Lines & Characters in Ranges
  ///   * Note: doesn't test if range is inside text! Just simple positive test.
  /// * Start Range must be before or equal End Range
  /// * Does something (-> must insert or delete (or both -> replace) something)
  ///   * Note: empty edit is technically valid, but in practice it's most likely an error
  val tryFindError: edit: TextEdit -> string option

module TextEdits =
  /// Checks edits for:
  /// * There's at least one TextEdit
  /// * All TextEdits are valid (`TextEdit.tryFindError`)
  /// * Edits don't overlap
  /// * For same position: All inserted before at most one replace (or delete)
  ///
  ///
  /// [LSP Specification for `TextEdit[]`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textEditArray)
  /// > Text edits ranges must never overlap, that means no part of the original document must be manipulated by more than one edit.
  /// > However, it is possible that multiple edits have the same start position: multiple inserts,
  /// > or any number of inserts followed by a single remove or replace edit.
  /// > If multiple inserts have the same position, the order in the array defines the order
  /// > in which the inserted strings appear in the resulting text.
  val tryFindError: edits: TextEdit[] -> string option
  /// Sorts edits by range (`Start`).
  /// Order is preserved for edits with same `Start`.
  val sortByRange: edits: TextEdit[] -> TextEdit[]
  /// Applies the passed edits from last to first (sorted by range)
  val apply: edits: TextEdit[] -> text: string -> Result<string, string>
  /// `tryFindError` before `apply`
  val applyWithErrorCheck: edits: TextEdit[] -> text: string -> Result<string, string>

module WorkspaceEdit =
  /// Extract `TextEdit[]` from either `DocumentChanges` or `Changes`.
  /// All edits MUST be for passed `textDocument`.
  ///
  /// Checks for errors:
  /// * Either `DocumentChanges` or `Changes`, but not both
  ///   * FsAutoComplete sends only `DocumentChanges`
  /// * All edits inside `textDocument`
  ///   * Version is only checked if: Version in `textDocument` and Version in `workspaceEdit.DocumentChanges.*`
  /// * Using `TextEdit.tryFindError`:
  ///   * At least one edit
  ///   * No empty edit
  ///   * No overlaps
  val tryExtractTextEditsInSingleFile:
    textDocument: VersionedTextDocumentIdentifier -> workspaceEdit: WorkspaceEdit -> Result<TextEdit[], string>
