module FsAutoComplete.CodeFix.RemoveUnnecessaryReturnOrYield

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: keyword: string -> string

/// a codefix that removes 'return' or 'yield' (or bang-variants) when the compiler says they're not necessary
val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  getLineText: GetLineText ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
