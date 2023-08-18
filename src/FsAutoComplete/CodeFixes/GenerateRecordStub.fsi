module FsAutoComplete.CodeFix.GenerateRecordStub

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string

/// a codefix that generates member stubs for a record declaration
val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  genRecordStub: (ParseAndCheckResults -> FcsPos -> IFSACSourceText -> string -> Async<CoreResponse<string * FcsPos>>) ->
  getTextReplacements: (unit -> Map<string, string>) ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
