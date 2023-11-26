module FsAutoComplete.CodeFix.GenerateUnionCases

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.CodeFix.Navigation

val title: string

/// a codefix that generates union cases for an incomplete match expression
val fix:
  getFileLines: GetFileLines ->
  getParseResultsForFile: GetParseResultsForFile ->
  generateCases: (ParseAndCheckResults -> FcsPos -> IFSACSourceText -> Async<CoreResponse<string * FcsPos>>) ->
  getTextReplacements: (unit -> Map<string, string>) ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
