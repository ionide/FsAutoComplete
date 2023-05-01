module FsAutoComplete.CodeFix.GenerateAbstractClassStub

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.UMX

val title: string

/// a codefix that generates stubs for required override members in abstract types
val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  genAbstractClassStub:
    (ParseAndCheckResults -> FcsRange -> NamedText -> string -> Async<CoreResponse<FcsPos * string>>) ->
  getTextReplacements: (unit -> Map<string, string>) ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
