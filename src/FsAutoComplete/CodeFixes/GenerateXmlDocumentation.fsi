module FsAutoComplete.CodeFix.GenerateXmlDocumentation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> (CodeActionParams -> Async<Result<Fix list, string>>)
