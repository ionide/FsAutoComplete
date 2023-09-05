module FsAutoComplete.CodeFix.ToInterpolatedString

open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types

val title: string

val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  getLanguageVersion: GetLanguageVersion ->
  codeActionParams: CodeActionParams ->
    Async<Result<Fix list, string>>
