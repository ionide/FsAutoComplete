module FsAutoComplete.CodeFix.AddMissingWildcardOperator

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string
/// a codefix that adds a missing wildcard pattern to a match case
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
