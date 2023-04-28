module FsAutoComplete.CodeFix.UseTripleQuotedInterpolation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.FCSPatches

val title: string

/// a codefix that replaces erroring single-quoted interpolations with triple-quoted interpolations
val fix:
    getParseResultsForFile: GetParseResultsForFile ->
    getRangeText: GetRangeText ->
        (CodeActionParams -> Async<Result<Fix list, string>>)
