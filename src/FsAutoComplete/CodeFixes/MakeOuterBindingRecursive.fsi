/// a codefix that makes a binding 'rec' if something inside the binding requires recursive access
module FsAutoComplete.CodeFix.MakeOuterBindingRecursive

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string

val fix:
    getParseResultsForFile: GetParseResultsForFile ->
    getLineText: GetLineText ->
        (CodeActionParams -> Async<Result<Fix list, string>>)
