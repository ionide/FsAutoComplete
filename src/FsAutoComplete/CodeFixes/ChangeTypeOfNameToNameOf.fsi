/// a codefix that replaces typeof<'t>.Name with nameof('t)
module FsAutoComplete.CodeFix.ChangeTypeOfNameToNameOf

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

val title: string

val fix:
    getParseResultsForFile: GetParseResultsForFile ->
        (Ionide.LanguageServerProtocol.Types.CodeActionParams -> Async<Result<Fix list, string>>)
