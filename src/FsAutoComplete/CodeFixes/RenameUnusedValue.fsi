module FsAutoComplete.CodeFix.RenameUnusedValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols
open FSharp.UMX
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

val titleReplace: string
val titlePrefix: string

/// a codefix that suggests prepending a _ to unused values
val fix:
    getParseResultsForFile: GetParseResultsForFile ->
        (Ionide.LanguageServerProtocol.Types.CodeActionParams -> Async<Result<Fix list, string>>)
