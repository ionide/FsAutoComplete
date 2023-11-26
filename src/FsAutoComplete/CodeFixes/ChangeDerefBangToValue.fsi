/// replace use of ! operator on ref cells with calls to .Value
module FsAutoComplete.CodeFix.ChangeDerefBangToValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.UMX

val title: string

val fix:
  getParseResultsForFile: GetParseResultsForFile ->
    (Ionide.LanguageServerProtocol.Types.CodeActionParams -> Async<Result<Fix list, string>>)
