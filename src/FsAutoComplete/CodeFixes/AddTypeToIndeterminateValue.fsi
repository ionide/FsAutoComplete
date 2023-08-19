module FsAutoComplete.CodeFix.AddTypeToIndeterminateValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.UMX

val title: string

/// fix inderminate type errors by adding an explicit type to a value
val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  getProjectOptionsForFile: GetProjectOptionsForFile ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
