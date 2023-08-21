module FsAutoComplete.CodeFix.RemoveUnusedBinding

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

val titleParameter: string
val titleBinding: string
val fix: getParseResults: GetParseResultsForFile -> (CodeActionParams -> Async<Result<Fix list, string>>)
