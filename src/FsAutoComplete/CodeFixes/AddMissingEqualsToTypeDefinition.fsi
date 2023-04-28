module FsAutoComplete.CodeFix.AddMissingEqualsToTypeDefinition

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string
/// a codefix that adds in missing '=' characters in type declarations
val fix: getFileLines: GetFileLines -> (CodeActionParams -> Async<Result<Fix list, string>>)
