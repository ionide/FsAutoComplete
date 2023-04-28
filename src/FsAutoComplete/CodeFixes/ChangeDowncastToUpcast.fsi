module FsAutoComplete.CodeFix.ChangeDowncastToUpcast

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val titleUpcastOperator: string
val titleUpcastFunction: string
/// a codefix that replaces unsafe casts with safe casts
val fix: getRangeText: GetRangeText -> (CodeActionParams -> Async<Result<Fix list, string>>)
