module FsAutoComplete.CodeFix.AddNewKeywordToDisposableConstructorInvocation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string
/// a codefix that suggests using the 'new' keyword on IDisposables
val fix: getRangeText: GetRangeText -> (CodeActionParams -> Async<Result<Fix list, string>>)
