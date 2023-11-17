module FsAutoComplete.CodeFix.WrapExpressionInParentheses

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete

val title: string
/// a codefix that parenthesizes a member expression that needs it
val fix: (CodeActionParams -> Async<Result<Fix list, string>>)
