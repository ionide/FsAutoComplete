module FsAutoComplete.CodeFix.ChangePrefixNegationToInfixSubtraction

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

val title: string
/// a codefix that corrects -<something> to - <something> when negation is not intended
val fix: getFileLines: GetFileLines -> (CodeActionParams -> Async<Result<Fix list, string>>)
