module FsAutoComplete.CodeFix.RemoveUnusedOpens

open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.CodeFix.Navigation

val title: string
/// a codefix that removes unused open statements from the source
val fix: getFileLines: GetFileLines -> (CodeActionParams -> Async<Result<Fix list, string>>)
