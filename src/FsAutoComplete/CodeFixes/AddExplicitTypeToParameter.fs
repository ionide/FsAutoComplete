module FsAutoComplete.CodeFix.AddExplicitTypeToParameter

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let fix : CodeFix =
  fun codeActionParams ->
    asyncResult {
      return []
    }
