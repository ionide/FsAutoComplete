module FsAutoComplete.CodeFix.AddMissingRecKeyword

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.UMX

val title: symbolName: 'a -> string
/// a codefix that adds the 'rec' modifier to a binding in a mutually-recursive loop
val fix: getFileLines: GetFileLines -> getLineText: GetLineText -> (CodeActionParams -> Async<Result<Fix list, string>>)
