module FsAutoComplete.CodeFix.RemoveRedundantQualifier

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types

val title: string
/// a codefix that removes unnecessary qualifiers from an identifier
val fix: (CodeActionParams -> Async<Result<Fix list, string>>)
