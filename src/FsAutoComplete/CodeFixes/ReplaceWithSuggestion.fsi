module FsAutoComplete.CodeFix.ReplaceWithSuggestion

open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete
open FSharp.Compiler.Syntax

val title: suggestion: string -> string
/// a codefix that replaces the use of an unknown identifier with a suggested identifier
val fix: (Ionide.LanguageServerProtocol.Types.CodeActionParams -> Async<Result<Fix list, string>>)
