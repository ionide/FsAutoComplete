module FsAutoComplete.CodeFix.UseMutationWhenValueIsMutable

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols

val title: string
/// a codefix that changes equality checking to mutable assignment when the compiler thinks it's relevant
val fix: getParseResultsForFile: GetParseResultsForFile -> (CodeActionParams -> Async<Result<Fix list, string>>)
