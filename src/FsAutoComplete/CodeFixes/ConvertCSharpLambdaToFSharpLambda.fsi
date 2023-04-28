/// a codefix that rewrites C#-style '=>' lambdas to F#-style 'fun _ -> _' lambdas
module FsAutoComplete.CodeFix.ConvertCSharpLambdaToFSharpLambda

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

val title: string
val fix:
    getParseResultsForFile: GetParseResultsForFile ->
    getLineText: GetLineText ->
        (CodeActionParams -> Async<Result<Fix list, string>>)
