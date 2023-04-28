module FsAutoComplete.CodeFix.ConvertTripleSlashCommentToXmlTaggedDoc

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open System

val title: string

val fix:
    getParseResultsForFile: GetParseResultsForFile ->
    getRangeText: GetRangeText ->
        (CodeActionParams -> Async<Result<Fix list, string>>)
