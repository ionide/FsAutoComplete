module FsAutoComplete.CodeFix.AddExplicitTypeAnnotation

open System
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FsAutoComplete.Core.InlayHints
open FsAutoComplete.Core

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> (CodeActionParams -> Async<Result<Fix list, string>>)
