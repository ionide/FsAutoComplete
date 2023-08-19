module FsAutoComplete.CodeFix.ImplementInterface

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type Config =
  { ObjectIdentifier: string
    MethodBody: string
    IndentationSize: int }

val titleWithTypeAnnotation: string
val titleWithoutTypeAnnotation: string

/// codefix that generates members for an interface implementation
val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  getProjectOptionsForFile: GetProjectOptionsForFile ->
  config: (unit -> Config) ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
