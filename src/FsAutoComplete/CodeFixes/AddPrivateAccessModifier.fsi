module FsAutoComplete.CodeFix.AddPrivateAccessModifier

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text.Range

val title: string

type SymbolUseWorkspace =
  bool
    -> bool
    -> bool
    -> FSharp.Compiler.Text.Position
    -> LineStr
    -> IFSACSourceText
    -> ParseAndCheckResults
    -> Async<
      Result<
        System.Collections.Generic.IDictionary<FSharp.UMX.string<LocalPath>, FSharp.Compiler.Text.range array>,
        string
       >
     >

val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  symbolUseWorkspace: SymbolUseWorkspace ->
    (CodeActionParams -> Async<Result<Fix list, string>>)
