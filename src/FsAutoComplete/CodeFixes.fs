/// This module contains the logic for codefixes that FSAC surfaces, as well as conversion logic between
/// compiler diagnostics and LSP diagnostics/code actions
namespace FsAutoComplete.CodeFix

open FsAutoComplete
open FsAutoComplete.LspHelpers
open LanguageServerProtocol.Types
open FsAutoComplete.Logging
open FSharp.UMX
open FsToolkit.ErrorHandling

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
type FcsPos = FSharp.Compiler.Text.Pos

module LspTypes = LanguageServerProtocol.Types

module Types =
  type IsEnabled = unit -> bool

  type FileLines = string []
  type FileLine = string
  type GetRangeText = string<LocalPath> -> LspTypes.Range -> ResultOrString<FileLine>
  type GetFileLines = string<LocalPath> -> ResultOrString<FileLines>
  type GetLineText = FileLines -> LspTypes.Range -> FileLine
  type GetParseResultsForFile = string<LocalPath> -> FSharp.Compiler.Text.Pos -> Async<ResultOrString<ParseAndCheckResults * string * string array>>
  type GetProjectOptionsForFile = string<LocalPath> -> ResultOrString<FSharp.Compiler.SourceCodeServices.FSharpProjectOptions>

  type FixKind =
    | Fix
    | Refactor
    | Rewrite

  type Fix =
    { Edits: TextEdit []
      File: TextDocumentIdentifier
      Title: string
      SourceDiagnostic: Diagnostic option
      Kind: FixKind }

  type CodeFix = CodeActionParams -> Async<Fix list>

  type CodeAction with
    static member OfFix getFileVersion clientCapabilities (fix: Fix) =
      let filePath = fix.File.GetFilePath() |> Utils.normalizePath
      let fileVersion = getFileVersion filePath

      CodeAction.OfDiagnostic fix.File fileVersion fix.Title fix.SourceDiagnostic fix.Edits fix.Kind clientCapabilities

    static member OfDiagnostic (fileUri) (fileVersion) title (diagnostic) (edits) fixKind clientCapabilities
                               : CodeAction =

      let edit =
        { TextDocument =
            { Uri = fileUri.Uri
              Version = fileVersion }
          Edits = edits }

      let workspaceEdit =
        WorkspaceEdit.Create([| edit |], clientCapabilities)

      { CodeAction.Title = title
        Kind =
          Some
            (match fixKind with
             | Fix -> "quickfix"
             | Refactor -> "refactor"
             | Rewrite -> "refactor.rewrite")
        Diagnostics = diagnostic |> Option.map Array.singleton
        Edit = workspaceEdit
        Command = None }

/// helpers for iterating along text lines
module Navigation =

  let findPosForCharacter (lines: string []) (pos: int) =
    let mutable lineNumber = 0
    let mutable runningLength = 0
    let mutable found = false

    let mutable fcsPos =
      Unchecked.defaultof<FcsPos>

    while not found do
      let line = lines.[lineNumber]
      let lineLength = line.Length

      if pos <= runningLength + lineLength then
        let column = pos - runningLength
        found <- true
        fcsPos <- FSharp.Compiler.Text.Pos.mkPos lineNumber column
      else
        lineNumber <- lineNumber + 1
        runningLength <- runningLength + lineLength

    fcsPos

  /// advance along positions from a starting location, incrementing in a known way until a condition is met.
  /// when the condition is met, return that position.
  /// if the condition is never met, return None
  let walkPos (lines: string []) (pos: LspTypes.Position) posChange condition: LspTypes.Position option =
    let charAt (pos: LspTypes.Position) = lines.[pos.Line].[pos.Character]

    let firstPos = { Line = 0; Character = 0 }

    let finalPos =
      { Line = lines.Length - 1
        Character = lines.[lines.Length - 1].Length - 1 }

    let rec loop pos =
      if firstPos = pos || finalPos = pos then None
      else if not (condition (charAt pos)) then loop (posChange pos)
      else Some pos

    loop pos

  let inc (lines: string []) (pos: LspTypes.Position): LspTypes.Position =
    let lineLength = lines.[pos.Line].Length

    if pos.Character = lineLength - 1 then
      { Line = pos.Line + 1; Character = 0 }
    else
      { pos with
          Character = pos.Character + 1 }

  let dec (lines: string []) (pos: LspTypes.Position): LspTypes.Position =
    if pos.Character = 0 then
      let newLine = pos.Line - 1
      // decrement to end of previous line
      { pos with
          Line = newLine
          Character = lines.[newLine].Length - 1 }
    else
      { pos with
          Character = pos.Character - 1 }

  let rec decMany lines pos count =
    if count <= 0 then pos
    else decMany lines (dec lines pos) (count - 1)

  let rec incMany lines pos count =
    if count <= 0 then pos
    else incMany lines (inc lines pos) (count - 1)

  let walkBackUntilCondition (lines: string []) (pos: LspTypes.Position) condition =
    walkPos lines pos (dec lines) condition

  let walkForwardUntilCondition (lines: string []) (pos: LspTypes.Position) condition =
    walkPos lines pos (inc lines) condition

module Run =
  open Types

  let ifEnabled enabled codeFix: CodeFix =
    fun codeActionParams -> if enabled () then codeFix codeActionParams else async.Return []

  let private runDiagnostics pred handler: CodeFix =
    let logger = LogProvider.getLoggerByName "CodeFixes"
    fun codeActionParams ->
      codeActionParams.Context.Diagnostics
      |> Array.choose (fun d -> if pred d then Some d else None)
      |> Array.toList
      |> List.traverseAsyncResultM (fun d -> handler d codeActionParams)
      |> AsyncResult.map List.concat
      |> AsyncResult.foldResult id (fun errs ->
        logger.warn (Log.setMessage "CodeFix returned an error: {error}" >> Log.addContextDestructured "error" errs)
        []
      )

  let ifDiagnosticByMessage (checkMessage: string) handler : CodeFix =
    runDiagnostics (fun d -> d.Message.Contains checkMessage) handler

  let ifDiagnosticByType (diagnosticType: string) handler : CodeFix =
    runDiagnostics (fun d -> d.Source.Contains diagnosticType) handler

  let ifDiagnosticByCode codes handler: CodeFix =
    runDiagnostics (fun d -> d.Code.IsSome && Set.contains d.Code.Value codes) handler
