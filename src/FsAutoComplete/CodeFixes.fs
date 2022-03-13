/// This module contains the logic for codefixes that FSAC surfaces, as well as conversion logic between
/// compiler diagnostics and LSP diagnostics/code actions
namespace FsAutoComplete.CodeFix

open FsAutoComplete
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Logging
open FSharp.UMX
open FsToolkit.ErrorHandling
open FSharp.Compiler.Text

module FcsRange = FSharp.Compiler.Text.Range
type FcsRange = FSharp.Compiler.Text.Range
type FcsPos = FSharp.Compiler.Text.Position

module LspTypes = Ionide.LanguageServerProtocol.Types

module Types =
  type IsEnabled = unit -> bool

  type GetRangeText = string<LocalPath> -> LspTypes.Range -> ResultOrString<string>
  type GetFileLines = string<LocalPath> -> ResultOrString<NamedText>
  type GetLineText = NamedText -> LspTypes.Range -> Result<string, string>
  type GetParseResultsForFile = string<LocalPath> -> FSharp.Compiler.Text.Position -> Async<ResultOrString<ParseAndCheckResults * string * NamedText>>
  type GetProjectOptionsForFile = string<LocalPath> -> ResultOrString<FSharp.Compiler.CodeAnalysis.FSharpProjectOptions>

  [<RequireQualifiedAccess>]
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

  type CodeFix = CodeActionParams -> Async<Result<Fix list, string>>

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
             | FixKind.Fix -> "quickfix"
             | FixKind.Refactor -> "refactor"
             | FixKind.Rewrite -> "refactor.rewrite")
        Diagnostics = diagnostic |> Option.map Array.singleton
        IsPreferred = None
        Disabled = None
        Edit = Some workspaceEdit
        Command = None
        Data = None }

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
        fcsPos <- FSharp.Compiler.Text.Position.mkPos lineNumber column
      else
        lineNumber <- lineNumber + 1
        runningLength <- runningLength + lineLength

    fcsPos

  let inc (lines: NamedText) (pos: LspTypes.Position): LspTypes.Position option =
    lines.NextPos (protocolPosToPos pos)
    |> Option.map fcsPosToLsp

  let dec (lines: NamedText) (pos: LspTypes.Position): LspTypes.Position option =
    lines.PrevPos (protocolPosToPos pos)
    |> Option.map fcsPosToLsp

  let rec decMany lines pos count =
    option {
      let mutable pos = pos
      let mutable count = count
      while count > 0 do
        let! nextPos = dec lines pos
        pos <- nextPos
        count <- count - 1
      return pos
    }

  let rec incMany lines pos count =
    option {
      let mutable pos = pos
      let mutable count = count
      while count > 0 do
        let! nextPos = inc lines pos
        pos <- nextPos
        count <- count - 1
      return pos
    }

  let walkBackUntilConditionWithTerminal (lines: NamedText) pos condition terminal =
    let fcsStartPos = protocolPosToPos pos
    lines.WalkBackwards(fcsStartPos, terminal, condition)
    |> Option.map fcsPosToLsp

  let walkForwardUntilConditionWithTerminal (lines: NamedText) pos condition terminal =
    let fcsStartPos = protocolPosToPos pos
    lines.WalkForward(fcsStartPos, terminal, condition)
    |> Option.map fcsPosToLsp

  let walkBackUntilCondition lines pos condition =
    walkBackUntilConditionWithTerminal lines pos condition (fun _ -> false)

  let walkForwardUntilCondition lines pos condition =
    walkForwardUntilConditionWithTerminal lines pos condition (fun _ -> false)

module Run =
  open Types

  let ifEnabled enabled codeFix: CodeFix =
    fun codeActionParams -> if enabled () then codeFix codeActionParams else AsyncResult.retn []

  let private runDiagnostics pred handler: CodeFix =
    fun codeActionParams ->
      codeActionParams.Context.Diagnostics
      |> Array.choose (fun d -> if pred d then Some d else None)
      |> Array.toList
      |> List.traverseAsyncResultM (fun d -> handler d codeActionParams)
      |> AsyncResult.map List.concat


  let ifDiagnosticByMessage (checkMessage: string) handler : CodeFix =
    runDiagnostics (fun d -> d.Message.Contains checkMessage) handler

  let ifDiagnosticByType (diagnosticType: string) handler : CodeFix =
    runDiagnostics (fun d -> d.Source.Contains diagnosticType) handler

  let ifDiagnosticByCode codes handler: CodeFix =
    runDiagnostics (fun d -> d.Code.IsSome && Set.contains d.Code.Value codes) handler
