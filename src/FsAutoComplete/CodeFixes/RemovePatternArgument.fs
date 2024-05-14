module FsAutoComplete.CodeFix.RemovePatternArgument

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

let title = "Remove argument"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "725"; "3191" ]) (fun _ codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! parseAndCheck, _, _ = getParseResultsForFile filePath fcsPos

      return
        (fcsPos, parseAndCheck.GetAST)
        ||> ParsedInput.tryPick (fun _path node ->
          match node with
          | SyntaxNode.SynPat(SynPat.LongIdent(longDotId = synLongIdent; argPats = SynArgPats.Pats(_ :: _); range = m)) when
            rangeContainsPos m fcsPos
            ->
            Some(mkRange m.FileName synLongIdent.Range.End m.End)
          | _ -> None)
        |> Option.toList
        |> List.map (fun range ->
          { Edits =
              [| { Range = fcsRangeToLsp range
                   NewText = "" } |]
            File = codeActionParams.TextDocument
            Title = title
            SourceDiagnostic = None
            Kind = FixKind.Refactor })
    })
