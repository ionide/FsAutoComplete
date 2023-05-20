module FsAutoComplete.CodeFix.GenerateXmlDocumentation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Generate placeholder XML documentation"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, _sourceText) = getParseResultsForFile filePath fcsPos

      let! docEdit = Commands.GenerateXmlDocumentation(parseAndCheck, fcsPos, lineStr)

      match docEdit with
      | Some({ InsertPosition = insertPosition
               InsertText = formattedXmlDoc }) ->
        let protocolPos = fcsPosToLsp insertPosition

        let editRange =
          { Start = protocolPos
            End = protocolPos }

        let text = formattedXmlDoc

        return
          [ { Edits = [| { Range = editRange; NewText = text } |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
      | _ -> return []
    }
