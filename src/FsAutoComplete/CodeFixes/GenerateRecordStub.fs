module FsAutoComplete.CodeFix.GenerateRecordStub

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that generates member stubs for a record declaration
let fix (getParseResultsForFile: GetParseResultsForFile)
        (genRecordStub: _ -> _ -> _ -> _ -> Async<CoreResponse<string * FcsPos>>)
        (getTextReplacements: unit -> Map<string, string>)
        : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let fileName =
        codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let pos =
        protocolPosToPos codeActionParams.Range.Start

      let! (tyRes, line, lines) = getParseResultsForFile fileName pos

      match! genRecordStub tyRes pos lines line with
      | CoreResponse.Res (text, position) ->
          let replacements = getTextReplacements ()

          let replaced =
            (text, replacements)
            ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))

          return
            [ { SourceDiagnostic = None
                Title = "Generate record stub"
                File = codeActionParams.TextDocument
                Edits =
                  [| { Range = fcsPosToProtocolRange position
                       NewText = replaced } |]
                Kind = Fix } ]
      | _ -> return []
    }
    |> AsyncResult.foldResult id (fun _ -> [])
