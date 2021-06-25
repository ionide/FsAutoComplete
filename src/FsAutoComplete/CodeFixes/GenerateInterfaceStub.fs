module FsAutoComplete.CodeFix.GenerateInterfaceStub

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open System.IO

/// a codefix that generates member stubs for an interface declaration
let fix (getParseResultsForFile: GetParseResultsForFile)
        (genInterfaceStub: _ -> _ -> _ -> _ -> Async<CoreResponse<string * FcsPos>>)
        (getTextReplacements: unit -> Map<string, string>)
        : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let fileName =
        codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let pos =
        protocolPosToPos codeActionParams.Range.Start

      let! (tyRes, line, lines) = getParseResultsForFile fileName pos

      match! genInterfaceStub tyRes pos lines line with
      | CoreResponse.Res (text, position) ->
          let replacements = getTextReplacements ()

          let replaced =
            (text, replacements)
            ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))

          return
            [ { SourceDiagnostic = None
                Title = "Generate interface stub"
                File = codeActionParams.TextDocument
                Edits =
                  [| { Range = fcsPosToProtocolRange position
                       NewText = replaced } |]
                Kind = FixKind.Fix } ]
      | _ -> return []
    }
