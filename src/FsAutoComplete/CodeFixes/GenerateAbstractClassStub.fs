module FsAutoComplete.CodeFix.GenerateAbstractClassStub

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.UMX

let title = "Generate abstract class members"

/// a codefix that generates stubs for required override members in abstract types
let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (genAbstractClassStub: _ -> _ -> _ -> _ -> Async<CoreResponse<string * FcsPos>>)
  (getTextReplacements: unit -> Map<string, string>)
  : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "365" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      // the object expression diagnostic covers the entire interesting range
      let interestingRange = diagnostic.Range

      let fcsRange = interestingRange |> protocolRangeToRange (UMX.untag fileName)

      let! (tyRes, line, lines) = getParseResultsForFile fileName fcsRange.Start

      match! genAbstractClassStub tyRes fcsRange lines line with
      | CoreResponse.Res (text, position) ->
        let replacements = getTextReplacements ()

        let replaced =
          (text, replacements)
          ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))

        return
          [ { SourceDiagnostic = Some diagnostic
              Title = title
              File = codeActionParams.TextDocument
              Edits =
                [| { Range = fcsPosToProtocolRange position
                     NewText = replaced } |]
              Kind = FixKind.Fix } ]
      | _ -> return []
    })
