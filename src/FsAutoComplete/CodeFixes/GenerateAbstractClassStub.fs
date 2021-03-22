module FsAutoComplete.CodeFix.GenerateAbstractClassStub

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.UMX

/// a codefix that generates stubs for required override members in abstract types
let fix (getParseResultsForFile: GetParseResultsForFile)
        (genAbstractClassStub: _ -> _ -> _ -> _ -> Async<CoreResponse<string * FcsPos>>)
        (getTextReplacements: unit -> Map<string, string>)
        : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "365"; "54" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let interestingRange =
          (match diagnostic.Code with
           | Some "365" ->
               // the object expression diagnostic covers the entire interesting range
               diagnostic.Range
           | Some "54" ->
               // the full-class range is on the typename, which should be enough to enable traversal
               diagnostic.Range
           | _ ->
               // everything else is a best guess
               codeActionParams.Range)
          |> protocolRangeToRange (UMX.untag fileName)

        let! (tyRes, line, lines) = getParseResultsForFile fileName interestingRange.Start

        match! genAbstractClassStub tyRes interestingRange lines line with
        | CoreResponse.Res (text, position) ->
            let replacements = getTextReplacements ()

            let replaced =
              (text, replacements)
              ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))

            return
              [ { SourceDiagnostic = Some diagnostic
                  Title = "Generate abstract class members"
                  File = codeActionParams.TextDocument
                  Edits =
                    [| { Range = fcsPosToProtocolRange position
                         NewText = replaced } |]
                  Kind = Fix } ]
        | _ -> return []
      }
      )
