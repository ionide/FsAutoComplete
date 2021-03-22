module FsAutoComplete.CodeFix.GenerateUnionCases

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that generates union cases for an incomplete match expression
let fix (getFileLines: GetFileLines)
                       (getParseResultsForFile: GetParseResultsForFile)
                       (generateCases: _ -> _ -> _ -> _ -> Async<CoreResponse<_>>)
                       (getTextReplacements: unit -> Map<string, string>)
                       =
  Run.ifDiagnosticByMessage
    "Incomplete pattern matches on this expression. For example"
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let! (lines: string []) = getFileLines fileName
        let caseLine = diagnostic.Range.Start.Line + 1
        let col = lines.[caseLine].IndexOf('|') + 3 // Find column of first case in patern matching

        let pos =
          FSharp.Compiler.Text.Pos.mkPos (caseLine + 1) (col + 1) //Must points on first case in 1-based system

        let! (tyRes, line, lines) = getParseResultsForFile fileName pos

        match! generateCases tyRes pos lines line |> Async.map Ok with
        | CoreResponse.Res (insertString: string, insertPosition) ->
            let range =
              { Start = fcsPosToLsp insertPosition
                End = fcsPosToLsp insertPosition }

            let replacements = getTextReplacements ()

            let replaced =
              (insertString, replacements)
              ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))

            return
              [ { SourceDiagnostic = Some diagnostic
                  File = codeActionParams.TextDocument
                  Title = "Generate union pattern match cases"
                  Edits = [| { Range = range; NewText = replaced } |]
                  Kind = Fix } ]

          | _ -> return []
        }
      )
