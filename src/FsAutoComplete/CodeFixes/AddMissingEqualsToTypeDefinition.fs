module FsAutoComplete.CodeFix.AddMissingEqualsToTypeDefinition

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Add missing '=' to type definition"
/// a codefix that adds in missing '=' characters in type declarations
let fix (getFileLines: GetFileLines) =
  Run.ifDiagnosticByCode
    (Set.ofList [ "3360" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let! lines = getFileLines fileName
        let! walkPos = dec lines diagnostic.Range.Start |> Result.ofOption (fun _ -> "No walk pos")
        match walkBackUntilCondition lines walkPos (System.Char.IsWhiteSpace >> not) with
        | Some firstNonWhitespaceChar ->
            let! insertPos = inc lines firstNonWhitespaceChar |> Result.ofOption (fun _ -> "No insert pos")

            return
              [ { SourceDiagnostic = Some diagnostic
                  Title = title
                  File = codeActionParams.TextDocument
                  Edits =
                    [| { Range = { Start = insertPos; End = insertPos }
                         NewText = "= " } |]
                  Kind = FixKind.Fix } ]
        | None -> return []
      }
      )
