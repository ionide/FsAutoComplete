module FsAutoComplete.CodeFix.MissingEquals

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that adds in missing '=' characters in type declarations
let fix (getFileLines: GetFileLines) =
  Run.ifDiagnosticByCode
    (Set.ofList [ "10"; "3360" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        if diagnostic.Message.Contains "Unexpected symbol '{' in type definition"
           || diagnostic.Message.Contains "Unexpected keyword 'member' in type definition" then
          let fileName =
            codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

          let! lines = getFileLines fileName

          match walkBackUntilCondition lines (dec lines diagnostic.Range.Start) (System.Char.IsWhiteSpace >> not) with
          | Some firstNonWhitespaceChar ->
              let insertPos = inc lines firstNonWhitespaceChar

              return
                [ { SourceDiagnostic = Some diagnostic
                    Title = "Add missing '=' to type definition"
                    File = codeActionParams.TextDocument
                    Edits =
                      [| { Range = { Start = insertPos; End = insertPos }
                           NewText = " =" } |]
                    Kind = FixKind.Fix } ]
          | None -> return []
        else
          return []
      }
      )
