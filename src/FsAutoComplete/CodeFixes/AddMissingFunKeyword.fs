module FsAutoComplete.CodeFix.AddMissingFunKeyword

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// a codefix that adds a missing 'fun' keyword to a lambda
let fix (getFileLines: GetFileLines) (getLineText: GetLineText): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "10" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let! lines = getFileLines fileName
        let! errorText = getLineText lines diagnostic.Range
        do! Result.guard (fun _ -> errorText = "->") "Expected error source code text not matched"

        let lineLen =
          lines.GetLineString(diagnostic.Range.Start.Line).Length

        let! line =
          getLineText
            lines
            { Start =
                { diagnostic.Range.Start with
                    Character = 0 }
              End =
                { diagnostic.Range.End with
                    Character = lineLen } }

        let charAtPos =
          getLineText
            lines
            ({ Start = diagnostic.Range.Start
               End = inc lines diagnostic.Range.Start })

        let adjustedPos =
          walkBackUntilCondition lines (dec lines diagnostic.Range.Start) (System.Char.IsWhiteSpace >> not)

        match adjustedPos with
        | None -> return []
        | Some firstNonWhitespacePos ->
            let fcsPos = protocolPosToPos firstNonWhitespacePos

            match Lexer.getSymbol fcsPos.Line fcsPos.Column line SymbolLookupKind.Fuzzy [||] with
            | Some lexSym ->
                let fcsStartPos =
                  FSharp.Compiler.Text.Position.mkPos lexSym.Line lexSym.LeftColumn

                let symbolStartRange = fcsPosToProtocolRange fcsStartPos

                return
                  [ { Title = "Add missing 'fun' keyword"
                      File = codeActionParams.TextDocument
                      SourceDiagnostic = Some diagnostic
                      Edits =
                        [| { Range = symbolStartRange
                             NewText = "fun " } |]
                      Kind = FixKind.Fix } ]
            | None -> return []
      }
      )
