module FsAutoComplete.CodeFix.AddMissingRecKeyword

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.UMX

let title symbolName = $"Make '{symbolName}' recursive"
/// a codefix that adds the 'rec' modifier to a binding in a mutually-recursive loop
let fix (getFileLines: GetFileLines) (getLineText: GetLineText): CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "576" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let! lines = getFileLines fileName
        let endOfError = diagnostic.Range.End
        // this next bit is a bit 'extra': we technically could just slap an ' rec' at the end of the error diagnostic,
        // but instead we're fancy and:
        // * find the first character of the name of the binding
        // * resolve that position to a symbol
        // * get the range of the symbol, in order to
        // * get the symbol name
        // * so we can format a nice message in the code fix
        let! nextPos = inc lines endOfError |> Result.ofOption (fun _ -> "next position wasn't valid")
        let firstWhiteSpaceAfterError =
          walkForwardUntilCondition lines nextPos (System.Char.IsWhiteSpace >> not)

        match firstWhiteSpaceAfterError with
        | None -> return []
        | Some startOfBindingName ->
            let fcsPos = protocolPosToPos startOfBindingName

            let! lineLen =
              lines.GetLineLength (protocolPosToPos diagnostic.Range.Start)
              |> Result.ofOption (fun _ -> "Could not get line length")

            let! line =
              getLineText
                lines
                { Start =
                    { diagnostic.Range.Start with
                        Character = 0 }
                  End =
                    { diagnostic.Range.End with
                        Character = lineLen } }

            match Lexer.getSymbol fcsPos.Line fcsPos.Column line SymbolLookupKind.Fuzzy [||] with
            | Some lexSym ->
                let fcsStartPos =
                  FSharp.Compiler.Text.Position.mkPos lexSym.Line lexSym.LeftColumn

                let fcsEndPos =
                  FSharp.Compiler.Text.Position.mkPos lexSym.Line lexSym.RightColumn

                let protocolRange =
                  fcsRangeToLsp (FSharp.Compiler.Text.Range.mkRange (UMX.untag fileName) fcsStartPos fcsEndPos)

                let! symbolName = getLineText lines protocolRange

                return
                  [ { Title = title symbolName
                      File = codeActionParams.TextDocument
                      SourceDiagnostic = Some diagnostic
                      Edits =
                        [| { Range = { Start = endOfError; End = endOfError }
                             NewText = " rec" } |]
                      Kind = FixKind.Fix } ]
            | None -> return []
      }
      )
