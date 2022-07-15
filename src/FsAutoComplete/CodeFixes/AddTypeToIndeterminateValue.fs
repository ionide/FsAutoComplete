module FsAutoComplete.CodeFix.AddTypeToIndeterminateValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.UMX

let title = "Add explicit type annotation"

/// fix inderminate type errors by adding an explicit type to a value
let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (getProjectOptionsForFile: GetProjectOptionsForFile)
  : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "72"; "3245" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let fcsRange =
        protocolRangeToRange (codeActionParams.TextDocument.GetFilePath()) diagnostic.Range

      let! (tyRes, line, lines) = getParseResultsForFile fileName fcsRange.Start

      let! (endColumn, identIslands) =
        Lexer.findLongIdents (fcsRange.Start.Column, line)
        |> Result.ofOption (fun _ -> "No long ident at position")

      match
        tyRes.GetCheckResults.GetDeclarationLocation(fcsRange.Start.Line, endColumn, line, List.ofArray identIslands)
      with
      | FindDeclResult.DeclFound declRange when declRange.FileName = UMX.untag fileName ->
        let! projectOptions = getProjectOptionsForFile fileName
        let protocolDeclRange = fcsRangeToLsp declRange
        let! declText = lines.GetText declRange

        let! declTextLine =
          lines.GetLine declRange.Start
          |> Result.ofOption (fun _ -> "No line found at pos")

        let! declLexerSymbol =
          Lexer.getSymbol
            declRange.Start.Line
            declRange.Start.Column
            declText
            SymbolLookupKind.ByLongIdent
            projectOptions.OtherOptions
          |> Result.ofOption (fun _ -> "No lexer symbol for declaration")

        let! declSymbolUse =
          tyRes.GetCheckResults.GetSymbolUseAtLocation(
            declRange.Start.Line,
            declRange.End.Column,
            declTextLine,
            declLexerSymbol.Text.Split('.') |> List.ofArray
          )
          |> Result.ofOption (fun _ -> "No lexer symbol")

        match declSymbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
          let typeString = mfv.FullType.Format declSymbolUse.DisplayContext

          if mfv.FullType.IsGenericParameter then
            return []
          else
            let alreadyWrappedInParens =
              let hasLeftParen =
                Navigation.walkBackUntilConditionWithTerminal
                  lines
                  protocolDeclRange.Start
                  (fun c -> c = '(')
                  System.Char.IsWhiteSpace

              let hasRightParen =
                Navigation.walkForwardUntilConditionWithTerminal
                  lines
                  protocolDeclRange.End
                  (fun c -> c = ')')
                  System.Char.IsWhiteSpace

              hasLeftParen.IsSome && hasRightParen.IsSome

            let changedText, changedRange =
              if alreadyWrappedInParens then
                ": " + typeString,
                { Start = protocolDeclRange.End
                  End = protocolDeclRange.End }
              else
                "(" + declText + ": " + typeString + ")", protocolDeclRange

            return
              [ { Title = title
                  File = codeActionParams.TextDocument
                  SourceDiagnostic = Some diagnostic
                  Kind = FixKind.Fix
                  Edits =
                    [| { Range = changedRange
                         NewText = changedText } |] } ]
        | _ -> return []

      | _ -> return []
    })
