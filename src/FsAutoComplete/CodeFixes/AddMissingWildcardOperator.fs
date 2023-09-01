module FsAutoComplete.CodeFix.AddMissingWildcardOperator

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTrivia

let title = "Add missing wildcard operator"


let tryFindPattern pos input =

  let visitor =
    { new SyntaxVisitorBase<range>() with

        member _.VisitExpr(path, traverseSynExpr, defaultTraverse, expr) =
          match expr with
          | SynExpr.LongIdent(
                      longDotId =
                          SynLongIdent.SynLongIdent(
                              trivia = [ Some(IdentTrivia.OriginalNotation("|->")) ]
                          )
                      range = range) when FSharp.Compiler.Text.Range.rangeContainsPos range pos ->

                  Some(range)

          | _ -> defaultTraverse expr
    }

  SyntaxTraversal.Traverse(pos, input, visitor)



/// a codefix that adds a missing 'fun' keyword to a lambda
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "43" ]) (fun diagnostic codeActionParams ->
    asyncResult {

      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, _sourceText) = getParseResultsForFile filePath fcsPos

      match tryFindPattern fcsPos parseAndCheck.GetAST with
      | None -> return []
      | Some operatorRange ->

        let lspRange = fcsRangeToLsp operatorRange

        return
            [ { Title = title
                File = codeActionParams.TextDocument
                SourceDiagnostic = Some diagnostic
                Edits =
                  [| { Range = lspRange
                       NewText = "| _ ->" }
                  |]
                Kind = FixKind.Fix } ]
      // let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      // let! lines = getFileLines fileName
      // let! errorText = getLineText lines diagnostic.Range
      // do! Result.guard (fun _ -> errorText = "->") "Expected error source code text not matched"

      // let! lineLen =
      //   lines.GetLineLength(protocolPosToPos diagnostic.Range.Start)
      //   |> Result.ofOption (fun _ -> "Could not get line length")

      // let! line =
      //   getLineText
      //     lines
      //     { Start =
      //         { diagnostic.Range.Start with
      //             Character = 0 }
      //       End =
      //         { diagnostic.Range.End with
      //             Character = lineLen } }

      // let! prevPos =
      //   dec lines diagnostic.Range.Start
      //   |> Result.ofOption (fun _ -> "previous position wasn't valid")

      // let adjustedPos =
      //   walkBackUntilCondition lines prevPos (System.Char.IsWhiteSpace >> not)

      // match adjustedPos with
      // | None -> return []
      // | Some firstNonWhitespacePos ->
      //   let fcsPos = protocolPosToPos firstNonWhitespacePos

      //   match Lexer.getSymbol fcsPos.Line fcsPos.Column line SymbolLookupKind.Fuzzy [||] with
      //   | Some lexSym ->
      //     let fcsStartPos = FSharp.Compiler.Text.Position.mkPos lexSym.Line lexSym.LeftColumn

      //     let symbolStartRange = fcsPosToProtocolRange fcsStartPos

      //     return
      //       [ { Title = title
      //           File = codeActionParams.TextDocument
      //           SourceDiagnostic = Some diagnostic
      //           Edits =
      //             [| { Range = symbolStartRange
      //                  NewText = "fun " } |]
      //           Kind = FixKind.Fix } ]
      //   | None -> return []
    })
