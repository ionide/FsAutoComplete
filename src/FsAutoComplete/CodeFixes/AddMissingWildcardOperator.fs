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
              longDotId = SynLongIdent.SynLongIdent(trivia = [ Some(IdentTrivia.OriginalNotation("|->")) ])
              range = range) when FSharp.Compiler.Text.Range.rangeContainsPos range pos ->

            Some(range)

          | _ -> defaultTraverse expr }

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
              Edits = [| { Range = lspRange; NewText = "| _ ->" } |]
              Kind = FixKind.Fix } ]
    })
