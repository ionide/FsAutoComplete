/// replace use of ! operator on ref cells with calls to .Value
module FsAutoComplete.CodeFix.ChangeDerefBangToValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.UMX

/// adopted from `dotnet/fsharp` -> `FSharp.Compiler.CodeAnalysis.FSharpParseFileResults.TryRangeOfExpressionBeingDereferencedContainingPos`
let private tryGetRangeOfDeref input derefPos =
  SyntaxTraversal.Traverse(
    derefPos,
    input,
    { new SyntaxVisitorBase<_>() with
        member _.VisitExpr(_, _, defaultTraverse, expr) =
          match expr with
          | SynExpr.App (_, false, SynExpr.Ident funcIdent, expr, _) ->
            if
              funcIdent.idText = "op_Dereference"
              && rangeContainsPos funcIdent.idRange derefPos
            then
              Some(funcIdent.idRange, expr.Range)
            else
              None
          | _ -> defaultTraverse expr }
  )

let title = "Use `.Value` instead of dereference operator"

let fix (getParseResultsForFile: GetParseResultsForFile) (getLineText: GetLineText) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3370" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let derefOpRange = protocolRangeToRange (UMX.untag fileName) diagnostic.Range
      let! parseResults, _, _ = getParseResultsForFile fileName derefOpRange.Start

      let! (derefOpRange', exprRange) =
        tryGetRangeOfDeref parseResults.GetParseResults.ParseTree derefOpRange.End
        |> Result.ofOption (fun _ -> "No deref found at that pos")

      assert (derefOpRange = derefOpRange')

      return
        [ { Title = title
            File = codeActionParams.TextDocument
            SourceDiagnostic = None
            Kind = FixKind.Refactor
            Edits =
              [|
                 // remove leading `!` (and whitespaces after `!`)
                 { Range =
                     { Start = fcsPosToLsp derefOpRange'.Start
                       End = fcsPosToLsp exprRange.Start }
                   NewText = "" }
                 // Append trailing `.Value`
                 { Range =
                     let lspPos = fcsPosToLsp exprRange.End
                     { Start = lspPos; End = lspPos }
                   NewText = ".Value" } |] } ]
    })
