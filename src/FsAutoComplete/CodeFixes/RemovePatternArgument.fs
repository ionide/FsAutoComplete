module FsAutoComplete.CodeFix.RemovePatternArgument

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text

let title = "Remove argument"

let tryFindPattern pos input =
  let visitor =
    { new SyntaxVisitorBase<range>() with

        member _.VisitExpr(path, traverseSynExpr, defaultTraverse, expr) = defaultTraverse expr

        member this.VisitPat(path: SyntaxVisitorPath, defaultTraverse: SynPat -> range option, synPat: SynPat) =
          match synPat with
          | SynPat.LongIdent(longDotId = synLongIdent; argPats = SynArgPats.Pats(pats); range = m) when
            rangeContainsPos synPat.Range pos && not pats.IsEmpty
            ->
            let mRemove = mkRange m.FileName synLongIdent.Range.End m.End
            Some mRemove
          | SynPat.Paren(pat = innerPat) ->
            let nextPath = SyntaxNode.SynPat(synPat) :: path
            this.VisitPat(nextPath, defaultTraverse, innerPat)
          | _ -> None }

  SyntaxTraversal.Traverse(pos, input, visitor)

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "725"; "3191" ]) (fun _ codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! parseAndCheck, _, _ = getParseResultsForFile filePath fcsPos

      match tryFindPattern fcsPos parseAndCheck.GetAST with
      | None -> return []
      | Some removeRange ->
        let e =
          { Range = fcsRangeToLsp removeRange
            NewText = "" }

        return
          [ { Edits = [| e |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
    })
