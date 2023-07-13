module FsAutoComplete.CodeFix.ToInterpolatedString

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax

let title = "To interpolated string"

let tryFindSprintfApplication (parseAndCheck: ParseAndCheckResults) lineStr fcsPos =
  let application =
    SyntaxTraversal.Traverse(
      fcsPos,
      parseAndCheck.GetParseResults.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(path, traverseSynExpr, defaultTraverse, synExpr) =
            match synExpr with
            | SynExpr.App(ExprAtomicFlag.NonAtomic,
                          false,
                          SynExpr.Ident(sprintfIdent),
                          SynExpr.Const(SynConst.String(text = formatString; synStringKind = SynStringKind.Regular),
                                        mString),
                          mApp) ->
              if
                sprintfIdent.idText = "sprintf"
                && rangeContainsPos mApp fcsPos
                && mApp.StartLine = mApp.EndLine // only support single line for now
                && formatString.Contains("%i") // TODO: the better regex magic to detect the number of arguments
              then
                Some(sprintfIdent.idRange, mString, formatString, path) // probably don't want to return the entire path but just the detected argument
              else
                None
            | _ -> defaultTraverse synExpr }
    )

  application
  |> Option.bind (fun (mSprintf, mString, formatString, path) ->
    parseAndCheck.TryGetSymbolUse mSprintf.End lineStr
    |> Option.bind (fun symbolUse ->
      match symbolUse.Symbol with
      | :? FSharpMemberOrFunctionOrValue as mfv when mfv.Assembly.QualifiedName.StartsWith("FSharp.Core") ->
        // Verify the `sprintf` is the one from F# Core.
        Some(mSprintf, mString, formatString, path)
      | _ -> None))

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! parseAndCheck, lineString, _ = getParseResultsForFile filePath fcsPos

      match tryFindSprintfApplication parseAndCheck lineString fcsPos with
      | None -> return []
      | Some(mSprintf, mString, formatString, path) ->

        return
          [ { Edits =
                [| { Range = fcsRangeToLsp (unionRanges mSprintf mString.StartRange)
                     NewText = "$" } |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
    }
