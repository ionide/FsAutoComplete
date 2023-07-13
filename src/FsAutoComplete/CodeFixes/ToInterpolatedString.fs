module FsAutoComplete.CodeFix.ToInterpolatedString

open System.Text.RegularExpressions
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let title = "To interpolated string"

let specifierRegex = Regex(@"\%(s|i)")

let tryFindSprintfApplication (parseAndCheck: ParseAndCheckResults) (sourceText: IFSACSourceText) lineStr fcsPos =
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
                          SynExpr.Const(SynConst.String(synStringKind = SynStringKind.Regular), mString),
                          mApp) ->
              // Don't trust the value of SynConst.String, it is already a somewhat optimized version of what the user actually code.
              match sourceText.GetText mString with
              | Error _ -> None
              | Ok formatString ->
                if
                  sprintfIdent.idText = "sprintf"
                  && rangeContainsPos mApp fcsPos
                  && mApp.StartLine = mApp.EndLine // only support single line for now
                then
                  // Find all the format parameters in the source string
                  // Things like `%i` or `%s`
                  let arguments =
                    specifierRegex.Matches(formatString) |> Seq.cast<Match> |> Seq.toList

                  if arguments.IsEmpty || path.Length < arguments.Length then
                    None
                  else
                    let xs =
                      let argumentsInPath = List.take arguments.Length path

                      (arguments, argumentsInPath)
                      ||> List.zip
                      |> List.choose (fun (regexMatch, node) ->
                        match node with
                        | SyntaxNode.SynExpr(SynExpr.App(argExpr = ae)) -> Some(regexMatch, ae.Range)
                        | _ -> None)

                    List.tryLast xs
                    |> Option.bind (fun (_, mLastArg) ->
                      // Ensure the last argument of the current application is also on the same line.
                      if mApp.StartLine <> mLastArg.EndLine then
                        None
                      else
                        Some(sprintfIdent.idRange, mString, xs, mLastArg))
                else
                  None
            | _ -> defaultTraverse synExpr }
    )

  application
  |> Option.bind (fun (mSprintf, mString, xs, mLastArg) ->
    parseAndCheck.TryGetSymbolUse mSprintf.End lineStr
    |> Option.bind (fun symbolUse ->
      match symbolUse.Symbol with
      | :? FSharpMemberOrFunctionOrValue as mfv when mfv.Assembly.QualifiedName.StartsWith("FSharp.Core") ->
        // Verify the `sprintf` is the one from F# Core.
        Some(mSprintf, mString, xs, mLastArg)
      | _ -> None))

// TODO: this whole thing should only work if the language version is high enough

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! parseAndCheck, lineString, sourceText = getParseResultsForFile filePath fcsPos

      match tryFindSprintfApplication parseAndCheck sourceText lineString fcsPos with
      | None -> return []
      | Some(mSprintf, mString, arguments, mLastArg) ->
        let replaceSprintfEdit =
          { Range = fcsRangeToLsp (unionRanges mSprintf mString.StartRange)
            NewText = "$" }

        let insertArgumentEdits =
          arguments
          |> List.choose (fun (regexMatch, mArg) ->
            match sourceText.GetText(mArg) with
            | Error _ -> None
            | Ok argText ->
              let mReplace =
                let stringPos =
                  Position.mkPos mString.StartLine (mString.StartColumn + regexMatch.Index + regexMatch.Length)

                mkRange mSprintf.FileName stringPos stringPos

              Some
                { Range = fcsRangeToLsp mReplace
                  NewText = $"{{{argText}}}" })

        let removeArgumentEdits =
          let m = mkRange mSprintf.FileName mString.End mLastArg.End

          { Range = fcsRangeToLsp m
            NewText = "" }

        return
          [ { Edits =
                [| yield replaceSprintfEdit
                   yield! insertArgumentEdits
                   yield removeArgumentEdits |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
    }
