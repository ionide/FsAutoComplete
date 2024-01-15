module FsAutoComplete.CodeFix.ToInterpolatedString

open System
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
open FsAutoComplete.FCSPatches

let title = "To interpolated string"

let languageFeature = lazy (LanguageFeatureShim("StringInterpolation"))

/// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting#format-specifiers-for-printf
let specifierRegex =
  Regex(@"\%(\+|\-)?\.?\d*(b|s|c|d|i|u|x|X|o|B|e|E|f|F|g|G|M|O|A)")

let validFunctionNames = set [| "printf"; "printfn"; "sprintf" |]

let inline synExprNeedsSpaces synExpr =
  match synExpr with
  | SynExpr.AnonRecd _
  | SynExpr.Record _
  | SynExpr.ObjExpr _ -> true
  | _ -> false

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
                          SynExpr.Ident(functionIdent),
                          SynExpr.Const(SynConst.String(synStringKind = SynStringKind.Regular), mString),
                          mApp) ->
              // Don't trust the value of SynConst.String, it is already a somewhat optimized version of what the user actually code.
              match sourceText.GetText mString with
              | Error _ -> None
              | Ok formatString ->
                if
                  validFunctionNames.Contains functionIdent.idText
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
                        | SyntaxNode.SynExpr(SynExpr.App(argExpr = ae)) ->
                          Some(regexMatch, ae.Range, synExprNeedsSpaces ae)
                        | _ -> None)

                    List.tryLast xs
                    |> Option.bind (fun (_, mLastArg, _) ->
                      // Ensure the last argument of the current application is also on the same line.
                      if mApp.StartLine <> mLastArg.EndLine then
                        None
                      else
                        Some(functionIdent, mString, xs, mLastArg))
                else
                  None
            | _ -> defaultTraverse synExpr }
    )

  application
  |> Option.bind (fun (functionIdent, mString, xs, mLastArg) ->
    parseAndCheck.TryGetSymbolUse functionIdent.idRange.End lineStr
    |> Option.bind (fun symbolUse ->
      match symbolUse.Symbol with
      | :? FSharpMemberOrFunctionOrValue as mfv when
        mfv.Assembly.QualifiedName.StartsWith("FSharp.Core", StringComparison.Ordinal)
        ->
        // Verify the function is from F# Core.
        Some(functionIdent, mString, xs, mLastArg)
      | _ -> None))

let fix (getParseResultsForFile: GetParseResultsForFile) (getLanguageVersion: GetLanguageVersion) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let! languageVersion = getLanguageVersion filePath

      if not (languageVersion.SupportsFeature languageFeature.Value) then
        return []
      else
        let fcsPos = protocolPosToPos codeActionParams.Range.Start
        let! parseAndCheck, lineString, sourceText = getParseResultsForFile filePath fcsPos

        match tryFindSprintfApplication parseAndCheck sourceText lineString fcsPos with
        | None -> return []
        | Some(functionIdent, mString, arguments, mLastArg) ->
          let functionEdit =
            if functionIdent.idText = "sprintf" then
              // Remove the `sprintf` function call
              { Range = fcsRangeToLsp (unionRanges functionIdent.idRange mString.StartRange)
                NewText = "$" }
            else
              // Insert the dollar sign before the string
              { Range = fcsRangeToLsp mString.StartRange
                NewText = "$" }

          let insertArgumentEdits =
            arguments
            |> List.choose (fun (regexMatch, mArg, surroundWithSpaces) ->
              match sourceText.GetText(mArg) with
              | Error _ -> None
              | Ok argText ->
                let mReplace =
                  let stringPos =
                    Position.mkPos mString.StartLine (mString.StartColumn + regexMatch.Index + regexMatch.Length)

                  mkRange functionIdent.idRange.FileName stringPos stringPos

                Some
                  { Range = fcsRangeToLsp mReplace
                    NewText =
                      sprintf
                        "%s%s%s"
                        (if surroundWithSpaces then "{ " else "{")
                        argText
                        (if surroundWithSpaces then " }" else "}") })

          let removeArgumentEdits =
            let m = mkRange functionIdent.idRange.FileName mString.End mLastArg.End

            { Range = fcsRangeToLsp m
              NewText = "" }

          return
            [ { Edits = [| yield functionEdit; yield! insertArgumentEdits; yield removeArgumentEdits |]
                File = codeActionParams.TextDocument
                Title = title
                SourceDiagnostic = None
                Kind = FixKind.Refactor } ]
    }
