module FsAutoComplete.CodeFix.ExprTypeMismatch

#nowarn "57"

open FSharp.Compiler.Diagnostics.ExtendedData
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let findReturnType (cursor: pos) (tree: ParsedInput) =
  let visitor =
    { new SyntaxVisitorBase<range>() with
        member _.VisitBinding(path, defaultTraverse, synBinding) =
          match synBinding with
          | SynBinding(returnInfo = Some(SynBindingReturnInfo(typeName = t)); expr = bodyExpr) when
            Range.rangeContainsPos bodyExpr.Range cursor
            ->
            Some t.Range
          | _ -> None }

  SyntaxTraversal.Traverse(cursor, tree, visitor)

let needParenthesisWhenWrappedInSome (diagnosticRange: range) (tree: ParsedInput) =
  let visitor =
    { new SyntaxVisitorBase<bool>() with
        member _.VisitExpr(path, traverseSynExpr, defaultTraverse, synExpr) =
          if not (Range.equals synExpr.Range diagnosticRange) then
            defaultTraverse synExpr
          else
            match synExpr with
            | SynExpr.Const _
            | SynExpr.Ident _ -> Some false
            | e -> defaultTraverse e }

  SyntaxTraversal.Traverse(diagnosticRange.Start, tree, visitor)
  |> Option.defaultValue true

let title = "ExprTypeMismatch Codefix"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (set [ "1" ]) (fun diagnostic (codeActionParams: CodeActionParams) ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos diagnostic.Range.Start

      let! (parseAndCheckResults: ParseAndCheckResults, _line: string, sourceText: IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      let diagnosticWithExtendedData =
        parseAndCheckResults.GetCheckResults.Diagnostics
        |> Array.tryPick (fun d ->
          match d.ExtendedData with
          | Some(:? TypeMismatchDiagnosticExtendedData as data) -> Some(d, data)
          | _ -> None)

      match diagnosticWithExtendedData with
      | None -> return []
      | Some(diagnostic, extendedData) ->
        let updateReturnType =
          findReturnType fcsPos parseAndCheckResults.GetParseResults.ParseTree
          |> Option.map (fun mReturnType ->
            let currentType = sourceText.GetSubTextFromRange mReturnType
            let actualType = extendedData.ActualType.Format(extendedData.DisplayContext)

            { SourceDiagnostic = None
              Title = $"Update %s{currentType} to %s{actualType}"
              File = codeActionParams.TextDocument
              Edits =
                [| { Range = fcsRangeToLsp mReturnType
                     NewText = actualType } |]
              Kind = FixKind.Fix })
          |> Option.toList

        let optionFixes =
          if diagnostic.Range.StartLine <> diagnostic.Range.EndLine then
            []
          elif
            extendedData.ExpectedType.BasicQualifiedName = "Microsoft.FSharp.Core.option`1"
            || extendedData.ExpectedType.BasicQualifiedName = "Microsoft.FSharp.Core.voption`1"
          then
            let currentExpr = sourceText.GetSubTextFromRange diagnostic.Range

            let isValueOption =
              extendedData.ExpectedType.BasicQualifiedName = "Microsoft.FSharp.Core.voption`1"

            let wrapIn = if isValueOption then "ValueSome" else "Some"
            let replaceWithNone = if isValueOption then "ValueNone" else "None"

            let needsParenthesis =
              needParenthesisWhenWrappedInSome diagnostic.Range parseAndCheckResults.GetParseResults.ParseTree

            let space, openP, closeP =
              if not needsParenthesis then " ", "", "" else "", "(", ")"

            [ { SourceDiagnostic = None
                Title = $"Wrap expression in %s{wrapIn}"
                File = codeActionParams.TextDocument
                Edits =
                  [| { Range = fcsRangeToLsp diagnostic.Range
                       NewText = $"%s{wrapIn}%s{space}%s{openP}%s{currentExpr}%s{closeP}" } |]
                Kind = FixKind.Fix }
              { SourceDiagnostic = None
                Title = $"Replace expression with %s{replaceWithNone}"
                File = codeActionParams.TextDocument
                Edits =
                  [| { Range = fcsRangeToLsp diagnostic.Range
                       NewText = replaceWithNone } |]
                Kind = FixKind.Fix } ]
          else
            []

        return [ yield! updateReturnType; yield! optionFixes ]
    })
