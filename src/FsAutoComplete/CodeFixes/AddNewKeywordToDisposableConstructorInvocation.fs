module FsAutoComplete.CodeFix.AddNewKeywordToDisposableConstructorInvocation

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let title = "Add 'new'"

/// a codefix that suggests using the 'new' keyword on IDisposables
let fix (getParseResultsForFile: GetParseResultsForFile) =
  Run.ifDiagnosticByCode (Set.ofList [ "760" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> normalizePath
      let fcsRange = protocolRangeToRange (string fileName) diagnostic.Range
      let fcsPos = protocolPosToPos diagnostic.Range.Start
      let! parseResults, _, sourceText = getParseResultsForFile fileName fcsPos

      // Constructor arg
      // Qualified.Constructor arg
      // Constructor<TypeArg> arg
      // Qualified.Constructor<TypeArg> arg
      let matchingApp path node =
        let (|TargetTy|_|) expr =
          match expr with
          | SynExpr.Ident id -> Some(SynType.LongIdent(SynLongIdent([ id ], [], [])))
          | SynExpr.LongIdent(longDotId = longDotId) -> Some(SynType.LongIdent longDotId)
          | SynExpr.TypeApp(SynExpr.Ident id, lessRange, typeArgs, commaRanges, greaterRange, _, range) ->
            Some(
              SynType.App(
                SynType.LongIdent(SynLongIdent([ id ], [], [])),
                Some lessRange,
                typeArgs,
                commaRanges,
                greaterRange,
                false,
                range
              )
            )
          | SynExpr.TypeApp(SynExpr.LongIdent(longDotId = longDotId),
                            lessRange,
                            typeArgs,
                            commaRanges,
                            greaterRange,
                            _,
                            range) ->
            Some(
              SynType.App(
                SynType.LongIdent longDotId,
                Some lessRange,
                typeArgs,
                commaRanges,
                greaterRange,
                false,
                range
              )
            )
          | _ -> None

        match node with
        | SyntaxNode.SynExpr(SynExpr.App(funcExpr = TargetTy targetTy; argExpr = argExpr; range = m)) when
          m |> Range.equals fcsRange
          ->
          Some(targetTy, argExpr, path)
        | _ -> None

      return
        (fcsRange.Start, parseResults.GetAST)
        ||> ParsedInput.tryPick matchingApp
        |> Option.toList
        |> List.map (fun (targetTy, argExpr, path) ->
          // Adding `new` may require additional parentheses: https://github.com/dotnet/fsharp/issues/15622
          let needsParens =
            let newExpr = SynExpr.New(false, targetTy, argExpr, fcsRange)

            argExpr
            |> SynExpr.shouldBeParenthesizedInContext sourceText.GetLineString (SyntaxNode.SynExpr newExpr :: path)

          let newText =
            let targetTyText = sourceText.GetSubTextFromRange targetTy.Range

            // Constructor namedArg  → new Constructor(namedArg)
            // Constructor "literal" → new Constructor "literal"
            // Constructor ()        → new Constructor ()
            // Constructor()         → new Constructor()
            // Constructor           → new Constructor
            // ····indentedArg         ····(indentedArg)
            let textBetween =
              let range = Range.mkRange (string fileName) targetTy.Range.End argExpr.Range.Start

              if needsParens && range.StartLine = range.EndLine then
                ""
              else
                sourceText.GetSubTextFromRange range

            let argExprText =
              let originalArgText = sourceText.GetSubTextFromRange argExpr.Range

              if needsParens then
                $"(%s{originalArgText})"
              else
                originalArgText

            $"new %s{targetTyText}%s{textBetween}%s{argExprText}"

          { SourceDiagnostic = Some diagnostic
            File = codeActionParams.TextDocument
            Title = title
            Edits =
              [| { Range =
                     { Start = diagnostic.Range.Start
                       End = diagnostic.Range.End }
                   NewText = newText } |]
            Kind = FixKind.Refactor })
    })
