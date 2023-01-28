/// a codefix that rewrites C#-style '=>' lambdas to F#-style 'fun _ -> _' lambdas
module FsAutoComplete.CodeFix.ConvertCSharpLambdaToFSharpLambda

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let title = "Replace C#-style lambda with F# lambda"
// adopted from `FSharp.Compiler.CodeAnalysis.FSharpParseFileResults.TryRangeOfParenEnclosingOpEqualsGreaterUsage`
let private tryRangeOfParenEnclosingOpEqualsGreaterUsage input pos =
  let (|Ident|_|) ofName =
    function
    | SynExpr.Ident ident when ident.idText = ofName -> Some()
    | _ -> None

  let (|InfixAppOfOpEqualsGreater|_|) =
    function
    | SynExpr.App(ExprAtomicFlag.NonAtomic,
                  false,
                  SynExpr.App(ExprAtomicFlag.NonAtomic, true, Ident "op_EqualsGreater", actualParamListExpr, range),
                  actualLambdaBodyExpr,
                  _) ->
      let opEnd = range.End
      let opStart = Position.mkPos (range.End.Line) (range.End.Column - 2)
      let opRange = Range.mkRange range.FileName opStart opEnd

      let argsRange = actualParamListExpr.Range

      Some(argsRange, opRange)
    | _ -> None

  SyntaxTraversal.Traverse(
    pos,
    input,
    { new SyntaxVisitorBase<_>() with
        member _.VisitExpr(_, _, defaultTraverse, expr) =
          match expr with
          | SynExpr.Paren(InfixAppOfOpEqualsGreater(argsRange, opRange), _, _, _) -> Some(argsRange, opRange)
          | _ -> defaultTraverse expr

        member _.VisitBinding(_path, defaultTraverse, binding) =
          match binding with
          | SynBinding(kind = SynBindingKind.Normal; expr = InfixAppOfOpEqualsGreater(argsRange, opRange)) ->
            Some(argsRange, opRange)
          | _ -> defaultTraverse binding }
  )

let fix (getParseResultsForFile: GetParseResultsForFile) (getLineText: GetLineText) : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList
      [ "39" // undefined value
        "43" ]) // operator not defined
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let fcsPos = protocolPosToPos diagnostic.Range.Start
        let! (tyRes, _, lines) = getParseResultsForFile fileName fcsPos

        match tryRangeOfParenEnclosingOpEqualsGreaterUsage tyRes.GetAST fcsPos with
        | Some(argsRange, opRange) ->
          return
            [ { Title = title
                File = codeActionParams.TextDocument
                SourceDiagnostic = Some diagnostic
                Edits =
                  [|
                     // add `fun ` in front of args
                     { Range =
                         { Start = fcsPosToLsp argsRange.Start
                           End = fcsPosToLsp argsRange.Start }
                       NewText = "fun " }
                     // replace `=>` with `->`
                     { Range = fcsRangeToLsp opRange
                       NewText = "->" } |]
                Kind = FixKind.Refactor } ]
        | None -> return []
      })
