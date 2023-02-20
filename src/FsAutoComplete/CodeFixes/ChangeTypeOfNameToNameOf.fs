/// a codefix that replaces typeof<'t>.Name with nameof('t)
module FsAutoComplete.CodeFix.ChangeTypeOfNameToNameOf

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

type FSharpParseFileResults with

  member this.TryRangeOfTypeofWithNameAndTypeExpr pos =
    SyntaxTraversal.Traverse(
      pos,
      this.ParseTree,
      { new SyntaxVisitorBase<_>() with
          member _.VisitExpr(_path, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.DotGet(expr, _, _, range) ->
              match expr with
              | SynExpr.TypeApp(SynExpr.Ident(ident), _, typeArgs, _, _, _, _) ->
                let onlyOneTypeArg =
                  match typeArgs with
                  | [] -> false
                  | [ _ ] -> true
                  | _ -> false

                if ident.idText = "typeof" && onlyOneTypeArg then
                  Some
                    {| NamedIdentRange = typeArgs.Head.Range
                       FullExpressionRange = range |}
                else
                  defaultTraverse expr
              | _ -> defaultTraverse expr
            | _ -> defaultTraverse expr }
    )

let title = "Use 'nameof'"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let pos = protocolPosToPos codeActionParams.Range.Start

      let! (tyRes, line, sourceText) = getParseResultsForFile fileName pos

      let! results =
        tyRes.GetParseResults.TryRangeOfTypeofWithNameAndTypeExpr(pos)
        |> Result.ofOption (fun _ -> "no typeof expr found")

      let! typeName = sourceText.GetText results.NamedIdentRange
      let replacement = $"nameof({typeName})"

      return
        [ { Edits =
              [| { Range = fcsRangeToLsp results.FullExpressionRange
                   NewText = replacement } |]
            File = codeActionParams.TextDocument
            Title = title
            SourceDiagnostic = None
            Kind = FixKind.Refactor } ]
    }
