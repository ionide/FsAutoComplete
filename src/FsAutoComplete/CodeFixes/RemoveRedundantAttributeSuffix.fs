module FsAutoComplete.CodeFix.RemoveRedundantAttributeSuffix

open System
open FSharp.Compiler.Syntax
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Remove redundant attribute suffix"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! parseAndCheck, _, _ = getParseResultsForFile filePath fcsPos

      let isAttributeWithRedundantSuffix =
        SyntaxTraversal.Traverse(
          fcsPos,
          parseAndCheck.GetParseResults.ParseTree,
          { new SyntaxVisitorBase<_>() with
              member _.VisitAttributeApplication(path, attributes) =
                let attributesWithRedundantSuffix =
                  attributes.Attributes
                  |> List.choose (fun a ->
                    match List.tryLast a.TypeName.LongIdent with
                    | Some ident when ident.idText.EndsWith("Attribute", StringComparison.Ordinal) -> Some ident
                    | _ -> None)

                if List.isEmpty attributesWithRedundantSuffix then
                  None
                else
                  Some attributesWithRedundantSuffix }
        )

      match isAttributeWithRedundantSuffix with
      | None -> return []
      | Some redundantSuffixIdents ->
        return
          redundantSuffixIdents
          |> List.map (fun ident ->
            let updateText = ident.idText.Replace("Attribute", "")

            { Edits =
                [| { Range = fcsRangeToLsp ident.idRange
                     NewText = updateText } |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor })
    }
