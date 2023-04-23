module FsAutoComplete.CodeFix.RenameUnusedValue

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols
open FSharp.UMX
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let titleReplace = "Replace with _"
let titlePrefix = "Prefix with _"

/// `let private foo = ...`: `foo` cannot be `_`: `private` not allowed -> must be replaced too
///
/// But current FCS version doesn't include range for `SynAccess`
/// -> no (easy) way to get range with accessibility
/// -> instead of range to replace, just if there's accessibility
let private accessibilityRange (ast: ParsedInput) (pos: Position) =
  SyntaxTraversal.Traverse(
    pos,
    ast,
    { new SyntaxVisitorBase<_>() with
        member _.VisitPat(_, defaultTraverse, pat) =
          match pat with
          | SynPat.Named(accessibility = Some (SynAccess.Private (range = accessRange)); range = range) when Range.rangeContainsPos range pos ->
            Some (accessRange.WithEnd(accessRange.End.WithColumn(accessRange.End.Column + 1))) // add an additional column to remove the 'space' between private and identifier
          | _ -> defaultTraverse pat }
  )

/// a codefix that suggests prepending a _ to unused values
let fix (getParseResultsForFile: GetParseResultsForFile) =
  Run.ifDiagnosticByCode (Set.ofList [ "FSAC0003" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let startPos = protocolPosToPos codeActionParams.Range.Start
      let! (tyRes, line, lines) = getParseResultsForFile fileName startPos

      let underscore range: Ionide.LanguageServerProtocol.Types.TextEdit = { Range = range; NewText = "_" }
      let mkFix title range =
        { SourceDiagnostic = Some diagnostic
          File = codeActionParams.TextDocument
          Title = title
          Edits = [| underscore range |]
          Kind = FixKind.Refactor }

      let mkReplaceFix identRange accessRange =
        match accessRange with
        | None -> mkFix titleReplace identRange
        | Some accessRange ->
          { mkFix titleReplace identRange with
             Edits = [| underscore identRange; { Range = accessRange; NewText = "" } |]}

      let tryMkPrefixFix range =
        match lines.GetText(protocolRangeToRange (UMX.untag fileName) range) with
        //              cannot prefix backticks -> exclude
        | Ok ident when PrettyNaming.IsIdentifierName ident ->
          mkFix titlePrefix (protocolPosToRange range.Start) |> Some
        | _ -> None

      let tryMkValueReplaceFix (range: Ionide.LanguageServerProtocol.Types.Range) =
        mkReplaceFix range (accessibilityRange tyRes.GetAST startPos |> Option.map fcsRangeToLsp) |> Some

      // CodeFixes:
      // * Replace with _
      //  * variable
      //  * this
      // * Prefix with _
      //  * variable
      // * Otherwise: neither
      match tyRes.TryGetSymbolUse startPos line with
      | None -> return []
      | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
          if mfv.IsMemberThisValue then
            return [ mkReplaceFix diagnostic.Range None ]
          elif mfv.IsValue then
            let symbolText =
              lines.GetText symbolUse.Range
              |> function
                | Ok r -> r
                | Error _ -> ""

            if symbolText <> "_" then
              return
                [ tryMkValueReplaceFix diagnostic.Range; tryMkPrefixFix diagnostic.Range ]
                |> List.choose id
            else
              return []
          else
            return []
        | _ -> return []
    })
