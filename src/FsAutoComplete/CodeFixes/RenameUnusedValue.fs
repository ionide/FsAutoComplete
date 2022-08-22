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
let private variableHasAccessibility (ast: ParsedInput) (pos: Position) =
  SyntaxTraversal.Traverse(
    pos,
    ast,
    { new SyntaxVisitorBase<_>() with
        member _.VisitPat(_, defaultTraverse, pat) =
          match pat with
          | SynPat.Named (accessibility = Some _; range = range) when Range.rangeContainsPos range pos ->
            // `SynAccess` in FCS version currently used in FSAC doesn't contain its range
            // -> no easy way to get range with accessibility
            // -> instead of returning range with accessibility, just info if there's accessibility
            // TODO: return range with accessibility once available (https://github.com/dotnet/fsharp/pull/13304)
            Some true
          | _ -> defaultTraverse pat }
  )
  |> Option.defaultValue false

/// a codefix that suggests prepending a _ to unused values
let fix (getParseResultsForFile: GetParseResultsForFile) =
  Run.ifDiagnosticByCode (Set.ofList [ "FSAC0003" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let startPos = protocolPosToPos codeActionParams.Range.Start
      let! (tyRes, line, lines) = getParseResultsForFile fileName startPos

      let mkFix title range =
        { SourceDiagnostic = Some diagnostic
          File = codeActionParams.TextDocument
          Title = title
          Edits = [| { Range = range; NewText = "_" } |]
          Kind = FixKind.Refactor }

      let mkReplaceFix = mkFix titleReplace

      let tryMkPrefixFix range =
        match lines.GetText(protocolRangeToRange (UMX.untag fileName) range) with
        //              cannot prefix backticks -> exclude
        | Ok ident when PrettyNaming.IsIdentifierName ident ->
          mkFix titlePrefix (protocolPosToRange range.Start) |> Some
        | _ -> None

      let tryMkValueReplaceFix (range: Ionide.LanguageServerProtocol.Types.Range) =
        // // `let private foo = ...` -> `private` must be removed (`let private _ = ...` is not valid)
        if variableHasAccessibility tyRes.GetAST (protocolPosToPos range.Start) then
          None
        else
          mkReplaceFix range |> Some

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
            return [ mkReplaceFix diagnostic.Range ]
          elif mfv.IsValue then
            return
              [ tryMkValueReplaceFix diagnostic.Range; tryMkPrefixFix diagnostic.Range ]
              |> List.choose id
          else
            return []
        | _ -> return []
    })
