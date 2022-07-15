/// <summary>A codefix that converts DU case matches from positional form to named form</summary>
/// <example id="sample transformation">
/// Given this type:
/// <code lang="fsharp">
/// type Person = Person of first: string * middle: string option * last: string
/// </code>
///
/// This codefix will take the following destructuring pattern:
/// <code lang="fsharp">
/// let (Person(f, m, l)) = person
/// </code>
/// and convert it to the following pattern:
/// <code lang="fsharp">
/// let (Person(first = f; middle = m; last = l)) = person
/// </code>
/// </example>
module FsAutoComplete.CodeFix.ConvertPositionalDUToNamed

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FsAutoComplete.FCSPatches
open FSharp.Compiler.Syntax
open FSharp.Compiler.Syntax.SyntaxTraversal

type ParseAndCheckResults with

  member x.TryGetPositionalUnionPattern(pos: FcsPos) =
    let rec (|UnionNameAndPatterns|_|) =
      function
      | SynPat.LongIdent (longDotId = ident
                          argPats = SynArgPats.Pats [ SynPat.Paren (pat = SynPat.Tuple (elementPats = duFieldPatterns)
                                                                    range = parenRange) ]) ->
        Some(ident, duFieldPatterns, parenRange)
      | SynPat.LongIdent (longDotId = ident
                          argPats = SynArgPats.Pats [ SynPat.Paren (pat = singleDUFieldPattern; range = parenRange) ]) ->
        Some(ident, [ singleDUFieldPattern ], parenRange)
      | SynPat.Paren(pat = UnionNameAndPatterns (ident, duFieldPatterns, parenRange)) ->
        Some(ident, duFieldPatterns, parenRange)
      | _ -> None

    let visitor =
      { new SyntaxVisitorBase<_>() with
          member x.VisitBinding(path, defaultTraverse, binding) =
            match binding with
            // DU case with multiple
            | SynBinding(headPat = UnionNameAndPatterns (ident, duFieldPatterns, parenRange)) ->
              Some(ident, duFieldPatterns, parenRange)
            | _ -> defaultTraverse binding

          // I shouldn't have to override my own VisitExpr, but the default traversal doesn't seem to be triggering the `VisitMatchClause` method I've defined below.
          // TODO: reevaluate after https://github.com/dotnet/fsharp/pull/12837 merges
          member x.VisitExpr(path, traverse, defaultTraverse, expr) =
            match expr with
            | SynExpr.Match (expr = argExpr; clauses = clauses) ->
              let path = SyntaxNode.SynExpr argExpr :: path

              match x.VisitExpr(path, traverse, defaultTraverse, argExpr) with
              | Some x -> Some x
              | None ->
                clauses
                |> List.tryPick (function
                  | SynMatchClause(pat = UnionNameAndPatterns (ident, duFieldPatterns, parenRange)) ->
                    Some(ident, duFieldPatterns, parenRange)
                  | _ -> None)
            | _ -> defaultTraverse expr

          member x.VisitMatchClause(path, defaultTraverse, matchClause) =
            match matchClause with
            | SynMatchClause(pat = UnionNameAndPatterns (ident, duFieldPatterns, parenRange)) ->
              Some(ident, duFieldPatterns, parenRange)
            | _ -> defaultTraverse matchClause }

    Traverse(pos, x.GetParseResults.ParseTree, visitor)

let private (|MatchedFields|UnmatchedFields|NotEnoughFields|) (astFields: SynPat list, unionFields: string list) =
  let userFieldsCount = astFields.Length
  let typeFieldsCount = unionFields.Length

  match compare userFieldsCount typeFieldsCount with
  | -1 -> UnmatchedFields(List.zip astFields unionFields[0 .. userFieldsCount - 1], unionFields.[userFieldsCount..])
  | 0 -> MatchedFields(List.zip astFields unionFields)
  | 1 -> NotEnoughFields
  | _ -> failwith "impossible"

let private createEdit (astField: SynPat, duField: string) : TextEdit list =
  let prefix = $"{duField} = "
  let startRange = astField.Range.Start |> fcsPosToProtocolRange
  let suffix = ";"
  let endRange = astField.Range.End |> fcsPosToProtocolRange

  [ { NewText = prefix; Range = startRange }
    { NewText = suffix; Range = endRange } ]

let private createWildCard endRange (duField: string) : TextEdit =
  let wildcard = $" {duField} = _;"
  let range = endRange
  { NewText = wildcard; Range = range }

let private toPosSeq (range: FSharp.Compiler.Text.Range, text: NamedText) =
  range.Start
  |> Seq.unfold (fun currentPos ->
    match text.NextPos currentPos with
    | None -> None
    | Some nextPos ->
      if FSharp.Compiler.Text.Range.rangeContainsPos range nextPos then
        Some(currentPos, nextPos)
      else
        None)

let title = "Convert to named patterns"

let fix (getParseResultsForFile: GetParseResultsForFile) (getRangeText: GetRangeText) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! (parseAndCheck, lineStr, sourceText) = getParseResultsForFile filePath fcsPos

      let! (duIdent, duFields, parenRange) =
        parseAndCheck.TryGetPositionalUnionPattern(fcsPos)
        |> Result.ofOption (fun _ -> "Not inside a DU pattern")

      let! symbolUse =
        parseAndCheck.TryGetSymbolUse duIdent.Range.Start lineStr
        |> Result.ofOption (fun _ -> "No matching symbol for position")

      let! unionCase =
        match symbolUse.Symbol with
        | :? FSharpUnionCase as uc -> Ok uc
        | _ -> Error "Not a union case"

      let allFieldNames = unionCase.Fields |> List.ofSeq |> List.map (fun f -> f.Name)

      let notInsidePatterns =
        let ranges = duFields |> List.map (fun f -> f.Range)

        fun (pos: FSharp.Compiler.Text.Position) ->
          ranges
          |> List.forall (fun r -> not (FSharp.Compiler.Text.Range.rangeContainsPos r pos))


      let commasBetweenFields =
        toPosSeq (parenRange, sourceText)
        |> Seq.filter notInsidePatterns
        |> Seq.filter (fun pos -> sourceText.GetCharUnsafe pos = ',')

      let removeCommaEdits =
        commasBetweenFields
        |> Seq.map (fun pos ->
          let startPos =
            fcsPosToLsp (FSharp.Compiler.Text.Position.mkPos pos.Line (pos.Column - 1))

          let endPos = fcsPosToLsp pos

          { NewText = ""
            Range = { Start = startPos; End = endPos } })
        |> Seq.toArray

      let! patternEdits =
        match (duFields, allFieldNames) with
        | MatchedFields pairs -> pairs |> List.collect createEdit |> List.toArray |> Ok

        | UnmatchedFields (pairs, leftover) ->
          result {
            let! endPos =
              dec sourceText (fcsPosToLsp parenRange.End)
              |> Option.map protocolPosToRange
              |> Result.ofOption (fun _ -> "No end position for range")

            let matchedEdits = pairs |> List.collect createEdit
            let leftoverEdits = leftover |> List.map (createWildCard endPos)

            return List.append matchedEdits leftoverEdits |> List.toArray
          }
        | NotEnoughFields -> Ok [||]

      match patternEdits with
      | [||] -> return []
      | patternEdits ->
        let allEdits =
          Array.append patternEdits removeCommaEdits |> Array.sortBy (fun e -> e.Range)

        return
          [ { Edits = allEdits
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor } ]
    }
