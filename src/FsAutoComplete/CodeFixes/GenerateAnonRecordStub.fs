module FsAutoComplete.CodeFix.GenerateAnonRecordStub

open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Add missing anonymous record fields"

/// Parse field names out of a bracket list section of the FS3578 message,
/// e.g. the content `"A"; "B"` from inside `["A"; "B"]`.
let private parseFieldNames (bracketContent: string) =
  Regex.Matches(bracketContent, "\"([^\"]+)\"")
  |> Seq.cast<Match>
  |> Seq.map (fun m -> m.Groups.[1].Value)
  |> Set.ofSeq

// FS3578 diagnostic message format:
//   Two anonymous record types have mismatched sets of field names '["A"; "B"]' and '["A"]'
let private msgPattern =
  Regex(@"'\[([^\]]*)\]' and '\[([^\]]*)\]'", RegexOptions.Compiled)

/// A code fix for FS3578: when an anonymous record literal is missing fields required by its
/// expected type, inserts stub bindings `fieldName = failwith "Not Implemented"` for each
/// missing field before the closing `|}`.
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3578" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos diagnostic.Range.Start
      let! (parseAndCheck, _, _sourceText) = getParseResultsForFile fileName fcsPos

      let m = msgPattern.Match(diagnostic.Message)

      if not m.Success then
        return []
      else

        let set1 = parseFieldNames m.Groups.[1].Value
        let set2 = parseFieldNames m.Groups.[2].Value

        // Find the innermost anonymous record expression that contains the diagnostic start position.
        let anonRecdOpt =
          (fcsPos, parseAndCheck.GetParseResults.ParseTree)
          ||> ParsedInput.tryPick (fun _path node ->
            match node with
            | SyntaxNode.SynExpr(SynExpr.AnonRecd(recordFields = fields; range = r)) when
              Range.rangeContainsPos r fcsPos
              ->
              let currentNames =
                fields
                |> List.map (fun (synLongIdent, _, _) -> (synLongIdent.LongIdent |> List.last).idText)
                |> Set.ofList

              Some(r, currentNames)
            | _ -> None)

        match anonRecdOpt with
        | None -> return []
        | Some(r, currentFields) ->

          // Determine which fields are missing: present in one of the two error sets but absent
          // from the current anonymous record literal.
          let missingFromSet1 = set1 - currentFields
          let missingFromSet2 = set2 - currentFields

          let missingFields =
            if missingFromSet1.IsEmpty then missingFromSet2
            elif missingFromSet2.IsEmpty then missingFromSet1
            else Set.union missingFromSet1 missingFromSet2

          if missingFields.IsEmpty then
            return []
          else

            // Build "; fieldName = failwith "Not Implemented"" stubs for each missing field.
            let fieldStubs =
              missingFields
              |> Set.toList // Set.toList is sorted, giving a deterministic field order
              |> List.map (fun f -> $"{f} = failwith \"Not Implemented\"")
              |> String.concat "; "

            // Prefix with "; " if there are already fields in the expression; with a space if the
            // record body is empty.
            let insertText =
              if currentFields.IsEmpty then
                $" {fieldStubs} "
              else
                $"; {fieldStubs} "

            // The anonymous record range ends just after '}' in '|}', so '|' is at EndColumn − 2.
            let insertPos = Position.mkPos r.EndLine (r.EndColumn - 2)
            let insertLspPos = fcsPosToLsp insertPos

            let insertRange =
              { Start = insertLspPos
                End = insertLspPos }

            return
              [ { Title = title
                  File = codeActionParams.TextDocument
                  SourceDiagnostic = Some diagnostic
                  Edits =
                    [| { Range = insertRange
                         NewText = insertText } |]
                  Kind = FixKind.Fix } ]
    })
