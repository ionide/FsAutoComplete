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

// FS0001 message patterns for anonymous record field mismatches (current F# compiler formats):
//   "This anonymous record is missing field 'B'."
//   "This anonymous record is missing fields 'B', 'C'."
//   "This anonymous record does not exactly match the expected shape. Add the missing fields [B; C] and remove the extra fields [D; E]."

/// Extract missing field names from an FS0001 anonymous-record diagnostic message.
/// Returns `Some fields` when the message describes fields that should be added; `None` otherwise.
let private tryParseMissingFields (message: string) : string list option =
  // Case 1: single missing field – "This anonymous record is missing field 'X'."
  let m1 = Regex.Match(message, @"missing field '([^']+)'")

  if m1.Success then
    Some [ m1.Groups.[1].Value ]
  else
    // Case 2: multiple missing fields in quotes – "This anonymous record is missing fields 'X', 'Y'."
    // Use a more specific pattern that requires quoted field names.
    let m2 = Regex.Match(message, @"missing fields '([^']+)'")

    if m2.Success then
      // The full field list group includes all quoted names; extract each individually.
      let fullMatch = Regex.Match(message, @"missing fields (.+?)\.")

      let fieldList =
        if fullMatch.Success then
          fullMatch.Groups.[1].Value
        else
          m2.Value

      let fields =
        Regex.Matches(fieldList, "'([^']+)'")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Groups.[1].Value)
        |> Seq.toList

      if fields.IsEmpty then None else Some fields
    else
      // Case 3: "does not exactly match" – extract from "Add the missing fields [X; Y]"
      let m3 = Regex.Match(message, @"Add the missing fields \[([^\]]+)\]")

      if m3.Success then
        let fieldsStr = m3.Groups.[1].Value

        let fields =
          fieldsStr.Split(';')
          |> Array.map (fun s -> s.Trim())
          |> Array.filter (fun s -> s.Length > 0)
          |> Array.toList

        if fields.IsEmpty then None else Some fields
      else
        None

/// A code fix for FS0001 anonymous-record type mismatches: when an anonymous record literal is
/// missing fields required by its expected type, inserts stub bindings
/// `fieldName = failwith "Not Implemented"` for each missing field before the closing `|}`.
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "1" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      // Only act on anonymous-record field-mismatch errors
      do!
        Result.guard
          (fun _ ->
            diagnostic.Message.Contains("anonymous record")
            && diagnostic.Message.Contains("missing"))
          "Diagnostic is not an anonymous record missing-field error"

      let missingFields =
        match tryParseMissingFields diagnostic.Message with
        | Some fields -> fields
        | None -> []

      if missingFields.IsEmpty then
        return []
      else

        let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
        let fcsPos = protocolPosToPos diagnostic.Range.Start
        let! (parseAndCheck, _, _sourceText) = getParseResultsForFile fileName fcsPos

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

          // Exclude any fields that are already present (defensive: should already be absent).
          let fieldsToAdd =
            missingFields |> List.filter (fun f -> not (Set.contains f currentFields))

          if fieldsToAdd.IsEmpty then
            return []
          else

            // Build "fieldName = failwith "Not Implemented"" stubs for each missing field.
            let fieldStubs =
              fieldsToAdd
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
