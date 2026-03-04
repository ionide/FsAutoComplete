module FsAutoComplete.CodeFix.IntroduceMissingBinding

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title (name: string) = $"Introduce local binding '{name}'"

/// A code fix that introduces a local 'let' binding for an undefined value or constructor.
/// Triggered by FS0039 "The value or constructor 'x' is not defined."
let fix (getParseResultsForFile: GetParseResultsForFile) (getLineText: GetLineText) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "39" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      // Only act on "value or constructor not defined" errors, not type/module errors
      do!
        Result.guard
          (fun _ -> diagnostic.Message.Contains("The value or constructor '"))
          "Diagnostic is not a missing value or constructor"

      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos diagnostic.Range.Start
      let! (_parseAndCheck, _line, sourceText) = getParseResultsForFile fileName fcsPos

      // Get the undefined identifier name from the diagnostic range in the source text
      let! missingName = getLineText sourceText diagnostic.Range

      // Only offer for simple (unqualified) identifiers — dotted names cannot be introduced as local bindings
      do!
        Result.guard
          (fun _ -> missingName.Length > 0 && not (missingName.Contains('.')))
          "Qualified or empty names cannot be introduced as local bindings"

      // Get the text from column 0 to the diagnostic start to measure indentation
      let lineStart =
        { diagnostic.Range.Start with
            Character = 0u }

      let! linePrefixText =
        getLineText
          sourceText
          { Start = lineStart
            End = diagnostic.Range.Start }

      let indent =
        linePrefixText
        |> Seq.takeWhile System.Char.IsWhiteSpace
        |> Seq.toArray
        |> System.String

      // Insert the new binding on a new line immediately before the current line
      let insertionRange = { Start = lineStart; End = lineStart }
      let newBinding = $"{indent}let {missingName} = failwith \"Not Implemented\"\n"

      return
        [ { Title = title missingName
            File = codeActionParams.TextDocument
            SourceDiagnostic = Some diagnostic
            Kind = FixKind.Fix
            Edits =
              [| { Range = insertionRange
                   NewText = newBinding } |] } ]
    })
