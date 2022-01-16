module FsAutoComplete.CodeFix.ChangeComparisonToMutableAssignment

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.CodeFix.Navigation
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols

/// a codefix that changes equality checking to mutable assignment when the compiler thinks it's relevant
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "20" ])
    (fun diagnostic codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let fcsPos = protocolPosToPos diagnostic.Range.Start
        let! (tyRes, line, lines) = getParseResultsForFile fileName fcsPos

        match walkForwardUntilCondition lines diagnostic.Range.Start System.Char.IsWhiteSpace with
        | None -> return []
        | Some endPos ->
          let fcsPos = protocolPosToPos endPos
          let line = lines.GetLineString endPos.Line

          let! symbol =
            tyRes.TryGetSymbolUse fcsPos line
            |> Result.ofOption (fun _ -> "No symbol found at position")

          match symbol.Symbol with
          // only do anything if the value is mutable
          | :? FSharpMemberOrFunctionOrValue as mfv when mfv.IsMutable || mfv.HasSetterMethod ->
              // try to find the '=' at from the start of the range
              let endOfMutableValue = fcsPosToLsp symbol.Range.End

              match walkForwardUntilCondition lines endOfMutableValue (fun c -> c = '=') with
              | Some equalsPos ->
                  return
                    [ { File = codeActionParams.TextDocument
                        Title = "Use '<-' to mutate value"
                        SourceDiagnostic = Some diagnostic
                        Edits =
                          [| { Range =
                                 { Start = equalsPos
                                   End = (inc lines equalsPos) }
                               NewText = "<-" } |]
                        Kind = FixKind.Refactor } ]
              | None -> return []
          | _ -> return []
      }
      )
