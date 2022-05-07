module FsAutoComplete.CodeFix.MakeDeclarationMutable

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.UMX

let title = "Make declaration 'mutable'"

/// a codefix that makes a binding mutable when a user attempts to mutably set it
let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (getProjectOptionsForFile: GetProjectOptionsForFile)
  : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "27" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fileName =
        codeActionParams.TextDocument.GetFilePath()
        |> Utils.normalizePath

      let fcsPos = protocolPosToPos diagnostic.Range.Start
      let! (tyRes, line, lines) = getParseResultsForFile fileName fcsPos
      let! opts = getProjectOptionsForFile fileName

      match Lexer.getSymbol fcsPos.Line fcsPos.Column line SymbolLookupKind.Fuzzy opts.OtherOptions with
      | Some symbol ->
        match! tyRes.TryFindDeclaration fcsPos line with
        | FindDeclarationResult.Range declRange when declRange.FileName = (UMX.untag fileName) ->
          let lspRange = fcsRangeToLsp declRange

          if tyRes.GetParseResults.IsPositionContainedInACurriedParameter declRange.Start then
            return []
          else
            return
              [ { File = codeActionParams.TextDocument
                  SourceDiagnostic = Some diagnostic
                  Title = title
                  Edits =
                    [| { Range =
                           { Start = lspRange.Start
                             End = lspRange.Start }
                         NewText = "mutable " } |]
                  Kind = FixKind.Refactor } ]
        | _ -> return []
      | None -> return []
    })
