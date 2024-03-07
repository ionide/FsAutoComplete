module FsAutoComplete.CodeFix.UpdateTypeAbbreviationInSignatureFile

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

// TODO: add proper title for code fix
let title = "UpdateTypeAbbreviationInSignatureFile Codefix"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "318" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let implFilePath = codeActionParams.TextDocument.GetFilePath()
      let implFileName = Utils.normalizePath implFilePath

      let! (implParseAndCheckResults: ParseAndCheckResults, _implLine: string, implSourceText: IFSACSourceText) =
        getParseResultsForFile implFileName (protocolPosToPos diagnostic.Range.Start)

      let mDiag =
        protocolRangeToRange implParseAndCheckResults.GetParseResults.FileName diagnostic.Range

      let implTypeName =
        (mDiag.Start, implParseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun _ node ->
          match node with
          | SyntaxNode.SynTypeDefn(SynTypeDefn(
              typeInfo = SynComponentInfo(longId = [ typeIdent ])
              typeRepr = SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev _; range = mBody))) when
            Range.equals typeIdent.idRange mDiag
            ->
            Some(typeIdent, mBody)
          | _ -> None)

      match implTypeName with
      | None -> return []
      | Some(typeName, mImplBody) ->
        match implParseAndCheckResults.TryGetSymbolUseFromIdent implSourceText typeName with
        | None -> return []
        | Some typeSymbolUse ->
          match typeSymbolUse.Symbol with
          | :? FSharpEntity as entity ->
            match entity.SignatureLocation with
            | None -> return []
            | Some signatureLocation ->
              if not (Utils.isSignatureFile signatureLocation.FileName) then
                // A little weird
                return []
              else
                let sigFilePath = $"%s{implFilePath}i"
                let sigFileName = Utils.normalizePath sigFilePath

                let sigTextDocumentIdentifier: TextDocumentIdentifier =
                  { Uri = $"%s{codeActionParams.TextDocument.Uri}i" }

                let! (sigParseAndCheckResults: ParseAndCheckResults, _sigLine: string, _sigSourceText: IFSACSourceText) =
                  getParseResultsForFile sigFileName (Position.mkPos 1 0)

                let mSigTypeAbbrev =
                  (signatureLocation.Start, sigParseAndCheckResults.GetParseResults.ParseTree)
                  ||> ParsedInput.tryPick (fun _path node ->
                    match node with
                    | SyntaxNode.SynTypeDefnSig(SynTypeDefnSig(
                        typeInfo = SynComponentInfo(longId = [ typeIdent ])
                        typeRepr = SynTypeDefnSigRepr.Simple(repr = SynTypeDefnSimpleRepr.TypeAbbrev _; range = m))) when
                      Range.equals typeIdent.idRange signatureLocation
                      ->
                      Some m
                    | _ -> None)

                match mSigTypeAbbrev with
                | None -> return []
                | Some mSigTypeAbbrev ->
                  let newText = implSourceText.GetSubTextFromRange mImplBody

                  return
                    [ { SourceDiagnostic = None
                        Title = title
                        File = sigTextDocumentIdentifier
                        Edits =
                          [| { Range = fcsRangeToLsp mSigTypeAbbrev
                               NewText = newText } |]
                        Kind = FixKind.Fix } ]
          | _ -> return []
    })
