module FsAutoComplete.CodeFix.UpdateValueInSignatureFile

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let visitSynModuleSigDecl (name: string) (decl: SynModuleSigDecl) =
  match decl with
  | SynModuleSigDecl.Val(valSig = SynValSig(ident = SynIdent(ident = ident)); range = m) when ident.idText = name ->
    Some m
  | _ -> None

let visitSynModuleOrNamespaceSig (name: string) (SynModuleOrNamespaceSig(decls = decls)) =
  decls |> List.tryPick (visitSynModuleSigDecl name)

let visitParsedSigFileInput (name: string) (ParsedSigFileInput(contents = contents)) =
  contents |> List.tryPick (visitSynModuleOrNamespaceSig name)

let visitTree (name: string) (tree: ParsedInput) =
  match tree with
  | ParsedInput.ImplFile _ -> None
  | ParsedInput.SigFile parsedSigFileInput -> visitParsedSigFileInput name parsedSigFileInput

let title = "Update val in signature file"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "34" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let implFilePath = codeActionParams.TextDocument.GetFilePath()
      let sigFilePath = $"%s{implFilePath}i"

      let implFileName = Utils.normalizePath implFilePath
      let sigFileName = Utils.normalizePath sigFilePath

      let sigTextDocumentIdentifier: TextDocumentIdentifier =
        { Uri = $"%s{codeActionParams.TextDocument.Uri}i" }

      let! (implParseAndCheckResults: ParseAndCheckResults, implLine: string, implSourceText: IFSACSourceText) =
        getParseResultsForFile implFileName (protocolPosToPos diagnostic.Range.Start)

      let! implBindingName =
        implSourceText.GetText(protocolRangeToRange implParseAndCheckResults.GetParseResults.FileName diagnostic.Range)

      let! (sigParseAndCheckResults: ParseAndCheckResults, _sigLine: string, _sigSourceText: IFSACSourceText) =
        getParseResultsForFile sigFileName (protocolPosToPos diagnostic.Range.Start)

      match visitTree implBindingName sigParseAndCheckResults.GetParseResults.ParseTree with
      | None -> return []
      | Some mVal ->
        let endPos = protocolPosToPos diagnostic.Range.End

        let symbolUse =
          implParseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(
            endPos.Line,
            endPos.Column,
            implLine,
            [ implBindingName ]
          )

        match symbolUse with
        | None -> return []
        | Some symbolUse ->
          match symbolUse.Symbol with
          | :? FSharpMemberOrFunctionOrValue as mfv ->
            match mfv.GetValSignatureText(symbolUse.DisplayContext, symbolUse.Range) with
            | None -> return []
            | Some valText ->
              return
                [ { SourceDiagnostic = None
                    Title = title
                    File = sigTextDocumentIdentifier
                    Edits =
                      [| { Range = fcsRangeToLsp mVal
                           NewText = valText } |]
                    Kind = FixKind.Fix } ]
          | _ -> return []
    })
