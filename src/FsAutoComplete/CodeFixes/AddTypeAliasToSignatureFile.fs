module FsAutoComplete.CodeFix.AddTypeAliasToSignatureFile

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

// TODO: add proper title for code fix
let title = "AddTypeAliasToSignatureFile Codefix"

let fix (getParseResultsForFile : GetParseResultsForFile) : CodeFix =
    fun (codeActionParams : CodeActionParams) ->
        asyncResult {
            // TODO: check if the file even has a signature file

            let fileName = codeActionParams.TextDocument.GetFilePath () |> Utils.normalizePath
            // The converted LSP start position to an FCS start position.
            let fcsPos = protocolPosToPos codeActionParams.Range.Start
            // The syntax tree and typed tree, current line and sourceText of the current file.
            let! (parseAndCheckResults : ParseAndCheckResults, _line : string, sourceText : IFSACSourceText) =
                getParseResultsForFile fileName fcsPos

            let typeDefnInfo =
                (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
                ||> ParsedInput.tryPick (fun path node ->
                    match node with
                    | SyntaxNode.SynTypeDefn (SynTypeDefn (
                        typeInfo = SynComponentInfo (longId = [ typeIdent ])
                        typeRepr = SynTypeDefnRepr.Simple (simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev _)
                        range = m)) when (Range.rangeContainsPos m fcsPos) ->
                        Some (typeIdent, m, path)
                    | _ -> None
                )

            match typeDefnInfo with
            | None -> return []
            | Some (typeName, mTypeDefn, implPath) ->

            match parseAndCheckResults.TryGetSymbolUseFromIdent sourceText typeName with
            | None -> return []
            | Some typeSymbolUse ->

            match typeSymbolUse.Symbol with
            | :? FSharpEntity as entity ->
                let isPartOfSignature =
                    match entity.SignatureLocation with
                    | None -> false
                    | Some sigLocation -> Utils.isSignatureFile sigLocation.FileName

                if isPartOfSignature then
                    return []
                else

                let implFilePath = codeActionParams.TextDocument.GetFilePath ()
                let sigFilePath = $"%s{implFilePath}i"
                let sigFileName = Utils.normalizePath sigFilePath

                let sigTextDocumentIdentifier : TextDocumentIdentifier =
                    {
                        Uri = $"%s{codeActionParams.TextDocument.Uri}i"
                    }

                let! (sigParseAndCheckResults : ParseAndCheckResults,
                      _sigLine : string,
                      _sigSourceText : IFSACSourceText) = getParseResultsForFile sigFileName (Position.mkPos 1 0)

                // Find a good location to insert the type alias
                ignore (sigTextDocumentIdentifier, sigParseAndCheckResults, mTypeDefn, implPath)

                return
                    [
                        {
                            SourceDiagnostic = None
                            Title = title
                            File = codeActionParams.TextDocument
                            // Based on conditional logic, you typically want to suggest a text edit to the user.
                            Edits =
                                [|
                                // {
                                //     // When dealing with FCS, we typically want to use the FCS flavour of range.
                                //     // However, to interact correctly with the LSP protocol, we need to return an LSP range.
                                //     Range = fcsRangeToLsp mBindingName
                                //     NewText = "Text replaced by AddTypeAliasToSignatureFile"
                                // }
                                |]
                            Kind = FixKind.Fix
                        }
                    ]
            | _ -> return []
        }
