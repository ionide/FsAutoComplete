module FsAutoComplete.CodeFix.AddTypeAliasToSignatureFile

open System
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let mkLongIdRange (lid : LongIdent) = lid |> List.map (fun ident -> ident.idRange) |> List.reduce Range.unionRanges

let (|AllOpenOrHashDirective|_|) (decls : SynModuleSigDecl list) : range option =
    match decls with
    | [] -> None
    | decls ->

    let allOpenOrHashDirective =
        decls
        |> List.forall (
            function
            | SynModuleSigDecl.Open _
            | SynModuleSigDecl.HashDirective _ -> true
            | _ -> false
        )

    if not allOpenOrHashDirective then
        None
    else
        Some (List.last decls).Range.EndRange

// TODO: add proper title for code fix
let title = "Add type alias to signature file"

let codeFixForImplementationFileWithSignature
    (getProjectOptionsForFile : GetProjectOptionsForFile)
    (codeFix : CodeFix)
    (codeActionParams : CodeActionParams)
    : Async<Result<Fix list, string>>
    =
    async {
        let fileName = codeActionParams.TextDocument.GetFilePath () |> Utils.normalizePath
        let! project = getProjectOptionsForFile fileName

        match project with
        | Error _ -> return Ok []
        | Ok projectOptions ->

        let signatureFile = String.Concat (fileName, "i")
        let hasSig = projectOptions.SourceFiles |> Array.contains signatureFile

        if not hasSig then
            return Ok []
        else
            return! codeFix codeActionParams
    }

let fix
    (getProjectOptionsForFile : GetProjectOptionsForFile)
    (getParseResultsForFile : GetParseResultsForFile)
    : CodeFix
    =
    codeFixForImplementationFileWithSignature
        getProjectOptionsForFile
        (fun (codeActionParams : CodeActionParams) ->
            asyncResult {
                let fileName = codeActionParams.TextDocument.GetFilePath () |> Utils.normalizePath
                // The converted LSP start position to an FCS start position.
                let fcsPos = protocolPosToPos codeActionParams.Range.Start
                // The syntax tree and typed tree, current line and sourceText of the current file.
                let! (parseAndCheckResults : ParseAndCheckResults, _line : string, sourceText : IFSACSourceText) =
                    getParseResultsForFile fileName fcsPos

                let typeDefnInfo =
                    (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
                    ||> ParsedInput.tryPick (fun _path node ->
                        match node with
                        | SyntaxNode.SynTypeDefn (SynTypeDefn (
                            typeInfo = SynComponentInfo (longId = [ typeIdent ])
                            typeRepr = SynTypeDefnRepr.Simple (simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev _)
                            range = m
                            trivia = trivia)) when (Range.rangeContainsPos m fcsPos) ->
                            let mFull = Range.unionRanges trivia.LeadingKeyword.Range m
                            Some (typeIdent, mFull)
                        | _ -> None
                    )

                match typeDefnInfo with
                | None -> return []
                | Some (typeName, mTypeDefn) ->

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

                    let parentSigLocation =
                        entity.DeclaringEntity
                        |> Option.bind (fun parentEntity ->
                            match parentEntity.SignatureLocation with
                            | Some sigLocation when Utils.isSignatureFile sigLocation.FileName -> Some sigLocation
                            | _ -> None
                        )

                    match parentSigLocation with
                    | None -> return []
                    | Some parentSigLocation ->

                    // Find a good location to insert the type alias
                    let insertText =
                        (parentSigLocation.Start, sigParseAndCheckResults.GetParseResults.ParseTree)
                        ||> ParsedInput.tryPick (fun _path node ->
                            match node with
                            | SyntaxNode.SynModuleOrNamespaceSig (SynModuleOrNamespaceSig (
                                longId = longId ; decls = decls))
                            | SyntaxNode.SynModuleSigDecl (SynModuleSigDecl.NestedModule (
                                moduleInfo = SynComponentInfo (longId = longId) ; moduleDecls = decls)) ->
                                let mSigName = mkLongIdRange longId

                                // `parentSigLocation` will only contain the single identifier in case a module is prefixed with a namespace.
                                if not (Range.rangeContainsRange mSigName parentSigLocation) then
                                    None
                                else

                                let aliasText = sourceText.GetSubTextFromRange mTypeDefn

                                match decls with
                                | [] -> failwith "todo: empty module"
                                | AllOpenOrHashDirective mLastDecl ->
                                    Some (mLastDecl, String.Concat ("\n\n", sourceText.GetSubTextFromRange mTypeDefn))
                                | decls ->

                                decls
                                // Skip open statements
                                |> List.tryFind (
                                    function
                                    | SynModuleSigDecl.Open _ -> false
                                    | _ -> true
                                )
                                |> Option.map (fun mdl -> mdl.Range.StartRange, String.Concat (aliasText, "\n\n"))
                            | _ -> None
                        )

                    match insertText with
                    | None -> return []
                    | Some (mInsert, newText) ->

                    return
                        [
                            {
                                SourceDiagnostic = None
                                Title = title
                                File = sigTextDocumentIdentifier
                                Edits =
                                    [|
                                        {
                                            Range = fcsRangeToLsp mInsert
                                            NewText = newText
                                        }
                                    |]
                                Kind = FixKind.Fix
                            }
                        ]
                | _ -> return []
            }
        )
