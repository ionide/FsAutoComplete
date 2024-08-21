module FsAutoComplete.CodeFix.AddBindingToSignatureFile

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.Patterns.SymbolUse

let title = "Add binding to signature file"

let (|IdentifierFromHeadPat|_|) (pat: SynPat) =
  match pat with
  | SynPat.LongIdent(longDotId = SynLongIdent(id = [ nameIdent ]))
  | SynPat.Named(ident = SynIdent(ident = nameIdent)) -> Some nameIdent
  | _ -> None

let (|SignatureValText|_|) (displayContext: FSharpDisplayContext) (symbolUse: FSharpSymbolUse) =
  match symbolUse.Symbol with
  | :? FSharpMemberOrFunctionOrValue as mfv -> mfv.GetValSignatureText(displayContext, symbolUse.Range)
  | _ -> None

let mkLongIdRange (lid: LongIdent) = lid |> List.map (fun ident -> ident.idRange) |> List.reduce Range.unionRanges

[<RequireQualifiedAccess>]
type InsertLocation =
  /// Could be parent node or last sibling
  | AfterNode of columnOffset: int * endRange: range
  | ReplaceEmptyNestedModule of moduleKeywordStart: int * afterEqualsTillEnd: range

let fix
  (getProjectOptionsForFile: GetProjectOptionsForFile)
  (getParseResultsForFile: GetParseResultsForFile)
  : CodeFix =
  Run.ifImplementationFileBackedBySignature getProjectOptionsForFile (fun (codeActionParams: CodeActionParams) ->
    asyncResult {
      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      // The converted LSP start position to an FCS start position.
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      // The syntax tree and typed tree, current line and sourceText of the current file.
      let! (parseAndCheckResults: ParseAndCheckResults, _line: string, sourceText: IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      // Find a top level binding ident
      let topLevelBindingIdentName =
        (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun path node ->
          match List.tryHead path, node with
          | Some(SyntaxNode.SynModule(SynModuleDecl.Let _)),
            SyntaxNode.SynBinding(SynBinding(
              headPat = IdentifierFromHeadPat nameIdent
              trivia = { LeadingKeyword = lk
                         EqualsRange = Some mEq })) ->
            let mLetTillEquals = Range.unionRanges lk.Range mEq

            if Range.rangeContainsPos mLetTillEquals fcsPos then
              Some nameIdent
            else
              None
          | _ -> None)

      match topLevelBindingIdentName with
      | None -> return []
      | Some identName ->

        // Check if the parent of its symbol exists in the signature file.
        match parseAndCheckResults.TryGetSymbolUseFromIdent sourceText identName with
        | Some(IsParentInSignature parentSigLocation as bindingSymbolUse) ->

          let implFilePath = codeActionParams.TextDocument.GetFilePath()
          let sigFilePath = $"%s{implFilePath}i"
          let sigFileName = Utils.normalizePath sigFilePath

          let sigTextDocumentIdentifier: TextDocumentIdentifier =
            { Uri = $"%s{codeActionParams.TextDocument.Uri}i" }

          let! (sigParseAndCheckResults: ParseAndCheckResults, _sigLine: string, sigSourceText: IFSACSourceText) =
            getParseResultsForFile sigFileName (Position.mkPos 1 0)

          let sigParentIdent =
            let text = sigSourceText.GetSubTextFromRange parentSigLocation
            FSharp.Compiler.Syntax.Ident(text, parentSigLocation)

          match sigParseAndCheckResults.TryGetSymbolUseFromIdent sigSourceText sigParentIdent with
          | None -> return []
          | Some parentSigSymbolUse ->
            // Get the val text (using the DisplayContext from the parent in the signature file).
            match bindingSymbolUse with
            | SignatureValText parentSigSymbolUse.DisplayContext valText ->
              // Find the end of the parent (in the signature file)
              let insertLocation: InsertLocation option =
                (parentSigLocation.Start, sigParseAndCheckResults.GetParseResults.ParseTree)
                ||> ParsedInput.tryPick (fun _path node ->
                  match node with
                  | SyntaxNode.SynModuleOrNamespaceSig(SynModuleOrNamespaceSig(
                      longId = longId; range = mParent; decls = decls))
                  | SyntaxNode.SynModuleSigDecl(SynModuleSigDecl.NestedModule(
                    moduleInfo = SynComponentInfo(longId = longId); range = mParent; moduleDecls = decls)) ->
                    let mSigName = mkLongIdRange longId

                    // `parentSigLocation` will only contain the single identifier in case a module is prefixed with a namespace.
                    if not (Range.rangeContainsRange mSigName parentSigLocation) then
                      None
                    else
                      // Use the last decl to get the indentation right in case of a nested module.
                      match List.tryLast decls with
                      | None ->
                        match node with
                        | SyntaxNode.SynModuleSigDecl(SynModuleSigDecl.NestedModule(
                            trivia = { ModuleKeyword = Some mk
                                       EqualsRange = Some mEq }
                            range = mFull)) ->
                          let mAfterEqualsTillEnd = Range.unionRanges mEq.EndRange mFull.EndRange
                          Some(InsertLocation.ReplaceEmptyNestedModule(mk.StartColumn, mAfterEqualsTillEnd))
                        | _ -> Some(InsertLocation.AfterNode(mParent.StartColumn, mParent.EndRange))
                      | Some lastDecl ->
                        Some(InsertLocation.AfterNode(lastDecl.Range.StartColumn, lastDecl.Range.EndRange))

                  | _ -> None)

              match insertLocation with
              | None -> return []
              | Some insertLocation ->

                let newText, m =
                  match insertLocation with
                  | InsertLocation.AfterNode(columnOffset, endRange) ->
                    let indent = String.replicate columnOffset " "
                    $"\n\n%s{indent}{valText}", endRange
                  | InsertLocation.ReplaceEmptyNestedModule(columnOffset, mReplace) ->
                    // TODO: can we get the indent_size from configuration??
                    let indent = String.replicate (columnOffset + 4) " "
                    $"\n%s{indent}%s{valText}", mReplace

                return
                  [ { SourceDiagnostic = None
                      Title = title
                      File = sigTextDocumentIdentifier
                      Edits =
                        [| { Range = fcsRangeToLsp m
                             NewText = newText } |]
                      Kind = FixKind.Fix } ]
            | _ -> return []
        | _ -> return []
    })
