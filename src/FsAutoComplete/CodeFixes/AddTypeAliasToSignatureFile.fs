module FsAutoComplete.CodeFix.AddTypeAliasToSignatureFile

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FsAutoComplete.Patterns.SymbolUse

let mkLongIdRange (lid: LongIdent) = lid |> List.map (fun ident -> ident.idRange) |> List.reduce Range.unionRanges

let (|AllOpenOrHashDirective|_|) (decls: SynModuleSigDecl list) : range option =
  match decls with
  | [] -> None
  | decls ->

    let allOpenOrHashDirective =
      decls
      |> List.forall (function
        | SynModuleSigDecl.Open _
        | SynModuleSigDecl.HashDirective _ -> true
        | _ -> false)

    if not allOpenOrHashDirective then
      None
    else
      Some (List.last decls).Range.EndRange

type SynTypeDefn with

  member x.FullRange =
    match x with
    | SynTypeDefn(range = m; trivia = { LeadingKeyword = lk }) -> Range.unionRanges lk.Range m

let title = "Add type alias to signature file"

let codeFixForImplementationFileWithSignature
  (getProjectOptionsForFile: GetProjectOptionsForFile)
  (codeFix: CodeFix)
  (codeActionParams: CodeActionParams)
  : Async<Result<Fix list, string>> =
  async {
    let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
    let! project = getProjectOptionsForFile fileName

    match project with
    | Error _ -> return Ok []
    | Ok projectOptions ->

      let signatureFile = String.Concat(fileName, "i")
      let hasSig = projectOptions.SourceFiles |> List.exists(fun s -> s.FileName = signatureFile )

      if not hasSig then
        return Ok []
      else
        return! codeFix codeActionParams
  }

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

      let typeDefnInfo =
        (fcsPos, parseAndCheckResults.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun _path node ->
          match node with
          | SyntaxNode.SynTypeDefn(SynTypeDefn(
              typeInfo = SynComponentInfo(longId = [ typeIdent ])
              typeRepr = SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev _)) as tdn) when
            (Range.rangeContainsPos tdn.FullRange fcsPos)
            ->
            Some(typeIdent, tdn.FullRange)
          | _ -> None)

      match typeDefnInfo with
      | None -> return []
      | Some(typeName, mTypeDefn) ->

        match parseAndCheckResults.TryGetSymbolUseFromIdent sourceText typeName with
        | Some(IsParentInSignature parentSigLocation) ->

          let implFilePath = codeActionParams.TextDocument.GetFilePath()
          let sigFilePath = $"%s{implFilePath}i"
          let sigFileName = Utils.normalizePath sigFilePath

          let sigTextDocumentIdentifier: TextDocumentIdentifier =
            { Uri = $"%s{codeActionParams.TextDocument.Uri}i" }

          let! (sigParseAndCheckResults: ParseAndCheckResults, _sigLine: string, sigSourceText: IFSACSourceText) =
            getParseResultsForFile sigFileName (Position.mkPos 1 0)

          // Find a good location to insert the type alias
          let insertText =
            (parentSigLocation.Start, sigParseAndCheckResults.GetParseResults.ParseTree)
            ||> ParsedInput.tryPick (fun _path node ->
              match node with
              | SyntaxNode.SynModuleOrNamespaceSig(SynModuleOrNamespaceSig(longId = longId; decls = decls))
              | SyntaxNode.SynModuleSigDecl(SynModuleSigDecl.NestedModule(
                moduleInfo = SynComponentInfo(longId = longId); moduleDecls = decls)) ->
                let mSigName = mkLongIdRange longId

                // `parentSigLocation` will only contain the single identifier in case a module is prefixed with a namespace.
                if not (Range.rangeContainsRange mSigName parentSigLocation) then
                  None
                else

                  let aliasText =
                    let text = sourceText.GetSubTextFromRange mTypeDefn

                    if not (text.StartsWith("and", StringComparison.Ordinal)) then
                      text
                    else
                      String.Concat("type", text.Substring 3)

                  match decls with
                  | [] ->
                    match node with
                    | SyntaxNode.SynModuleOrNamespaceSig nm ->
                      Some(nm.Range.EndRange, String.Concat("\n\n", aliasText))

                    | SyntaxNode.SynModuleSigDecl(SynModuleSigDecl.NestedModule(
                        range = mNested
                        trivia = { ModuleKeyword = Some mModule
                                   EqualsRange = Some mEquals })) ->
                      let moduleEqualsText =
                        sigSourceText.GetSubTextFromRange(Range.unionRanges mModule mEquals)
                      // Can this grabbed from configuration?
                      let indent = "    "

                      Some(mNested, String.Concat(moduleEqualsText, "\n", indent, aliasText))
                    | _ -> None
                  | AllOpenOrHashDirective mLastDecl -> Some(mLastDecl, String.Concat("\n\n", aliasText))
                  | decls ->

                    decls
                    // Skip open statements
                    |> List.tryFind (function
                      | SynModuleSigDecl.Open _
                      | SynModuleSigDecl.HashDirective _ -> false
                      | _ -> true)
                    |> Option.map (fun mdl ->
                      let offset =
                        if mdl.Range.StartColumn = 0 then
                          String.Empty
                        else
                          String.replicate mdl.Range.StartColumn " "

                      mdl.Range.StartRange, String.Concat(aliasText, "\n\n", offset))
              | _ -> None)

          match insertText with
          | None -> return []
          | Some(mInsert, newText) ->

            return
              [ { SourceDiagnostic = None
                  Title = title
                  File = sigTextDocumentIdentifier
                  Edits =
                    [| { Range = fcsRangeToLsp mInsert
                         NewText = newText } |]
                  Kind = FixKind.Fix } ]
        | _ -> return []
    })
