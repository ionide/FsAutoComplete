module FsAutoComplete.CodeFix.UpdateValueInSignatureFile

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Update val in signature file"

let assertPaths (sigPath: SyntaxVisitorPath) (implPath: SyntaxVisitorPath) =
  let extractPath (path: SyntaxVisitorPath) =
    path
    |> List.collect (function
      | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(longId = lid))
      | SyntaxNode.SynModule(SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(longId = lid)))
      | SyntaxNode.SynModuleOrNamespaceSig(SynModuleOrNamespaceSig(longId = lid))
      | SyntaxNode.SynModuleSigDecl(SynModuleSigDecl.NestedModule(moduleInfo = SynComponentInfo(longId = lid))) -> lid
      | _ -> [])
    |> List.map (fun ident -> ident.idText)
    |> String.concat "."

  let impl = extractPath implPath
  let sign = extractPath sigPath
  impl = sign


let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "34" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let implFilePath = codeActionParams.TextDocument.GetFilePath()
      let sigFilePath = $"%s{implFilePath}i"

      let implFileName = Utils.normalizePath implFilePath
      let sigFileName = Utils.normalizePath sigFilePath

      let sigTextDocumentIdentifier: TextDocumentIdentifier =
        { Uri = $"%s{codeActionParams.TextDocument.Uri}i" }

      let! (implParseAndCheckResults: ParseAndCheckResults, implLine: string, _implSourceText: IFSACSourceText) =
        getParseResultsForFile implFileName (protocolPosToPos diagnostic.Range.Start)

      let mDiag =
        protocolRangeToRange implParseAndCheckResults.GetParseResults.FileName diagnostic.Range

      // Find the binding name in the implementation file.
      let impVisitor =
        { new SyntaxVisitorBase<_>() with
            override x.VisitBinding(path, defaultTraverse, SynBinding(headPat = pat)) =
              match pat with
              | SynPat.LongIdent(longDotId = SynLongIdent(id = [ ident ])) when Range.equals mDiag ident.idRange ->
                Some(ident, path)
              | _ -> None }

      match SyntaxTraversal.Traverse(mDiag.Start, implParseAndCheckResults.GetParseResults.ParseTree, impVisitor) with
      | None -> return []
      | Some(implBindingIdent, implPath) ->
        let endPos = implBindingIdent.idRange.End

        let symbolUse =
          implParseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(
            endPos.Line,
            endPos.Column,
            implLine,
            [ implBindingIdent.idText ]
          )

        match symbolUse with
        | None -> return []
        | Some implSymbolUse ->

          match implSymbolUse.Symbol with
          | :? FSharpMemberOrFunctionOrValue as mfv when mfv.SignatureLocation.IsSome ->
            let mSig = mfv.SignatureLocation.Value

            // Find a matching val in the signature file.
            let sigVisitor =
              { new SyntaxVisitorBase<_>() with
                  override x.VisitModuleSigDecl
                    (
                      path: SyntaxVisitorPath,
                      defaultTraverse,
                      synModuleSigDecl: SynModuleSigDecl
                    ) =
                    defaultTraverse synModuleSigDecl

                  override x.VisitValSig
                    (
                      path,
                      defaultTraverse,
                      SynValSig(ident = SynIdent(ident, _); range = mValSig)
                    ) =
                    if ident.idText = implBindingIdent.idText then
                      Some(mValSig, path)
                    else
                      None }

            let! (sigParseAndCheckResults: ParseAndCheckResults, sigLine: string, _sigSourceText: IFSACSourceText) =
              getParseResultsForFile sigFileName mSig.End

            match
              SyntaxTraversal.Traverse(mSig.End, sigParseAndCheckResults.GetParseResults.ParseTree, sigVisitor)
            with
            | None -> return []
            | Some(mValSig, sigPath) ->

              // Verify both nodes share the same path.
              if not (assertPaths sigPath implPath) then
                return []
              else
                // Find matching symbol in signature file, we need it for its DisplayContext
                let sigSymbolUseOpt =
                  sigParseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(
                    mSig.End.Line,
                    mSig.End.Column,
                    sigLine,
                    [ implBindingIdent.idText ]
                  )

                match sigSymbolUseOpt with
                | None -> return []
                | Some sigSymbolUse ->

                  match mfv.GetValSignatureText(sigSymbolUse.DisplayContext, implSymbolUse.Range) with
                  | None -> return []
                  | Some valText ->
                    return
                      [ { SourceDiagnostic = None
                          Title = title
                          File = sigTextDocumentIdentifier
                          Edits =
                            [| { Range = fcsRangeToLsp mValSig
                                 NewText = valText } |]
                          Kind = FixKind.Fix } ]
          | _ -> return []
    })
