module FsAutoComplete.CodeFix.UpdateValueInSignatureFile

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Diagnostics.ExtendedData
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

#nowarn "57"

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

      let! (implParseAndCheckResults: ParseAndCheckResults, implLine: string, _implSourceText: IFSACSourceText) =
        getParseResultsForFile implFileName (protocolPosToPos diagnostic.Range.Start)

      let mDiag =
        protocolRangeToRange implParseAndCheckResults.GetParseResults.FileName diagnostic.Range

      let! extendedDiagnosticData =
        implParseAndCheckResults.GetCheckResults.Diagnostics
        |> Array.tryPick (fun (diag: FSharpDiagnostic) ->
          if diag.ErrorNumber <> 34 || not (Range.equals diag.Range mDiag) then
            None
          else
            match diag.ExtendedData with
            | Some(:? ValueNotContainedDiagnosticExtendedData as extendedData) -> Some extendedData
            | _ -> None)
        |> Result.ofOption (fun () -> "No extended data")

      // Find the binding name in the implementation file.
      let impVisitor =
        { new SyntaxVisitorBase<_>() with
            override x.VisitBinding(path, defaultTraverse, SynBinding(headPat = pat)) =
              match pat with
              | SynPat.LongIdent(longDotId = SynLongIdent(id = [ ident ])) when Range.equals mDiag ident.idRange ->
                Some ident
              | _ -> None }

      let! implBindingIdent =
        SyntaxTraversal.Traverse(mDiag.Start, implParseAndCheckResults.GetParseResults.ParseTree, impVisitor)
        |> Result.ofOption (fun () -> "No binding name found")

      let endPos = implBindingIdent.idRange.End

      let! symbolUse =
        implParseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(
          endPos.Line,
          endPos.Column,
          implLine,
          [ implBindingIdent.idText ]
        )
        |> Result.ofOption (fun () -> "No symbolUse found")

      let! valText =
        extendedDiagnosticData.ImplementationValue.GetValSignatureText(symbolUse.DisplayContext, symbolUse.Range)
        |> Result.ofOption (fun () -> "No val text found.")

      // Find a matching val in the signature file.
      let sigVisitor =
        { new SyntaxVisitorBase<_>() with
            override x.VisitValSig(path, defaultTraverse, SynValSig(range = mValSig)) =
              if Range.rangeContainsRange mValSig extendedDiagnosticData.SignatureValue.DeclarationLocation then
                Some mValSig
              else
                None }

      let! (sigParseAndCheckResults: ParseAndCheckResults, _sigLine: string, _sigSourceText: IFSACSourceText) =
        getParseResultsForFile sigFileName extendedDiagnosticData.SignatureValue.DeclarationLocation.End

      let! mVal =
        SyntaxTraversal.Traverse(
          extendedDiagnosticData.SignatureValue.DeclarationLocation.End,
          sigParseAndCheckResults.GetParseResults.ParseTree,
          sigVisitor
        )
        |> Result.ofOption (fun () -> "No val range found in signature file")

      return
        [ { SourceDiagnostic = None
            Title = title
            File = sigTextDocumentIdentifier
            Edits =
              [| { Range = fcsRangeToLsp mVal
                   NewText = valText } |]
            Kind = FixKind.Fix } ]
    })
