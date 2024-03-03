module FsAutoComplete.CodeFix.RemoveRedundantAttributeSuffix

open System
open FSharp.Compiler.Syntax
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let title = "Remove redundant attribute suffix"

let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  fun codeActionParams ->
    asyncResult {
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      let! parseAndCheck, _, _ = getParseResultsForFile filePath fcsPos

      return
        (fcsPos, parseAndCheck.GetParseResults.ParseTree)
        ||> ParsedInput.tryPick (fun _path node ->
          let (|RecordFieldAttributes|) =
            List.collect (fun (SynField(attributes = attributes)) -> attributes)

          let (|UnionFieldAttributes|) =
            List.collect (fun (SynUnionCase(attributes = attributes)) -> attributes)

          let (|EnumFieldAttributes|) =
            List.collect (fun (SynEnumCase(attributes = attributes)) -> attributes)

          let (|Suffixed|): SynAttributes -> Ident list =
            List.collect (fun { Attributes = attributes } ->
              attributes
              |> List.choose (fun attr ->
                attr.TypeName.LongIdent
                |> List.tryLast
                |> Option.filter (fun ident -> ident.idText.EndsWith("Attribute", StringComparison.Ordinal))))

          match node with
          | SyntaxNode.SynModule(SynModuleDecl.Attributes(attributes = Suffixed(_ :: _ as attributes)))
          | SyntaxNode.SynTypeDefn(SynTypeDefn(typeInfo = SynComponentInfo(attributes = Suffixed(_ :: _ as attributes))))
          | SyntaxNode.SynTypeDefn(SynTypeDefn(
            typeRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(
                                                recordFields = RecordFieldAttributes(Suffixed(_ :: _ as attributes))),
                                              _)))
          | SyntaxNode.SynTypeDefn(SynTypeDefn(
            typeRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(
                                                unionCases = UnionFieldAttributes(Suffixed(_ :: _ as attributes))),
                                              _)))
          | SyntaxNode.SynTypeDefn(SynTypeDefn(
            typeRepr = SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Enum(
                                                cases = EnumFieldAttributes(Suffixed(_ :: _ as attributes))),
                                              _)))
          | SyntaxNode.SynMemberDefn(SynMemberDefn.AutoProperty(attributes = Suffixed(_ :: _ as attributes)))
          | SyntaxNode.SynMemberDefn(SynMemberDefn.AbstractSlot(
            slotSig = SynValSig(attributes = Suffixed(_ :: _ as attributes))))
          | SyntaxNode.SynBinding(SynBinding(attributes = Suffixed(_ :: _ as attributes)))
          | SyntaxNode.SynPat(SynPat.Attrib(attributes = Suffixed(_ :: _ as attributes))) -> Some attributes
          | _ -> None)
        |> Option.toList
        |> List.collect (
          List.map (fun ident ->
            let updateText = ident.idText.Replace("Attribute", "")

            { Edits =
                [| { Range = fcsRangeToLsp ident.idRange
                     NewText = updateText } |]
              File = codeActionParams.TextDocument
              Title = title
              SourceDiagnostic = None
              Kind = FixKind.Refactor })
        )
    }
