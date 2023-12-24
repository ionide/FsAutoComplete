module FsAutoComplete.CodeFix.TopLevelTypes

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.UMX
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.CodeAnalysis
open TopLevelTypeAnalysis

let private collectMissingTypeInformation
    (forceGetFSharpProjectOptions: string<LocalPath> -> Async<Result<FSharpProjectOptions, string>>)
    (getParseResultsForFile: GetParseResultsForFile)
    (forceChecker: unit -> FSharpCompilerServiceChecker)
    (codeActionParams: CodeActionParams)
    : Async<Result<MissingTypeInfo list, string>>
    =
    asyncResult {
        let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
        let! options = forceGetFSharpProjectOptions fileName
        let checker = forceChecker ()
        do! checker.NotifyFileChanged(UMX.untag fileName, options) // workaround for https://github.com/dotnet/fsharp/issues/15960
        let! projectCheckResults = checker.ParseAndCheckProject options
        let allSymbolUses = projectCheckResults.GetAllUsesOfAllSymbols()

        if Array.isEmpty allSymbolUses then
            return []
        else

        let! parseAndCheckResult, _, fsacSourceText = getParseResultsForFile fileName Position.pos0

        let sourceText: ISourceText = fsacSourceText

        let parsedInput, checkFileResults =
            parseAndCheckResult.GetParseResults.ParseTree, parseAndCheckResult.GetCheckResults

        return
            TopLevelTypeAnalysis.findMissingTypeInformation sourceText parsedInput checkFileResults projectCheckResults
    }

let titleMkPrivate = "Make as much as possible private"
let titleAddTopLevelTypes = "Add missing top level types"

let private (|OneLinerParameter|_|) (p: MissingParameterType) =
    match p with
    | MissingParameterType.SingleParameter info -> info.Range.EndLine = info.Range.StartLine
    | MissingParameterType.SimpleTupleParameter items ->
        items
        |> List.collect (fun p -> [ p.Range.EndLine; p.Range.StartLine ])
        |> List.distinct
        |> List.tryExactlyOne
        |> Option.isSome
    | MissingParameterType.Pattern info -> info.Range.EndLine = info.Range.StartLine
    |> fun isOneliner -> if isOneliner then Some() else None

let private mkGenericParameterEdits (missingTypeInfo: MissingTypeInfo) : TextEdit list =
    if missingTypeInfo.GenericParameters.IsEmpty then
        []
    else

    match missingTypeInfo.CurrentGenericParameters with
    | Some _ ->
        // TODO: update generic parameter or constraints
        []
    | None ->

    match missingTypeInfo.Declaration with
    | Declaration.AutoProperty _ -> []
    | Declaration.ImplicitCtor(typeName = ident)
    | Declaration.Binding(name = ident) ->
        let text =
            missingTypeInfo.GenericParameters
            |> List.map (fun gp -> gp.Name)
            |> String.concat ", "

        [
            {
                NewText = $"%s{ident.idText}<%s{text}>"
                Range = fcsRangeToLsp ident.idRange
            }
        ]

let private mkEditsForMissingTypes (missingTypeInfo: MissingTypeInfo) : TextEdit list =
    [
        // generic parameters
        yield! mkGenericParameterEdits missingTypeInfo

        // parameters
        for p in missingTypeInfo.Parameters do
            // TODO: deal with multiline parameter patterns
            match p with
            | OneLinerParameter & MissingParameterType.SingleParameter info ->
                {
                    NewText =
                        if info.InsideConstructor then
                            $"%s{info.SourceText}: %s{info.TypeName}"
                        else
                            $"(%s{info.SourceText}: %s{info.TypeName})"
                    Range = fcsRangeToLsp info.Range
                }
            | OneLinerParameter & MissingParameterType.SimpleTupleParameter items ->
                yield!
                    items
                    |> List.map (fun item ->
                        {
                            NewText = $"%s{item.SourceText}: %s{item.TypeName}"
                            Range = fcsRangeToLsp item.Range
                        })
            | _ -> ()

        // declaration specific
        match missingTypeInfo.Declaration with
        | Declaration.Binding(returnType = Some returnType) ->
            {
                NewText = $": %s{returnType.TypeName} ="
                Range = fcsRangeToLsp returnType.Equals
            }
        | Declaration.AutoProperty(ident, typeName) ->
            {
                NewText = $"%s{ident.idText} : %s{typeName}"
                Range = fcsRangeToLsp ident.idRange
            }
        | _ -> ()
    ]

let private mkEditToPrivate (missingTypeInfo: MissingTypeInfo) : TextEdit list =
    match missingTypeInfo.Declaration with
    | Declaration.Binding(leadingKeyword = leadingKeyword) ->
        [
            match leadingKeyword with
            | SynLeadingKeyword.Let _ ->
                {
                    NewText = "let private"
                    Range = fcsRangeToLsp leadingKeyword.Range
                }
            | SynLeadingKeyword.And _ ->
                {
                    NewText = "and private"
                    Range = fcsRangeToLsp leadingKeyword.Range
                }
            | SynLeadingKeyword.LetRec _ ->
                {
                    NewText = "let rec private"
                    Range = fcsRangeToLsp leadingKeyword.Range
                }
            | _ -> ()
        ]
    | _ -> List.empty

let fix
    (forceGetFSharpProjectOptions: string<LocalPath> -> Async<Result<FSharpProjectOptions, string>>)
    (getParseResultsForFile: GetParseResultsForFile)
    (forceChecker: unit -> FSharpCompilerServiceChecker)
    (codeActionParams: CodeActionParams)
    : Async<Result<Fix list, string>>
    =
    asyncResult {
        let! missingTypeInformation =
            collectMissingTypeInformation
                forceGetFSharpProjectOptions
                getParseResultsForFile
                forceChecker
                codeActionParams

        ignore missingTypeInformation

        if missingTypeInformation.IsEmpty then
            return []
        else

        let editsMkPrivate =
            missingTypeInformation
            |> List.collect (fun missingTypeInfo ->
                if
                    (not missingTypeInfo.ValueIsUsedOutsideTheFileInTheProject
                     && missingTypeInfo.Declaration.IsLetBinding)
                then
                    mkEditToPrivate missingTypeInfo
                else
                    mkEditsForMissingTypes missingTypeInfo)
            |> List.toArray

        let editsAddTopLevelTypes =
            missingTypeInformation |> List.collect mkEditsForMissingTypes |> List.toArray

        return
            [
                {
                    Edits = editsMkPrivate
                    File = codeActionParams.TextDocument
                    Title = titleMkPrivate
                    SourceDiagnostic = None
                    Kind = FixKind.Refactor
                }
                {
                    Edits = editsAddTopLevelTypes
                    File = codeActionParams.TextDocument
                    Title = titleAddTopLevelTypes
                    SourceDiagnostic = None
                    Kind = FixKind.Refactor
                }
            ]
    }
