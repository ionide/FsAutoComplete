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

let private mkEditsForMissingTypes (missingTypeInfo: MissingTypeInfo) : TextEdit list =
    [
        // generic parameters

        // parameters
        for p in missingTypeInfo.Parameters do
            // TODO: deal with multiline parameter patterns
            if p.Range.StartLine = p.Range.EndLine && not p.AddParentheses then
                {
                    NewText = $"(%s{p.SourceText}: %s{p.TypeName})"
                    Range = fcsRangeToLsp p.Range
                }

        // declaration specific
        match missingTypeInfo.Declaration with
        | Declaration.Binding(returnType = Some returnType) ->
            {
                NewText = $": %s{returnType.TypeName} ="
                Range = fcsRangeToLsp returnType.Equals
            }
        | _ -> ()
    ]

let private mkEditToPrivate (missingTypeInfo: MissingTypeInfo) : TextEdit list =
    match missingTypeInfo.Declaration with
    | Declaration.Binding(leadingKeyword = leadingKeyword) ->
        match leadingKeyword with
        | SynLeadingKeyword.Let mLet ->
            List.singleton
                {
                    NewText = "let private"
                    Range = fcsRangeToLsp mLet
                }
        | _ -> List.empty
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
                if missingTypeInfo.ValueIsUsedOutsideTheFileInTheProject then
                    mkEditsForMissingTypes missingTypeInfo
                else
                    mkEditToPrivate missingTypeInfo)
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
