#r "FSharp.Compiler.Service"
open FSharp.Compiler.EditorServices
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open System.IO
let fcsVersion =
    System
        .Reflection
        .Assembly
        .GetAssembly(
            typeof<FSharpChecker>
        )
        .GetName()
        .Version
printfn "%A" fcsVersion
let checker = FSharpChecker.Create()
let sourceText path =
    let contents = File.ReadAllText(path)
    SourceText.ofString contents
let getNetCoreScriptOptions (path: string) =
    checker.GetProjectOptionsFromScript(
        path,
        sourceText path,
        assumeDotNetFramework = false,
        useSdkRefs = true,
        useFsiAuxLib = true,
        sdkDirOverride = @"C:\Program Files\dotnet\sdk\5.0.402\",
        otherFlags = [| "--targetprofile:netstandard" |]
    )
    |> Async.RunSynchronously
let parseAndCheckFile (path, options) =
    let p, c =
        checker.ParseAndCheckFileInProject(path, 0, sourceText path, options)
        |> Async.RunSynchronously
    match c with
    | FSharpCheckFileAnswer.Aborted -> p, None
    | FSharpCheckFileAnswer.Succeeded c -> p, Some c
let scriptRoot =
    @"..\test\FsAutoComplete.Tests.Lsp\TestCases"
let testGroup = "CompletionAutoOpenTests"
let testCase = "ModuleDocsAndNewLineBeforeOpen.fsx"
let fullPath =
    Path.Combine(scriptRoot, testGroup, testCase) |> Path.GetFullPath
let projectOpts, projectOptsErrors = getNetCoreScriptOptions fullPath
let parseResult, checkResult =
    parseAndCheckFile (fullPath, projectOpts)
let pErrors =
    parseResult.Diagnostics
let cErrors =
    checkResult.Value.Diagnostics
for f in projectOpts.SourceFiles do
    printfn "File: %s" f
for opt in projectOpts.OtherOptions do
    printfn "Opt: %s" opt
printfn "%A" pErrors
printfn "%A" cErrors