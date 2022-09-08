module FsAutoComplete.Tests.Adaptive
open Expecto
open Ionide.ProjInfo
open FSharp.Compiler.CodeAnalysis
open FsAutoComplete
open FSharp.Compiler.Text
open System.Diagnostics
open FSharp.Data.Adaptive
open System
open Microsoft.Build.Graph

// search tags: debug, debugger, attach
module Debugging =
  let waitForDebuggerAttached (programName) =
#if DEBUG
    if not(System.Diagnostics.Debugger.IsAttached) then
      printfn "Please attach a debugger for %s, PID: %d" programName (System.Diagnostics.Process.GetCurrentProcess().Id)
    while not(System.Diagnostics.Debugger.IsAttached) do
      System.Threading.Thread.Sleep(100)
#else
    ()
#endif
  let waitForDebuggerAttachedAndBreak (programName) =
#if DEBUG
    waitForDebuggerAttached programName
    System.Diagnostics.Debugger.Break()
#else
    ()
#endif

let hasAnalyzers = false
let checker =
    FSharpChecker.Create(
      projectCacheSize = 200,
      keepAllBackgroundResolutions = true,
      keepAssemblyContents = hasAnalyzers,
      suggestNamesForErrors = true,
      enablePartialTypeChecking = not hasAnalyzers,
      enableBackgroundItemKeyStoreAndSemanticClassification = true,
      keepAllBackgroundSymbolUses = true
    )

let measureTime name f =
    let sw = Stopwatch.StartNew()
    let r = f ()
    printfn $"{name} : {sw.ElapsedMilliseconds}ms"
    r

let measureTimeAsync name f = async {
    let sw = Stopwatch.StartNew()
    let! r = f
    printfn $"{name} : {sw.ElapsedMilliseconds}ms"
    return r
}

let adaptiveFile filePath =
    AdaptiveFile.GetLastWriteTimeUtc filePath
    |> AVal.map(fun writeTime -> filePath, writeTime)

let awaitManualInput (msg : string) =
    Console.WriteLine(msg)
    Console.ReadLine() |> ignore

let tests toolsPath =
    ptestList "Adaptive" [
        testCaseAsync "Lol" <| async {
            let loader = WorkspaceLoaderViaProjectGraph.Create(toolsPath)
            let slnPath = "/Users/theangrybyrd/Repositories/public/FsToolkit.ErrorHandling/FsToolkit.ErrorHandling.sln"
            let observeASet msg = ASet.force >> printfn "%s : %A" msg
            let observeAVal msg = AVal.force >> printfn "%s : %A" msg
            // let results = measureTime "Buildalyzer" (fun () -> getOpsWithBuildalyzer slnPath)
            //

            let aSlnFile = adaptiveFile slnPath
            // AdaptiveFile.GetAttributes slnPath |> AVal.force |> printfn "Attributes"

            // let (name,ellapsed,options) = time "Load Solution Initial" (fun () -> loader.LoadSln(slnPath))
            // printfn $"{name}, {ellapsed}"
            let graph = ProjectGraph slnPath
            let filesThatChange = cset<string> [slnPath]
            // graph.ProjectNodesTopologicallySorted
            // |> Seq.iter(fun p ->
            //     p.ProjectInstance.ProjectFileLocation.LocationString )
            let changedFiles =
                filesThatChange
                // |> ASet.mapA(fun file -> adaptiveFile file)
                |> ASet.collect(fun (filepath ) ->
                    let entryPoint =
                        adaptiveFile filepath
                    let additionalProjs =
                        (ProjectGraph filepath).ProjectNodesTopologicallySorted
                        |> Seq.map(fun p -> adaptiveFile p.ProjectInstance.ProjectFileLocation.LocationString)
                        |> Seq.toList
                    let fooby = ASet.ofAVal
                    let filestoWatch =
                        (entryPoint :: additionalProjs)
                        |> ASet.ofList
                        |> ASet.mapA id
                    filestoWatch
                )

            let changedValuesSnap =
                changedFiles
                |> ASet.toAVal


            let aOptions =
                changedValuesSnap
                |> AVal.map(fun items ->
                    let (file,time) = items |> Seq.maxBy(fun (name, time) -> time)
                    printfn $"running load because of {file}"
                    let opts = measureTime "loader.LoadSln(file)" (fun () -> loader.LoadSln(slnPath))
                    opts
                )


            // observeASet (nameof(filesThatChange)) filesThatChange
            // observeASet (nameof(changedFiles)) changedFiles
            // observeAVal (nameof(changedValuesSnap)) changedValuesSnap

            // let aOptionsV = measureTime "allLoads" <| fun () -> aOptions |> AVal.force

            // observeASet (nameof(filesThatChange)) filesThatChange
            // observeASet (nameof(changedFiles)) changedFiles
            // observeAVal (nameof(changedValuesSnap)) changedValuesSnap

            // awaitManualInput "Go edit a project or sln file"

            // observeASet (nameof(filesThatChange)) filesThatChange
            // observeASet (nameof(changedFiles)) changedFiles
            // observeAVal (nameof(changedValuesSnap)) changedValuesSnap

            // let aOptionsV = measureTime "allLoads after change" <| fun () -> aOptions |> AVal.force

            // observeASet (nameof(filesThatChange)) filesThatChange
            // observeASet (nameof(changedFiles)) changedFiles
            // observeAVal (nameof(changedValuesSnap)) changedValuesSnap
            // printfn $"{foo2}"
            // let aOptions =
            //     aSlnFile
            //     |> AVal.map(fun (file, time) ->
            //         let opts = measureTime "loader.LoadSln(file)" (fun () -> loader.LoadSln(file))
            //         opts
            //     )

            // let fsharpOptions = options |> Seq.map (fun o -> FCS.mapToFSharpProjectOptions o options) |> Seq.toList
            let aFSharpOptions =
                aOptions
                |> AVal.map(fun options ->
                    let op1 = options |> Seq.map (fun o -> FCS.mapToFSharpProjectOptions o options) |> Seq.toList

                    // Debugging.waitForDebuggerAttached "adaptive tests"
                    op1
                )

            let incomingFile = "/Users/theangrybyrd/Repositories/public/FsToolkit.ErrorHandling/tests/FsToolkit.ErrorHandling.JobResult.Tests/JobResultCE.fs"


            let aFile =
                ((adaptiveFile incomingFile), aFSharpOptions)
                ||> AVal.map2(fun (name, writeTime) fsharpOptions ->
                    let fileVersion = writeTime.Ticks |> int
                    printfn $"{nameof(fileVersion)} : {fileVersion}"
                    let sourceText = SourceText.ofString (System.IO.File.ReadAllText name)
                    let projectOptions =
                        fsharpOptions |> List.find(fun x -> x.SourceFiles |> Array.contains(name))
                    // let results =
                    //     measureTime "typecheck total:" <| fun () ->
                    //         fsharpOptions
                    //         |> List.map(fun o ->

                    //             measureTimeAsync $"{o.ProjectFileName} typecheck" (checker.ParseAndCheckProject(o))
                    //         )
                    //         |> Async.Parallel
                    //         |> Async.RunSynchronously

                    let result = measureTime "Single file parse" <| fun () -> checker.ParseAndCheckFileInProject(name,fileVersion, sourceText,projectOptions) |> Async.RunSynchronously


                    result
            )


            // let fileVersion = 0
            // let sourceText = SourceText.ofString (System.IO.File.ReadAllText filename)
            // let projectOptions = fsharpOptions |> List.find(fun x -> x.SourceFiles |> Array.contains(filename))
            // let! (name,ellapsed, (result, answer)) = timeAsync "Parse and Check" (checker.ParseAndCheckFileInProject(filename,fileVersion, sourceText,projectOptions))

            // printfn $"{name}, {ellapsed}"
            // options |> Seq.iter(fun o -> printfn "%A" o.Items)
            // printfn "%A" fsharpOptions
            let results = measureTime "Load and Typecheck Solution" (fun () -> aFile |> AVal.force)

            // awaitManualInput $"Go edi {incomingFile}"
            let results = measureTime "Load and Typecheck Solution" (fun () -> aFile |> AVal.force)
            // printfn "results: %A %A" result answer
            // let result, answer = measureTime "Typecheck2" (fun () -> aFile |> AVal.force)
            // printfn "results: %A %A" result answer
            // Console.ReadLine() |> ignore

            // let result, answer = time "Typecheck2" (fun () -> aFile |> AVal.force)
            // printfn "results: %A %A" result answer
            ()
        }
    ]

