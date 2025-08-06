module FsAutoComplete.Tests.TestExplorer

open Expecto
open Helpers
open System.IO
open FsAutoComplete.LspHelpers
open System.Threading
open Helpers.Expecto.ShadowedTimeouts

let tests createServer =
    let initializeServer workspaceRoot =
        async {
            let! (server, event) = serverInitialize workspaceRoot defaultConfigDto createServer
            do! waitForWorkspaceFinishedParsing event

            return (server, event)
        } |> Async.Cache

    testSequenced <| testList "TestExplorerTests" 
        [ testCaseAsync "it should report a processId when debug a test project" <| async {
            // let workspaceRoot = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "XunitTests")
            //X:\source\dotnet\FsAutoComplete\test\FsAutoComplete.Tests.Lsp\TestCases\XunitTests\bin\Debug\net8.0
            //X:\source\dotnet\FsAutoComplete\test\SampleTestProjects\VSTest.XUnit.RunResults\bin\Debug\net8.0
            let workspaceRoot = Path.Combine(__SOURCE_DIRECTORY__, "..", "SampleTestProjects", "VSTest.XUnit.RunResults")
            let! server, clientNotifications = initializeServer workspaceRoot

            use server = server
            
            use tokenSource = new CancellationTokenSource()
            let mutable processIdSpy : string option = None 
            use! _onCancel = Async.OnCancel(fun () -> tokenSource.Cancel())
            use _ = clientNotifications.Subscribe(fun (msgType: string, data: obj) ->
                if msgType = "test/processWaitingForDebugger" then 
                    printfn $"Sya: update message {data}"
                    let processId : string = data :?> PlainNotification |> _.Content |> FsAutoComplete.JsonSerializer.readJson
                    processIdSpy <- Some processId
                    tokenSource.Cancel()
                    let tryParseProcessId (str: string) = 
                        let (success, value) = System.Int32.TryParse(str)
                        printfn $"Sya: parsed process id: {success}, {value}. Original: {str}"
                        if success then Some value else None

                    printfn $"Sya: process spy {processIdSpy}"
                    processId |> tryParseProcessId |> Option.iter(fun pid -> 
                        try 
                            printfn $"Sya: trying to kill process {pid}"
                            System.Diagnostics.Process.GetProcessById(pid).Kill(true) 
                            printfn $"Sya: killed process {pid}"
                        with e -> 
                            printfn $"Sya: failed to kill process {pid}"
                    )
            )
            Expect.throws (fun () ->
                let runRequest : TestRunRequest = {
                    TestCaseFilter = None
                    AttachDebugger = true
                }
                Async.RunSynchronously(server.TestRunTests(runRequest) |> Async.Ignore, cancellationToken = tokenSource.Token)
            ) ""
            printfn "Sya: Server test run closed"
            Expect.isSome processIdSpy ""
            printfn "Sya: test complete"
        }]
    
