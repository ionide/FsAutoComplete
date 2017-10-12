#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System
open System.Diagnostics

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let startWrapper (workingDir: string) (exePath: string) (args: string) f =
    printfn "Running '%s %s' in working dir '%s'" exePath args workingDir
    let psi = ProcessStartInfo()
    psi.FileName <- exePath
    psi.WorkingDirectory <- workingDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Arguments <- args
    psi.CreateNoWindow <- true
    psi.UseShellExecute <- false

    let p = new Process()
    p.StartInfo <- psi

    p.OutputDataReceived
    |> Event.map (fun ea -> ea.Data)
    |> Event.filter (not << isNull)
    |> Event.add (fun s ->
        printfn "%s" s
        f (p, s))

    p.ErrorDataReceived
    |> Event.add (fun ea ->  printfn "%s" (ea.Data))

    p.Start() |> ignore
    p.BeginOutputReadLine()
    p.BeginErrorReadLine()
    p


let rec retry (times, sleepTime: TimeSpan) f = async {
    if times < 0 then
        return false
    elif f () then
        return true
    else
        do! Async.Sleep (sleepTime.TotalMilliseconds |> int)
        return! retry (times - 1, sleepTime) f
    }

let testOutput = Text.StringBuilder()
let log s =
    printfn "%s" s
    testOutput.Append(s + "\n") |> ignore

/// parse the started info message who contains the PID
let (|FSACStartedMsg|_|) (s: string) =
    let startedInfoMessage = """{"Kind":"info","Data":"Started (PID="""
    let startedInfoMessageEnd = """)"}"""
    if s.Contains(startedInfoMessage) then
        s.Replace(startedInfoMessage, "").Replace(startedInfoMessageEnd, "").Trim()
        |> int
        |> Some
    else
        None

let run () =
    let fsacExePath = FsAutoCompleteWrapper.ExePath ()
    let requireDotnetFlag =
      match testConfig.Runtime with
      | FSACRuntime.NETCoreFDD -> true
      | FSACRuntime.NET | FSACRuntime.NETCoreSCD -> false

    let start =
        let isWindows = Environment.OSVersion.Platform = PlatformID.Win32NT
        if isWindows then
            let useDotnetFlag = if requireDotnetFlag then "-UseDotnet" else ""
            startWrapper __SOURCE_DIRECTORY__ "powershell.exe" (sprintf """-NonInteractive -ExecutionPolicy Bypass -File runfsac.ps1 "%s" %s """ fsacExePath useDotnetFlag)
        else
            let useDotnetFlag = if requireDotnetFlag then "--use-dotnet" else ""
            startWrapper __SOURCE_DIRECTORY__ "sh" (sprintf """./runfsac.sh "%s" %s """ fsacExePath useDotnetFlag)

    let mutable fsacProc : Process option = None

    let manualResetEvent = new System.Threading.ManualResetEvent(true)
    
    use p = start (fun (hostProc, s) ->
        match s with
        | FSACStartedMsg fsacPID ->
            log "FSAC started"
            let fsac = Process.GetProcessById fsacPID
            fsacProc <- Some fsac

            log "FSAC process found by PID"

            // let's kill the host process
            printfn "killing host (PID=%i)" hostProc.Id
            log "killing host"
            
            manualResetEvent.Reset() |> ignore
            
            hostProc.Kill()
            log "killed host"
            
            manualResetEvent.Set() |> ignore
        | _ -> ()
        )

    let exited = p.WaitForExit(TimeSpan.FromSeconds(30.0).TotalMilliseconds |> int)
    
    //wait until log is written
    manualResetEvent.WaitOne() |> ignore

    // check fsac shouldnt be alive
    match exited, fsacProc with
    | false, Some fsacProc ->
        log "timeout out, with fsac running"
    | false, None ->
        log "timeout out, fsac not started"
    | true, None ->
        log "unexpected, exited without fsac process"
    | true, Some fsacProc ->
        log "waiting FSAC to quit"
        let fsacExited =
            retry (5, TimeSpan.FromSeconds(0.5)) (fun () ->
                printfn "checking..."
                let e = fsacProc.HasExited
                e)
            |> Async.RunSynchronously

        log (sprintf "FSAC exited: %s" (fsacExited.ToString()))

    // cleanup test, if needed
    match fsacProc with
    | Some proc when not (proc.HasExited) ->
        printfn "cleanup test..."
        try proc.Kill(); proc.Dispose() with _ -> ()
    | _ -> ()

run ()

testOutput.ToString()
|> writeNormalizedOutput "output.json"
