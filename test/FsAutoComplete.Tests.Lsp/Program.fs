module LspTest

open Expecto
open Serilog
open FsAutoComplete.Logging
open System
open Serilog.Core
open Serilog.Events
open FsAutoComplete.Tests
open FsAutoComplete.Tests.CoreTest
open FsAutoComplete.Tests.ScriptTest
open FsAutoComplete.Tests.ExtensionsTests
open FsAutoComplete.Tests.InteractiveDirectivesTests
open Ionide.ProjInfo
open System.Threading

let testTimeout =
  Environment.GetEnvironmentVariable "TEST_TIMEOUT_MINUTES"
  |> Int32.TryParse
  |> function true, duration -> duration
            | false, _ -> 10
  |> float
  |> TimeSpan.FromMinutes

let loaders = [
  "Ionide WorkspaceLoader",  WorkspaceLoader.Create
  "MSBuild Project Graph WorkspaceLoader", WorkspaceLoaderViaProjectGraph.Create
]

let tests loaders =
  let toolsPath = Ionide.ProjInfo.Init.init (System.IO.DirectoryInfo Environment.CurrentDirectory)
  testSequenced <| testList "lsp" [
    for (name, workspaceLoaderFactory) in loaders do
      testSequenced <| testList name [
        Templates.tests()
        let state = FsAutoComplete.State.Initial toolsPath workspaceLoaderFactory
        initTests state
        codeLensTest state
        documentSymbolTest state
        Completion.autocompleteTest state
        Completion.autoOpenTests state
        Rename.tests state
        foldingTests state
        tooltipTests state
        Highlighting.tests state
        scriptPreviewTests state
        scriptEvictionTests state
        scriptProjectOptionsCacheTests state
        dependencyManagerTests  state
        interactiveDirectivesUnitTests

        // commented out because FSDN is down
        //fsdnTest state
        uriTests
        linterTests state
        formattingTests state
        // fake isn't updated to FCS 39, disabling tests until that's resolved
        //fakeInteropTests toolsPath
        analyzerTests state
        signatureTests state
        SignatureHelp.tests state
        CodeFixTests.tests state
        Completion.tests state
        GoTo.tests state
        FindReferences.tests state
      ]
  ]


[<EntryPoint>]
let main args =
  let outputTemplate = "[{Timestamp:HH:mm:ss} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"
  let verbose = args |> Seq.contains "--debug"
  let switch = LoggingLevelSwitch()

  if verbose
  then
    switch.MinimumLevel <- LogEventLevel.Verbose
  else
    switch.MinimumLevel <- LogEventLevel.Information

  let argsToRemove, loaders =
    args
    |> Array.windowed 2
    |> Array.tryPick (function [| "--loader"; "ionide" |] as args -> Some(args, ["Ionide WorkspaceLoader", WorkspaceLoader.Create])
                               | [| "--loader"; "graph" |] as args -> Some(args, ["MSBuild Project Graph WorkspaceLoader", WorkspaceLoaderViaProjectGraph.Create])
                               | _ -> None )
    |> Option.defaultValue ([||], loaders)

  let serilogLogger =
    LoggerConfiguration()
      .Enrich.FromLogContext()
      .MinimumLevel.ControlledBy(switch)
      .Filter.ByExcluding(fun ev ->
        match ev.Properties.["SourceContext"] with
        | null -> false
        | :? ScalarValue as scalar ->
          match scalar.Value with
          | null -> false
          | :? string as text when text = "FileSystem" -> true
          | _ -> false
        | _ -> false
      )

      .Destructure.FSharpTypes()
      .Destructure.ByTransforming<FSharp.Compiler.Text.Range>(fun r -> box {| FileName = r.FileName; Start = r.Start; End = r.End |})
      .Destructure.ByTransforming<FSharp.Compiler.Text.Pos>(fun r -> box {| Line = r.Line; Column = r.Column |})
      .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
      .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
      .WriteTo.Async(
        fun c -> c.Console(outputTemplate = outputTemplate, standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose), theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code) |> ignore
      ).CreateLogger() // make it so that every console log is logged to stderr
  Serilog.Log.Logger <- serilogLogger
  LogProvider.setLoggerProvider (Providers.SerilogProvider.create())

  let fixedUpArgs =
    args
    |> Array.except ["--debug"]
    |> Array.except argsToRemove

  let cts = new CancellationTokenSource(testTimeout)
  let config =
    { defaultConfig
      with runInParallel = false
           failOnFocusedTests = true
           printer = Expecto.Impl.TestPrinters.summaryPrinter defaultConfig.printer
           verbosity = if verbose then Expecto.Logging.LogLevel.Debug else Expecto.Logging.LogLevel.Info }
  runTestsWithArgsAndCancel cts.Token config fixedUpArgs (tests loaders)
