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

///Global list of tests
[<Tests>]
let tests =
  let toolsPath = Ionide.ProjInfo.Init.init ()
  testSequenced <| testList "lsp" [
    for (name, workspaceLoaderFactory) in loaders do
      testSequenced <| testList name [
        let state = FsAutoComplete.State.Initial toolsPath workspaceLoaderFactory
        initTests state
        basicTests state
        codeLensTest state
        documentSymbolTest state
        Completion.autocompleteTest state
        Rename.tests state
        foldingTests state
        tooltipTests state
        highlightingTests state
        scriptPreviewTests state
        scriptEvictionTests state
        scriptProjectOptionsCacheTests state
        dependencyManagerTests  state
        interactiveDirectivesUnitTests

        // commented out because FSDN is down
        //fsdnTest state
        uriTests
        // fsharplint isn't updated to FCS 39, disabling tests until that's resolved
        // linterTests toolsPath
        formattingTests state
        // fake isn't updated to FCS 39, disabling tests until that's resolved
        //fakeInteropTests toolsPath
        analyzerTests state
        SignatureHelp.tests state
        CodeFixTests.tests state
        Completion.tests state
        GoTo.tests state
      ]
  ]


[<EntryPoint>]
let main args =
  let outputTemplate = "[{Timestamp:HH:mm:ss} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"

  let switch = LoggingLevelSwitch()
  if args |> Seq.contains "--debug"
  then
    switch.MinimumLevel <- LogEventLevel.Verbose
  else
    switch.MinimumLevel <- LogEventLevel.Information

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

  let cts = new CancellationTokenSource(testTimeout)
  let config =
    { defaultConfig
      with runInParallel = false
           failOnFocusedTests = true
           printer = Expecto.Impl.TestPrinters.summaryPrinter defaultConfig.printer }
  runTestsWithArgsAndCancel cts.Token config args tests
