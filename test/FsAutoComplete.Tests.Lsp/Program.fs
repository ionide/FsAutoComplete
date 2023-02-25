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
open Serilog.Filters
open System.IO

Expect.defaultDiffPrinter <- Diff.colourisedDiff

let testTimeout =
  Environment.GetEnvironmentVariable "TEST_TIMEOUT_MINUTES"
  |> Int32.TryParse
  |> function
    | true, duration -> duration
    | false, _ -> 10
  |> float
  |> TimeSpan.FromMinutes

// delay in ms between workspace start + stop notifications because the system goes too fast :-/
Environment.SetEnvironmentVariable("FSAC_WORKSPACELOAD_DELAY", "250")

let loaders =
  [
    "Ionide WorkspaceLoader", (fun toolpath props ->
      let props = FsAutoComplete.Utils.Map.merge FsAutoComplete.Core.ProjectLoader.globalProperties props |> Map.toList
      WorkspaceLoader.Create(toolpath, props))
    // "MSBuild Project Graph WorkspaceLoader", (fun toolpath -> WorkspaceLoaderViaProjectGraph.Create(toolpath, FsAutoComplete.Core.ProjectLoader.globalProperties))
  ]

let fsharpLspServerFactory toolsPath workspaceLoaderFactory =
  let testRunDir =
    Path.Combine(Path.GetTempPath(), "FsAutoComplete.Tests", Guid.NewGuid().ToString())
    |> DirectoryInfo

  let createServer () =
    FsAutoComplete.State.Initial toolsPath testRunDir workspaceLoaderFactory

  Helpers.createServer createServer

let adaptiveLspServerFactory toolsPath workspaceLoaderFactory =
  Helpers.createAdaptiveServer (fun () -> workspaceLoaderFactory toolsPath)

let lspServers =
  [
    // "FSharpLspServer", fsharpLspServerFactory
    "AdaptiveLspServer", adaptiveLspServerFactory
    ]

let mutable toolsPath =
  Ionide.ProjInfo.Init.init (System.IO.DirectoryInfo Environment.CurrentDirectory) None

let lspTests =
  testList
    "lsp"
    [ for (loaderName, workspaceLoaderFactory) in loaders do
        for (lspName, lspFactory) in lspServers do
          testList
            $"{loaderName}.{lspName}"
            [ 
              Templates.tests ()
              let createServer () =
                lspFactory toolsPath workspaceLoaderFactory

              initTests createServer
              closeTests createServer

              Utils.Tests.Server.tests createServer
              Utils.Tests.CursorbasedTests.tests createServer

              CodeLens.tests createServer
              documentSymbolTest createServer
              Completion.autocompleteTest createServer
              Completion.autoOpenTests createServer
              foldingTests createServer
              tooltipTests createServer
              Highlighting.tests createServer
              scriptPreviewTests createServer
              scriptEvictionTests createServer
              scriptProjectOptionsCacheTests createServer
              dependencyManagerTests createServer
              interactiveDirectivesUnitTests

              // commented out because FSDN is down
              //fsdnTest createServer

              //linterTests createServer
              uriTests
              formattingTests createServer
              analyzerTests createServer
              signatureTests createServer
              SignatureHelp.tests createServer
              CodeFixTests.Tests.tests createServer
              Completion.tests createServer
              GoTo.tests createServer

              FindReferences.tests createServer
              Rename.tests createServer

              InfoPanelTests.docFormattingTest createServer
              DetectUnitTests.tests createServer
              XmlDocumentationGeneration.tests createServer
              InlayHintTests.tests createServer
              DependentFileChecking.tests createServer
              UnusedDeclarationsTests.tests createServer

              ] ]
      
/// Tests that do not require a LSP server
let generalTests = testList "general" [
  testList (nameof (Utils)) [ Utils.Tests.Utils.tests; Utils.Tests.TextEdit.tests ]
  InlayHintTests.explicitTypeInfoTests

  FindReferences.tryFixupRangeTests
]

[<Tests>]
let tests =
  testList
    "FSAC"
    [
      generalTests
      lspTests 
    ]


[<EntryPoint>]
let main args =
  let outputTemplate =
    "[{Timestamp:HH:mm:ss} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"

  let parseLogLevel (args: string[]) =
    let logMarker = "--log="

    let logLevel =
      match
        args
        |> Array.tryFind (fun arg -> arg.StartsWith logMarker)
        |> Option.map (fun log -> log.Substring(logMarker.Length))
      with
      | Some ("warn" | "warning") -> Logging.LogLevel.Warn
      | Some "error" -> Logging.LogLevel.Error
      | Some "fatal" -> Logging.LogLevel.Fatal
      | Some "info" -> Logging.LogLevel.Info
      | Some "verbose" -> Logging.LogLevel.Verbose
      | Some "debug" -> Logging.LogLevel.Debug
      | _ -> Logging.LogLevel.Info

    let args =
      args
      |> Array.filter (fun arg -> not <| arg.StartsWith logMarker)

    logLevel, args

  let expectoToSerilogLevel =
    function
    | Logging.LogLevel.Debug -> LogEventLevel.Debug
    | Logging.LogLevel.Verbose -> LogEventLevel.Verbose
    | Logging.LogLevel.Info -> LogEventLevel.Information
    | Logging.LogLevel.Warn -> LogEventLevel.Warning
    | Logging.LogLevel.Error -> LogEventLevel.Error
    | Logging.LogLevel.Fatal -> LogEventLevel.Fatal

  let parseLogExcludes (args: string[]) =
    let excludeMarker = "--exclude-from-log="

    let toExclude =
      args
      |> Array.filter (fun arg -> arg.StartsWith excludeMarker)
      |> Array.collect (fun arg -> arg.Substring(excludeMarker.Length).Split(','))

    let args = args |> Array.filter (fun arg -> not <| arg.StartsWith excludeMarker)

    toExclude, args

  let logLevel, args = parseLogLevel args
  let switch = LoggingLevelSwitch(expectoToSerilogLevel logLevel)
  let logSourcesToExclude, args = parseLogExcludes args

  let sourcesToExclude =
    Matching.WithProperty<string>(
      Constants.SourceContextPropertyName,
      fun s -> s <> null && logSourcesToExclude |> Array.contains s
    )

  let argsToRemove, loaders =
    args
    |> Array.windowed 2
    |> Array.tryPick (function
      | [| "--loader"; "ionide" |] as args ->
        let (name, factory) = loaders |> Seq.find(fun (k,v) -> k = "Ionide WorkspaceLoader")
        Some(args, [ name, factory ])
      | [| "--loader"; "graph" |] as args ->
        let (name, factory) = loaders |> Seq.find(fun (k,v) -> k = "MSBuild Project Graph WorkspaceLoader")
        Some(args, [ name, factory ])
      | _ -> None)
    |> Option.defaultValue ([||], loaders)

  let serilogLogger =
    LoggerConfiguration()
      .Enrich.FromLogContext()
      .MinimumLevel.ControlledBy(switch)
      .Filter.ByExcluding(Matching.FromSource("FileSystem"))
      .Filter.ByExcluding(sourcesToExclude)

      .Destructure
      .FSharpTypes()
      .Destructure
      .ByTransforming<FSharp.Compiler.Text.Range>(fun r ->
        box
          {| FileName = r.FileName
             Start = r.Start
             End = r.End |})
      .Destructure.ByTransforming<FSharp.Compiler.Text.Position>(fun r -> box {| Line = r.Line; Column = r.Column |})
      .Destructure.ByTransforming<Newtonsoft.Json.Linq.JToken>(fun tok -> tok.ToString() |> box)
      .Destructure.ByTransforming<System.IO.DirectoryInfo>(fun di -> box di.FullName)
      .WriteTo
      .Async(fun c ->
        c.Console(
          outputTemplate = outputTemplate,
          standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose),
          theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code
        )
        |> ignore)
      .CreateLogger() // make it so that every console log is logged to stderr

  // uncomment these next two lines if you want verbose output from the LSP server _during_ your tests
  Serilog.Log.Logger <- serilogLogger
  LogProvider.setLoggerProvider (Providers.SerilogProvider.create ())

  let fixedUpArgs = args |> Array.except argsToRemove

  let cts = new CancellationTokenSource(testTimeout)

  let config =
    { defaultConfig with
        // failOnFocusedTests = true
        printer = Expecto.Impl.TestPrinters.summaryWithLocationPrinter defaultConfig.printer
        verbosity = logLevel
        // runInParallel = false
         }

  runTestsWithArgsAndCancel cts.Token config fixedUpArgs tests
