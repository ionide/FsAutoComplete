module LspTest

open Expecto
open Serilog
open FsAutoComplete.Logging
open System
open Serilog.Events
open FsAutoComplete.Tests.CoreTest
open FsAutoComplete.Tests.ScriptTest
open FsAutoComplete.Tests.ExtensionsTests
open FsAutoComplete.Tests.InteractiveDirectivesTests

///Global list of tests
let tests =
   testSequenced <| testList "lsp" [
    // initTests
    basicTests
    codeLensTest
    documentSymbolTest
    autocompleteTest
    renameTest
    gotoTest
    foldingTests
    tooltipTests
    // highlightingTets
    signatureHelpTests

    scriptPreviewTests
    scriptEvictionTests
    scriptProjectOptionsCacheTests
    //dependencyManagerTests //Requires .Net 5 preview
    scriptGotoTests
    interactiveDirectivesUnitTests

    fsdnTest
    uriTests
    linterTests
    formattingTests
    fakeInteropTests
    analyzerTests
  ]


[<EntryPoint>]
let main args =
  let outputTemplate = "[{Timestamp:HH:mm:ss} {Level:u3}] [{SourceContext}] {Message:lj}{NewLine}{Exception}"
  let serilogLogger =
    LoggerConfiguration()
      .Enrich.FromLogContext()
      .MinimumLevel.Information()
      .Destructure.FSharpTypes()
      .WriteTo.Async(
        fun c -> c.Console(outputTemplate = outputTemplate, standardErrorFromLevel = Nullable<_>(LogEventLevel.Verbose), theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code) |> ignore
      ).CreateLogger() // make it so that every console log is logged to stderr
  Serilog.Log.Logger <- serilogLogger
  LogProvider.setLoggerProvider (Providers.SerilogProvider.create())


  runTestsWithArgs defaultConfig args tests
