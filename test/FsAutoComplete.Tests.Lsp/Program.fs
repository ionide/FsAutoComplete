module LspTest

open Expecto
open Serilog
open FsAutoComplete.Logging
open System
open Serilog.Core
open Serilog.Events
open FsAutoComplete.Tests.CoreTest
open FsAutoComplete.Tests.ScriptTest
open FsAutoComplete.Tests.ExtensionsTests
open FsAutoComplete.Tests.InteractiveDirectivesTests
open Ionide.ProjInfo

let loaders = [
  "Ionide WorkspaceLoader",  WorkspaceLoader.Create
  "MSBuild Project Graph WorkspaceLoader", WorkspaceLoaderViaProjectGraph.Create
]

///Global list of tests
let tests toolsPath =
  testSequenced <| testList "lsp" [
    for (name, workspaceLoaderFactory) in loaders do
      testSequenced <| testList name [
        // initTests
        basicTests toolsPath workspaceLoaderFactory
        codeLensTest toolsPath workspaceLoaderFactory
        documentSymbolTest toolsPath workspaceLoaderFactory
        autocompleteTest toolsPath workspaceLoaderFactory
        renameTest toolsPath workspaceLoaderFactory
        gotoTest toolsPath workspaceLoaderFactory
        foldingTests toolsPath workspaceLoaderFactory
        tooltipTests toolsPath workspaceLoaderFactory
        highlightingTests toolsPath workspaceLoaderFactory
        signatureHelpTests toolsPath workspaceLoaderFactory

        scriptPreviewTests toolsPath workspaceLoaderFactory
        scriptEvictionTests toolsPath workspaceLoaderFactory
        scriptProjectOptionsCacheTests toolsPath workspaceLoaderFactory
        dependencyManagerTests  toolsPath workspaceLoaderFactory
        scriptGotoTests toolsPath workspaceLoaderFactory
        interactiveDirectivesUnitTests

        // commented out because FSDN is down
        //fsdnTest toolsPath workspaceLoaderFactory
        uriTests
        // linterTests toolsPath
        formattingTests toolsPath workspaceLoaderFactory
        //fakeInteropTests toolsPath
        analyzerTests toolsPath workspaceLoaderFactory
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
  let toolsPath = Ionide.ProjInfo.Init.init ()

  runTestsWithArgs defaultConfig args (tests toolsPath)
