/// Integration tests for <inheritdoc cref="..."/> resolution in hover tooltips.
/// These tests verify that when a C# library member uses <inheritdoc cref="..."/> to point to
/// another member in the same assembly, FSAC resolves and shows the inherited documentation
/// instead of an empty tooltip.
///
/// Related issue: https://github.com/ionide/FsAutoComplete/issues/1415
/// Fix: https://github.com/ionide/FsAutoComplete/pull/1468
module FsAutoComplete.Tests.InheritDocTooltipTests

open System
open System.IO
open Expecto
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open FsToolkit.ErrorHandling
open FsAutoComplete.Tests.Lsp.Helpers
open Helpers.Expecto.ShadowedTimeouts

/// Extract the plain-text (documentation/description) parts from a Hover response.
/// Within U3.C3 responses, FSAC puts the signature in a U2.C2 code block and the
/// doc comment as a plain U2.C1 string. We collect only the U2.C1 strings so that
/// the assertion checks documentation text rather than the signature.
let private hoverText (hover: Hover) =
  match hover.Contents with
  | U3.C3 parts ->
    parts
    |> Array.choose (function
      | U2.C1 s -> Some s
      | _ -> None)
    |> String.concat "\n"
  | _ -> ""

/// Build the C# library and load the F# test project into a fresh LSP server.
let private inheritDocServer state =
  async {
    let csharpLibPath =
      Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "InheritDocCref", "InheritDocLib")

    let projectPath = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "InheritDocCref")

    // Pre-build the C# library so the XML documentation file is generated alongside the DLL.
    let buildResult = DotnetCli.build csharpLibPath

    if buildResult.ExitCode <> 0 then
      failtest $"C# library build failed:\n{buildResult.StdErr}"

    let! (server, events) = serverInitialize projectPath defaultConfigDto state
    do! waitForWorkspaceFinishedParsing events

    let scriptPath = Path.Combine(projectPath, "Script.fs")
    do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

    match! waitForParseResultsForFile "Script.fs" events with
    | Ok() -> ()
    | Core.Result.Error errors -> failtestf "Parse errors in Script.fs: %A" errors

    return server, scriptPath
  }
  |> Async.Cache

let tests state =
  let server = inheritDocServer state

  let hoverAt line char =
    async {
      let! (srv, scriptPath) = server

      let pos: HoverParams =
        { TextDocument = { Uri = Path.FilePathToUri scriptPath }
          Position = { Line = line; Character = char }
          WorkDoneToken = None }

      return! srv.TextDocumentHover pos
    }

  testSequenced
  <| testList
    "inheritdoc cref hover integration tests"
    [
      // ── Baseline: direct documentation should render normally ──────────────────────
      testCaseAsync
        "direct doc: hover on DocumentedProvider.GetValue shows its summary"
        (async {
          // Line 11, char 32 → `provider.GetValue()`; GetValue is directly documented.
          match! hoverAt 11u 32u with
          | Ok(Some hover) ->
            let text = hoverText hover
            Expect.stringContains text "Gets the documented value" "direct summary should appear in hover"
          | Ok None -> failtest "Hover returned None; expected documentation for GetValue"
          | Result.Error e -> failtestf "Hover request errored: %A" e
        })

      testCaseAsync
        "direct doc: hover on DocumentedProvider.GetNamed shows its summary"
        (async {
          // Line 12, char 31 → `provider.GetNamed(...)`; GetNamed is directly documented.
          match! hoverAt 12u 31u with
          | Ok(Some hover) ->
            let text = hoverText hover
            Expect.stringContains text "Gets a named result" "direct summary should appear in hover"
          | Ok None -> failtest "Hover returned None; expected documentation for GetNamed"
          | Result.Error e -> failtestf "Hover request errored: %A" e
        })

      // ── Core feature: <inheritdoc cref="..."/> resolution ──────────────────────────
      testCaseAsync
        "inheritdoc cref: hover on InheritDocConsumer.GetValue shows inherited summary"
        (async {
          // Line 6, char 35 → `consumer.GetValue()`.
          // InheritDocConsumer.GetValue has only `<inheritdoc cref="DocumentedProvider.GetValue"/>`.
          // Without the fix this would be an empty tooltip; with the fix it shows the inherited docs.
          match! hoverAt 6u 35u with
          | Ok(Some hover) ->
            let text = hoverText hover
            Expect.stringContains text "Gets the documented value" "inherited summary should appear in hover"
          | Ok None ->
            failtest
              "Hover returned None for InheritDocConsumer.GetValue; \
               the <inheritdoc cref=\"...\"/> was not resolved"
          | Result.Error e -> failtestf "Hover request errored: %A" e
        })

      testCaseAsync
        "inheritdoc cref: hover on InheritDocConsumer.GetNamed shows inherited summary"
        (async {
          // Line 7, char 34 → `consumer.GetNamed(...)`.
          // InheritDocConsumer.GetNamed has only `<inheritdoc cref="DocumentedProvider.GetNamed"/>`.
          match! hoverAt 7u 34u with
          | Ok(Some hover) ->
            let text = hoverText hover
            Expect.stringContains text "Gets a named result" "inherited summary should appear in hover"
          | Ok None ->
            failtest
              "Hover returned None for InheritDocConsumer.GetNamed; \
               the <inheritdoc cref=\"...\"/> was not resolved"
          | Result.Error e -> failtestf "Hover request errored: %A" e
        }) ]
