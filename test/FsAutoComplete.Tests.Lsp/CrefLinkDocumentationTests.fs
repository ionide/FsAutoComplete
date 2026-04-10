module FsAutoComplete.Tests.CrefLinkDocumentationTests

open System.IO
open Expecto
open Ionide.LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open Helpers
open Helpers.Expecto.ShadowedTimeouts

let private creflinkServer state =
  async {
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CrefLinks")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let! (server, events) = serverInitialize path defaultConfigDto state
    do! server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

    match! waitForParseResultsForFile "Script.fsx" events with
    | Ok() -> ()
    | Error errors -> failtestf "Errors while parsing script %s: %A" scriptPath errors

    return server, scriptPath
  }
  |> Async.Cache

let private hoverText (hover: Hover) =
  match hover.Contents with
  | U3.C3 parts ->
    parts
    |> Array.choose (function
      | U2.C1 s -> Some s
      | _ -> None)
    |> String.concat "\n"
  | _ -> ""

let tests state =
  let server = creflinkServer state

  let hoverAt line character =
    async {
      let! (srv, scriptPath) = server

      let pos: HoverParams =
        { TextDocument = { Uri = sprintf "file://%s" scriptPath }
          Position = { Line = line; Character = character }
          WorkDoneToken = None }

      return! srv.TextDocumentHover pos
    }

  let documentationAt line character =
    async {
      let! (srv, scriptPath) = server

      return!
        srv.FSharpDocumentation
          { TextDocument = { Uri = sprintf "file://%s" scriptPath }
            Position = { Line = line; Character = character } }
    }

  testSequenced
  <| testList
    "cref link integration tests"
    [ testCaseAsync
        "hover on local symbol renders resolved cref link"
        (async {
          match! hoverAt 5u 0u with
          | Ok(Some hover) ->
            let text = hoverText hover
            Expect.stringContains text "command:fsharp.showDocumentation?" "hover should contain a documentation command link"
            Expect.stringContains text "Async</code>" "hover should render the resolved Async display name"
          | Ok None -> failtest "Hover returned None; expected documentation for x"
          | Error e -> failtestf "Hover request errored: %A" e
        })

      testCaseAsync
        "documentation endpoint on local symbol renders resolved cref link"
        (async {
          match! documentationAt 5u 0u with
          | Ok(Some(As(model: FsAutoComplete.CommandResponse.DocumentationDescription))) ->
            Expect.stringContains
              model.Comment
              "command:fsharp.showDocumentation?"
              "documentation endpoint should contain a documentation command link"

            Expect.stringContains
              model.Comment
              "Async</code>"
              "documentation endpoint should render the resolved Async display name"
          | Ok None -> failtest "Documentation request returned None; expected documentation for x"
          | Ok _ -> failtest "Could not deserialize the documentation response"
          | Error e -> failtestf "Documentation request errored: %A" e
        }) ]
