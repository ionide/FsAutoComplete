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

  let documentationForSymbol xmlSig assembly fileName line character =
    async {
      let! (srv, _) = server

      return!
        srv.FSharpDocumentationSymbol
          { XmlSig = xmlSig
            Assembly = assembly
            FileName = fileName
            Line = line
            Character = character }
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

            Expect.stringContains
              text
              "command:fsharp.showDocumentation?"
              "hover should contain a documentation command link"

            Expect.stringContains
              text
              "[`Async`](command:fsharp.showDocumentation?"
              "hover should render the resolved Async display name as an inline-code markdown link"

            Expect.stringContains
              text
              "%22FileName%22%3A"
              "hover should include source context in documentation command links"

            Expect.isFalse
              (text.Contains "<code>")
              "hover should not include literal code tags in the rendered cref link"
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
              "[`Async`](command:fsharp.showDocumentation?"
              "documentation endpoint should render the resolved Async display name as an inline-code markdown link"

            Expect.stringContains
              model.Comment
              "%22FileName%22%3A"
              "documentation endpoint should include source context in documentation command links"

            Expect.isFalse
              (model.Comment.Contains "<code>")
              "documentation endpoint should not include literal code tags in the rendered cref link"
          | Ok None -> failtest "Documentation request returned None; expected documentation for x"
          | Ok _ -> failtest "Could not deserialize the documentation response"
          | Error e -> failtestf "Documentation request errored: %A" e
        })

      testCaseAsync
        "documentationSymbol returns Async documentation after priming request"
        (async {
          let! _ = documentationAt 5u 0u

          match! documentationForSymbol "T:Microsoft.FSharp.Control.FSharpAsync`1" "FSharp.Core" None None None with
          | Ok(Some(As(model: FsAutoComplete.CommandResponse.DocumentationDescription))) ->
            Expect.stringContains model.Signature "Async" "documentationSymbol should render Async docs"
            Expect.isFalse (model.Signature.Contains "val x") "documentationSymbol should not render the local symbol"
          | Ok None -> failtest "documentationSymbol returned None; expected documentation for Async"
          | Ok _ -> failtest "Could not deserialize the documentationSymbol response"
          | Error e -> failtestf "documentationSymbol request errored: %A" e
        })

      testCaseAsync
        "documentationSymbol returns Async documentation from source context without priming request"
        (async {
          let! (_, scriptPath) = server

          match!
            documentationForSymbol
              "T:Microsoft.FSharp.Control.FSharpAsync`1"
              "FSharp.Core"
              (Some scriptPath)
              (Some 5)
              (Some 0)
          with
          | Ok(Some(As(model: FsAutoComplete.CommandResponse.DocumentationDescription))) ->
            Expect.stringContains model.Signature "Async" "documentationSymbol should render Async docs"
            Expect.isFalse (model.Signature.Contains "val x") "documentationSymbol should not render the local symbol"

            Expect.stringContains
              model.Comment
              "%22FileName%22%3A"
              "documentationSymbol should preserve source context in returned command links"

            let attributeLinks =
              model.Attributes
              |> Array.filter (fun attributeLine -> attributeLine.Contains "command:fsharp.showDocumentation?")

            Expect.isGreaterThan
              attributeLinks.Length
              0
              "documentationSymbol should include documentation command links in attributes for Async"

            attributeLinks
            |> Array.iter (fun attributeLine ->
              Expect.stringContains
                attributeLine
                "%22FileName%22%3A"
                "documentationSymbol attribute links should preserve source context")
          | Ok None -> failtest "documentationSymbol returned None; expected documentation for Async"
          | Ok _ -> failtest "Could not deserialize the documentationSymbol response"
          | Error e -> failtestf "documentationSymbol request errored: %A" e
        }) ]
