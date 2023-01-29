module FsAutoComplete.Tests.XmlDocumentationGeneration

open Expecto
open Expecto.Tests
open FsAutoComplete.Utils
open FsToolkit.ErrorHandling
open Helpers
open Ionide.LanguageServerProtocol.Types
open System.IO
open Utils.Utils

open type System.Environment

let tests state =
  let testPath = Path.Combine(File.CurrentDir(), "TestCases", "XmlDocGen")
  let scriptPath = Path.Combine(testPath, "Script.fsx")

  let server =
    async {
      let! (server, serverRequests) = serverInitialize testPath defaultConfigDto state
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }

      do! server.TextDocumentDidOpen tdop

      match! waitForParseResultsForFile "Script.fsx" serverRequests with
      | Ok () -> return server, scriptPath, serverRequests
      | Error errors ->
        let errorStrings =
          errors
          |> Array.map (fun e -> e.DebuggerDisplay)
          |> String.concat "\n\t* "

        return failtestf "Errors while parsing xml doc generation script:\n\t* %s" errorStrings
    }
    |> Async.Cache

  testList
    "xml doc comments"
    [ testCaseAsync
        "generate xml docs for function"
        (async {
          let! server, filePath, serverRequests = server
          let fileUri = Path.FilePathToUri filePath

          let! edits =
            waitForEditsForFile filePath serverRequests
            |> Async.StartChild

          let! result =
            server.FSharpDocumentationGenerator(
              { TextDocument = { Uri = fileUri; Version = Some 1 }
                // the start of the 'add' symbol name
                Position = { Line = 0; Character = 5 } }
            )

          match result with
          | Error e -> failtestf "Couldn't generate xml docs: %A" e
          | Ok () -> ()

          let! edits = edits

          let expectedXml =
            $"""/// <summary></summary>{NewLine}/// <param name="left"></param>{NewLine}/// <param name="right"></param>{NewLine}/// <returns></returns>{NewLine}"""

          let expectedEdits: TextEdit [] =
            [| { Range =
                   { Start = { Line = 0; Character = 0 }
                     End = { Line = 0; Character = 0 } }
                 NewText = expectedXml } |]

          Expect.equal edits expectedEdits "Should have generated the xml docs at the start position"
        }) ]
