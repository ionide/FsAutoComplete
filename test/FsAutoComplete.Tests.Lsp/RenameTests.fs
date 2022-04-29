module FsAutoComplete.Tests.Rename

open System.IO
open Helpers
open Expecto.Tests
open Ionide.LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open Expecto
open FsAutoComplete.Utils

let tests state =
  let server =
    async {
      let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest")
      let! (server, event) = serverInitialize testDir defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event

      return (server, testDir, event)
    }

  ptestList
    "Rename Tests"
    [ testCaseAsync
        "Rename from usage within project file"
        (async {
          let! server, testDir, events = server
          let path = Path.Combine(testDir, "Program.fs")
          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
          do! server.TextDocumentDidOpen tdop

          do!
            waitForParseResultsForFile "Test.fs" events
            |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

          let p: RenameParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 7; Character = 12 }
              NewName = "y" }

          let! res = server.TextDocumentRename p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            match res.DocumentChanges with
            | None -> failtest "No changes"
            | Some result ->
              Expect.equal result.Length 2 "Rename has all changes"

              Expect.exists
                result
                (fun n ->
                  n.TextDocument.Uri.Contains "Program.fs"
                  && n.Edits
                     |> Seq.exists (fun r ->
                       r.Range = { Start = { Line = 7; Character = 12 }
                                   End = { Line = 7; Character = 13 } }))
                "Rename contains changes in Program.fs"

              Expect.exists
                result
                (fun n ->
                  n.TextDocument.Uri.Contains "Test.fs"
                  && n.Edits
                     |> Seq.exists (fun r ->
                       r.Range = { Start = { Line = 2; Character = 4 }
                                   End = { Line = 2; Character = 5 } }))
                "Rename contains changes in Test.fs"

              ()
        })

      testCaseAsync
        "Rename from definition across project files"
        (async {
          let! server, testDir, events = server
          let path = Path.Combine(testDir, "Program.fs")
          let pathTest = Path.Combine(testDir, "Test.fs")
          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument pathTest }
          do! server.TextDocumentDidOpen tdop
          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
          do! server.TextDocumentDidOpen tdop

          do!
            waitForParseResultsForFile "Test.fs" events
            |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

          do!
            waitForParseResultsForFile "Program.fs" events
            |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

          let p: RenameParams =
            { TextDocument = { Uri = Path.FilePathToUri pathTest }
              Position = { Line = 2; Character = 4 }
              NewName = "y" }

          let! res = server.TextDocumentRename p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            match res.DocumentChanges with
            | None -> failtest "No changes"
            | Some result ->
              // TODO
              Expect.equal result.Length 2 "Rename has all changes"

              Expect.exists
                result
                (fun n ->
                  n.TextDocument.Uri.Contains "Program.fs"
                  && n.Edits
                     |> Seq.exists (fun r ->
                       r.Range = { Start = { Line = 7; Character = 12 }
                                   End = { Line = 7; Character = 13 } }))
                "Rename contains changes in Program.fs"

              Expect.exists
                result
                (fun n ->
                  n.TextDocument.Uri.Contains "Test.fs"
                  && n.Edits
                     |> Seq.exists (fun r ->
                       r.Range = { Start = { Line = 2; Character = 4 }
                                   End = { Line = 2; Character = 5 } }))
                "Rename contains changes in Test.fs"

              ()
        })

      testCaseAsync
        "Rename from definition within script file"
        (async {
          let! server, testDir, events = server
          let path = Path.Combine(testDir, "Script.fsx")
          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
          do! server.TextDocumentDidOpen tdop

          do!
            waitForParseResultsForFile "Script.fsx" events
            |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

          let newName = "afterwards"
          let sourceFile: TextDocumentIdentifier = { Uri = Path.FilePathToUri path }

          let p: RenameParams =
            { TextDocument = sourceFile
              Position = { Line = 0; Character = 4 } // beginning of the 'initial' identifier
              NewName = newName }

          let! res = server.TextDocumentRename p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some { DocumentChanges = Some edits }) ->
            Expect.hasLength edits 1 "should have just one file worth of edits"
            let edit = edits.[0]

            Expect.equal edit.TextDocument.Uri sourceFile.Uri "should be for this file"
            let edits = edit.Edits

            let expected: TextEdit [] =
              [| { NewText = newName
                   Range =
                     { Start = { Line = 0; Character = 4 }
                       End = { Line = 0; Character = 11 } } }
                 { NewText = newName
                   Range =
                     { Start = { Line = 2; Character = 0 }
                       End = { Line = 2; Character = 7 } } } |]

            Expect.equal edits expected "Should change the two usages"
          | Result.Ok edits -> failtestf "got some enexpected edits: %A" edits
        })

      testCaseAsync
        "Rename from usage within script file"
        (async {
          let! server, testDir, events = server
          let path = Path.Combine(testDir, "Script.fsx")
          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
          do! server.TextDocumentDidOpen tdop

          do!
            waitForParseResultsForFile "Script.fsx" events
            |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

          let newName = "afterwards"
          let sourceFile: TextDocumentIdentifier = { Uri = Path.FilePathToUri path }

          let p: RenameParams =
            { TextDocument = sourceFile
              Position = { Line = 2; Character = 0 } // beginning of the 'initial' identifier usage
              NewName = newName }

          let! res = server.TextDocumentRename p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some { DocumentChanges = Some edits }) ->
            Expect.hasLength edits 1 "should have just one file worth of edits"
            let edit = edits.[0]

            Expect.equal edit.TextDocument.Uri sourceFile.Uri "should be for this file"
            let edits = edit.Edits

            let expected: TextEdit [] =
              [| { NewText = newName
                   Range =
                     { Start = { Line = 0; Character = 4 }
                       End = { Line = 0; Character = 11 } } }
                 { NewText = newName
                   Range =
                     { Start = { Line = 2; Character = 0 }
                       End = { Line = 2; Character = 7 } } } |]

            Expect.equal edits expected "Should change the two usages"
          | Result.Ok edits -> failtestf "got some enexpected edits: %A" edits
        }) ]
