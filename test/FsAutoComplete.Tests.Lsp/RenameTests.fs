module FsAutoComplete.Tests.Rename

open System.IO
open Helpers
open Expecto.Tests
open Ionide.LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open Expecto
open FsAutoComplete.Utils
open Utils.ServerTests
open Utils.Server
open Utils.Utils
open Utils.TextEdit
open Helpers.Expecto.ShadowedTimeouts

let private normalizePathCasing =
  Path.FilePathToUri
  >> Path.FileUriToLocalPath
  >> Path.FilePathToUri

let tests state =
  let sameProjectTests =
    let testDir =
      Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest", "SameProject")

    serverTestList "Within Same Project" state defaultConfigDto (Some testDir) (fun server ->
      [ testCaseAsync
          "Rename from usage within same project but different file"
          (async {
            let! (doc, diags) = server |> Server.openDocument "Program.fs"
            Expect.isEmpty diags "There should be no errors in the checked file"

            let p: RenameParams =
              { TextDocument = doc.TextDocumentIdentifier
                Position = { Line = 7; Character = 12 }
                NewName = "y" }

            let! server = server
            let! res = server.Server.TextDocumentRename p

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
          "Rename from definition within the same project with usages across different files"
          (async {

            let! (programDoc, programDiags) = server |> Server.openDocument "Program.fs"
            let! (testDoc, testDiags) = server |> Server.openDocument "Test.fs"

            Expect.isEmpty programDiags "There should be no errors in Program.fs"
            Expect.isEmpty testDiags "There should be no errors in Test.fs"

            let p: RenameParams =
              { TextDocument = testDoc.TextDocumentIdentifier
                Position = { Line = 2; Character = 4 }
                NewName = "y" }

            let! server = server
            let! res = server.Server.TextDocumentRename p

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

        ])

  let sameScriptTests =
    serverTestList "Within same script file" state defaultConfigDto None (fun server -> [
      let checkRename textWithCursor newName expectedText = async {
        let (cursor, text) =
          textWithCursor
          |> Text.trimTripleQuotation
          |> Cursor.assertExtractPosition

        let! (doc, diags) = server |> Server.createUntitledDocument text
        use doc = doc
        Expect.isEmpty diags "There should be no diags"

        let p: RenameParams =
          {
            TextDocument = doc.TextDocumentIdentifier
            Position = cursor
            NewName = newName
          }
        let! res = doc.Server.Server.TextDocumentRename p
        let edits =
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some { DocumentChanges = Some edits }) -> edits
          | Result.Ok edits -> failtestf "got some unexpected edits: %A" edits

        Expect.hasLength edits 1 "should have just one file worth of edits"
        let edit = edits.[0]
        Expect.equal edit.TextDocument.Uri doc.Uri "should be for this file"
        let edits = edit.Edits |> List.ofArray |> TextEdits.sortByRange
        let actual =
          text
          |> TextEdits.applyWithErrorCheck edits
          |> Flip.Expect.wantOk "TextEdits should be valid"

        let expected = expectedText |> Text.trimTripleQuotation

        Expect.equal actual expected "Text after TextEdits should be correct"
      }
      testCaseAsync "Rename from definition within script file" <|
        checkRename
          """
          let $0initial () = printfn "hi"

          initial ()
          """
          "afterwards"
          """
          let afterwards () = printfn "hi"

          afterwards ()
          """
      testCaseAsync "Rename from usage within script file" <|
        checkRename
          """
          let initial () = printfn "hi"

          $0initial ()
          """
          "afterwards"
          """
          let afterwards () = printfn "hi"

          afterwards ()
          """
      testCaseAsync "can add backticks to new name with space" <|
        checkRename
          """
          let initial () = printfn "hi"

          $0initial ()
          """
          "hello world"
          """
          let ``hello world`` () = printfn "hi"

          ``hello world`` ()
          """
      testCaseAsync "doesn't add additional backticks to new name with backticks" <|
        checkRename
          """
          let initial () = printfn "hi"

          $0initial ()
          """
          "``hello world``"
          """
          let ``hello world`` () = printfn "hi"

          ``hello world`` ()
          """
    ])

  let crossProjectTests =
    let server =
      async {
        let testDir =
          Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest", "CrossProject")

        let! (server, event) = serverInitialize testDir defaultConfigDto state
        do! waitForWorkspaceFinishedParsing event

        return (server, testDir, event)
      }

    testSequenced
    <| testList
         "Across projects"
         [ testCaseAsync
             "Rename from usage across projects"
             (async {
               let! (server, rootDir, events) = server
               let declarationFile = Path.Combine(rootDir, "LibA", "Library.fs")
               let usageFile = Path.Combine(rootDir, "LibB", "Library.fs")

               // open and parse the usage file
               let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument usageFile }
               do! server.TextDocumentDidOpen tdop

               do!
                 waitForParseResultsForFile (Path.GetFileName usageFile) events
                 |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

               // now, request renames
               let renameHelloUsageInUsageFile: RenameParams =
                 { TextDocument = { Uri = normalizePathCasing usageFile }
                   Position = { Line = 6; Character = 28 }
                   NewName = "sup" }

               let! res = server.TextDocumentRename(renameHelloUsageInUsageFile)

               match res with
               | Result.Error e -> failtest $"Expected to get renames, but got error: {e.Message}"
               | Result.Ok None -> failtest $"Expected to get renames, but got none"
               | Result.Ok (Some { DocumentChanges = Some edits }) ->
                 Expect.equal edits.Length 2 "Rename has the correct expected edits"

                 Expect.exists
                   edits
                   (fun n ->
                     n.TextDocument.Uri.Contains "LibA"
                     && n.TextDocument.Uri.Contains "Library.fs"
                     && n.Edits
                        |> Seq.exists (fun r ->
                          r.Range = { Start = { Line = 3; Character = 8 }
                                      End = { Line = 3; Character = 13 } }
                          && r.NewText = "sup"))
                   "Rename contains changes in LibA"

                 Expect.exists
                   edits
                   (fun n ->
                     n.TextDocument.Uri.Contains "LibB"
                     && n.TextDocument.Uri.Contains "Library.fs"
                     && n.Edits
                        |> Seq.exists (fun r ->
                          r.Range = { Start = { Line = 6; Character = 28 }
                                      End = { Line = 6; Character = 33 } }
                          && r.NewText = "sup"))
                   "Rename contains changes in LibB"
               | Result.Ok edits -> failtestf "got some unexpected edits: %A" edits
             })
           testCaseAsync
             "Rename where there are fully-qualified usages"
             (async {
               let! (server, rootDir, events) = server
               let declarationFile = Path.Combine(rootDir, "LibA", "Library.fs")
               let usageFile = Path.Combine(rootDir, "LibB", "Library.fs")

               // open and parse the usage file
               let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument usageFile }
               do! server.TextDocumentDidOpen tdop

               do!
                 waitForParseResultsForFile (Path.GetFileName usageFile) events
                 |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

               // now, request renames
               let renameHelloUsageInUsageFile: RenameParams =
                 { TextDocument = { Uri = normalizePathCasing usageFile }
                   Position = { Line = 9; Character = 37 } // in the 'yell' part of 'A.Say.yell'
                   NewName = "sup" }

               let! res = server.TextDocumentRename(renameHelloUsageInUsageFile)

               match res with
               | Result.Error e -> failtest $"Expected to get renames, but got error: {e.Message}"
               | Result.Ok None -> failtest $"Expected to get renames, but got none"
               | Result.Ok (Some { DocumentChanges = Some edits }) ->
                 Expect.equal edits.Length 2 "Rename has the correct expected edits"

                 Expect.exists
                   edits
                   (fun n ->
                     n.TextDocument.Uri.Contains "LibA"
                     && n.TextDocument.Uri.Contains "Library.fs"
                     && n.Edits
                        |> Seq.exists (fun r ->
                          r.Range = { Start = { Line = 6; Character = 8 }
                                      End = { Line = 6; Character = 12 } }
                          && r.NewText = "sup"))
                   $"Rename contains changes in LibA in the list %A{edits}"

                 Expect.exists
                   edits
                   (fun n ->
                     n.TextDocument.Uri.Contains "LibB"
                     && n.TextDocument.Uri.Contains "Library.fs"
                     && n.Edits
                        |> Seq.exists (fun r ->
                          r.Range = { Start = { Line = 9; Character = 37 }
                                      End = { Line = 9; Character = 41 } }
                          && r.NewText = "sup"))
                   $"Rename contains changes in LibB in the list %A{edits}"
               | Result.Ok edits -> failtestf "got some unexpected edits: %A" edits
             }) ]

  testList
    "Rename Tests"
    [ sameProjectTests
      sameScriptTests
      crossProjectTests ]
