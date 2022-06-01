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
    let server =
      async {
        let testDir =
          Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest", "SameScript")

        let! (server, event) = serverInitialize testDir defaultConfigDto state
        do! waitForWorkspaceFinishedParsing event

        return (server, testDir, event)
      }

    testSequenced
    <| testList
         "Within same script file"
         [ testCaseAsync
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
               let sourceFile: TextDocumentIdentifier = { Uri = normalizePathCasing path }

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

                 let expected: TextEdit[] =
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
               let sourceFile: TextDocumentIdentifier = { Uri = normalizePathCasing path }

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

                 let expected: TextEdit[] =
                   [| { NewText = newName
                        Range =
                          { Start = { Line = 0; Character = 4 }
                            End = { Line = 0; Character = 11 } } }
                      { NewText = newName
                        Range =
                          { Start = { Line = 2; Character = 0 }
                            End = { Line = 2; Character = 7 } } } |]

                 Expect.equal edits expected "Should change the two usages"
               | Result.Ok edits -> failtestf "got some unexpected edits: %A" edits
             }) ]

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
