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
  Path.FilePathToUri >> Path.FileUriToLocalPath >> Path.FilePathToUri

let renameRaw ident pos newName =
  { TextDocument = ident
    Position = pos
    NewName = newName
    WorkDoneToken = None }

let rename (doc: Document) = renameRaw doc.TextDocumentIdentifier

let pos l c = { Line = l; Character = c }

module Expect =
  [<return: Struct>]
  let private (|ForFile|_|) (fileName: string) (edit: TextDocumentEdit) =
    if edit.TextDocument.Uri.Contains fileName then
      ValueSome()
    else
      ValueNone

  [<return: Struct>]
  let private (|HasEditIn|_|) (range: Ionide.LanguageServerProtocol.Types.Range) (edit: TextDocumentEdit) =
    if
      edit.Edits
      |> Array.exists (fun r ->
        match r with
        | U2.C1 e -> e.Range = range
        | U2.C2 e -> e.Range = range)
    then
      ValueSome()
    else
      ValueNone

  let hasEditAtRange (edits: U4<TextDocumentEdit, CreateFile, RenameFile, DeleteFile> array) fileName range =
    Expect.exists
      edits
      (fun n ->
        match n with
        | U4.C1(ForFile fileName & HasEditIn range) -> true
        | _ -> false)
      $"Rename contains changes in %s{fileName}"

let private sameProjectTests state =
  let testDir =
    Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest", "SameProject")

  serverTestList "Within Same Project" state defaultConfigDto (Some testDir) (fun server ->
    [ testCaseAsync
        "Rename from usage within same project but different file"
        (async {
          let! (doc, diags) = server |> Server.openDocument "Program.fs"
          Expect.isEmpty diags "There should be no errors in the checked file"

          let p: RenameParams = rename doc (pos 7u 12u) "y"

          let! server = server
          let! res = server.Server.TextDocumentRename p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res.DocumentChanges with
            | None -> failtest "No changes"
            | Some result ->
              Expect.equal result.Length 2 "Rename has all changes"

              Expect.hasEditAtRange
                result
                "Program.fs"
                { Start = { Line = 7u; Character = 12u }
                  End = { Line = 7u; Character = 13u } }

              Expect.hasEditAtRange
                result
                "Test.fs"
                { Start = { Line = 2u; Character = 4u }
                  End = { Line = 20u; Character = 5u } }
        })

      testCaseAsync
        "Rename from definition within the same project with usages across different files"
        (async {

          let! (_programDoc, programDiags) = server |> Server.openDocument "Program.fs"
          let! (testDoc, testDiags) = server |> Server.openDocument "Test.fs"

          Expect.isEmpty programDiags "There should be no errors in Program.fs"
          Expect.isEmpty testDiags "There should be no errors in Test.fs"

          let p: RenameParams = rename testDoc (pos 2u 4u) "y"

          let! server = server
          let! res = server.Server.TextDocumentRename p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res.DocumentChanges with
            | None -> failtest "No changes"
            | Some result ->
              // TODO
              Expect.equal result.Length 2 "Rename has all changes"

              Expect.hasEditAtRange
                result
                "Program.fs"
                { Start = { Line = 7u; Character = 12u }
                  End = { Line = 7u; Character = 13u } }

              Expect.hasEditAtRange
                result
                "Test.fs"
                { Start = { Line = 2u; Character = 4u }
                  End = { Line = 2u; Character = 5u } }
        })

      ])

let private sameScriptTests state =
  serverTestList "Within same script file" state defaultConfigDto None (fun server ->
    [ /// `expectedText = None` -> Rename not valid at location
      let checkRename' textWithCursor newName (expectedText: string option) =
        async {
          let (cursor, text) =
            textWithCursor |> Text.trimTripleQuotation |> Cursor.assertExtractPosition

          let! (doc, diags) = server |> Server.createUntitledDocument text
          use doc = doc
          Expect.isEmpty diags "There should be no diags"

          let p: RenameParams = rename doc cursor newName

          let! res = doc.Server.Server.TextDocumentRename p

          match expectedText with
          | None ->
            // Note: `Error` instead of `Ok None` -> error message
            Expect.isError res "Rename should not be valid!"
          | Some expectedText ->
            let edits =
              match res with
              | Result.Error e -> failtestf "Request failed: %A" e
              | Result.Ok None -> failtest "Request none"
              | Result.Ok(Some { DocumentChanges = Some edits }) -> edits
              | Result.Ok edits -> failtestf "got some unexpected edits: %A" edits

            Expect.hasLength edits 1 "should have just one file worth of edits"

            match edits.[0] with
            | U4.C1 edit ->
              Expect.equal edit.TextDocument.Uri doc.Uri "should be for this file"

              let edits =
                edit.Edits
                |> Array.choose (function
                  | U2.C1 x -> Some x
                  | _ -> None)
                |> TextEdits.sortByRange

              let actual =
                text
                |> TextEdits.applyWithErrorCheck edits
                |> Flip.Expect.wantOk "TextEdits should be valid"

              let expected = expectedText |> Text.trimTripleQuotation

              Expect.equal actual expected "Text after TextEdits should be correct"
            | _ -> failtest "should be a text document edit"
        }

      let checkRename textWithCursor newName expectedText = checkRename' textWithCursor newName (Some expectedText)
      let checkRenameNotValid textWithCursor newName = checkRename' textWithCursor newName None

      testCaseAsync "Rename from definition within script file"
      <| checkRename
        """
        let $0initial () = printfn "hi"

        initial ()
        """
        "afterwards"
        """
        let afterwards () = printfn "hi"

        afterwards ()
        """

      testCaseAsync "Rename from usage within script file"
      <| checkRename
        """
        let initial () = printfn "hi"

        $0initial ()
        """
        "afterwards"
        """
        let afterwards () = printfn "hi"

        afterwards ()
        """

      testCaseAsync "can add backticks to new name with space"
      <| checkRename
        """
        let initial () = printfn "hi"

        $0initial ()
        """
        "hello world"
        """
        let ``hello world`` () = printfn "hi"

        ``hello world`` ()
        """

      testCaseAsync "doesn't add additional backticks to new name with backticks"
      <| checkRename
        """
        let initial () = printfn "hi"

        $0initial ()
        """
        "``hello world``"
        """
        let ``hello world`` () = printfn "hi"

        ``hello world`` ()
        """

      testCaseAsync "can rename operator to valid operator name"
      <| checkRename
        """
        let (+$0++) a b = a + b
        """
        "---"
        """
        let (---) a b = a + b
        """

      testCaseAsync "cannot rename operator to invalid operator name"
      <| checkRenameNotValid
        """
        let (+$0++) a b = a + b
        """
        "foo"

      testCaseAsync "removes backticks for new name without backticks"
      <| checkRename
        """
        let ``my $0value`` = 42

        let _ = ``my value`` + 42
        """
        "value"
        """
        let value = 42

        let _ = value + 42
        """ ])

let private crossProjectTests state =
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
          let _declarationFile = Path.Combine(rootDir, "LibA", "Library.fs")
          let usageFile = Path.Combine(rootDir, "LibB", "Library.fs")

          // open and parse the usage file
          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument usageFile }
          do! server.TextDocumentDidOpen tdop

          do!
            waitForParseResultsForFile (Path.GetFileName usageFile) events
            |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

          // now, request renames
          let renameHelloUsageInUsageFile: RenameParams =
            renameRaw { Uri = normalizePathCasing usageFile } (pos 6u 28u) "sup"

          let! res = server.TextDocumentRename(renameHelloUsageInUsageFile)

          match res with
          | Result.Error e -> failtest $"Expected to get renames, but got error: {e.Message}"
          | Result.Ok None -> failtest $"Expected to get renames, but got none"
          | Result.Ok(Some { DocumentChanges = Some edits }) ->
            Expect.equal edits.Length 2 "Rename has the correct expected edits"

            Expect.hasEditAtRange
              edits
              "LibA/Library.fs"
              { Start = { Line = 3u; Character = 8u }
                End = { Line = 3u; Character = 13u } }

            Expect.hasEditAtRange
              edits
              "LibB/Library.fs"
              { Start = { Line = 6u; Character = 28u }
                End = { Line = 6u; Character = 33u } }

          | Result.Ok edits -> failtestf "got some unexpected edits: %A" edits
        })
      testCaseAsync
        "Rename where there are fully-qualified usages"
        (async {
          let! (server, rootDir, events) = server
          let _declarationFile = Path.Combine(rootDir, "LibA", "Library.fs")
          let usageFile = Path.Combine(rootDir, "LibB", "Library.fs")

          // open and parse the usage file
          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument usageFile }
          do! server.TextDocumentDidOpen tdop

          do!
            waitForParseResultsForFile (Path.GetFileName usageFile) events
            |> AsyncResult.foldResult id (fun e -> failtestf "%A" e)

          // now, request renames
          let renameHelloUsageInUsageFile: RenameParams =
            renameRaw { Uri = normalizePathCasing usageFile } (pos 9u 37u) "sup"

          let! res = server.TextDocumentRename(renameHelloUsageInUsageFile)

          match res with
          | Result.Error e -> failtest $"Expected to get renames, but got error: {e.Message}"
          | Result.Ok None -> failtest $"Expected to get renames, but got none"
          | Result.Ok(Some { DocumentChanges = Some edits }) ->
            Expect.equal edits.Length 2 "Rename has the correct expected edits"

            Expect.hasEditAtRange
              edits
              "LibA/Library.fs"
              { Start = { Line = 6u; Character = 8u }
                End = { Line = 6u; Character = 12u } }

            Expect.hasEditAtRange
              edits
              "LibB/Library.fs"
              { Start = { Line = 9u; Character = 37u }
                End = { Line = 9u; Character = 41u } }
          | Result.Ok edits -> failtestf "got some unexpected edits: %A" edits
        }) ]

let private prepareRenameTests state =
  serverTestList "Prepare Rename tests" state defaultConfigDto None (fun server ->
    [ let check shouldBeAbleToRename sourceWithCursor =
        async {
          let (cursor, text) =
            sourceWithCursor |> Text.trimTripleQuotation |> Cursor.assertExtractPosition

          let! (doc, _diags) = server |> Server.createUntitledDocument text

          let p: PrepareRenameParams =
            { TextDocument = doc.TextDocumentIdentifier
              Position = cursor
              WorkDoneToken = None }

          let! res = doc.Server.Server.TextDocumentPrepareRename p

          if shouldBeAbleToRename then
            res
            |> Flip.Expect.wantOk "Should be able to rename"
            |> Flip.Expect.isSome "Should be able to rename"
          else
            // Note: we always want `Error` instead of `Ok None` because of Error message
            Expect.isError res "Should not be able to rename"
        }

      let checkCanRename = check true
      let checkCannotRename = check false

      testCaseAsync "can rename variable at decl"
      <| checkCanRename
        """
      let val$0ue = 42
      let _ = value + 42
      """

      testCaseAsync "can rename variable at usage"
      <| checkCanRename
        """
      let value = 42
      let _ = val$0ue + 42
      """

      testCaseAsync "can rename unnecessarily backticked variable at decl"
      <| checkCanRename
        """
      let ``val$0ue`` = 42
      let _ = value + 42
      """

      testCaseAsync "can rename unnecessarily backticked variable at usage"
      <| checkCanRename
        """
      let value = 42
      let _ = ``val$0ue`` + 42
      """

      testCaseAsync "can rename variable with required backticks at decl"
      <| checkCanRename
        """
      let ``my v$0alue`` = 42
      let _ = ``my value`` + 42
      """

      testCaseAsync "can rename variable with required backticks at usage"
      <| checkCanRename
        """
      let ``my value`` = 42
      let _ = ``my va$0lue`` + 42
      """

      testCaseAsync "can rename function at decl"
      <| checkCanRename
        """
      let myFun$0ction value = value + 42
      myFunction 42 |> ignore
      """

      testCaseAsync "can rename function at usage"
      <| checkCanRename
        """
      let myFunction value = value + 42
      myFun$0ction 42 |> ignore
      """

      testCaseAsync "can rename function parameter at decl"
      <| checkCanRename
        """
      let myFunction va$0lue = value + 42
      myFunction 42 |> ignore
      """

      testCaseAsync "can rename function parameter at usage"
      <| checkCanRename
        """
      let myFunction va$0lue = value + 42
      myFunction 42 |> ignore
      """

      testCaseAsync "Can rename Type at decl"
      <| checkCanRename
        """
      type MyT$0ype () = class end
      let v: MyType = MyType()
      """

      testCaseAsync "Can rename Type at instantiation usage"
      <| checkCanRename
        """
      type MyType () = class end
      let v: MyType = My$0Type()
      """

      testCaseAsync "Can rename Type at Type usage"
      <| checkCanRename
        """
      type MyType () = class end
      let v: MyT$0ype = MyType()
      """

      testCaseAsync "Can rename Method at decl"
      <| checkCanRename
        """
      type MyType =
        static member DoS$0tuff () = ()
      MyType.DoStuff ()
      """

      testCaseAsync "Can rename Method at usage"
      <| checkCanRename
        """
      type MyType =
        static member DoStuff () = ()
      MyType.DoS$0tuff ()
      """


      testCaseAsync "cannot rename Active Pattern at decl"
      <| checkCannotRename
        """
      let (|Ev$0en|Odd|) v =
        if v % 2 = 0 then Even else Odd
      let _ = (|Even|Odd|) 42
      """

      testCaseAsync "cannot rename Active Pattern at usage"
      <| checkCannotRename
        """
      let (|Even|Odd|) v =
        if v % 2 = 0 then Even else Odd
      let _ = (|Ev$0en|Odd|) 42
      """

      testCaseAsync "cannot rename Active Pattern Case at decl"
      <| checkCannotRename
        """
      let (|Ev$0en|Odd|) v =
        if v % 2 = 0 then Ev$0en else Odd
      match 42 with
      | Even -> ()
      | Odd -> ()
      """

      testCaseAsync "cannot rename Active Pattern Case at usage"
      <| checkCannotRename
        """
      let (|Even|Odd|) v =
        if v % 2 = 0 then Ev$0en else Odd
      match 42 with
      | Ev$0en -> ()
      | Odd -> ()
      """

      testCaseAsync "cannot rename external function"
      <| checkCannotRename
        """
      42 |> igno$0re
      """

      testCaseAsync "cannot rename external method"
      <| checkCannotRename
        """
      String.IsNullOr$0WhiteSpace "foo"
      |> ignore
      """

      testCaseAsync "cannot rename number"
      <| checkCannotRename
        """
      4$02 |> ignore
      """

      testCaseAsync "cannot rename string"
      <| checkCannotRename
        """
      "hel$0lo world" |> ignore
      """

      testCaseAsync "cannot rename keyword"
      <| checkCannotRename
        """
      l$0et foo = 42
      """

      testCaseAsync "cannot rename comment"
      <| checkCannotRename
        """
      /// So$0me value
      let foo = 42
      """

      testCaseAsync "cannot rename space"
      <| checkCannotRename
        """
      let foo = 42
        $0
      let bar = 42
      """ ])

let tests state =
  testList
    "Rename Tests"
    [ sameProjectTests state
      sameScriptTests state
      crossProjectTests state

      prepareRenameTests state ]
