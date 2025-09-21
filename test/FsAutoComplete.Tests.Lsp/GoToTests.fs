module FsAutoComplete.Tests.GoTo

open System
open System.IO
open Expecto
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open FsAutoComplete
open Utils.ServerTests
open Utils.Server
open Utils.Utils
open Utils.TextEdit
open Helpers.Expecto.ShadowedTimeouts

let executeProcess (wd: string) (processName: string) (processArgs: string) =
  let psi = new Diagnostics.ProcessStartInfo(processName, processArgs)
  psi.UseShellExecute <- false
  psi.RedirectStandardOutput <- true
  psi.RedirectStandardError <- true
  psi.CreateNoWindow <- true
  psi.WorkingDirectory <- wd
  let proc = Diagnostics.Process.Start(psi)
  let output = new Text.StringBuilder()
  let error = new Text.StringBuilder()
  proc.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
  proc.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
  proc.BeginErrorReadLine()
  proc.BeginOutputReadLine()
  proc.WaitForExit()

  {| ExitCode = proc.ExitCode
     StdOut = output.ToString()
     StdErr = error.ToString() |}

///GoTo tests
let private gotoTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

      let csharpPath = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToCSharp")
      let _buildInfo = executeProcess csharpPath "dotnet" "build"

      let! (server, event) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event

      let definitionPath = Path.Combine(path, "Definition.fs")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument definitionPath }
      do! server.TextDocumentDidOpen tdop

      let externalPath = Path.Combine(path, "External.fs")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument externalPath }
      do! server.TextDocumentDidOpen tdop

      let path = Path.Combine(path, "Library.fs")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      do!
        waitForParseResultsForFile "Definition.fs" event
        |> AsyncResult.foldResult id (failtestf "%A")

      do!
        waitForParseResultsForFile "External.fs" event
        |> AsyncResult.foldResult id (failtestf "%A")

      do!
        waitForParseResultsForFile "Library.fs" event
        |> AsyncResult.foldResult id (failtestf "%A")

      return (server, path, externalPath, definitionPath)
    }
    |> Async.Cache

  // sequenced because we don't provide safe concurrent access to file downloads in Sourcelink.fs
  testSequenced
  <| testList
    "GoTo Tests"
    [ testCaseAsync
        "Go-to-definition on external symbol (System.Net.HttpWebRequest)"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 4u; Character = 30u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some(U2.C2 _t)) -> failtest "Should only get one location"
          | Result.Ok(Some(U2.C1(U2.C1 r))) when r.Uri.EndsWith("startup", StringComparison.Ordinal) ->
            failtest "Should not generate the startup dummy file"
          | Result.Ok(Some(U2.C1(U2.C1 r))) ->
            Expect.stringEnds r.Uri ".cs" "should have generated a C# code file"

            Expect.stringContains
              r.Uri
              "System.Net.HttpWebRequest"
              "The generated file should be for the HttpWebRequest type"

            ()
          | Result.Ok(_resultValue) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-definition on external namespace (System.Net) should error when going to a namespace "
        (async {
          let! server, _path, externalPath, _definitionPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 2u; Character = 15u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e ->
            Expect.equal "Could not find declaration" e.Message "Should report failure for navigating to a namespace"
          | Result.Ok r -> failtestf "Declaration request should not work on a namespace, instead we got %A" r
        })

      testCaseAsync
        "Go-to-definition"
        (async {
          let! server, path, _externalPath, _definitionPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 2u; Character = 29u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"

              Expect.equal
                res.Range
                { Start = { Line = 2u; Character = 4u }
                  End = { Line = 2u; Character = 16u } }
                "Result should have correct range"
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-definition-with-line-directive"
        (async {
          let! server, path, _externalPath, _definitionPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 16u; Character = 24u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"

              Expect.equal
                res.Range
                { Start = { Line = 15u; Character = 4u }
                  End = { Line = 15u; Character = 13u } }
                "Result should have correct range (line directives not applied)"
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-definition on custom type binding"
        (async {
          let! server, path, _externalPath, _definitionPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 4u; Character = 24u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"

              Expect.equal
                res.Range
                { Start = { Line = 6u; Character = 4u }
                  End = { Line = 6u; Character = 19u } }
                "Result should have correct range"
            | U2.C1(_) -> failwith "Not Implemented"
        })

      ptestCaseAsync
        "Go-to-implementation-on-interface-definition"
        (async {
          let! server, _path, _externalPath, definitionPath = server

          let p: ImplementationParams =
            { TextDocument = { Uri = Path.FilePathToUri definitionPath }
              Position = { Line = 8u; Character = 11u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentImplementation p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C1 _res -> failtest "Should be multiple GotoResult"
            | U2.C2 _res ->
              // TODO???
              // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 7; Character = 8 }; End = {Line = 7; Character = 30 }}) "First result should be in Library.fs"
              // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 13; Character = 14 }; End = {Line = 13; Character = 36 }}) "Second result should be in Library.fs"
              ()
        })

      testCaseAsync
        "Go-to-implementation on sourcelink file with sourcelink in PDB"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for the 'button' member in giraffe view engine
          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 9u; Character = 34u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "GiraffeViewEngine.fs" "Result should be in GiraffeViewEngine"
              let localPath = Path.FileUriToLocalPath res.Uri

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-implementation on sourcelink file with sourcelink in DLL"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for the 'List.concat' member in FSharp.Core
          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 12u; Character = 36u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "FSharp.Core/list.fs" "Result should be in FSharp.Core's list.fs"
              let localPath = Path.FileUriToLocalPath res.Uri

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        })

      // marked pending because we don't have filename information for C# sources
      ptestCaseAsync
        "Go-to-implementation on C# file"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for the 'Stirng.Join' member in the BCL
          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 14u; Character = 79u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              let localPath = Path.FileUriToLocalPath res.Uri

              if
                localPath.Contains
                  "System.String netstandard_ Version_2.0.0.0_ Culture_neutral_ PublicKeyToken_cc7b13ffcd2ddd51"
              then
                failtestf "should not decompile when sourcelink is available"

              Expect.stringContains localPath "System.String" "Result should be in the BCL's source files"

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        })

      ptestCaseAsync
        "Go-to-definition from C# file"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 26u; Character = 23u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some _res) ->
            //Do some assertions
            ()
        })

      testCaseAsync
        "Go-to-type-definition"
        (async {
          let! server, path, _externalPath, _definitionPath = server

          let p: TypeDefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 4u; Character = 24u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentTypeDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"

              Expect.equal
                res.Range
                { Start = { Line = 4u; Character = 5u }
                  End = { Line = 4u; Character = 6u } }
                "Result should have correct range"
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-type-definition on first char of identifier"
        (async {
          let! server, path, _externalPath, _definitionPath = server

          let p: TypeDefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri path }
              Position = { Line = 4u; Character = 20u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentTypeDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"

              Expect.equal
                res.Range
                { Start = { Line = 4u; Character = 5u }
                  End = { Line = 4u; Character = 6u } }
                "Result should have correct range"
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-type-defintion on parameter"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for parameter of type `'a list` -> FSharp.Core
          (*
          `let myConcat listA listB = List.concat [listA; listB]`
                          ^
                          position
        *)
          let p: TypeDefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 12u; Character = 16u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentTypeDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
              let localPath = Path.FileUriToLocalPath res.Uri

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-type-defintion on variable"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for variable of type `System.Collections.Generic.List<_>`
          (*
          `let myList = System.Collections.Generic.List<string>()`
                 ^
                 position
        *)
          let p: TypeDefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 16u; Character = 6u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentTypeDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              let localPath = Path.FileUriToLocalPath res.Uri

              Expect.stringContains
                res.Uri
                "System.Collections.Generic.List"
                "Result should be for System.Collections.Generic.List"

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-type-defintion on constructor"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for constructor of type `System.Collections.Generic.List<_>`
          (*
          `let myList = System.Collections.Generic.List<string>()`
                                                     ^
                                                     position
        *)
          let p: TypeDefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 16u; Character = 42u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentTypeDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              let localPath = Path.FileUriToLocalPath res.Uri

              Expect.stringContains
                res.Uri
                "System.Collections.Generic.List"
                "Result should be for System.Collections.Generic.List"

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-type-defintion on union case"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for union case of type `_ option`
          (*
          `let o v = Some v`
                       ^
                       position
        *)
          let p: TypeDefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 18u; Character = 12u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentTypeDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
              let localPath = Path.FileUriToLocalPath res.Uri

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        })

      testCaseAsync
        "Go-to-type-definition on property"
        (async {
          let! server, _path, externalPath, _definitionPath = server

          // check for property of type `string option`
          (*
          `b.Value |> ignore`
                ^
                position
        *)
          let p: TypeDefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri externalPath }
              Position = { Line = 24u; Character = 5u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentTypeDefinition p

          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok(Some res) ->
            match res with
            | U2.C2 _ -> failtest "Should be single GotoResult"
            | U2.C1(U2.C1 res) ->
              Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
              let localPath = Path.FileUriToLocalPath res.Uri

              Expect.isTrue
                (System.IO.File.Exists localPath)
                (sprintf "File '%s' should exist locally after being downloaded" localPath)
            | U2.C1(_) -> failwith "Not Implemented"
        }) ]

let private scriptGotoTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")
      let! (server, event) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event
      let scriptPath = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }
      do! server.TextDocumentDidOpen tdop
      return server, scriptPath
    }

  // sequenced because we don't provide safe concurrent access to file downloads in Sourcelink.fs
  testSequenced
  <| testList
    "Script GoTo Tests"
    [ testCaseAsync
        "Go-to-definition on #load integration test"
        (async {
          let! server, scriptPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri scriptPath }
              Position = { Line = 0u; Character = 10u }
              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Error e -> failtestf "Request failed: %A" e
          | Ok None -> failtest "Request none"
          | Ok(Some(U2.C2 _)) -> failtest "Should only get one location"
          | Ok(Some(U2.C1(U2.C1 r))) ->
            Expect.stringEnds r.Uri "/simple.fsx" "should navigate to the mentioned script file"
          | Ok(_resultValue) -> failwith "Not Implemented"
        })
      testCaseAsync
        "Go-to-definition on first char of identifier works"
        (async {
          let! server, scriptPath = server

          let p: DefinitionParams =
            { TextDocument = { Uri = Path.FilePathToUri scriptPath }
              Position = { Line = 5u; Character = 0u } // beginning of the usage of `testFunction` in the script file

              WorkDoneToken = None
              PartialResultToken = None }

          let! res = server.TextDocumentDefinition p

          match res with
          | Error e -> failtestf "Request failed: %A" e
          | Ok None -> failtest "Request none"
          | Ok(Some(U2.C2 _)) -> failtest "Should only get one location"
          | Ok(Some(U2.C1(U2.C1 r))) ->
            Expect.stringEnds r.Uri (Path.GetFileName scriptPath) "should navigate to the mentioned script file"

            Expect.equal
              r.Range
              { Start = { Line = 3u; Character = 4u }
                End = { Line = 3u; Character = 16u } }
              "should point to the range of the definition of `testFunction`"
          | Ok(_resultValue) -> failwith "Not Implemented"
        }) ]

let private untitledGotoTests state =
  serverTestList "Untitled GoTo Tests" state defaultConfigDto None (fun server ->
    [ testCaseAsync "can go to variable declaration"
      <| async {
        let (usagePos, declRange, text) =
          """
        let $0x$0 = 1
        let _ = ()
        let a = $0x
        """
          |> Text.trimTripleQuotation
          |> Cursor.assertExtractRange
          |> fun (decl, text) ->
              let (pos, text) = text |> Cursor.assertExtractPosition
              (pos, decl, text)

        let! (doc, _diags) = server |> Server.createUntitledDocument text

        use doc = doc

        let p: DefinitionParams =
          { TextDocument = doc.TextDocumentIdentifier
            Position = usagePos

            WorkDoneToken = None
            PartialResultToken = None }

        let! res = doc.Server.Server.TextDocumentDefinition p

        match res with
        | Error e -> failtestf "Request failed: %A" e
        | Ok None -> failtest "Request none"
        | Ok(Some(U2.C2 _)) -> failtest "Should only get one location"
        | Ok(Some(U2.C1(U2.C1 r))) ->
          Expect.stringEnds r.Uri doc.Uri "should navigate to source file"
          Expect.equal r.Range declRange "should point to the range of variable declaration"
        | Ok(_resultValue) -> failwith "Not Implemented"
      }
      testCaseAsync "can go to function declaration"
      <| async {
        let (usagePos, declRange, text) =
          """
        let $0myFun$0 a b = a + b
        let _ = ()
        let a = my$0Fun 1 1
        """
          |> Text.trimTripleQuotation
          |> Cursor.assertExtractRange
          |> fun (decl, text) ->
              let (pos, text) = text |> Cursor.assertExtractPosition
              (pos, decl, text)

        let! (doc, _diags) = server |> Server.createUntitledDocument text
        use doc = doc

        let p: DefinitionParams =
          { TextDocument = doc.TextDocumentIdentifier
            Position = usagePos

            WorkDoneToken = None
            PartialResultToken = None }

        let! res = doc.Server.Server.TextDocumentDefinition p

        match res with
        | Error e -> failtestf "Request failed: %A" e
        | Ok None -> failtest "Request none"
        | Ok(Some(U2.C2 _)) -> failtest "Should only get one location"
        | Ok(Some(U2.C1(U2.C1 r))) ->
          Expect.stringEnds r.Uri doc.Uri "should navigate to source file"
          Expect.equal r.Range declRange "should point to the range of function declaration"
        | Ok(_resultValue) -> failwith "Not Implemented"
      } ])

let tests createServer =
  testSequenced
  <| testList
    "Go to definition tests"
    [ gotoTest createServer
      scriptGotoTests createServer
      untitledGotoTests createServer ]
