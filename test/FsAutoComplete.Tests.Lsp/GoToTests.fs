module FsAutoComplete.Tests.GoTo


open Expecto
open System.IO
open Helpers
open LanguageServerProtocol.Types
open FsToolkit.ErrorHandling
open FsAutoComplete

///GoTo tests
let private gotoTest state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

      let! (server, event) = serverInitialize path defaultConfigDto  state
      do! waitForWorkspaceFinishedParsing event

      let definitionPath = Path.Combine(path, "Definition.fs")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument definitionPath }
      do! server.TextDocumentDidOpen tdop

      let externalPath = Path.Combine(path, "External.fs")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument externalPath }
      do! server.TextDocumentDidOpen tdop

      let path = Path.Combine(path, "Library.fs")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      do! waitForParseResultsForFile "Definition.fs" event |> AsyncResult.foldResult id (failtestf "%A")
      do! waitForParseResultsForFile "External.fs" event |> AsyncResult.foldResult id (failtestf "%A")
      do! waitForParseResultsForFile "Library.fs" event |> AsyncResult.foldResult id (failtestf "%A")

      return (server, path, externalPath, definitionPath)
    }
    |> Async.Cache

  testSequenced <| testList "GoTo Tests" [
      testCaseAsync "Go-to-definition on external symbol (System.Net.HttpWebRequest)" (async {
        let! server, path, externalPath, definitionPath = server
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 4; Character = 30 }
        }

        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
        | Result.Ok (Some (GotoResult.Single r)) when r.Uri.EndsWith("startup") ->
          failtest "Should not generate the startup dummy file"
        | Result.Ok (Some (GotoResult.Single r)) ->
          Expect.stringEnds r.Uri ".cs" "should have generated a C# code file"
          Expect.stringContains r.Uri "System.Net.HttpWebRequest" "The generated file should be for the HttpWebRequest type"
          () // should
      })

      testCaseAsync "Go-to-definition on external namespace (System.Net) should error when going to a namespace " (async {
        let! server, path, externalPath, definitionPath = server
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 2; Character = 15 }
        }

        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e ->
          Expect.equal "Could not find declaration" e.Message "Should report failure for navigating to a namespace"
        | Result.Ok r -> failtestf "Declaration request should not work on a namespace, instead we got %A" r
      })

      testCaseAsync "Go-to-definition" (async {
        let! server, path, externalPath, definitionPath = server
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 2; Character = 29}}
        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 16 }} "Result should have correct range"
      })

      testCaseAsync "Go-to-definition on custom type binding" (async {
        let! server, path, externalPath, definitionPath = server

        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 6; Character = 4 }; End = {Line = 6; Character = 19 }} "Result should have correct range"
      })

      testCaseAsync "Go-to-implementation-on-interface-definition" (async {
        let! server, path, externalPath, definitionPath = server
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri definitionPath}
            Position = { Line = 8; Character = 11 } }
        let! res = server.TextDocumentImplementation p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Single res -> failtest "Should be multiple GotoResult"
          | GotoResult.Multiple res ->
            // TODO???
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 7; Character = 8 }; End = {Line = 7; Character = 30 }}) "First result should be in Library.fs"
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 13; Character = 14 }; End = {Line = 13; Character = 36 }}) "Second result should be in Library.fs"
            ()
      })

      testCaseAsync "Go-to-implementation on sourcelink file with sourcelink in PDB" (async {
        let! server, path, externalPath, definitionPath = server

        // check for the 'button' member in giraffe view engine
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 9; Character = 34} }

        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "GiraffeViewEngine.fs" "Result should be in GiraffeViewEngine"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      testCaseAsync "Go-to-implementation on sourcelink file with sourcelink in DLL" (async {
        let! server, path, externalPath, definitionPath = server

        // check for the 'List.concat' member in FSharp.Core
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 36} }

        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/list.fs" "Result should be in FSharp.Core's list.fs"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      // marked pending because we don't have filename information for C# sources
      ptestCaseAsync "Go-to-implementation on C# file" (async {
        let! server, path, externalPath, definitionPath = server

        // check for the 'Stirng.Join' member in the BCL
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 14; Character = 79} }

        let! res = server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            if localPath.Contains "System.String netstandard_ Version_2.0.0.0_ Culture_neutral_ PublicKeyToken_cc7b13ffcd2ddd51"
            then failtestf "should not decompile when sourcelink is available"
            Expect.stringContains localPath "System.String" "Result should be in the BCL's source files"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      testCaseAsync "Go-to-type-definition" (async {
        let! server, path, externalPath, definitionPath = server

        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let! res = server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 4; Character = 5 }; End = {Line = 4; Character = 6 }} "Result should have correct range"
      })

      testCaseAsync "Go-to-type-definition on first char of identifier" (async {
        let! server, path, externalPath, definitionPath = server

        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 20 } }
        let! res = server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 4; Character = 5 }; End = {Line = 4; Character = 6 }} "Result should have correct range"
      })

      testCaseAsync "Go-to-type-defintion on parameter" (async {
        let! server, path, externalPath, definitionPath = server

        // check for parameter of type `'a list` -> FSharp.Core
        (*
          `let myConcat listA listB = List.concat [listA; listB]`
                          ^
                          position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 16}}
        let! res = server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      testCaseAsync "Go-to-type-defintion on variable" (async {
        let! server, path, externalPath, definitionPath = server

        // check for variable of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                 ^
                 position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 6}}
        let! res = server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      testCaseAsync "Go-to-type-defintion on constructor" (async {
        let! server, path, externalPath, definitionPath = server

        // check for constructor of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                                                     ^
                                                     position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 42}}
        let! res = server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      testCaseAsync "Go-to-type-defintion on union case" (async {
        let! server, path, externalPath, definitionPath = server

        // check for union case of type `_ option`
        (*
          `let o v = Some v`
                       ^
                       position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 18; Character = 12}}
        let! res = server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      testCaseAsync "Go-to-type-definition on property" (async {
        let! server, path, externalPath, definitionPath = server

        // check for property of type `string option`
        (*
          `b.Value |> ignore`
                ^
                position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 24; Character = 5}}
        let! res = server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      testCaseAsync "cleanup" (async {
          let! server, _, _, _ = server
          do! server.Shutdown()
        })
  ]

let private scriptGotoTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")
      let! (server, event) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing event
      let scriptPath = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }
      do! server.TextDocumentDidOpen tdop
      return server, scriptPath
    }

  testSequenced <| testList "Script GoTo Tests" [
    testCaseAsync "Go-to-definition on #load integration test" (async {
      let! server, scriptPath = server
      let p : TextDocumentPositionParams = {
        TextDocument = { Uri = Path.FilePathToUri scriptPath }
        Position = { Line = 0; Character = 10 }
      }
      let! res = server.TextDocumentDefinition p
      match res with
      | Error e -> failtestf "Request failed: %A" e
      | Ok None -> failtest "Request none"
      | Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
      | Ok (Some (GotoResult.Single r)) ->
        Expect.stringEnds r.Uri "/simple.fsx" "should navigate to the mentioned script file"
    })
    testCaseAsync "Go-to-definition on first char of identifier works" (async {
      let! server, scriptPath = server
      let p : TextDocumentPositionParams = {
        TextDocument = { Uri = Path.FilePathToUri scriptPath }
        Position = { Line = 5; Character = 0 } // beginning of the usage of `testFunction` in the script file
      }
      let! res = server.TextDocumentDefinition p
      match res with
      | Error e -> failtestf "Request failed: %A" e
      | Ok None -> failtest "Request none"
      | Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
      | Ok (Some (GotoResult.Single r)) ->
        Expect.stringEnds r.Uri (Path.GetFileName scriptPath) "should navigate to the mentioned script file"
        Expect.equal r.Range {Start = { Line = 3; Character = 4 }; End = { Line = 3; Character = 16 }} "should point to the range of the definition of `testFunction`"
    })
    testCaseAsync "cleanup" (async {
      let! server, _ = server
      do! server.Shutdown()
    })
  ]

let tests state = testList "Go to definition tests" [
  gotoTest state
  scriptGotoTests state
]
