module FsAutoComplete.Tests.CodeFixTests

open Expecto
open System.IO
open Helpers
open LanguageServerProtocol.Types
open FsAutoComplete.Utils

let (|Refactor|_|) title newText action =
    match action with
    | { Title = title'
        Kind = Some "refactor"
        Edit = { DocumentChanges = Some [|
          { Edits = [| { NewText = newText' } |] }
        |] }
      }
      when title' = title && newText' = newText -> Some ()
    | _ -> None

let abstractClassGenerationTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AbstractClassGeneration")
      let! (server, events) = serverInitialize path { defaultConfigDto with AbstractClassStubGeneration = Some true } state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  let canGenerateForLongIdent = testCaseAsync "can generate a derivative of a long ident - System.IO.Stream" (async {
    let! server, file, diagnostics = server
    let diagnostic = diagnostics |> Array.tryFind (fun d -> d.Code = Some "365" && d.Range.Start.Line = 0 ) |> Option.defaultWith (fun _ -> failtest "Should have gotten an error of type 365")
    let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                    Range = diagnostic.Range
                                                    Context = { Diagnostics = [| diagnostic |] } }
    match response with
    | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate abstract class members" } |] )) -> ()
    | Ok other -> failtestf $"Should have generated the rest of the base class, but instead generated %A{other}"
    | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
  })

  let canGenerateForIdent = testCaseAsync "can generate a derivative for a simple ident - Stream" (async {
    let! server, file, diagnostics = server
    let diagnostic = diagnostics |> Array.tryFind (fun d -> d.Code = Some "365" && d.Range.Start.Line = 5 ) |> Option.defaultWith (fun _ -> failtest "Should have gotten an error of type 365")
    let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                    Range = diagnostic.Range
                                                    Context = { Diagnostics = [| diagnostic |] } }
    match response with
    | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate abstract class members" } |] )) -> ()
    | Ok other -> failtestf $"Should have generated the rest of the base class, but instead generated %A{other}"
    | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
  })

  testList "abstract class generation" [
    canGenerateForLongIdent
    canGenerateForIdent
  ]

let generateMatchTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MatchCaseGeneration")
      let! (server, events) = serverInitialize path { defaultConfigDto with UnionCaseStubGeneration = Some true } state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList "generate match cases" [
    testCaseAsync "can generate match cases for a simple DU" (async {
      let! server, file, diagnostics = server
      let expectedDiagnostic = diagnostics.[0]
      Expect.equal expectedDiagnostic.Code (Some "25") "Should have a empty match warning"
      let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                      Range = expectedDiagnostic.Range
                                                      Context = { Diagnostics = [| expectedDiagnostic |] } }
      match response with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate union pattern match cases" } |] )) -> ()
      | Ok other -> failtestf $"Should have generated the rest of match cases, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let missingFunKeywordTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MissingFunKeyword")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList "missing fun keyword" [
    testCaseAsync "can generate the fun keyword when error 10 is raised" (async {
      let! server, file, diagnostics = server
      let expectedDiagnostic = diagnostics.[0]
      Expect.equal expectedDiagnostic.Code (Some "10") "Should have a missing fun keyword error"
      let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                      Range = expectedDiagnostic.Range
                                                      Context = { Diagnostics = [| expectedDiagnostic |] } }
      match response with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Add missing 'fun' keyword"
                                                                Kind = Some "quickfix"
                                                                Edit = { DocumentChanges = Some [| { Edits = [| { NewText = "fun " } |] } |] } } |] )) -> ()
      | Ok other -> failtestf $"Should have generated missing fun keyword, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let outerBindingRecursiveTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "OuterBindingRecursive")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList "outer binding recursive" [
    testCaseAsync "can make the outer binding recursive when self-referential" (async {
      let! server, file, diagnostics = server
      let expectedDiagnostic = diagnostics.[0]
      Expect.equal expectedDiagnostic.Code (Some "39") "Should have a not defined value error"
      let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                      Range = expectedDiagnostic.Range
                                                      Context = { Diagnostics = [| expectedDiagnostic |] } }
      match response with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Make outer binding recursive"
                                                                Kind = Some "quickfix"
                                                                Edit = { DocumentChanges = Some [| { Edits = [| { NewText = "rec " } |] } |] } } |] )) -> ()
      | Ok other -> failtestf $"Should have generated a rec keyword, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let nameofInsteadOfTypeofNameTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "NameofInsteadOfTypeofName")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap id (fun _ -> failtest "Should not have had errors")
      return (server, path)
    }
    |> Async.Cache

  testList "use nameof instead of typeof.Name" [
    testCaseAsync "can suggest fix" (async {
      let! server, file = server
      let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                      Range = { Start = { Line = 0; Character = 8 }; End = { Line = 0; Character = 8 }}
                                                      Context = { Diagnostics = [| |] } }
      match response with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Use 'nameof'"
                                                                Kind = Some "refactor"
                                                                Edit = { DocumentChanges = Some [| { Edits = [| { NewText = "nameof(Async<string>)" } |] } |] } } |] )) -> ()
      | Ok other -> failtestf $"Should have generated nameof, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let missingInstanceMemberTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MissingInstanceMember")
      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics = waitForParseResultsForFile "Script.fsx" events |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)
      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList "missing instance member" [
    testCaseAsync "can add this member prefix" (async {
      let! server, file, diagnostics = server
      let expectedDiagnostic = diagnostics.[0]
      Expect.equal expectedDiagnostic.Code (Some "673") "Should have a missing self identifier error"
      let! response = server.TextDocumentCodeAction { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                                                      Range = expectedDiagnostic.Range
                                                      Context = { Diagnostics = [| expectedDiagnostic |] } }
      match response with
      | Ok(
          Some (
            TextDocumentCodeActionResult.CodeActions [| { Title = "Add missing instance member parameter"
                                                          Kind = Some "quickfix"
                                                          Edit = {
                                                            DocumentChanges = Some [|
                                                              { Edits = [|
                                                                { NewText = "x." } |] } |] } } |] )) -> ()
      | Ok other -> failtestf $"Should have generated an instance member, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let unusedValueTests state =
  let (|ActReplace|_|) = (|Refactor|_|) "Replace with _" "_"

  let (|ActPrefix|_|) oldText = (|Refactor|_|) "Prefix with _" $"_{oldText}"

  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "UnusedValue")
      let cfg = { defaultConfigDto with UnusedDeclarationsAnalyzer = Some true }
      let! (server, events) = serverInitialize path cfg  state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      let! diagnostics =
          events
          |> waitForFsacDiagnosticsForFile "Script.fsx"
          |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") id
      return (server, path, diagnostics)
    }
    |> Async.Cache

  let canReplaceUnusedSelfReference = testCaseAsync "can replace unused self-reference" (async {
    let! server, file, diagnostics = server
    let diagnostic =
      diagnostics
      |> Array.tryFind (fun d ->
          d.Range.Start = { Line = 2; Character =  9 } &&
          d.Range.End   = { Line = 2; Character = 13 }
      ) |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range")
    let detected = {
        CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
        Range = diagnostic.Range
        Context = { Diagnostics = [| diagnostic |] }
    }
    match! server.TextDocumentCodeAction detected with
    | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ActReplace |])) -> ()
    | Ok other ->
        failtestf $"Should have generated _, but instead generated %A{other}"
    | Error reason ->
        failtestf $"Should have succeeded, but failed with %A{reason}"
  })

  let canReplaceUnusedBinding = testCaseAsync "can replace unused binding" (async {
    let! server, file, diagnostics = server
    let diagnostic =
      diagnostics
      |> Array.tryFind (fun d ->
          d.Range.Start = { Line = 9; Character = 4 } &&
          d.Range.End   = { Line = 9; Character = 7 }
      ) |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range")
    let detected = {
        CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
        Range = diagnostic.Range
        Context = { Diagnostics = [| diagnostic |] }
    }
    match! server.TextDocumentCodeAction detected with
    | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ActReplace; ActPrefix "six" |])) -> ()
    | Ok other ->
        failtestf $"Should have generated _, but instead generated %A{other}"
    | Error reason ->
        failtestf $"Should have succeeded, but failed with %A{reason}"
  })

  let canReplaceUnusedParameter = testCaseAsync "can replace unused parameter" (async {
    let! server, file, diagnostics = server
    let diagnostic =
      diagnostics
      |> Array.tryFind (fun d ->
          d.Range.Start = { Line = 15; Character = 16 } &&
          d.Range.End   = { Line = 15; Character = 21 }
      ) |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range")
    let detected = {
        CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
        Range = diagnostic.Range
        Context = { Diagnostics = [| diagnostic |] }
    }
    match! server.TextDocumentCodeAction detected with
    | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ActReplace; ActPrefix "three"; _ (* explicit type annotation codefix *) |])) -> ()
    | Ok other ->
        failtestf $"Should have generated _, but instead generated %A{other}"
    | Error reason ->
        failtestf $"Should have succeeded, but failed with %A{reason}"
  })

  testList "unused value" [
    canReplaceUnusedSelfReference
    canReplaceUnusedBinding
    canReplaceUnusedParameter
  ]

let addExplicitTypeAnnotationTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ExplicitTypeAnnotations")
      let cfg = defaultConfigDto
      let! (server, events) = serverInitialize path cfg  state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop
      do!
          events
          |> waitForParseResultsForFile "Script.fsx"
          |> AsyncResult.bimap id (fun _ -> failtest "Should not have had errors")
      return (server, path)
    }
    |> Async.Cache

  let (|ExplicitAnnotation|_|) = (|Refactor|_|) "Add explicit type annotation"

  testList "explicit type annotations" [
    testCaseAsync "can suggest explicit parameter for record-typed function parameters" (async {
      let! (server, filePath) = server
      let context : CodeActionParams = {
        Context = { Diagnostics = [||] }
        Range = { Start = { Line = 3; Character = 9 }
                  End = { Line = 3; Character = 9 } }
        TextDocument = { Uri = Path.FilePathToUri filePath }
      }
      match! server.TextDocumentCodeAction context with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ExplicitAnnotation "(f: Foo)" |])) -> ()
      | Ok other ->
        failtestf $"Should have generated explicit type annotation, but instead generated %A{other}"
      | Error reason ->
        failtestf $"Should have succeeded, but failed with %A{reason}"
    })
  ]

let tests state = testList "codefix tests" [
  abstractClassGenerationTests state
  generateMatchTests state
  missingFunKeywordTests state
  outerBindingRecursiveTests state
  nameofInsteadOfTypeofNameTests state
  missingInstanceMemberTests state
  unusedValueTests state
  addExplicitTypeAnnotationTests state
]
