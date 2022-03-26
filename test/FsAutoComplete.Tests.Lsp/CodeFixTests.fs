module FsAutoComplete.Tests.CodeFixTests

open Expecto
open System.IO
open Helpers
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.Utils

let pos (line, character) = { Line = line; Character = character }
let range st e = { Start = pos st; End = pos e }
let rangeP st e = { Start = st; End = e }

/// naive iteration, assumes start and end are on same line
let iterateRange (r: Range) =
  seq {
    if r.Start = r.End then
      yield r.Start
    else
      for c = r.Start.Character to r.End.Character do
        yield pos (r.Start.Line, c)
  }

let (|Refactor|_|) title newText action =
  match action with
  | { Title = title'
      Kind = Some "refactor"
      Edit = Some { DocumentChanges = Some [| { Edits = [| { NewText = newText' } |] } |] } } when
    title' = title && newText' = newText
    ->
    Some()
  | _ -> None

let (|AtRange|_|) range (action: CodeAction) =
  match action with
  | { Edit = Some { DocumentChanges = Some [| { Edits = [| { Range = range' } |] } |] } } when range = range' -> Some()
  | _ -> None

let abstractClassGenerationTests state =
  let server =
    async {
      let path =
        Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AbstractClassGeneration")

      let! (server, events) =
        serverInitialize path { defaultConfigDto with AbstractClassStubGeneration = Some true } state

      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path, diagnostics)
    }
    |> Async.Cache

  let canGenerateForLongIdent =
    testCaseAsync
      "can generate a derivative of a long ident - System.IO.Stream"
      (async {
        let! server, file, diagnostics = server

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d -> d.Code = Some "365" && d.Range.Start.Line = 0)
          |> Option.defaultWith (fun _ -> failtest "Should have gotten an error of type 365")

        let! response =
          server.TextDocumentCodeAction
            { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
              Range = diagnostic.Range
              Context = { Diagnostics = [| diagnostic |] } }

        match response with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate abstract class members" } |])) -> ()
        | Ok other -> failtestf $"Should have generated the rest of the base class, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })

  let canGenerateForIdent =
    testCaseAsync
      "can generate a derivative for a simple ident - Stream"
      (async {
        let! server, file, diagnostics = server

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d -> d.Code = Some "365" && d.Range.Start.Line = 5)
          |> Option.defaultWith (fun _ -> failtest "Should have gotten an error of type 365")

        let! response =
          server.TextDocumentCodeAction
            { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
              Range = diagnostic.Range
              Context = { Diagnostics = [| diagnostic |] } }

        match response with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate abstract class members" } |])) -> ()
        | Ok other -> failtestf $"Should have generated the rest of the base class, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })

  testList
    "abstract class generation"
    [ canGenerateForLongIdent
      canGenerateForIdent ]

let generateMatchTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MatchCaseGeneration")

      let! (server, events) = serverInitialize path { defaultConfigDto with UnionCaseStubGeneration = Some true } state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList
    "generate match cases"
    [ testCaseAsync
        "can generate match cases for a simple DU"
        (async {
          let! server, file, diagnostics = server
          let expectedDiagnostic = diagnostics.[0]
          Expect.equal expectedDiagnostic.Code (Some "25") "Should have a empty match warning"

          let! response =
            server.TextDocumentCodeAction
              { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                Range = expectedDiagnostic.Range
                Context = { Diagnostics = [| expectedDiagnostic |] } }

          match response with
          | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate union pattern match cases" } |])) ->
            ()
          | Ok other -> failtestf $"Should have generated the rest of match cases, but instead generated %A{other}"
          | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]

let generateRecordStubTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RecordStubGeneration")

      let! (server, events) = serverInitialize path { defaultConfigDto with RecordStubGeneration = Some true } state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path, diagnostics)
    }
    |> Async.Cache

  let (|NormalizeNewlines|) (s: string) = s.Replace("\r\n", System.Environment.NewLine)

  testList
    "generate record stubs"
    [ testCaseAsync
        "can generate record stubs for every pos in the record as soon as one field is known"
        (async {
          let! server, file, diagnostics = server
          let expectedDiagnostic = diagnostics.[0]
          Expect.equal expectedDiagnostic.Code (Some "764") "Should have missing record field diagnostic"

          for pos in iterateRange expectedDiagnostic.Range do
            let! response =
              server.TextDocumentCodeAction
                { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                  Range = rangeP pos pos
                  Context = { Diagnostics = [| expectedDiagnostic |] } }

            match response with
            | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Generate record stub"
                                                                      Edit = Some { DocumentChanges = Some [| { Edits = [| { Range = { Start = { Line = 2
                                                                                                                                                 Character = 18 }
                                                                                                                                       End = { Line = 2
                                                                                                                                               Character = 18 } }
                                                                                                                             NewText = NormalizeNewlines "\r\n           b = failwith \"Not Implemented\"" } |] } |] } } |])) ->
              ()
            | Ok other ->
              failtestf
                $"Should have generated the rest of the record body at %d{pos.Line},%d{pos.Character}, but instead generated %A{other}"
            | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]


let missingFunKeywordTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MissingFunKeyword")

      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList
    "missing fun keyword"
    [ testCaseAsync
        "can generate the fun keyword when error 10 is raised"
        (async {
          let! server, file, diagnostics = server
          let expectedDiagnostic = diagnostics.[0]
          Expect.equal expectedDiagnostic.Code (Some "10") "Should have a missing fun keyword error"

          let! response =
            server.TextDocumentCodeAction
              { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                Range = expectedDiagnostic.Range
                Context = { Diagnostics = [| expectedDiagnostic |] } }

          match response with
          | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Add missing 'fun' keyword"
                                                                    Kind = Some "quickfix"
                                                                    Edit = Some { DocumentChanges = Some [| { Edits = [| { NewText = "fun " } |] } |] } } |])) ->
            ()
          | Ok other -> failtestf $"Should have generated missing fun keyword, but instead generated %A{other}"
          | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]

let outerBindingRecursiveTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "OuterBindingRecursive")

      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList
    "outer binding recursive"
    [ testCaseAsync
        "can make the outer binding recursive when self-referential"
        (async {
          let! server, file, diagnostics = server
          let expectedDiagnostic = diagnostics.[0]
          Expect.equal expectedDiagnostic.Code (Some "39") "Should have a not defined value error"

          let! response =
            server.TextDocumentCodeAction
              { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                Range = expectedDiagnostic.Range
                Context = { Diagnostics = [| expectedDiagnostic |] } }

          match response with
          | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Make outer binding recursive"
                                                                    Kind = Some "quickfix"
                                                                    Edit = Some { DocumentChanges = Some [| { Edits = [| { NewText = "rec " } |] } |] } } |])) ->
            ()
          | Ok other -> failtestf $"Should have generated a rec keyword, but instead generated %A{other}"
          | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]

let nameofInsteadOfTypeofNameTests state =
  let server =
    async {
      let path =
        Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "NameofInsteadOfTypeofName")

      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap id (fun _ -> failtest "Should not have had errors")

      return (server, path)
    }
    |> Async.Cache

  testList
    "use nameof instead of typeof.Name"
    [ testCaseAsync
        "can suggest fix"
        (async {
          let! server, file = server

          let! response =
            server.TextDocumentCodeAction
              { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                Range =
                  { Start = { Line = 0; Character = 8 }
                    End = { Line = 0; Character = 8 } }
                Context = { Diagnostics = [||] } }

          match response with
          | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Use 'nameof'"
                                                                    Kind = Some "refactor"
                                                                    Edit = Some { DocumentChanges = Some [| { Edits = [| { NewText = "nameof(Async<string>)" } |] } |] } } |])) ->
            ()
          | Ok other -> failtestf $"Should have generated nameof, but instead generated %A{other}"
          | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]

let missingInstanceMemberTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "MissingInstanceMember")

      let! (server, events) = serverInitialize path defaultConfigDto state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        waitForParseResultsForFile "Script.fsx" events
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") (fun e -> e)

      return (server, path, diagnostics)
    }
    |> Async.Cache

  testList
    "missing instance member"
    [ testCaseAsync
        "can add this member prefix"
        (async {
          let! server, file, diagnostics = server
          let expectedDiagnostic = diagnostics.[0]
          Expect.equal expectedDiagnostic.Code (Some "673") "Should have a missing self identifier error"

          let! response =
            server.TextDocumentCodeAction
              { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
                Range = expectedDiagnostic.Range
                Context = { Diagnostics = [| expectedDiagnostic |] } }

          match response with
          | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Add missing instance member parameter"
                                                                    Kind = Some "quickfix"
                                                                    Edit = Some { DocumentChanges = Some [| { Edits = [| { NewText = "x." } |] } |] } } |])) ->
            ()
          | Ok other -> failtestf $"Should have generated an instance member, but instead generated %A{other}"
          | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]

let unusedValueTests state =
  let (|ActReplace|_|) = (|Refactor|_|) "Replace with _" "_"

  let (|ActPrefix|_|) oldText =
    (|Refactor|_|) "Prefix with _" $"_{oldText}"

  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "UnusedValue")

      let cfg = { defaultConfigDto with UnusedDeclarationsAnalyzer = Some true }

      let! (server, events) = serverInitialize path cfg state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        events
        |> waitForFsacDiagnosticsForFile "Script.fsx"
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") id

      return (server, path, diagnostics)
    }
    |> Async.Cache

  let canReplaceUnusedSelfReference =
    testCaseAsync
      "can replace unused self-reference"
      (async {
        let! server, file, diagnostics = server

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d ->
            d.Range.Start = { Line = 2; Character = 9 }
            && d.Range.End = { Line = 2; Character = 13 })
          |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range")

        let detected =
          { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
            Range = diagnostic.Range
            Context = { Diagnostics = [| diagnostic |] } }

        match! server.TextDocumentCodeAction detected with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ActReplace |])) -> ()
        | Ok other -> failtestf $"Should have generated _, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })

  let canReplaceUnusedBinding =
    testCaseAsync
      "can replace unused binding"
      (async {
        let! server, file, diagnostics = server

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d ->
            d.Range.Start = { Line = 9; Character = 4 }
            && d.Range.End = { Line = 9; Character = 7 })
          |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range")

        let detected =
          { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
            Range = diagnostic.Range
            Context = { Diagnostics = [| diagnostic |] } }

        match! server.TextDocumentCodeAction detected with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ActReplace; ActPrefix "six" |])) -> ()
        | Ok other -> failtestf $"Should have generated _, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })

  let canReplaceUnusedParameter =
    testCaseAsync
      "can replace unused parameter"
      (async {
        let! server, file, diagnostics = server

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d ->
            d.Range.Start = { Line = 15; Character = 16 }
            && d.Range.End = { Line = 15; Character = 21 })
          |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range")

        let detected =
          { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
            Range = diagnostic.Range
            Context = { Diagnostics = [| diagnostic |] } }

        match! server.TextDocumentCodeAction detected with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ActReplace
                                                                ActPrefix "three"
                                                                _ (* explicit type annotation codefix *)  |])) -> ()
        | Ok other -> failtestf $"Should have generated _, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })

  testList
    "unused value"
    [ canReplaceUnusedSelfReference
      canReplaceUnusedBinding
      canReplaceUnusedParameter ]

let removeUnusedBindingTests state =
  let (|RemoveBinding|_|) = (|Refactor|_|) "Remove unused binding" ""

  let (|RemoveParameter|_|) = (|Refactor|_|) "Remove unused parameter" ""

  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RemoveUnusedBinding")

      let cfg = { defaultConfigDto with FSIExtraParameters = Some [| "--warnon:1182" |] }

      let! (server, events) = serverInitialize path cfg state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        events
        |> waitForCompilerDiagnosticsForFile "Script.fsx"
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") id

      return (server, path, diagnostics)
    }
    |> Async.Cache

  let canRemoveUnusedSingleCharacterFunctionParameter =
    testCaseAsync
      "can remove unused single character function parameter"
      (async {
        let! server, file, diagnostics = server
        let targetRange = range (0, 9) (0, 10)

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d -> d.Range = targetRange && d.Code = Some "1182")
          |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range and code")

        let detected =
          { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
            Range = diagnostic.Range
            Context = { Diagnostics = [| diagnostic |] } }

        let replacementRange = range (0, 8) (0, 10)

        match! server.TextDocumentCodeAction detected with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| RemoveParameter & AtRange replacementRange
                                                                _ (* explicit type annotation codefix *)  |])) -> ()
        | Ok other -> failtestf $"Should have generated _, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })

  let canRemoveUnusedSingleCharacterFunctionParameterInParens =
    testCaseAsync
      "can remove unused single character function parameter in parens"
      (async {
        let! server, file, diagnostics = server
        let targetRange = range (2, 11) (2, 12)

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d -> d.Range = targetRange && d.Code = Some "1182")
          |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range and code")

        let detected =
          { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
            Range = diagnostic.Range
            Context = { Diagnostics = [| diagnostic |] } }

        let replacementRange = range (2, 9) (2, 13)

        match! server.TextDocumentCodeAction detected with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| RemoveParameter & AtRange replacementRange
                                                                _ (* explicit type annotation codefix *)  |])) -> ()
        | Ok other -> failtestf $"Should have generated _, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })

  let canRemoveUnusedBindingInsideTopLevel =
    testCaseAsync
      "can remove unused binding inside top level"
      (async {
        let! server, file, diagnostics = server
        let targetRange = range (5, 6) (5, 10)

        let diagnostic =
          diagnostics
          |> Array.tryFind (fun d -> d.Range = targetRange && d.Code = Some "1182")
          |> Option.defaultWith (fun () -> failwith "could not find diagnostic with expected range and code")

        let detected =
          { CodeActionParams.TextDocument = { Uri = Path.FilePathToUri file }
            Range = diagnostic.Range
            Context = { Diagnostics = [| diagnostic |] } }

        let replacementRange = range (5, 2) (5, 16) // span of whole `let incr...` binding

        match! server.TextDocumentCodeAction detected with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions [| RemoveBinding & AtRange replacementRange |])) -> ()
        | Ok other -> failtestf $"Should have generated _, but instead generated %A{other}"
        | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
      })


  testList
    "remove unused binding"
    [ canRemoveUnusedSingleCharacterFunctionParameter
      canRemoveUnusedSingleCharacterFunctionParameterInParens
      canRemoveUnusedBindingInsideTopLevel ]

let addExplicitTypeAnnotationTests state =
  let server =
    async {
      let path =
        Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ExplicitTypeAnnotations")

      let cfg = defaultConfigDto
      let! (server, events) = serverInitialize path cfg state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      do!
        events
        |> waitForParseResultsForFile "Script.fsx"
        |> AsyncResult.bimap id (fun _ -> failtest "Should not have had errors")

      return (server, path)
    }
    |> Async.Cache

  let (|ExplicitAnnotation|_|) = (|Refactor|_|) "Add explicit type annotation"

  testList
    "explicit type annotations"
    [ testCaseAsync
        "can suggest explicit parameter for record-typed function parameters"
        (async {
          let! (server, filePath) = server

          let context: CodeActionParams =
            { Context = { Diagnostics = [||] }
              Range =
                { Start = { Line = 3; Character = 9 }
                  End = { Line = 3; Character = 9 } }
              TextDocument = { Uri = Path.FilePathToUri filePath } }

          match! server.TextDocumentCodeAction context with
          | Ok (Some (TextDocumentCodeActionResult.CodeActions [| ExplicitAnnotation "(f: Foo)" |])) -> ()
          | Ok other -> failtestf $"Should have generated explicit type annotation, but instead generated %A{other}"
          | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]

let negationToSubstraction state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "NegationToSubstraction")

      let cfg = defaultConfigDto
      let! (server, events) = serverInitialize path cfg state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        events
        |> waitForParseResultsForFile "Script.fsx"
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") id

      return (server, path, diagnostics)
    }
    |> Async.Cache

  let (|NegationToSubstraction|_|) = (|Refactor|_|) "Negation to substraction"

  testList
    "negation to substraction"
    [ testCaseAsync
        "converts negation to substraction"
        (async {
          let! (server, filePath, diagnostics) = server

          let diagnostic =
            diagnostics
            |> Array.tryFind (fun d -> d.Code = Some "3" && d.Range.Start.Line = 2)
            |> Option.defaultWith (fun _ -> failtest "Should have gotten an error of type 3")

          let context: CodeActionParams =
            { Context = { Diagnostics = [| diagnostic |] }
              Range =
                { Start = { Line = 2; Character = 13 }
                  End = { Line = 2; Character = 14 } }
              TextDocument = { Uri = Path.FilePathToUri filePath } }

          match! server.TextDocumentCodeAction context with
          | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Use subtraction instead of negation"
                                                                    Kind = Some "quickfix"
                                                                    Edit = Some { DocumentChanges = Some [| { Edits = [| { Range = { Start = { Line = 2
                                                                                                                                               Character = 15 }
                                                                                                                                     End = { Line = 2
                                                                                                                                             Character = 16 } }
                                                                                                                           NewText = "- " } |] } |] } } |])) ->
            ()
          | Ok other -> failtestf $"Should have converted negation to substraction, but instead generated %A{other}"
          | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
        }) ]

let positionalToNamedDUTests state =
  let server =
    async {
      let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PositionalToNamedDU")

      let cfg = defaultConfigDto
      let! (server, events) = serverInitialize path cfg state
      do! waitForWorkspaceFinishedParsing events
      let path = Path.Combine(path, "Script.fsx")
      let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument path }
      do! server.TextDocumentDidOpen tdop

      let! diagnostics =
        events
        |> waitForParseResultsForFile "Script.fsx"
        |> AsyncResult.bimap (fun _ -> failtest "Should have had errors") id

      return (server, path)
    }
    |> Async.Cache

  let expectEdits invokePos edits =
    async {
      let! (server, filePath) = server

      let context: CodeActionParams =
        { Context = { Diagnostics = [||] }
          Range = invokePos
          TextDocument = { Uri = Path.FilePathToUri filePath } }

      match! server.TextDocumentCodeAction context with
      | Ok (Some (TextDocumentCodeActionResult.CodeActions [| { Title = "Convert to named patterns"
                                                                Kind = Some "refactor"
                                                                Edit = Some { DocumentChanges = Some [| { Edits = es } |] } } |])) when
        es = edits
        ->
        ()
      | Ok other -> failtestf $"Should have converted positional DUs to named patterns, but instead generated %A{other}"
      | Error reason -> failtestf $"Should have succeeded, but failed with %A{reason}"
    }

  testList
    "convert positional DU match to named"
    [ testCaseAsync
        "in parenthesized let binding"
        (let patternPos =
          { Start = { Line = 2; Character = 9 }
            End = { Line = 2; Character = 10 } }

         let edits =
           [| { Range =
                  { Start = { Line = 2; Character = 7 }
                    End = { Line = 2; Character = 7 } }
                NewText = "a = " }
              { Range =
                  { Start = { Line = 2; Character = 8 }
                    End = { Line = 2; Character = 8 } }
                NewText = ";" }
              { Range =
                  { Start = { Line = 2; Character = 8 }
                    End = { Line = 2; Character = 9 } }
                NewText = "" }
              { Range =
                  { Start = { Line = 2; Character = 10 }
                    End = { Line = 2; Character = 10 } }
                NewText = "b = " }
              { Range =
                  { Start = { Line = 2; Character = 11 }
                    End = { Line = 2; Character = 11 } }
                NewText = ";" } |]

         expectEdits patternPos edits)
      testCaseAsync
        "in simple match"
        (let patternPos =
          { Start = { Line = 5; Character = 5 }
            End = { Line = 5; Character = 6 } }

         let edits =
           [| { Range =
                  { Start = { Line = 5; Character = 4 }
                    End = { Line = 5; Character = 4 } }
                NewText = "a = " }
              { Range =
                  { Start = { Line = 5; Character = 5 }
                    End = { Line = 5; Character = 5 } }
                NewText = ";" }
              { Range =
                  { Start = { Line = 5; Character = 5 }
                    End = { Line = 5; Character = 6 } }
                NewText = "" }
              { Range =
                  { Start = { Line = 5; Character = 7 }
                    End = { Line = 5; Character = 7 } }
                NewText = "b = " }
              { Range =
                  { Start = { Line = 5; Character = 8 }
                    End = { Line = 5; Character = 8 } }
                NewText = ";" } |]

         expectEdits patternPos edits)
      testCaseAsync
        "in parenthesized match"
        (let patternPos =
          { Start = { Line = 8; Character = 7 }
            End = { Line = 8; Character = 8 } }

         let edits =
           [| { Range =
                  { Start = { Line = 8; Character = 5 }
                    End = { Line = 8; Character = 5 } }
                NewText = "a = " }
              { Range =
                  { Start = { Line = 8; Character = 6 }
                    End = { Line = 8; Character = 6 } }
                NewText = ";" }
              { Range =
                  { Start = { Line = 8; Character = 6 }
                    End = { Line = 8; Character = 7 } }
                NewText = "" }
              { Range =
                  { Start = { Line = 8; Character = 8 }
                    End = { Line = 8; Character = 8 } }
                NewText = "b = " }
              { Range =
                  { Start = { Line = 8; Character = 9 }
                    End = { Line = 8; Character = 9 } }
                NewText = ";" } |]

         expectEdits patternPos edits)
      testCaseAsync
        "when there are new fields on the DU"
        (let patternPos =
          { Start = { Line = 12; Character = 29 }
            End = { Line = 12; Character = 30 } }

         let edits =
           [| { Range =
                  { Start = { Line = 12; Character = 28 }
                    End = { Line = 12; Character = 28 } }
                NewText = "a = " }
              { Range =
                  { Start = { Line = 12; Character = 29 }
                    End = { Line = 12; Character = 29 } }
                NewText = ";" }
              { Range =
                  { Start = { Line = 12; Character = 29 }
                    End = { Line = 12; Character = 30 } }
                NewText = "" }
              { Range =
                  { Start = { Line = 12; Character = 31 }
                    End = { Line = 12; Character = 31 } }
                NewText = "b = " }
              { Range =
                  { Start = { Line = 12; Character = 32 }
                    End = { Line = 12; Character = 32 } }
                NewText = ";" }
              { Range =
                  { Start = { Line = 12; Character = 32 }
                    End = { Line = 12; Character = 32 } }
                NewText = "c = _;" } |]

         expectEdits patternPos edits) ]

let tests state =
  testList
    "codefix tests"
    [ abstractClassGenerationTests state
      generateRecordStubTests state
      generateMatchTests state
      missingFunKeywordTests state
      outerBindingRecursiveTests state
      nameofInsteadOfTypeofNameTests state
      missingInstanceMemberTests state
      unusedValueTests state
      addExplicitTypeAnnotationTests state
      negationToSubstraction state
      removeUnusedBindingTests state
      positionalToNamedDUTests state ]
