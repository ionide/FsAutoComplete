module Utils.Tests.Server
open System
open Expecto
open Helpers
open FsAutoComplete
open FsAutoComplete.Lsp
open FsAutoComplete.LspHelpers
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Utils.ServerTests
open Utils.Server
open Utils.Utils
open FsToolkit.ErrorHandling
open FSharpx.Control

let tests state = testList (nameof(Server)) [
  testList "no root path" [
    testList "can get diagnostics" [
      let config =
        { defaultConfigDto with
            UnusedOpensAnalyzer = Some false
            UnusedDeclarationsAnalyzer = Some false
            SimplifyNameAnalyzer = Some false
        }
      serverTestList "no analyzers" state config None (fun server -> [
        testCaseAsync "can get nothing wrong" (async {
          let! (doc, diags) = server |> Server.createUntitledDocument ""
          use doc = doc
          Expect.isEmpty diags "There should be no diagnostics"

          for i in 1..5 do
            let! diags = doc |> Document.changeTextTo (string i)
            Expect.isEmpty diags "There should be no diagnostics"
        })
        testCaseAsync "can get single error" (async {
          let! (doc, diags) = server |> Server.createUntitledDocument "let foo = notdefined"
          use doc = doc
          Expect.hasLength diags 1 "There should be 1 error"
          Expect.exists diags (fun d -> d.Message.Contains "notdefined") ""
          let! diags = doc |> Document.changeTextTo "let bar = doesnotexist"
          Expect.hasLength diags 1 "There should be 1 error"
          Expect.exists diags (fun d -> d.Message.Contains "doesnotexist") ""
          let! diags = doc |> Document.changeTextTo "let baz = nope"
          Expect.hasLength diags 1 "There should be 1 error"
          Expect.exists diags (fun d -> d.Message.Contains "nope") ""
        })
        testCaseAsync "can get multiple errors" (async {
          let source = "let foo = {0}\nlet bar = {1}\nlet baz = {2}"
          let names = [|"notdefined"; "doesnotexist"; "nope"|]
          let fnames i = names |> Array.map (fun n -> sprintf "%s%i" n i)
          let fsource i = String.Format(source, fnames i |> Seq.cast<obj> |> Seq.toArray)

          let! (doc, diags) = server |> Server.createUntitledDocument (fsource 0)
          use doc = doc
          Expect.hasLength diags (names.Length) ""
          for name in fnames 0 do
            Expect.exists diags (fun d -> d.Message.Contains name) ""

          for i in 1..2 do
            let! diags = doc |> Document.changeTextTo (fsource i)
            Expect.hasLength diags (names.Length) ""
            for name in fnames i do
              Expect.exists diags (fun d -> d.Message.Contains name) ""
        })
      ])

      let config =
        { defaultConfigDto with
            UnusedOpensAnalyzer = Some false
            UnusedDeclarationsAnalyzer = Some true
            SimplifyNameAnalyzer = Some false
        }
      serverTestList "just unused decl analyzer" state config None (fun server -> [
        testCaseAsync "can get nothing wrong" <| (async {
          let! (doc, diags) = server |> Server.createUntitledDocument ""
          use doc = doc
          Expect.isEmpty diags "There should be no diagnostics"

          for i in 1..5 do
            let! diags = doc |> Document.changeTextTo (string i)
            Expect.isEmpty diags "There should be no diagnostics"
        })
        testCaseAsync "can get diags for single line" (async {
          let! (doc, diags) = server |> Server.createUntitledDocument "let foo = notdefined"
          use doc = doc
          Expect.hasLength diags 2 ""
          Expect.exists diags (fun d -> d.Message.Contains "notdefined") ""
          Expect.exists diags (fun d -> d.Message = "This value is unused") ""
          let! diags = doc |> Document.changeTextTo "let bar = doesnotexist"
          Expect.hasLength diags 2 ""
          Expect.exists diags (fun d -> d.Message.Contains "doesnotexist") ""
          Expect.exists diags (fun d -> d.Message = "This value is unused") ""
          let! diags = doc |> Document.changeTextTo "let baz = nope"
          Expect.hasLength diags 2 ""
          Expect.exists diags (fun d -> d.Message.Contains "nope") ""
          Expect.exists diags (fun d -> d.Message = "This value is unused") ""
        })
        testCaseAsync "can get diags for multiple lines" (async {
          let nVars = 3
          let values i = Array.init nVars (sprintf "someValue%i%i" i)
          let source i =
            values i
            |> Seq.mapi (sprintf "let var%i = %s")
            |> String.concat "\n"

          let! (doc, diags) = server |> Server.createUntitledDocument (source 0)
          use doc = doc
          Expect.hasLength diags (nVars * 2) ""
          values 0
          |> Array.iteri (fun i name ->
            Expect.exists diags (fun d -> d.Message.Contains name)  $"No diags with name {name}"
            Expect.exists diags (fun d -> d.Message = "This value is unused" && d.Range.Start.Line = i) $"No unused value error in line {i}"
          )

          for i in 1..2 do
            let! diags = doc |> Document.changeTextTo (source i)
            Expect.hasLength diags (nVars * 2) ""
            values i
            |> Array.iteri (fun i name ->
              Expect.exists diags (fun d -> d.Message.Contains name) $"No diags with name {name}"
              Expect.exists diags (fun d -> d.Message = "This value is unused" && d.Range.Start.Line = i) $"No unused value error in line {i}"
            )
        })
      ])

      let config =
        { defaultConfigDto with
            UnusedOpensAnalyzer = Some true
            UnusedDeclarationsAnalyzer = Some true
            SimplifyNameAnalyzer = Some true
        }
      serverTestList "three analyzers" state config None (fun server -> [
        testCaseAsync "can get nothing wrong" (async {
          let! (doc, diags) = server |> Server.createUntitledDocument ""
          use doc = doc
          Expect.isEmpty diags "There should be no diagnostics"

          for i in 1..5 do
            let! diags = doc |> Document.changeTextTo (string i)
            Expect.isEmpty diags "There should be no diagnostics"
        })
        testCaseAsync "can get all diags" (async {
          let source = "open System\nlet foo = bar\nSystem.String.Empty |> ignore"
          let! (doc, diags) = server |> Server.createUntitledDocument source
          use doc = doc

          Expect.hasLength diags 4 ""
          Expect.exists diags (fun d -> d.Message = "Unused open statement" && d.Range.Start.Line = 0) ""
          Expect.exists diags (fun d -> d.Message = "This value is unused" && d.Range.Start.Line = 1) ""
          Expect.exists diags (fun d -> d.Message.Contains "bar" && d.Range.Start.Line = 1) ""
          Expect.exists diags (fun d -> d.Message = "This qualifier is redundant" && d.Range.Start.Line = 2) ""


          let source = "open System.Collections\nlet baz = foo\nSystem.Collections.Generic.List() |> ignore"
          let! diags = doc |> Document.changeTextTo source

          Expect.hasLength diags 4 ""
          Expect.exists diags (fun d -> d.Message = "Unused open statement" && d.Range.Start.Line = 0) ""
          Expect.exists diags (fun d -> d.Message = "This value is unused" && d.Range.Start.Line = 1) ""
          Expect.exists diags (fun d -> d.Message.Contains "foo" && d.Range.Start.Line = 1) ""
          Expect.exists diags (fun d -> d.Message = "This qualifier is redundant" && d.Range.Start.Line = 2) ""


          let source = "open System.Diagnostics\nlet bar = baz\nSystem.Diagnostics.Debugger.IsAttached"
          let! diags = doc |> Document.changeTextTo source

          Expect.hasLength diags 4 ""
          Expect.exists diags (fun d -> d.Message = "Unused open statement" && d.Range.Start.Line = 0) ""
          Expect.exists diags (fun d -> d.Message = "This value is unused" && d.Range.Start.Line = 1) ""
          Expect.exists diags (fun d -> d.Message.Contains "baz" && d.Range.Start.Line = 1) ""
          Expect.exists diags (fun d -> d.Message = "This qualifier is redundant" && d.Range.Start.Line = 2) ""
        })
      ])
    ]

    testList "untitled document" [
      serverTestList "untitled counter in server for createUntitledDocument" state defaultConfigDto None (fun server -> [
        testCaseAsync "creating document increases untitled counter" (async {
          let! actualServer = server
          let preCounter = actualServer.UntitledCounter
          let! (doc, _) = server |> Server.createUntitledDocument ""
          use doc = doc
          let postCounter = actualServer.UntitledCounter

          Expect.isGreaterThan postCounter preCounter "Untitled Counter should increase"
        })
        testCaseAsync "creating multiple documents increases untitled counter" (async {
          let getCounter server = server |> Async.map (fun s -> s.UntitledCounter)

          let! preCounter = getCounter server
          let mutable preCounter = preCounter
          for i in 1..5 do
            let! (doc, _) = server |> Server.createUntitledDocument ""
            use doc = doc
            let! postCounter = getCounter server
            Expect.isGreaterThan postCounter preCounter "Untitled Counter should increase"
            preCounter <- postCounter
        })
      ])

      serverTestList "document version" state defaultConfigDto None (fun server -> [
        testCaseAsync "changing document text increases document version" (async {
          let! (doc, _) = server |> Server.createUntitledDocument ""
          let preVersion = doc.Version
          let! _ = doc |> Document.changeTextTo "42"
          let postVersion = doc.Version

          Expect.isGreaterThan postVersion preVersion "Document Version should increase"
        })
        testCaseAsync "changing document text multiple times should always increase document version" (async {
          let! (doc, _) = server |> Server.createUntitledDocument ""
          let mutable preVersion = doc.Version
          for _ in 1..5 do
            let! _ = doc |> Document.changeTextTo ""
            let postVersion = doc.Version
            Expect.isGreaterThan postVersion preVersion "Document Version should increase"
            preVersion <- postVersion
        })
      ])
    ]
  ]

  testList "with root path" [
    let inTestCases name =
      System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "TestCases", "ServerTests", name)
      |> Some

    let noAnalyzersConfig =
      { defaultConfigDto with
          UnusedOpensAnalyzer = Some false
          UnusedDeclarationsAnalyzer = Some false
          SimplifyNameAnalyzer = Some false
      }
    let allAnalyzersConfig =
      { defaultConfigDto with
          UnusedOpensAnalyzer = Some true
          UnusedDeclarationsAnalyzer = Some true
          SimplifyNameAnalyzer = Some true
      }
    serverTestList "dir with just a script and no analyzers" state noAnalyzersConfig (inTestCases "JustScript") (fun server -> [
      testCaseAsync "can load script file" (async {
        let! (doc, diags) = server |> Server.openDocument "Script.fsx"
        use doc = doc

        Expect.hasLength diags 1 "Should be one diagnostics"
        let diag = diags |> Array.head
        Expect.stringContains diag.Message "The value or constructor 'bar' is not defined." "Should be not defined error"
        Expect.equal diag.Range.Start.Line 0 "Error should be in line 1"
      })
      testCaseAsync "can load script file again" (async {
        let! (doc, diags) = server |> Server.openDocument "Script.fsx"
        use doc = doc

        Expect.hasLength diags 1 "Should be one diagnostics"
        let diag = diags |> Array.head
        Expect.stringContains diag.Message "The value or constructor 'bar' is not defined." "Should be not defined error"
        Expect.equal diag.Range.Start.Line 0 "Error should be in line 1"
      })
    ])
    serverTestList "dir with just a script and all anaylzers" state allAnalyzersConfig (inTestCases "JustScript") (fun server -> [
      testCaseAsync "can load script file" (async {
        let! (doc, diags) = server |> Server.openDocument "Script.fsx"
        use doc = doc

        Expect.exists diags (fun diag -> diag.Message.Contains "The value or constructor 'bar' is not defined." && diag.Range.Start.Line = 0) "Should be not defined error"
        Expect.exists diags (fun diag -> diag.Message.Contains "This value is unused" && diag.Range.Start.Line = 0) "Should be unused value"
      })
      testCaseAsync "can load script file again" (async {
        let! (doc, diags) = server |> Server.openDocument "Script.fsx"
        use doc = doc

        Expect.exists diags (fun diag -> diag.Message.Contains "The value or constructor 'bar' is not defined." && diag.Range.Start.Line = 0) "Should be not defined error"
        Expect.exists diags (fun diag -> diag.Message.Contains "This value is unused" && diag.Range.Start.Line = 0) "Should be unused value"
      })
    ])

    testSequenced <| testList "contesting" [
      serverTestList "dir with project and no analyzers" state noAnalyzersConfig (inTestCases "Project") (fun server -> [
        testCaseAsync "can load file in project" (async {
          let! (doc, diags) = server |> Server.openDocument "Other.fs"
          use doc = doc

          Expect.hasLength diags 1 "Should be one diagnostics"
          let diag = diags |> Array.head
          Expect.stringContains diag.Message "The value or constructor 'otherBar' is not defined." "Should be not defined error"
          Expect.equal diag.Range.Start.Line 5 "Error should be in line 6"
        })
        testCaseAsync "can load file in project again" (async {
          let! (doc, diags) = server |> Server.openDocument "Other.fs"
          use doc = doc

          Expect.hasLength diags 1 "Should be one diagnostics"
          let diag = diags |> Array.head
          Expect.stringContains diag.Message "The value or constructor 'otherBar' is not defined." "Should be not defined error"
          Expect.equal diag.Range.Start.Line 5 "Error should be in line 6"
        })
        testCaseAsync "can load other file in project" (async {
          let! (doc, diags) = server |> Server.openDocument "Program.fs"
          use doc = doc

          Expect.hasLength diags 1 "Should be one diagnostics"
          let diag = diags |> Array.head
          Expect.stringContains diag.Message "The value or constructor 'programBar' is not defined." "Should be not defined error"
          Expect.equal diag.Range.Start.Line 4 "Error should be in line 5"
        })
      ])
      serverTestList "dir with project and all analyzers" state allAnalyzersConfig (inTestCases "Project") (fun server -> [
        testCaseAsync "can load file in project" (async {
          let! (doc, diags) = server |> Server.openDocument "Other.fs"
          use doc = doc

          Expect.hasLength diags 1 "Should be one diagnostics"
          let diag = diags |> Array.head
          Expect.stringContains diag.Message "The value or constructor 'otherBar' is not defined." "Should be not defined error"
          Expect.equal diag.Range.Start.Line 5 "Error should be in line 6"
        })
        testCaseAsync "can load file in project again" (async {
          let! (doc, diags) = server |> Server.openDocument "Other.fs"
          use doc = doc

          Expect.hasLength diags 1 "Should be one diagnostics"
          let diag = diags |> Array.head
          Expect.stringContains diag.Message "The value or constructor 'otherBar' is not defined." "Should be not defined error"
          Expect.equal diag.Range.Start.Line 5 "Error should be in line 6"
        })
        testCaseAsync "can load other file in project" (async {
          let! (doc, diags) = server |> Server.openDocument "Program.fs"
          use doc = doc

          Expect.exists diags (fun diag -> diag.Message.Contains "The value or constructor 'programBar' is not defined." && diag.Range.Start.Line = 4) "Should be not defined error"
          // `argv`
          Expect.exists diags (fun diag -> diag.Message.Contains "This value is unused" && diag.Range.Start.Line = 11) "Should be unused value"
          Expect.exists diags (fun diag -> diag.Message.Contains "Unused open statement" && diag.Range.Start.Line = 2) "Should be unused open"
        })
      ])
    ]
  ]

  testList "Waiting for diagnostics" [
    let allAnalyzersConfig =
      { defaultConfigDto with
          UnusedOpensAnalyzer = Some true
          UnusedDeclarationsAnalyzer = Some true
          SimplifyNameAnalyzer = Some true
      }
    serverTestList "waitForLatestDiagnostics" state allAnalyzersConfig None (fun server -> [
      // `Document.waitForLatestDiagnostics` is crucial for success of tests: Must wait for newest, current Diagnostics, but ignore diags from previous parses.
      // Issues:
      // * must ignore old events
      // * multiple `publishDiagnostics` for each parse

      // Test in here: a script with a lot of Analyzer Diagnostics:
      //  Analyzers are checked after F# Compiler Checking is done (-> already one `publishDiagnostics`)
      //  After analyzers `documentAnalyzed` gets sent. But might arrive before analyzer diags.

      let genSource nCompilerErrorsPerRepeat nUnusedOpensPerRepeat nUnusedDeclsPerRepeat nSimplifyNamesPerRepeat repeats identifier =
        // generate source with lots of Analyzer Diagnostics (and F# compiler errors)
        // identifier to force some textual changes
        let nss = [|
          "System"
          "System.Diagnostics"
          "System.Text"
          "System.Text.RegularExpressions"
          "System.Threading"
          "System.Runtime"
          "FSharp.Control"
          "FSharp.Linq"
          "FSharp.Quotations"
          "FSharp.Reflection"
        |]
        let tys = [|
          "System.String"
          "System.Index"
          "System.Int32"
          "System.Random"
          "System.Guid"
          "System.Text.RegularExpressions.Regex"
          "System.Text.RegularExpressions.Match"
          "System.Text.StringBuilder"
          "System.Diagnostics.TraceLevel"
          "System.Diagnostics.Stopwatch"
        |]

        let lines = [
          $"// {identifier}"
          for i in 1..repeats do
            $"// Rep {i}"
            for j in 1..nUnusedOpensPerRepeat do
              let o = Array.get nss ((j-1) % nss.Length)
              $"open {o}"

            for j in 1..nUnusedDeclsPerRepeat do
              $"let {identifier}Rep{i}Val{j} = 0"

            // note: requires at least 4 UnusedOpens (to `open ...` required for Simplify Name)
            for j in 1..nSimplifyNamesPerRepeat do
              let ty = Array.get tys ((j-1) % tys.Length )
              $"let {identifier}Rep{i}F{j} (v: {ty}) = v"

            // `let _identifier = value`:
            // * value not defined
            // * no unused warning because `_`
            for j in 1..nCompilerErrorsPerRepeat do
              $"let _{identifier}ErrorRep{i}Val{j} = valueRep{i}Val{j}"

            ""
        ]

        String.concat "\n" lines

      testCaseAsync "lots of diagnostics for all analyzers" (async {
        // count for each: n * repeats
        let nCompilerErrorsPerRepeat = 3
        let nUnusedOpensPerRepeat = 7
        let nUnusedDeclsPerRepeat = 5
        let nSimplifyNamesPerRepeat = 9
        let calcExpected repeats =
          {|
            UnusedOpens = nUnusedOpensPerRepeat * repeats
            UnusedDecls = nUnusedDeclsPerRepeat * repeats
            SimplifyNames = nSimplifyNamesPerRepeat * repeats
            CompilerErrors = nCompilerErrorsPerRepeat * repeats
          |}

        let repeats = 2
        let source = genSource nCompilerErrorsPerRepeat nUnusedOpensPerRepeat nUnusedDeclsPerRepeat nSimplifyNamesPerRepeat repeats "init"
        let! (doc, diags) = server |> Server.createUntitledDocument source
        use doc = doc

        let checkDiags repeats loop diags =
          let expected = calcExpected repeats
          let groups =
            diags
            |> Array.map (fun d ->
                // simplify `The value or constructor 'value' is not defined.` error (contains names and recommendations)
                if d.Code = Some "39" then
                  "The value or constructor is not defined"
                else
                  d.Message
            )
            |> Array.countBy id
            |> Map.ofArray
          let actual = {|
            UnusedOpens = groups.["Unused open statement"]
            UnusedDecls = groups.["This value is unused"]
            SimplifyNames = groups.["This qualifier is redundant"]
            CompilerErrors = groups.["The value or constructor is not defined"]
          |}

          // exact count isn't actually that important because each analyzers sends all its diags together.
          // important part is just: has arrived -> `waitForLatestDiagnostics` waited long enough for all diags
          Expect.equal actual expected $"Incorrect dags in loop {loop}"

        checkDiags repeats 0 diags

        for i in 1..5 do
          let repeats = repeats + i // to get different numbers of diagnostics
          let source = genSource nCompilerErrorsPerRepeat nUnusedOpensPerRepeat nUnusedDeclsPerRepeat nSimplifyNamesPerRepeat repeats $"loop{i}"
          let! diags = doc |> Document.changeTextTo source
          checkDiags repeats i diags
      })

      testCaseAsync "diagnostics for some analyzers" (async {
        let checkDiags (unusedOpen, unusedValue, simplifyName) diags =
          let actual = {|
            UnusedOpen = diags |> Array.exists (fun d -> d.Message =  "Unused open statement")
            UnusedDecl = diags |> Array.exists (fun d -> d.Message =  "This value is unused")
            SimplifyName = diags |> Array.exists (fun d -> d.Message =  "This qualifier is redundant")
          |}
          let expected = {|
            UnusedOpen = unusedOpen
            UnusedDecl = unusedValue
            SimplifyName = simplifyName
          |}

          Expect.equal actual expected "Should contain correct diagnostics"

        let source = Text.trimTripleQuotation """
open System
open System.Diagnostics
open System.Text

let x = 1
let y = 2
let z = 3
        """
        let! (doc, diags) = server |> Server.createUntitledDocument source
        use doc = doc
        checkDiags (true, true, false) diags

        let source = Text.trimTripleQuotation """
let x = 1
let y = 2
let z = 3
        """
        let! diags = doc |> Document.changeTextTo source
        checkDiags (false, true, false) diags

        let source = Text.trimTripleQuotation """
open System
open System.Diagnostics
open System.Text

()
        """
        let! diags = doc |> Document.changeTextTo source
        checkDiags (true, false, false) diags

        let source = Text.trimTripleQuotation """
open System
open System.Diagnostics
open System.Text

let f (v: System.String) = v
        """
        let! diags = doc |> Document.changeTextTo source
        checkDiags (true, false, true) diags

        let source = Text.trimTripleQuotation """
open System
open System.Diagnostics
open System.Text

let f (v: System.String) = ()

        """
        let! diags = doc |> Document.changeTextTo source
        checkDiags (true, true, true) diags

        let source = "()"
        let! diags = doc |> Document.changeTextTo source
        checkDiags (false, false, false) diags
      })
    ])
  ]

  testList "timing" [
    let allAnalyzersConfig =
      { defaultConfigDto with
          UnusedOpensAnalyzer = Some true
          UnusedDeclarationsAnalyzer = Some true
          SimplifyNameAnalyzer = Some true
      }
    let mkSource (msg: string) = Text.trimTripleQuotation  $"""
open System

// {msg}
let foo = "bar"
let bar = 42
let (x,y,z) = (1,2,3) // {msg}

let f1 (v: string) = sprintf "Hello %%s" v
let f2 v = sprintf "Hello %%s"  // {msg}

f1 foo
// {msg}
f2 "bar" |> ignore
    """
    serverTestList "server" state allAnalyzersConfig None (fun server -> [
      testList "single parse" [
        testCaseAsync "single parse" <| async {
          let! (doc, _) = server |> Server.createUntitledDocument (mkSource "single parse")
          use doc  = doc
          ()
        }
      ]
      testList "parse of same document" [
        testCaseAsync "single doc" <| async {
          let! (doc, _) = server |> Server.createUntitledDocument (mkSource "0 parse")
          use doc  = doc

          for i in 1..5 do
            let! _ = doc |> Document.changeTextTo (mkSource $"Parse {i}")
            ()
          ()
        }
      ]
      testList "parse in different documents" [
        for i in 0..5 do
          testCaseAsync $"doc {i}" <| async {
            let! (doc, _) = server |> Server.createUntitledDocument (mkSource "parse {i}")
            use doc  = doc
            ()
          }
      ]
    ])
  ]

  testList "Document" [
    serverTestList "no root path without analyzers" state defaultConfigDto None (fun server -> [
      testCaseAsync "can create Document by absolute path without root path" <| async {
        let relativePath = "../TestCases/ServerTests/JustScript/Script.fsx"
        let absolutePath = System.IO.Path.GetFullPath(System.IO.Path.Combine(__SOURCE_DIRECTORY__, relativePath))
        let! (doc, _) = server |> Server.openDocument absolutePath
        use doc = doc
        ()
      }

      let mutable docState = {| Uri = ""; Version = -1; CallCounter = 0 |}
      let getDoc server = async {
        let text = Text.trimTripleQuotation """
          let bar = "hello world"
          let foo = System.String.
        """
        let! (doc, diags) = server |> Server.createUntitledDocument text
        docState <- {|
          Uri = doc.Uri
          Version = doc.Version
          // tracks how often `getDoc` was called
          CallCounter = docState.CallCounter + 1
        |}
        return (doc, diags)
      }
      documentTestList "multiple actions on single document" server getDoc (fun doc -> [
        testCaseAsync "doc is doc returned from getDocument" <| async {
          let! (doc,_) = doc
          Expect.equal (doc.Uri, doc.Version) (docState.Uri, docState.Version) "Should be same doc"
          Expect.equal docState.CallCounter 1 "getDocument should only be called once"
        }
        testCaseAsync "doc stays same" <| async {
          let! (doc,_) = doc
          Expect.equal (doc.Uri, doc.Version) (docState.Uri, docState.Version) "Should be same doc"
          Expect.equal docState.CallCounter 1 "getDocument should only be called once"
        }
        let completionAt pos (doc: Document) = async {
          let ps: CompletionParams = {
            TextDocument = doc.TextDocumentIdentifier
            Position = pos
            Context = None
          }
          let! res = doc.Server.Server.TextDocumentCompletion ps
          Expect.isOk res "Should be ok result"
          return res |> Result.defaultWith (fun _ -> failtest "unreachable")
        }
        testCaseAsync "can get completions" <| async {
          let! (doc,_) = doc
          let! completions = doc |> completionAt { Line = 1; Character = 24 }
          Expect.isSome completions "Should be some completions"
          let completions = completions.Value
          Expect.isNonEmpty completions.Items "Should be completions"
          Expect.exists completions.Items (fun i -> i.Label = "IsNullOrWhiteSpace") "Should have `IsNullOrWhiteSpace` completion"
        }
        testCaseAsync "can get completions again" <| async {
          let! (doc,_) = doc
          let! completions = doc |> completionAt { Line = 1; Character = 24 }
          Expect.isSome completions "Should be some completions"
          let completions = completions.Value
          Expect.isNonEmpty completions.Items "Should be completions"
          Expect.exists completions.Items (fun i -> i.Label = "IsNullOrWhiteSpace") "Should have `IsNullOrWhiteSpace` completion"
        }
        testCaseAsync "can get signature help" <| async {
          let! (doc,_) = doc
          let ps: TextDocumentPositionParams = {
            TextDocument = doc.TextDocumentIdentifier
            Position = { Line = 0; Character = 6 }
          }
          let! res = doc.Server.Server.TextDocumentHover ps
          Expect.isOk res "Should have hover data"
        }
        testCaseAsync "doc is still same" <| async {
          let! (doc,_) = doc
          Expect.equal (doc.Uri, doc.Version) (docState.Uri, docState.Version) "Should be same doc"
          Expect.equal docState.CallCounter 1 "getDocument should only be called once"
        }
      ])
    ])
  ]
]
