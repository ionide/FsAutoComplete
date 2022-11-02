module FsAutoComplete.Tests.FindReferences

open Expecto
open System.IO
open FsAutoComplete
open Helpers
open Ionide.LanguageServerProtocol.Types
open Utils.ServerTests
open Utils.Server
open Utils.Utils
open Utils.TextEdit
open System.Collections.Generic
open FSharp.UMX
open FsAutoComplete.LspHelpers.Conversions
open FsToolkit.ErrorHandling
open FSharp.Compiler.CodeAnalysis
open Helpers.Expecto.ShadowedTimeouts

let private scriptTests state = 
  testList "script"
    [ let server =
        async {
          let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FindReferences", "Script")

          let! (server, event) = serverInitialize path defaultConfigDto state
          do! waitForWorkspaceFinishedParsing event
          let scriptPath = Path.Combine(path, "Script.fsx")

          let tdop: DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }

          do! server.TextDocumentDidOpen tdop
          return server, scriptPath
        }
        |> Async.Cache

      testCaseAsync
        "Can find references for foo identifier in script"
        (async {
          let! server, scriptPath = server

          let request: ReferenceParams =
            { TextDocument = { Uri = Path.FilePathToUri scriptPath }
              Position = { Line = 2; Character = 0 } // beginning of the usage of the `foo` function
              Context = { IncludeDeclaration = true } } // beginning of the usage of the `foo` function

          let! response = server.TextDocumentReferences request

          match response with
          | Ok None -> failtestf "Should have gotten some references for this identifier"
          | Error e -> failtestf "Errored while getting references for identifier: %A" e
          | Ok (Some references) ->
            Expect.hasLength references 2 "Should have a reference for the definition and usage"
            let reference = references.[0]
            Expect.stringEnds reference.Uri (Path.GetFileName scriptPath) "should point to the same script"

            Expect.equal
              reference.Range
              { Start = { Line = 0; Character = 4 }
                End = { Line = 0; Character = 7 } }
              "should point to the definition of `foo`"
        }) ]

module private Cursor =
  let cursor = Cursor.Marker
  let usageStart = "$<"
  let usageEnd = ">$"
  let defStart = "$D<"
  let defEnd = ">D$"
  
let private extractRanges (sourceWithCursors: string) =
  let (source, cursors) =
    sourceWithCursors
    |> Text.trimTripleQuotation
    |> Cursors.extractWith [| Cursor.cursor; Cursor.usageStart; Cursor.usageEnd; Cursor.defStart; Cursor.defEnd |]

  let cursor, cursors =
    let cs, cursors =
      cursors
      |> List.partition (fst >> (=) Cursor.cursor)
    if cs |> List.isEmpty then
      (None, cursors)
    else
      Expect.hasLength cs 1 "There should be either 0 or 1 cursor $0"
      (Some (snd cs[0]), cursors)

  let mkRange start fin = {
    Start = start
    End = fin
  }

  let rec collectRanges (cursors: (string*Position) list) ((decls, usages) as ranges) =
    match cursors with
    | [] -> ranges
    | [(c,p)] -> failwith $"Lonely last cursor {c} at {p}"
    | (c1,p1)::(c2,p2)::cursors when c1 = Cursor.usageStart && c2 = Cursor.usageEnd -> 
        let range = mkRange p1 p2
        let ranges = (range :: decls, usages)
        collectRanges cursors ranges
    | (c1,p1)::(c2,p2) :: cursors when c1 = Cursor.defStart && c2 = Cursor.defEnd -> 
        let range = mkRange p1 p2
        let ranges = (decls, range :: usages)
        collectRanges cursors ranges
    | (c1,p1)::(c2,p2):: _ ->
        failwith $"Cursor pair {c1} & {c2} do not match (at {p1} & {p2})"
  let (decls, usages) = collectRanges cursors ([],[])
  source, {|
    Cursor = cursor
    Declarations = decls |> List.rev |> List.toArray
    Usages = usages |> List.rev |> List.toArray
  |}
let private mkLocation doc range =
  {
    Uri = doc.Uri
    Range = range
  }
/// mark locations in text
/// -> differences gets highlighted in source instead of Location array
/// 
/// Locations are marked with `〈...〉`
let private markRanges (source: string) (locs: Location[]) =
  let ranges =
    locs
    |> Array.map (fun l -> l.Range)
    |> Array.sortByDescending (fun r -> (r.Start.Line, r.Start.Character))
  ranges
  |> Array.fold (fun source range ->
      source
      |> Text.insert range.End "〉"
      |> Flip.Expect.wantOk "Should be valid insert position"
      |> Text.insert range.Start "〈"
      |> Flip.Expect.wantOk "Should be valid insert position"
  ) source

module Expect =
  /// `exact`:
  /// * `true`: ranges of `actual` & `expected` must match exactly
  /// * `false`: ranges of `actual` must contain ranges of `expected` -> ranges in `actual` can cover a larger area
  ///     * Reason: ranges only get adjusted iff source file was loaded (into `state` in `Commands`).
  ///           Because of background loading (& maybe changes in FSAC implementation) not always predictable what is and isn't loaded  
  ///           -> Just check if correct range is covered  
  ///     * Example: Find References for `map`: FCS returns range covering `List.map`.  
  ///           That range gets reduced to just `map` iff source file is loaded (-> NamedText available).
  ///           But if file not loaded range stays `List.map`.  
  /// 
  /// -> 
  /// * Solution tests: multiple projects -> not everything loaded -> use `exact=false`  
  ///     -> tests for: every reference found?
  /// * untitled & range tests: in untitled doc -> loaded because passed to FSAC -> use `exact=true`  
  ///     -> tests for: correct range found?
  let locationsEqual (getSource: DocumentUri -> string) (exact: bool) (actual: Location[]) (expected: Location[]) =
    let inspect () =
      // Note: simplification: only find 1st doc that differs
      let actualByUri, expectedByUri =
        (
          actual |> Array.groupBy (fun l -> l.Uri) |> Array.sortBy fst,
          expected |> Array.groupBy (fun l -> l.Uri) |> Array.sortBy fst
        )
      // cannot directly use zip: might be unequal number of docs
      Expect.sequenceEqual (actualByUri |> Array.map fst) (expectedByUri |> Array.map fst) "Should find references in correct docs"
      // from here on: actualByUri & expectedByUri have same length and same docs in same order (because sorted)

      for ((uri, actual), (_, expected)) in Array.zip actualByUri expectedByUri do
        let source = getSource uri

        if exact then
          Expect.equal (markRanges source actual) (markRanges source expected) $"Should find correct & exact references in doc %s{uri}"
        else
          let actual = actual |> Array.sortBy (fun l -> l.Range.Start)
          let expected = expected |> Array.sortBy (fun l -> l.Range.Start)
          // actual & expected might have different length
          // again: find only first difference

          let exactDisclaimer = "\nNote: Ranges in actual might be longer than in expected. That's ok because `exact=false`\n"

          if actual.Length <> expected.Length then
            let msg = $"Found %i{actual.Length} references in doc %s{uri}, but expected %i{expected.Length} references.%s{exactDisclaimer}"
            // this fails -> used for pretty printing of diff
            Expect.equal (markRanges source actual) (markRanges source expected) msg

          for (i, (aLoc, eLoc)) in Seq.zip actual expected |> Seq.indexed do
            // expected must fit into actual
            let inside =
              aLoc.Range |> Range.containsStrictly eLoc.Range.Start
              &&
              aLoc.Range |> Range.containsStrictly eLoc.Range.End

            if not inside then
              let msg = $"%i{i}. reference inside %s{uri} has incorrect range.%s{exactDisclaimer}"
              Expect.equal (markRanges source [|aLoc|]) (markRanges source [|eLoc|]) msg


    if exact then
      try
        Expect.sequenceEqual actual expected "Should find all references with correct range"
      with
      | :? AssertException  ->
        // pretty printing: Source with marked locations instead of lists with locations
        inspect ()
    else
      inspect ()
    
let private solutionTests state =

  let marker = "//>"
  /// Format of Locations in file `path`:
  /// In line after range:
  /// * Mark start of line with `//>`
  /// * underline range (in prev line) with a char-sequence (like `^^^^^`)  
  /// * name after range marker (separated with single space from range) (name can contain spaces)
  ///   -> results are grouped by named
  ///  * no name: assigned empty string as name
  /// 
  /// Example:
  /// ```fsharp
  /// let foo bar =
  /// //>     ^^^ parameter
  ///     42 + bar
  /// //>      ^^^ parameter usage
  /// let alpha beta = 
  /// //>       ^^^^ parameter
  ///     beta + 42
  /// //> ^^^^ parameter usage
  /// ```
  /// -> 4 locations, two `parameter` and two `parameter usage`
  /// 
  /// Note: it's currently not possible to get two (or more) ranges for a single line!
  let readReferences path =
    let lines = File.ReadAllLines path
    let refs = Dictionary<string, IList<Location>>()
    for i in 0..(lines.Length-1) do
      let line = lines[i].TrimStart()
      if line.StartsWith marker then
        let l = line.Substring(marker.Length).Trim()
        let splits = l.Split([|' '|], 2)
        let mark = splits[0]
        let ty = mark[0]
        let range = 
          let col = line.IndexOf mark
          let length = mark.Length
          let line = i - 1  // marker is line AFTER actual range
          {
            Start = { Line = line; Character = col }
            End = { Line = line; Character = col + length }
          }
        let loc = {
          Uri =
            path
            |> normalizePath
            |> Path.LocalPathToUri
          Range = range
        }
        let name = 
          if splits.Length > 1 then
            splits[1]
          else
            ""

        if not (refs.ContainsKey name) then
          refs[name] <- List<_>()

        let existing = refs[name]
        // Note: we're currently dismissing type (declaration, usage)
        existing.Add loc |> ignore
    refs

  let readAllReferences dir = 
    // `.fs` & `.fsx`
    let files = Directory.GetFiles(dir, "*.fs*", SearchOption.AllDirectories)
    files
    |> Seq.map readReferences
    |> Seq.map (fun dict -> 
      dict
      |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    )
    |> Seq.collect id
    |> Seq.groupBy fst
    |> Seq.map (fun (name, locs) -> (name, locs |> Seq.map snd |> Seq.collect id |> Seq.toArray))
    |> Seq.map (fun (name, locs) -> {| Name=name; Locations=locs |})
    |> Seq.toArray


  let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FindReferences", "Solution")
  serverTestList "solution" state defaultConfigDto (Some path) (fun server -> [
    // extra function instead of (just) testCase in front: to be able to run single test case
    let mutable scriptFilesLoaded = false
    let assertScriptFilesLoaded = async {
      if not scriptFilesLoaded then
        let files = Directory.GetFiles(path, "*.fsx", SearchOption.AllDirectories)
        for file in files do
          let relativePath = Path.GetRelativePath(path, file)
          let! (doc, _) = server |> Server.openDocument relativePath
          do! doc |> Document.close
        scriptFilesLoaded <- true
    }
    testCaseAsync "open script files" (async {
      // script files aren't loaded in background
      // -> cannot find references in unopened script files

      // -> load script files (don't need to be kept open -- just loaded once -> in FSAC cache)
      do! assertScriptFilesLoaded

      //Enhancement: implement auto-load (like for projects)?
    })

    let mainDoc = Path.Combine("B", "WorkingModule.fs")
    documentTestList "inside B/WorkingModule.fs" server (Server.openDocument mainDoc) (fun doc -> [
      let refs = readAllReferences path
      for r in refs do
        testCaseAsync r.Name (async {
          do! assertScriptFilesLoaded

          let! (doc, _) =  doc
          let cursor = 
            let cursor =
              r.Locations
              |> Seq.filter (fun l -> l.Uri = doc.Uri) 
              |> Seq.minBy (fun l -> l.Range.Start)
            cursor.Range.Start

          let request: ReferenceParams =
            { TextDocument = doc.TextDocumentIdentifier
              Position = cursor
              Context = { IncludeDeclaration = true } }
          let! refs = doc.Server.Server.TextDocumentReferences request
          let refs = 
            refs
            |> Flip.Expect.wantOk "Should not fail"
            |> Flip.Expect.wantSome "Should return references"

          let expected = r.Locations

          let getSource uri =
            let path = Path.FileUriToLocalPath uri
            File.ReadAllText path
          
          Expect.locationsEqual getSource false refs expected
        })
    ])
  ])

/// multiple untitled files (-> all docs are unrelated)
/// -> Tests for external symbols (-> over all docs) & symbol just in current doc (-> no matches in other unrelated docs)
let private untitledTests state =
  serverTestList "untitled" state defaultConfigDto None (fun server -> [
    testCaseAsync "can find external `Delay` in all open untitled docs" (async {
      // Note: Cursor MUST be in first source
      let sources = 
        [|
          """
          open System
          open System.Threading.Tasks
          let _ = task {
            do! Task.$<De$0lay>$ (TimeSpan.MaxValue)
            do! Task.$<``Delay``>$ (TimeSpan.MaxValue)
            do! System.Threading.Tasks.Task.$<Delay>$ (TimeSpan.MaxValue)
            do! 
              System
                .Threading
                .Tasks
                .Task
                .$<Delay>$ (TimeSpan.MaxValue)
          } 
          """
          """
          open System
          open System.Threading.Tasks
          let _ = task {
            do! Task.$<Delay>$ (TimeSpan.MaxValue)
            printfn "..."
            do! Threading.Tasks.Task.$<Delay>$ (TimeSpan.MaxValue)
          }
          let _ = task {
            do! Task.$<Delay>$ (TimeSpan.MaxValue)
          }
          """
          // No Task.Delay
          """
          open System
          printfn "do stuff"
          """
        |]
        |> Array.map (extractRanges)

      let! docs =
        sources
        |> Seq.map fst
        |> Seq.map (fun source -> async {
            let! (doc, diags) = server |> Server.createUntitledDocument source
            Expect.hasLength diags 0 $"There should be no diags in doc {doc.Uri}"
            return doc
          })
        |> Async.Sequential
      
      let (cursorDoc, cursor) = 
        let cursors =
          Array.zip docs sources
          |> Array.choose (fun (doc, (_, cursors)) -> 
            cursors.Cursor
            |> Option.map (fun cursor -> (doc, cursor))
          )
        Expect.hasLength cursors 1 "There should be exactly one cursor"
        cursors[0]
      let request: ReferenceParams =
        { TextDocument = cursorDoc.TextDocumentIdentifier
          Position = cursor
          Context = { IncludeDeclaration = true } }
      let! refs = cursorDoc.Server.Server.TextDocumentReferences request
      let refs =
        refs
        |> Flip.Expect.wantOk "Should not fail"
        |> Flip.Expect.wantSome "Should return references"

      let expected =
        Array.zip docs sources
        |> Array.collect (fun (doc, (_, cursors)) ->
            Array.append cursors.Declarations cursors.Usages
            |> Array.map (mkLocation doc)
        )

      let getSource uri =
        let i = docs |> Array.findIndex (fun doc -> doc.Uri = uri)
        fst sources[i]

      Expect.locationsEqual getSource true refs expected

    })
  ])

/// Tests to check references span the correct range. For example: `Delay`, not `Task.Delay`
let private rangeTests state =
  let checkRanges
    server
    sourceWithCursors
    = async {
      let (source, cursors) =
        sourceWithCursors
        |> extractRanges
      let! (doc, diags) = server |> Server.createUntitledDocument source
      use doc = doc
      Expect.hasLength diags 0 "There should be no diags"

      let request: ReferenceParams =
        { TextDocument = doc.TextDocumentIdentifier
          Position = cursors.Cursor.Value
          Context = { IncludeDeclaration = true } }
      let! refs = doc.Server.Server.TextDocumentReferences request
      let refs = 
        refs
        |> Flip.Expect.wantOk "Should not fail"
        |> Flip.Expect.wantSome "Should return references"
        |> Array.sortBy (fun l -> l.Range.Start)

      Expect.all refs (fun r -> r.Uri = doc.Uri) "there should only be references in current doc"

      let expected =
        Array.append cursors.Declarations cursors.Usages
        |> Array.sortBy (fun r -> r.Start)
        |> Array.map (mkLocation doc)

      // Expect.sequenceEqual refs expected "Should find all refs with correct range"
      if refs <> expected then
        Expect.equal (markRanges source refs) (markRanges source expected) "Should find correct references"
    }
  serverTestList "range" state defaultConfigDto None (fun server -> [
    testCaseAsync "can get range of variable" <|
      checkRanges server
        """
        module MyModule =
          let $D<va$0lue>D$ = 42
        
        open MyModule
        let _ = $<value>$ + 42
        let _ = $<``value``>$ + 42
        let _ = MyModule.$<value>$ + 42
        let _ = MyModule.$<``value``>$ + 42
        """
    testCaseAsync "can get range of external function" <|
      checkRanges server
        """
        open System
        open System.Threading.Tasks
        let _ = task {
          do! Task.$<De$0lay>$ (TimeSpan.MaxValue)
          do! Task.$<``Delay``>$ (TimeSpan.MaxValue)
          do! System.Threading.Tasks.Task.$<Delay>$ (TimeSpan.MaxValue)
          do! 
            System
              .Threading
              .Tasks
              .Task
              .$<Delay>$ (TimeSpan.MaxValue)
        } 
        """
    testCaseAsync "can get range of variable with required backticks" <|
      checkRanges server
        """
        module MyModule =
          let $D<``hello$0 world``>D$ = 42

        open MyModule
        let _ = $<``hello world``>$ + 42
        let _ = MyModule.$<``hello world``>$ + 43
        """
    testCaseAsync "can get range of operator" <|
      // Note: Parens aren't part of result range
      //       Reason: range returned by FCS in last case (with namespace) contains opening paren, but not closing paren
      checkRanges server
        """
        let _ = 1 $0$<+>$ 2
        let _ = ($<+>$) 1 2
        let _ = Microsoft.FSharp.Core.Operators.($<+>$) 1 2
        """
    testCaseAsync "can get range of full Active Pattern" <|
      // Active Pattern is strange: all together are single symbol, but each individual too
      // * get references on `(|Even|Odd|)` -> finds exactly `(|Even|Odd|)`
      // * get references on `Even` -> finds single `Even` and `Even` inside Declaration `let (|Even|Odd|)`, but not usage `(|Even|Odd|)`
      //
      // Note: Find References in FCS return range with Namespace, Module, Type -> Find Refs for `XXXX` -> range is `MyModule.XXXX`
      // Note: When XXXX in parens and Namespace, FCS returns range including opening paren, but NOT closing paren `MyModule.(XXXX` (happens for operators)
      checkRanges server
        """
        module MyModule =
          let ($D<|Ev$0en|Odd|>D$) value =
            if value % 2 = 0 then Even else Odd

        open MyModule
        let _ = ($<|Even|Odd|>$) 42
        let _ = MyModule.($<|Even|Odd|>$) 42
        let _ =
          MyModule
            .($<|Even|
                Odd|>$) 42
        let _ =
          match 42 with
          | Even -> ()
          | Odd -> ()
        let _ =
          match 42 with
          | MyModule.Even -> ()
          | MyModule.Odd -> ()
        """
    testCaseAsync "can get range of partial Active Pattern (Even)" <|
      // Note: `Even` is found in Active Pattern declaration (`let (|Even|Odd|) = ...`)
      //       but NOT in usage of Full Active Pattern Name (`(|Even|Odd|)`)
      checkRanges server
        """
        module MyModule =
          let (|$D<Even>D$|Odd|) value =
            if value % 2 = 0 then $<Even>$ else Odd

        open MyModule
        let _ = (|Even|Odd|) 42
        let _ = MyModule.(|Even|Odd|) 42
        let _ =
          MyModule
            .(|Even|
                Odd|) 42
        let _ =
          match 42 with
          | $<Ev$0en>$ -> ()
          | Odd -> ()
        let _ =
          match 42 with
          | MyModule.$<Even>$ -> ()
          | MyModule.Odd -> ()
        """
    testCaseAsync "can get range of type for static function call" <|
      checkRanges server
        """
        open System
        open System.Threading.Tasks
        let _ = task {
          do! $<Ta$0sk>$.Delay(TimeSpan.MaxValue)
          do! $<Task>$.``Delay`` (TimeSpan.MaxValue)
          do! System.Threading.Tasks.$<Task>$.Delay (TimeSpan.MaxValue)
          do! 
            System
              .Threading
              .Tasks
              .$<Task>$
              .Delay (TimeSpan.MaxValue)
        }
        """
  ])

let tests state = testList "Find All References tests" [
  scriptTests state
  solutionTests state
  untitledTests state
  rangeTests state
]


let tryFixupRangeTests = testList (nameof Tokenizer.tryFixupRange) [
  let checker = lazy (FSharpChecker.Create())
  let getSymbolUses source cursor = async {
    let checker = checker.Value
    let file = "/code.fsx"
    let path: string<LocalPath> = UMX.tag file
    let source = NamedText(path, source)

    let! (projOptions, _) = checker.GetProjectOptionsFromScript(file, source)
    let! (parseResults, checkResults) = checker.ParseAndCheckFileInProject(file, 0, source, projOptions)
    // Expect.isEmpty parseResults.Diagnostics "There should be no parse diags"
    Expect.hasLength parseResults.Diagnostics 0 "There should be no parse diags"
    let checkResults =
      match checkResults with
      | FSharpCheckFileAnswer.Succeeded checkResults -> checkResults
      | _ -> failtest "CheckFile aborted"
    // Expect.isEmpty checkResults.Diagnostics "There should be no check diags"
    Expect.hasLength checkResults.Diagnostics 0 "There should be no check diags"
    let line = source.Lines[cursor.Line]
    let (col, idents) =
      Lexer.findIdents cursor.Character line SymbolLookupKind.Fuzzy
      |> Flip.Expect.wantSome "Should find idents"
    let symbolUse =
      checkResults.GetSymbolUseAtLocation(cursor.Line + 1, col, line, List.ofArray idents)
      |> Flip.Expect.wantSome "Should find symbol"

    let! ct = Async.CancellationToken
    let usages = checkResults.GetUsesOfSymbolInFile(symbolUse.Symbol, ct)

    return (source, symbolUse.Symbol, usages)
  }

  /// Markers:
  /// * Cursor: `$0`
  /// * Ranges: Inside `$<` ... `>$`
  let extractCursorAndRanges sourceWithCursorAndRanges =
    let (source, cursors) =
      sourceWithCursorAndRanges
      |> Text.trimTripleQuotation
      |> Cursors.extractWith [| "$0"; "$<"; ">$"|]
    let (cursor, cursors) =
      let (c, cs) =
        cursors
        |> List.partition (fst >> (=) "$0")
      let c = c |> List.map snd
      Expect.hasLength c 1 "There should be exactly 1 cursor (`$0`)"
      (c[0], cs)

    let rec collectRanges cursors ranges =
      match cursors with
      | [] -> List.rev ranges
      | ("$<", start)::(">$", fin)::cursors ->
          let range = {
            Start = start
            End = fin
          }
          collectRanges cursors (range::ranges)
      | _ ->
          failtest $"Expected matching range pair '$<', '>$', but got: %A{cursors}"
    let ranges = 
      collectRanges cursors []

    (source, cursor, ranges)

  let check includeBackticks sourceWithCursorAndRanges = async {
    let (source, cursor, expected) = extractCursorAndRanges sourceWithCursorAndRanges

    let! (source, symbol, usages) = getSymbolUses source cursor
    
    let symbolNameCore = symbol.DisplayNameCore
    let actual =
      usages
      |> Seq.map (fun u ->
        Tokenizer.tryFixupRange(symbolNameCore, u.Range, source, includeBackticks)
        |> Option.ofValueOption
        |> Flip.Expect.wantSome $"Should be able to fixup usage '%A{u}'"
      )
      |> Seq.map fcsRangeToLsp
      |> Seq.toArray
      |> Array.sortBy (fun r -> (r.Start.Line, r.Start.Character))

    let expected = expected |> Array.ofList

    // Expect.equal actual expected "Should be correct range"
    if actual <> expected then
      // mark ranges for visual diff instead of range diff
      let markRanges (ranges: Range[]) =
        let locs =
          ranges
          |> Array.map (fun r ->
              {
                Uri = ""
                Range = r
              }
          )
        let marked = markRanges source.String locs
        // append range strings for additional range diff
        let rangeStrs =
          ranges
          |> Seq.map (fun r -> r.DebuggerDisplay)
          |> String.concat "\n"
        marked
        + "\n"
        + "\n"
        + rangeStrs

      Expect.equal
        (markRanges actual)
        (markRanges expected)
        "Should be correct ranges" 
  }

  //TODO: why does this fail? Succeeds when run outside of FSAC
  //  For example in Script file with `#r "nuget: FSharp.Compiler.Service, 41.0.5"`
  // Note: It doesn't use any FSAC stuff -- just FCS
  ptestCase "FCS: Active Pattern Case in Declaration" <| fun _ ->
    let test () =
      let checker = FSharp.Compiler.CodeAnalysis.FSharpChecker.Create()

      let text = """
let (|Even|Odd|) v = 
  if v % 2 = 0 then Even else Odd
match 2 with
| Even -> ()
| Odd -> ()
    """
      let file = "/code.fsx"
      let source = FSharp.Compiler.Text.SourceText.ofString (text)
      // decl
      let pos = FSharp.Compiler.Text.Position.mkPos 3 24
      // usage
      // let pos = FSharp.Compiler.Text.Position.mkPos 5 6
      let names = ["Even"]

      let projOptions, _ =
        checker.GetProjectOptionsFromScript (file, source)
        |> Async.RunSynchronously
      let (parseResults, checkResults') =
        checker.ParseAndCheckFileInProject(file, 0, source, projOptions)
        |> Async.RunSynchronously
      let checkResults =
        match checkResults' with
        | FSharp.Compiler.CodeAnalysis.FSharpCheckFileAnswer.Succeeded checkResults -> checkResults
        | _ -> failwith "CheckFile -> aborted"

      let symbolUse = 
        checkResults.GetSymbolUseAtLocation(pos.Line, pos.Column, source.GetLineString (pos.Line-1), names)
        |> Option.defaultWith (fun _ -> failwith "no symbol at location")

      printfn "DisplayNameCore=%A" symbolUse.Symbol.DisplayNameCore
      printfn "FullName=%A" symbolUse.Symbol.FullName

      if symbolUse.Symbol.DisplayNameCore = "" then
        failwith $"No display name for symbol with full name '{symbolUse.Symbol.FullName}'"

    test()

  testCaseAsync "Active Pattern - simple" <|
    check false
      """
      module MyModule =
        let ($<|Even|Odd|>$) v = if v % 2 = 0 then Even else Odd
        let _ = ($<|Ev$0en|Odd|>$) 42

        // backticks
        let _ = ($<|``Even``|Odd|>$) 42
        let _ = ($<|``Even``|``Odd``|>$) 42
        let _ = (``$<|Even|Odd|>$``) 42

        // spaces
        let _ = ($<| Even | Odd |>$) 42
        let _ = ($<| Even|Odd |>$) 42
        let _ = ($<|Even | Odd|>$) 42

        // linebreaks
        let _ = ($<|Even|
                  Odd|>$) 42
        let _ = (
                  $<|Even|Odd|>$) 42
        let _ = (
                $<|Even|
                  Odd|>$
                ) 42

      let _ = MyModule.($<|Even|Odd|>$) 42

      // backticks
      let _ = MyModule.($<|``Even``|Odd|>$) 42
      let _ = MyModule.($<|``Even``|``Odd``|>$) 42
      // Invalid:
      // let _ = MyModule.(``|Even|Odd|``) 42

      // spaces
      let _ = MyModule.($<| Even | Odd |>$) 42
      let _ = MyModule.($<| Even|Odd |>$) 42
      let _ = MyModule.($<|Even | Odd|>$) 42

      // linebreaks
      let _ = MyModule.($<|Even|
                Odd|>$) 42
      let _ = MyModule.(
                $<|Even|Odd|>$) 42
      let _ = MyModule.(
              $<|Even|
                Odd|>$
              ) 42
      let _ = MyModule.(

              $<|Even|
                Odd|>$

              ) 42
      """
  testCaseAsync "Active Pattern - simple - with backticks" <|
    check true
      """
      module MyModule =
        let ($<|Even|Odd|>$) v = if v % 2 = 0 then Even else Odd
        let _ = ($<|Ev$0en|Odd|>$) 42

        // backticks
        let _ = ($<|``Even``|Odd|>$) 42
        let _ = ($<|``Even``|``Odd``|>$) 42
        let _ = ($<``|Even|Odd|``>$) 42

        // spaces
        let _ = ($<| Even | Odd |>$) 42
        let _ = ($<| Even|Odd |>$) 42
        let _ = ($<|Even | Odd|>$) 42

        // linebreaks
        let _ = ($<|Even|
                  Odd|>$) 42
        let _ = (
                  $<|Even|Odd|>$) 42
        let _ = (
                $<|Even|
                  Odd|>$
                ) 42

      let _ = MyModule.($<|Even|Odd|>$) 42

      // backticks
      let _ = MyModule.($<|``Even``|Odd|>$) 42
      let _ = MyModule.($<|``Even``|``Odd``|>$) 42
      // Invalid:
      // let _ = MyModule.(``|Even|Odd|``) 42

      // spaces
      let _ = MyModule.($<| Even | Odd |>$) 42
      let _ = MyModule.($<| Even|Odd |>$) 42
      let _ = MyModule.($<|Even | Odd|>$) 42

      // linebreaks
      let _ = MyModule.($<|Even|
                Odd|>$) 42
      let _ = MyModule.(
                $<|Even|Odd|>$) 42
      let _ = MyModule.(
              $<|Even|
                Odd|>$
              ) 42
      let _ = MyModule.(

              $<|Even|
                Odd|>$

              ) 42
      """

    testCaseAsync "Active Pattern - required backticks" <|
      check false
        """
        module MyModule =
          let ($<|``Hello World``|_|>$) v = Some v

          let _ = ($<|``Hel$0lo World``|_|>$) 42
          let _ = (``$<|Hello World|_|>$``) 42

          // spaces
          let _ = (  $<|  ``Hello World``  |  _  |>$  ) 42
          let _ = (  ``$<|Hello World|_|>$``   ) 42

          // linebreaks
          let _r = 
            (
              $<|
                ``Hello World``
                  |
                    _
                      |>$
                ) 42
          let _ = 
            (  
            ``$<|Hello World|_|>$``   
              ) 42

        let _ = MyModule.($<|``Hello World``|_|>$) 42
        // invalid
        // let _ = MyModule.(``|Hello World|_|``) 42

        // spaces
        let _ = MyModule.(  $<|  ``Hello World``  |  _  |>$  ) 42
        // invalid
        // let _ = MyModule.(  ``|Hello World|_|``   ) 42

        // linebreaks
        let _r = 
          MyModule.(
            $<|
              ``Hello World``
                |
                  _
                    |>$
              ) 42
        // invalid
        // let _ = 
        //   MyModule.(  
        //   ``|Hello World|_|``   
        //     ) 42
        """
    testCaseAsync "Active Pattern - required backticks - with backticks" <|
      check true
        """
        module MyModule =
          let ($<|``Hello World``|_|>$) v = Some v

          let _ = ($<|``Hel$0lo World``|_|>$) 42
          let _ = ($<``|Hello World|_|``>$) 42

          // spaces
          let _ = (  $<|  ``Hello World``  |  _  |>$  ) 42
          let _ = (  $<``|Hello World|_|``>$   ) 42

          // linebreaks
          let _r = 
            (
              $<|
                ``Hello World``
                  |
                    _
                      |>$
                ) 42
          let _ = 
            (  
            $<``|Hello World|_|``>$   
              ) 42

        let _ = MyModule.($<|``Hello World``|_|>$) 42
        // invalid
        // let _ = MyModule.(``|Hello World|_|``) 42

        // spaces
        let _ = MyModule.(  $<|  ``Hello World``  |  _  |>$  ) 42
        // invalid
        // let _ = MyModule.(  ``|Hello World|_|``   ) 42

        // linebreaks
        let _r = 
          MyModule.(
            $<|
              ``Hello World``
                |
                  _
                    |>$
              ) 42
        // invalid
        // let _ = 
        //   MyModule.(  
        //   ``|Hello World|_|``   
        //     ) 42
        """

    testCaseAsync "Active Pattern Case - simple - at usage" <|
      check false
        """
        module MyModule =
          let (|$<Even>$|Odd|) v = 
            if v % 2 = 0 then $<Even>$ else Odd

          do
            match 42 with
            | $<Ev$0en>$ -> ()
            | Odd -> ()

          do
            match 42 with
            | ``$<Even>$`` -> ()
            | ``Odd`` -> ()

        do
          match 42 with
          | MyModule.$<Even>$ -> ()
          | MyModule.Odd -> ()

        do
          match 42 with
          | MyModule.``$<Even>$`` -> ()
          | MyModule.``Odd`` -> ()
        """
    testCaseAsync "Active Pattern Case - simple - at usage - with backticks" <|
      check true
        """
        module MyModule =
          let (|$<Even>$|Odd|) v = 
            if v % 2 = 0 then $<Even>$ else Odd

          do
            match 42 with
            | $<Ev$0en>$ -> ()
            | Odd -> ()

          do
            match 42 with
            | $<``Even``>$ -> ()
            | ``Odd`` -> ()

        do
          match 42 with
          | MyModule.$<Even>$ -> ()
          | MyModule.Odd -> ()

        do
          match 42 with
          | MyModule.$<``Even``>$ -> ()
          | MyModule.``Odd`` -> ()
        """

    testCaseAsync "Active Pattern Case - simple - at decl" <|
      // Somehow `FSharpSymbolUse.Symbol.DisplayNameCore` is empty -- but references correct Even symbol
      // 
      // Why? Cannot reproduce with just FCS -> happens just in FSAC
      check false
        """
        module MyModule =
          let (|$<Even>$|Odd|) v = 
            if v % 2 = 0 then $<Ev$0en>$ else Odd

          do
            match 42 with
            | $<Even>$ -> ()
            | Odd -> ()

          do
            match 42 with
            | ``$<Even>$`` -> ()
            | ``Odd`` -> ()

        do
          match 42 with
          | MyModule.$<Even>$ -> ()
          | MyModule.Odd -> ()

        do
          match 42 with
          | MyModule.``$<Even>$`` -> ()
          | MyModule.``Odd`` -> ()
        """
    testCaseAsync "Active Pattern Case - simple - at decl - with backticks" <|
      check true
        """
        module MyModule =
          let (|$<Even>$|Odd|) v = 
            if v % 2 = 0 then $<Ev$0en>$ else Odd

          do
            match 42 with
            | $<Even>$ -> ()
            | Odd -> ()

          do
            match 42 with
            | $<``Even``>$ -> ()
            | ``Odd`` -> ()

        do
          match 42 with
          | MyModule.$<Even>$ -> ()
          | MyModule.Odd -> ()

        do
          match 42 with
          | MyModule.$<``Even``>$ -> ()
          | MyModule.``Odd`` -> ()
        """
    
    testCaseAsync "operator -.-" <|
      check false
        """
        module MyModule =
          let ($<-.->$) a b = a - b
          
          let _ = 1 $<-$0.->$ 2
          let _ = ($<-.->$) 1 2
          // invalid:
          // let _ = (``-.-``) 1 2
          let _ = (
                    $<-.->$
                      ) 1 2
          
        let _ = MyModule.($<-.->$) 1 2

        // linebreaks
        let _ = 
          MyModule
            .($<-.->$) 1 2
        let _ = 
          MyModule.
            ($<-.->$) 1 2
        let _ = 
          MyModule
            .(
                $<-.->$
                  ) 1 2
        let _ = 
          MyModule
            .(

                $<-.->$

                  ) 1 2
        let _ = 
          MyModule.
            (
              $<-.->$
              ) 1 2
        """
    testCaseAsync "operator -.- - with backticks" <|
      // same as above -- just to ensure same result
      check true
        """
        module MyModule =
          let ($<-.->$) a b = a - b
          
          let _ = 1 $<-$0.->$ 2
          let _ = ($<-.->$) 1 2
          // invalid:
          // let _ = (``-.-``) 1 2
          let _ = (
                    $<-.->$
                      ) 1 2
          
        let _ = MyModule.($<-.->$) 1 2

        // linebreaks
        let _ = 
          MyModule
            .($<-.->$) 1 2
        let _ = 
          MyModule.
            ($<-.->$) 1 2
        let _ = 
          MyModule
            .(
                $<-.->$
                  ) 1 2
        let _ = 
          MyModule
            .(

                $<-.->$

                  ) 1 2
        let _ = 
          MyModule.
            (
              $<-.->$
              ) 1 2
        """
]
