// FCS Bug Reproduction: Cross-file FindBackgroundReferencesInFile for active pattern CASE symbols
//
// This reproduces the EXACT structure of the FSAC test case
//
// To run: dotnet run

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols

printfn "FCS Version: %s" (typeof<FSharpChecker>.Assembly.GetName().Version.ToString())

// Create temp directory for test files
let tempDir = Path.Combine(Path.GetTempPath(), "FcsRepro_" + Guid.NewGuid().ToString("N").[..7])
Directory.CreateDirectory(tempDir) |> ignore

printfn "Temp directory: %s" tempDir

// EXACT copy of the FSAC test case - nested module structure
let patternsFs = """namespace ActivePatternProject

module Seq =
    let inline tryPickV chooser (source: seq<'T>) =
        use e = source.GetEnumerator()
        let mutable res = ValueNone
        while (ValueOption.isNone res && e.MoveNext()) do
            res <- chooser e.Current
        res

module Patterns =

    [<return: Struct>]
    let inline (|IsOneOfChoice|_|) (chooser: 'a -> 'b -> 'c voption, values : 'a seq) (item : 'b) =
        values |> Seq.tryPickV (fun x -> chooser x item)

    [<return: Struct>]
    let inline (|StrStartsWith|_|) (value : string) (item : string) =
        if item.StartsWith value then ValueSome ()
        else ValueNone

    [<return: Struct>]
    let inline (|StrStartsWithOneOf|_|) (values : string seq) (item : string) =
        (|IsOneOfChoice|_|) ((|StrStartsWith|_|), values) item
"""

let module1Fs = """namespace ActivePatternProject

module Module1 =
    open Patterns

    // Using StrStartsWithOneOf which uses IsOneOfChoice internally
    let checkGreeting input =
        match input with
        | StrStartsWithOneOf ["hello"; "hi"; "hey"] -> "greeting"
        | _ -> "not a greeting"
"""

let module2Fs = """namespace ActivePatternProject

module Module2 =

    // Using StrStartsWithOneOf with qualified access
    let checkGreetingQualified input =
        match input with
        | Patterns.StrStartsWithOneOf ["hello"; "hi"; "hey"] -> "greeting"
        | _ -> "not a greeting"
"""

// Write files
let patternsPath = Path.Combine(tempDir, "Patterns.fs")
let module1Path = Path.Combine(tempDir, "Module1.fs")
let module2Path = Path.Combine(tempDir, "Module2.fs")

File.WriteAllText(patternsPath, patternsFs)
File.WriteAllText(module1Path, module1Fs)
File.WriteAllText(module2Path, module2Fs)

printfn "Created test files"

// Get references from the runtime
let runtimeDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()

let references = 
    [|
        yield! Directory.GetFiles(runtimeDir, "System.*.dll")
               |> Array.filter (fun f -> not (f.Contains("Native")))
               |> Array.map (fun r -> "-r:" + r)
        yield "-r:" + Path.Combine(runtimeDir, "mscorlib.dll")
        yield "-r:" + Path.Combine(runtimeDir, "netstandard.dll")
        let fsc = Path.Combine(runtimeDir, "FSharp.Core.dll")
        if File.Exists(fsc) then yield "-r:" + fsc
    |]

let sourceFiles = [| patternsPath; module1Path; module2Path |]

let projectOptions: FSharpProjectOptions = {
    ProjectFileName = Path.Combine(tempDir, "Test.fsproj")
    ProjectId = None
    SourceFiles = sourceFiles
    OtherOptions = [| 
        yield! references
        "--targetprofile:netcore"
        "--noframework"
    |]
    ReferencedProjects = [||]
    IsIncompleteTypeCheckEnvironment = false
    UseScriptResolutionRules = false
    LoadTime = DateTime.Now
    UnresolvedReferences = None
    OriginalLoadReferences = []
    Stamp = Some(DateTime.Now.Ticks)
}

printfn "Source files: %A" sourceFiles

// Create checker with TransparentCompiler (like FSAC uses)
let checker = 
    FSharpChecker.Create(
        projectCacheSize = 200,
        keepAllBackgroundResolutions = true,
        keepAllBackgroundSymbolUses = true,
        enableBackgroundItemKeyStoreAndSemanticClassification = true,
        captureIdentifiersWhenParsing = true,
        useTransparentCompiler = true)

printfn "\n%s" (String.replicate 70 "=")
printfn "Testing FindBackgroundReferencesInFile with FSharpProjectSnapshot"
printfn "%s" (String.replicate 70 "=")

// Create a snapshot from the project options
let snapshot = 
    FSharpProjectSnapshot.FromOptions(projectOptions, DocumentSource.FileSystem)
    |> Async.RunSynchronously

printfn "Snapshot created: %d source files" (snapshot.SourceFiles |> Seq.length)

// First check what identifiers are captured in each file
printfn "\n--- Captured Identifiers ---"
for file in sourceFiles do
    let parseRes = checker.ParseFile(file, snapshot) |> Async.RunSynchronously
    let idents = parseRes.ParseTree.Identifiers |> Set.toList |> List.sort
    printfn "%s: %A" (Path.GetFileName file) idents

// Parse and check all files using the snapshot
let checkResultsMap = 
    sourceFiles 
    |> Array.choose (fun file ->
        let _parseRes, checkRes = 
            checker.ParseAndCheckFileInProject(file, snapshot)
            |> Async.RunSynchronously
        match checkRes with
        | FSharpCheckFileAnswer.Succeeded res -> Some (file, res)
        | FSharpCheckFileAnswer.Aborted -> 
            printfn "  WARNING: Check aborted for %s" file
            None)
    |> Map.ofArray

if checkResultsMap.Count < 3 then
    printfn "Not all files compiled successfully"
else
    // Get check results for Patterns.fs where we have the declaration
    let patternsCheck = checkResultsMap.[patternsPath]
    
    // Find StrStartsWithOneOf symbol - look for the case symbol
    printfn "\n--- Looking for StrStartsWithOneOf symbol in Patterns.fs ---"
    
    // Line with declaration: let inline (|StrStartsWithOneOf|_|) (values : string seq) (item : string) =
    // In the namespace/module structure, this is around line 23
    let allSymbols = patternsCheck.GetAllUsesOfAllSymbolsInFile() |> Seq.toArray
    
    let strStartsWithOneOfSymbols = 
        allSymbols 
        |> Array.filter (fun su -> 
            su.Symbol.DisplayName.Contains("StrStartsWithOneOf"))
    
    printfn "Found %d symbols containing 'StrStartsWithOneOf':" strStartsWithOneOfSymbols.Length
    for su in strStartsWithOneOfSymbols do
        printfn "  - %s (%s) at %A, IsFromDefinition=%b" 
            su.Symbol.DisplayName 
            (su.Symbol.GetType().Name)
            su.Range
            su.IsFromDefinition
    
    // Find the case symbol specifically
    let caseSymbol = 
        strStartsWithOneOfSymbols 
        |> Array.tryFind (fun su -> su.Symbol :? FSharpActivePatternCase)
    
    match caseSymbol with
    | None ->
        printfn "\n❌ No FSharpActivePatternCase found for StrStartsWithOneOf"
        
        // Try to find via function symbol
        let funcSymbol = 
            strStartsWithOneOfSymbols 
            |> Array.tryFind (fun su -> 
                match su.Symbol with
                | :? FSharpMemberOrFunctionOrValue as mfv -> mfv.IsActivePattern
                | _ -> false)
        
        match funcSymbol with
        | Some su ->
            printfn "Found function symbol instead: %s" su.Symbol.DisplayName
            let symbol = su.Symbol
            
            printfn "\n--- Searching for references using function symbol ---"
            for file in sourceFiles do
                let refs = 
                    checker.FindBackgroundReferencesInFile(file, snapshot, symbol)
                    |> Async.RunSynchronously
                    |> Seq.toArray
                printfn "  %s: %d references" (Path.GetFileName file) refs.Length
                for r in refs do
                    printfn "    - %A" r
        | None ->
            printfn "No function symbol found either"
            
    | Some su ->
        let symbol = su.Symbol
        printfn "\n✅ Found FSharpActivePatternCase: %s" symbol.DisplayName
        printfn "   DisplayNameCore: %s" symbol.DisplayNameCore
        printfn "   DeclarationLocation: %A" symbol.DeclarationLocation
        printfn "   ImplementationLocation: %A" symbol.ImplementationLocation
        printfn "   IsPrivateToFile: %b" su.IsPrivateToFile
        printfn "   IsInternalToProject: %b" symbol.IsInternalToProject
        
        match symbol with
        | :? FSharpActivePatternCase as apCase ->
            printfn "   Case name: %s" apCase.Name
            printfn "   Index: %d" apCase.Index
            printfn "   Group: %A" (apCase.Group.Names |> Seq.toList)
        | _ -> ()
        
        // Check if DisplayNameCore is in identifiers for each file
        printfn "\n--- Checking if '%s' is in identifiers ---" symbol.DisplayNameCore
        for file in sourceFiles do
            let parseRes = checker.ParseFile(file, snapshot) |> Async.RunSynchronously
            let idents = parseRes.ParseTree.Identifiers
            let contains = idents |> Set.contains symbol.DisplayNameCore
            printfn "  %s: contains '%s' = %b" (Path.GetFileName file) symbol.DisplayNameCore contains
        
        printfn "\n--- Searching for references ---"
        
        let totalRefs = ref 0
        for file in sourceFiles do
            let refs = 
                checker.FindBackgroundReferencesInFile(file, snapshot, symbol)
                |> Async.RunSynchronously
                |> Seq.toArray
            totalRefs.Value <- totalRefs.Value + refs.Length
            printfn "  %s: %d references" (Path.GetFileName file) refs.Length
            for r in refs do
                printfn "    - %A" r
        
        printfn "\nTotal references found: %d" totalRefs.Value
        
        if totalRefs.Value <= 1 then
            printfn "\n❌ BUG: Only found %d reference(s)!" totalRefs.Value
            printfn "Expected:"
            printfn "  - 1 in Patterns.fs (declaration)"
            printfn "  - 1 in Module1.fs (usage)"
            printfn "  - 1 in Module2.fs (qualified usage)"
        else
            printfn "\n✅ Multiple references found"

// Cleanup
printfn "\n\nCleaning up..."
Directory.Delete(tempDir, true)
printfn "Done."
