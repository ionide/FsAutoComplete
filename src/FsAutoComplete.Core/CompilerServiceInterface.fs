namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Utils

type ParseAndCheckResults
    (
        parseResults: FSharpParseFileResults,
        checkResults: FSharpCheckFileResults
    ) =

  member __.TryGetMethodOverrides (lines: LineStr[]) (pos: Pos) = async {
    // Find the starting point, ideally right after the first '('
    let lineCutoff = pos.Line - 6
    let commas, line, col =
      let rec prevPos (line,col) =
        match line, col with
        | 1, 1
        | _ when line < lineCutoff -> 1, 1
        | _, 1 ->
           let prevLine = lines.[line - 2]
           if prevLine.Length = 0 then prevPos(line-1, 1)
           else line - 1, prevLine.Length
        | _    -> line, col - 1

      let rec loop commas depth (line, col) =
        if (line,col) <= (1,1) then (0, line, col) else
        let ch = lines.[line - 1].[col - 1]
        let commas = if depth = 0 && ch = ',' then commas + 1 else commas
        if (ch = '(' || ch = '{' || ch = '[') && depth > 0 then loop commas (depth - 1) (prevPos (line,col))
        elif ch = ')' || ch = '}' || ch = ']' then loop commas (depth + 1) (prevPos (line,col))
        elif ch = '(' || ch = '<' then commas, line, col
        else loop commas depth (prevPos (line,col))
      match loop 0 0 (prevPos(pos.Line, pos.Col)) with
      | _, 1, 1 -> 0, pos.Line, pos.Col
      | newPos -> newPos

    let lineStr = lines.[line - 1]
    match Parsing.findLongIdentsAtGetMethodsTrigger(col - 1, lineStr) with
    | None -> return Failure "Could not find ident at this location"
    | Some identIsland ->

    let! meth = checkResults.GetMethodsAlternate(line, col, lineStr, Some identIsland)

    return Success(meth, commas) }

  member __.TryFindDeclaration (pos: Pos) (lineStr: LineStr) = async {
    match Parsing.findLongIdents(pos.Col - 1, lineStr) with
    | None -> return Failure "Could not find ident at this location"
    | Some(col, identIsland) ->

      let! declarations = checkResults.GetDeclarationLocationAlternate(pos.Line, col, lineStr, identIsland, false)

      match declarations with
      | FSharpFindDeclResult.DeclNotFound _ -> return Failure "Could not find declaration"
      | FSharpFindDeclResult.DeclFound range -> return Success range
    }

  member __.TryGetToolTip (pos: Pos) (lineStr: LineStr) = async {
    match Parsing.findLongIdents(pos.Col - 1, lineStr) with
    | None -> return Failure "Cannot find ident for tooltip"
    | Some(col,identIsland) ->

      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipTextAlternate(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)

      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall (function
        FSharpToolTipElement.None -> true | _ -> false) ->
         return Failure "No tooltip information"
      | _ -> return Success(tip)
  }

  member __.TryGetSymbolUse (pos: Pos) (lineStr: LineStr) =
    async {
        match Parsing.findLongIdents(pos.Col - 1, lineStr) with
        | None -> return (Failure "No ident at this location")
        | Some(colu, identIsland) ->

        let! symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
        match symboluse with
        | None -> return (Failure "No symbol information found")
        | Some symboluse ->

        let! symboluses = checkResults.GetUsesOfSymbolInFile symboluse.Symbol
        return Success (symboluse, symboluses) }

  member __.TryGetCompletions (pos: Pos) (lineStr: LineStr) filter = async {
    let longName, residue = Parsing.findLongIdentsAndResidue(pos.Col - 1, lineStr)
    try
      let! results = checkResults.GetDeclarationListInfo(Some parseResults, pos.Line, pos.Col, lineStr, longName, residue, fun (_,_) -> false)

      let decls =
        match filter with
        | Some "StartsWith" -> [| for d in results.Items do if d.Name.StartsWith(residue, StringComparison.InvariantCultureIgnoreCase) then yield d |]
        | Some "Contains" -> [| for d in results.Items do if d.Name.IndexOf(residue, StringComparison.InvariantCultureIgnoreCase) >= 0 then yield d |]
        | _ -> results.Items
      return Some (decls, residue)
    with :? TimeoutException -> return None
  }

  member __.GetExtraColorizations = checkResults.GetExtraColorizationsAlternate()
  member __.GetAST = parseResults.ParseTree
  member __.GetCheckResults = checkResults

type FSharpCompilerServiceChecker() =
  let checker =
    FSharpChecker.Create(
      projectCacheSize = 200,
      keepAllBackgroundResolutions = true,
      keepAssemblyContents = true)

  do checker.BeforeBackgroundFileCheck.Add ignore

  let ensureCorrectFSharpCore (options: string[]) =
    Environment.fsharpCoreOpt
    |> Option.map (fun path ->
                   let fsharpCoreRef = sprintf "-r:%s" path
                   [| yield fsharpCoreRef
                      yield! Seq.filter (fun (s: string) -> not (s.EndsWith("FSharp.Core.dll"))) options |])
    |> Option.getOrElse options

  let ensureCorrectVersions (options: string[]) =
    if Utils.runningOnMono then options
    else
      let version = Environment.dotNetVersions () |> Seq.head
      let oldRef = Environment.referenceAssembliesPath </> "v4.0"
      let newRef = Environment.referenceAssembliesPath </> version

      let fsharpCoreRef = options |> Seq.find (fun s -> s.EndsWith "FSharp.Core.dll")

      let newOptions =
        options
        |> Seq.filter (fun s -> not (s.EndsWith "FSharp.Core.dll"))
        |> Seq.map (fun (s : string) -> s.Replace(oldRef, newRef) )
      [| yield fsharpCoreRef
         yield! newOptions |]

  let chooseByPrefix prefix (s: string) =
    if s.StartsWith(prefix) then Some (s.Substring(prefix.Length))
    else None

  let rec allSymbolsInEntities (entities: Collections.Generic.IList<FSharpEntity>) =
    [ for e in entities do
          yield (e :> FSharpSymbol)
          for x in e.MembersFunctionsAndValues do
             yield (x :> FSharpSymbol)
          for x in e.UnionCases do
             yield (x :> FSharpSymbol)
          for x in e.FSharpFields do
             yield (x :> FSharpSymbol)
          yield! allSymbolsInEntities e.NestedEntities ]

  member __.GetUsesOfSymbol (options : (SourceFilePath * FSharpProjectOptions) seq, symbol) = async {
    let! res =
      options
      |> Seq.distinctBy(fun (_, v) -> v.ProjectFileName)
      |> Seq.map (fun (_, opts) -> async {
           let! res = checker.ParseAndCheckProject opts
           return! res.GetUsesOfSymbol symbol
         })
      |> Async.Parallel
    return res |> Array.collect id
  }

  member __.GetProjectOptionsFromScript(file, source) = async {
    let! rawOptions = checker.GetProjectOptionsFromScript(file, source)
    let opts =
      rawOptions.OtherOptions
      |> ensureCorrectFSharpCore
      |> ensureCorrectVersions

    return { rawOptions with OtherOptions = opts }
  }

  member __.ParseAndCheckAllProjects (options : (SourceFilePath * FSharpProjectOptions) seq) = async {
    let! res =
      options
      |> Seq.distinctBy(fun (_, v) -> v.ProjectFileName)
      |> Seq.map(fun (_, v) -> async {
          let! r = checker.ParseAndCheckProject v
          return r.Errors
        })
      |> Async.Parallel
    return res |> Array.collect id
  }

  member __.ParseAndCheckFileInProject(fileName, version, source, options) =
    checker.ParseAndCheckFileInProject(fileName, version, source, options)

  member __.TryGetRecentCheckResultsForFile(file, options, ?source) =
    checker.TryGetRecentCheckResultsForFile(file, options, ?source=source)
    |> Option.map (fun (pr, cr, _) -> ParseAndCheckResults (pr, cr))

  member __.GetDeclarations (fileName, source, options) = async {
    let! parseResult = checker.ParseFileInProject(fileName, source, options)
    return parseResult.GetNavigationItems().Declarations
  }

  member __.GetDeclarationsInProjects (options : seq<string * FSharpProjectOptions>) =
      options
      |> Seq.distinctBy(fun (_, v) -> v.ProjectFileName)
      |> Seq.map (fun (_, opts) -> async {
          let! _ = checker.ParseAndCheckProject opts
          return!
            options
            |> Seq.filter (fun (_, projectOpts) -> projectOpts = opts)
            |> Seq.map (fun (projectFile,_) -> async {
                let! parseRes, _ = checker.GetBackgroundCheckResultsForFileInProject(projectFile, opts)
                return (parseRes.GetNavigationItems().Declarations |> Array.map (fun decl -> decl, projectFile))
              })
            |> Async.Parallel
         })
      |> Async.Parallel
      |> Async.map (Seq.collect (Seq.collect id) >> Seq.toArray)

  member __.TryGetProjectOptions (file: SourceFilePath, verbose: bool) : Result<_> =
    if not (File.Exists file) then
      Failure (sprintf "File '%s' does not exist" file)
    else
      try
        let po, logMap =
          let p, logMap = ProjectCracker.GetProjectOptionsFromProjectFileLogged(file, enableLogging=verbose)
          let opts =
            if not (Seq.exists (fun (s: string) -> s.Contains "FSharp.Core.dll") p.OtherOptions) then
              ensureCorrectFSharpCore p.OtherOptions
            else
               p.OtherOptions
          { p with OtherOptions = opts }, logMap

        let compileFiles = Seq.filter (fun (s:string) -> s.EndsWith(".fs")) po.OtherOptions
        let outputFile = Seq.tryPick (chooseByPrefix "--out:") po.OtherOptions
        let references = Seq.choose (chooseByPrefix "-r:") po.OtherOptions

        Success (po, Seq.toList compileFiles, outputFile, Seq.toList references, logMap)
      with e ->
        Failure e.Message

  member __.TryGetCoreProjectOptions (file : SourceFilePath) : Result<_> =
    if not (File.Exists file) then
      Failure (sprintf "File '%s' does not exist" file)
    else
      try
        let po = ProjectCoreCracker.GetProjectOptionsFromProjectFile file
        let compileFiles = Seq.filter (fun (s:string) -> s.EndsWith(".fs")) po.OtherOptions
        let outputFile = Seq.tryPick (chooseByPrefix "--out:") po.OtherOptions
        let references = Seq.choose (chooseByPrefix "-r:") po.OtherOptions
        Success (po, Seq.toList compileFiles, outputFile, Seq.toList references, Map<string,string>([||]))
      with e ->
        Failure e.Message



