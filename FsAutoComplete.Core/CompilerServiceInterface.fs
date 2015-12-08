namespace FsAutoComplete

open System 
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

type ParseAndCheckResults(parseResults: FSharpParseFileResults,
                          checkResults: FSharpCheckFileResults,
                          _version: int) =

  member x.TryGetMethodOverrides (lines: string[]) (line: int) (col: int) =
    // Find the starting point, ideally right after the first '('
    let lineCutoff = line - 6
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
      match loop 0 0 (prevPos(line,col)) with
      | _, 1, 1 -> 0, line, col
      | newPos -> newPos

    let lineStr = lines.[line - 1]
    match Parsing.findLongIdentsAtGetMethodsTrigger(col - 1, lineStr) with
    | None -> Failure "Could not find ident at this location"
    | Some identIsland ->

    let meth = checkResults.GetMethodsAlternate(line, col, lineStr, Some identIsland)
               |> Async.RunSynchronously

    Success(meth, commas)

  member x.TryFindDeclaration line col lineStr =
    match Parsing.findLongIdents(col - 1, lineStr) with
    | None -> Failure "Could not find ident at this location"
    | Some(col,identIsland) ->

      let declarations = checkResults.GetDeclarationLocationAlternate(line, col + 1, lineStr, identIsland, false)
                         |> Async.RunSynchronously
      match declarations with
      | FSharpFindDeclResult.DeclNotFound _ -> Failure "Could not find declaration"
      | FSharpFindDeclResult.DeclFound range -> Success range

  member x.TryGetToolTip line col lineStr =
    match Parsing.findLongIdents(col - 1, lineStr) with
    | None -> Failure "Cannot find ident for tooltip"
    | Some(col,identIsland) ->

      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let tip = checkResults.GetToolTipTextAlternate(line, col + 1, lineStr, identIsland, FSharpTokenTag.Identifier)
                |> Async.RunSynchronously
      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall (function
        FSharpToolTipElement.None -> true | _ -> false) ->
          Failure "No tooltip information"
      | _ -> Success(tip)

  member x.TryGetSymbolUse line col lineStr =
    async {
        match Parsing.findLongIdents(col - 1, lineStr) with
        | None -> return (Failure "No ident at this location")
        | Some(colu, identIsland) ->

        let! symboluse = checkResults.GetSymbolUseAtLocation(line, colu + 1, lineStr, identIsland)
        match symboluse with
        | None -> return (Failure "No symbol information found")
        | Some symboluse ->

        let! symboluses = checkResults.GetUsesOfSymbolInFile symboluse.Symbol
        return Success (symboluse, symboluses) }
    |> Async.RunSynchronously

  member x.TryGetCompletions line col lineStr timeout filter =
    let longName, residue = Parsing.findLongIdentsAndResidue(col - 1, lineStr)
    try
      let results =
        Async.RunSynchronously (checkResults.GetDeclarationListInfo(Some parseResults, line, col, lineStr, longName, residue, fun (_,_) -> false),
                                ?timeout = timeout)
      let decls =
        match filter with
        | Some "StartsWith" -> [| for d in results.Items do if d.Name.StartsWith residue then yield d |]
        | Some "Contains" -> [| for d in results.Items do if d.Name.Contains residue then yield d |]
        | _ -> results.Items
      Some (decls, residue)
    with :? TimeoutException -> None

  member x.GetExtraColorizations =
    checkResults.GetExtraColorizationsAlternate()

  member x.GetAST =
    parseResults.ParseTree

  member x.GetCheckResults =
    checkResults

type FSharpCompilerServiceChecker() =
  let checker = FSharpChecker.Instance
  do checker.BeforeBackgroundFileCheck.Add (fun _ -> ())

  let ensureCorrectFSharpCore (options: string[]) =
    Environment.fsharpCoreOpt
    |> Option.map (fun path ->
                   let fsharpCoreRef = sprintf "-r:%s" path
                   [| yield fsharpCoreRef
                      yield! Seq.filter (fun (s: string) -> not (s.EndsWith("FSharp.Core.dll"))) options |])
    |> Option.getOrElse options

  member x.GetProjectOptionsFromScript(file, source) =
    let rawOptions = checker.GetProjectOptionsFromScript(file, source)
                     |> Async.RunSynchronously
    { rawOptions
      with OtherOptions = ensureCorrectFSharpCore rawOptions.OtherOptions }

  member x.ParseAndCheckFileInProject(fileName, version, source, options) =
    checker.ParseAndCheckFileInProject(fileName, version, source, options)

  member x.TryGetRecentTypeCheckResultsForFile(file, options, ?source) =
    checker.TryGetRecentTypeCheckResultsForFile(file, options, ?source=source)
    |> Option.map (fun x -> new ParseAndCheckResults(x))

  member x.GetDeclarations (fileName, source, options) =
    let parseResult =
      checker.ParseFileInProject(fileName, source, options)
      |> Async.RunSynchronously
    parseResult.GetNavigationItems().Declarations

  member x.TryGetProjectOptions (file: string, verbose: bool) : Result<_> =
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

        let chooseByPrefix prefix (s: string) =
          if s.StartsWith(prefix) then Some (s.Substring(prefix.Length))
          else None

        let compileFiles = Seq.filter (fun (s:string) -> s.EndsWith(".fs")) po.OtherOptions
        let outputFile = Seq.tryPick (chooseByPrefix "--out:") po.OtherOptions
        let references = Seq.choose (chooseByPrefix "-r:") po.OtherOptions

        Success (po, Seq.toList compileFiles, outputFile, Seq.toList references, logMap)
      with e ->
        Failure e.Message
