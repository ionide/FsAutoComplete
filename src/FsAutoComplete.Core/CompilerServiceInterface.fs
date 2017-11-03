namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Utils
open System.Collections.Concurrent
open FsAutoComplete.ProjectRecognizer

type ParseAndCheckResults
    (
        parseResults: FSharpParseFileResults,
        checkResults: FSharpCheckFileResults,
        entityCache: EntityCache
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
    | None -> return ResultOrString.Error "Could not find ident at this location"
    | Some identIsland ->

    let! meth = checkResults.GetMethods(line, col, lineStr, Some identIsland)

    return Ok(meth, commas) }

  member __.TryFindDeclaration (pos: Pos) (lineStr: LineStr) = async {
    match Parsing.findLongIdents(pos.Col - 1, lineStr) with
    | None -> return ResultOrString.Error "Could not find ident at this location"
    | Some(col, identIsland) ->

      let! declarations = checkResults.GetDeclarationLocation(pos.Line, col, lineStr, identIsland, false)

      match declarations with
      | FSharpFindDeclResult.DeclNotFound _ -> return ResultOrString.Error "Could not find declaration"
      | FSharpFindDeclResult.DeclFound range -> return Ok range
      | FSharpFindDeclResult.ExternalDecl(assembly, externalSym) -> return ResultOrString.Error "External declaration" //TODO: Handle external declarations
    }

  member __.TryFindTypeDeclaration (pos: Pos) (lineStr: LineStr) = async {
    match Parsing.findLongIdents(pos.Col - 1, lineStr) with
    | None -> return Error "Cannot find ident at this location"
    | Some(col,identIsland) ->
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)
      match symbol with
      | None -> return Error "Cannot find symbol at this locaion"
      | Some s ->
        let r =
          match s with
          | SymbolUse.Field f -> Some f.FieldType.TypeDefinition.DeclarationLocation
          | SymbolUse.Val v -> v.FullTypeSafe |> Option.map (fun f -> f.TypeDefinition.DeclarationLocation)
          | SymbolUse.Entity (e, _) -> Some e.DeclarationLocation
          | SymbolUse.Parameter p -> Some p.Type.TypeDefinition.DeclarationLocation
          | SymbolUse.TypeAbbreviation t -> Some t.DeclarationLocation
          | SymbolUse.Property p -> p.FullTypeSafe |> Option.map (fun f -> f.TypeDefinition.DeclarationLocation)
          | _ -> None
        match r with
        | Some r when File.Exists r.FileName -> return (Ok r)
        | _ -> return Error "No type information for the symbol at this location"

  }

  member __.TryGetToolTip (pos: Pos) (lineStr: LineStr) = async {
    match Parsing.findLongIdents(pos.Col - 1, lineStr) with
    | None -> return ResultOrString.Error "Cannot find ident for tooltip"
    | Some(col,identIsland) ->

      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      return
        match tip with
        | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) ->
            match identIsland with
            | [ident] ->
               KeywordList.tryGetKeywordDescription ident
               |> Option.map (fun desc -> FSharpToolTipText [FSharpToolTipElement.Single(ident, FSharpXmlDoc.Text desc)])
               |> function
               | Some tip -> Ok tip
               | None -> ResultOrString.Error "No tooltip information"
            | _ -> ResultOrString.Error "No tooltip information"
        | _ -> Ok(tip)
  }

  member __.TryGetToolTipEnhanced (pos: Pos) (lineStr: LineStr) = async {
    match Parsing.findLongIdents(pos.Col - 1, lineStr) with
    | None -> return Error "Cannot find ident for tooltip"
    | Some(col,identIsland) ->

      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)
      return
        match tip with
        | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) ->
            match identIsland with
            | [ident] ->
               KeywordList.tryGetKeywordDescription ident
               |> Option.map (fun desc -> FSharpToolTipText [FSharpToolTipElement.Single(ident, FSharpXmlDoc.Text desc)])
               |> function
               | Some tip -> Ok (tip, ident, "")
               | None -> Error "No tooltip information"
            | _ -> Error "No tooltip information"
        | _ ->
        match symbol with
        | None -> Error "No tooltip information"
        | Some s ->
          match SignatureFormatter.getTooltipDetailsFromSymbolUse s with
          | None -> Error "No tooltip information"
          | Some (s,f) -> Ok (tip, s, f)
  }

  member __.TryGetSymbolUse (pos: Pos) (lineStr: LineStr) =
    async {
        match Parsing.findLongIdents(pos.Col - 1, lineStr) with
        | None -> return (ResultOrString.Error "No ident at this location")
        | Some(colu, identIsland) ->

        let! symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
        match symboluse with
        | None -> return (ResultOrString.Error "No symbol information found")
        | Some symboluse ->

        let! symboluses = checkResults.GetUsesOfSymbolInFile symboluse.Symbol
        return Ok (symboluse, symboluses) }

  member __.TryGetSignatureData (pos: Pos) (lineStr: LineStr) =
    async {
        match Parsing.findLongIdents(pos.Col - 1, lineStr) with
        | None -> return (ResultOrString.Error "No ident at this location")
        | Some(colu, identIsland) ->

        let! symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
        match symboluse with
        | None -> return (ResultOrString.Error "No symbol information found")
        | Some symboluse ->
          let fsym = symboluse.Symbol
          match fsym with
          | :? FSharpMemberOrFunctionOrValue as symbol ->
            let parms =
              symbol.CurriedParameterGroups
              |> Seq.map (Seq.map (fun p -> p.DisplayName, p.Type.Format symboluse.DisplayContext) >> Seq.toList )
              |> Seq.toList
            let typ = symbol.ReturnParameter.Type.Format symboluse.DisplayContext
            return Ok(typ, parms)
          | _ ->
            return (ResultOrString.Error "Not a member, function or value" )
    }

  member __.TryGetF1Help (pos: Pos) (lineStr: LineStr) =
    async {
        match Parsing.findLongIdents(pos.Col - 1, lineStr) with
        | None -> return (ResultOrString.Error "No ident at this location")
        | Some(colu, identIsland) ->

        let! help = checkResults.GetF1Keyword(pos.Line, colu, lineStr, identIsland)
        match help with
        | None -> return (ResultOrString.Error "No symbol information found")
        | Some hlp -> return Ok hlp}

  member __.TryGetCompletions (pos: Pos) (lineStr: LineStr) filter = async {
    let ln, residue = Parsing.findLongIdentsAndResidue (pos.Col - 1, lineStr)
    try
      let longName = Microsoft.FSharp.Compiler.QuickParse.GetPartialLongNameEx(lineStr, pos.Col - 1)
      let longName = {longName with QualifyingIdents = ln; PartialIdent = residue }
      let! results = checkResults.GetDeclarationListInfo(Some parseResults, pos.Line, lineStr, longName, (fun () -> []))

      let decls =
        match filter with
        | Some "StartsWith" -> [| for d in results.Items do if d.Name.StartsWith(residue, StringComparison.InvariantCultureIgnoreCase) then yield d |]
        | Some "Contains" -> [| for d in results.Items do if d.Name.IndexOf(residue, StringComparison.InvariantCultureIgnoreCase) >= 0 then yield d |]
        | _ -> results.Items

      let decls = decls |> Array.sortBy (fun d -> d.Name)
      return Some (decls, residue)
    with :? TimeoutException -> return None
  }

  member __.GetAllEntities () =
    async {
      try
        return
          Some
            [
              yield! AssemblyContentProvider.getAssemblySignatureContent AssemblyContentType.Full checkResults.PartialAssemblySignature
              let ctx = checkResults.ProjectContext
              let assembliesByFileName =
                ctx.GetReferencedAssemblies()
                |> Seq.groupBy (fun asm -> asm.FileName)
                |> Seq.map (fun (fileName, asms) -> fileName, List.ofSeq asms)
                |> Seq.toList
                |> List.rev // if mscorlib.dll is the first then FSC raises exception when we try to
                            // get Content.Entities from it.

              for fileName, signatures in assembliesByFileName do
                let contentType = Public // it's always Public for now since we don't support InternalsVisibleTo attribute yet
                let content = AssemblyContentProvider.getAssemblyContent entityCache.Locking contentType fileName signatures
                yield! content

            ]
      with
      | _ -> return None
  }

  member __.GetSemanticClassification = checkResults.GetSemanticClassification None
  member __.GetAST = parseResults.ParseTree
  member __.GetCheckResults = checkResults
  member __.GetParseResults = parseResults
  member __.FileName = parseResults.FileName

type private FileState =
    | Checked
    | NeedChecking
    | BeingChecked
    | Cancelled

type Version = int

type FSharpCompilerServiceChecker() =
  let checker =
    FSharpChecker.Create(
      projectCacheSize = 200,
      keepAllBackgroundResolutions = true,
      keepAssemblyContents = true)

  do checker.BeforeBackgroundFileCheck.Add ignore
  let fixFileName path =
    if (try Path.GetFullPath path |> ignore; true with _ -> false) then path
    else
        match Environment.OSVersion.Platform with
        | PlatformID.Unix
        | PlatformID.MacOSX -> Environment.GetEnvironmentVariable "HOME"
        | _ -> Environment.ExpandEnvironmentVariables "%HOMEDRIVE%%HOMEPATH%"
        </> Path.GetFileName path

  let isFSharpCore (s : string) = s.EndsWith "FSharp.Core.dll"

  let ensureCorrectFSharpCore (options: string[]) =
    [| match Environment.fsharpCoreOpt with
       | Some path -> yield (sprintf "-r:%s" path)
       | None ->
          match options |> Array.tryFind isFSharpCore with
          | Some ref -> yield ref
          | None -> ()
       //ensure single FSharp.Core ref
       yield! options |> Array.filter (not << isFSharpCore) |]

#if SCRIPT_REFS_FROM_MSBUILD
#else
  let ensureCorrectVersions (options: string[]) =
    if Utils.runningOnMono then options
    else
      match Environment.referenceAssembliesPath (), Environment.netReferecesAssembliesTFMLatest () with
      | _, None -> options
      | Some referenceAssembliesPath, Some version ->
        let oldRef = referenceAssembliesPath </> "v4.0"
        let newRef = referenceAssembliesPath </> version

        let fsharpCoreRef = options |> Seq.find isFSharpCore

        let newOptions =
          options
          |> Seq.filter (not << isFSharpCore)
          |> Seq.map (fun (s : string) -> s.Replace(oldRef, newRef) )
        [| yield fsharpCoreRef
           yield! newOptions |]
      | None, _ -> options
#endif

  let getDependingProjects file (options : seq<string * FSharpProjectOptions>) =
    let project = options |> Seq.tryFind (fun (k,_) -> k = file)
    project |> Option.map (fun (name, option) ->
      [
        yield! options
               |> Seq.map snd
               |> Seq.filter (fun o -> o.ReferencedProjects |> Array.map (fun (k,v) -> v.ProjectFileName) |> Array.contains option.ProjectFileName )
        yield option
      ])

  let entityCache = EntityCache()

  member __.GetProjectOptionsFromScript(file, source) = async {

#if SCRIPT_REFS_FROM_MSBUILD

    let targetFramework = Environment.netReferecesAssembliesTFMLatest ()

    let additionaRefs =
      NETFrameworkInfoProvider.additionalArgumentsBy targetFramework
      |> Array.ofList

    let! (rawOptions, _) = checker.GetProjectOptionsFromScript(file, source, otherFlags = additionaRefs, assumeDotNetFramework = true)

    let opts =
      rawOptions.OtherOptions
      |> ensureCorrectFSharpCore

    let opts =
      opts
      |> Array.distinct

    return { rawOptions with OtherOptions = opts }
#else
    let! (rawOptions, _) = checker.GetProjectOptionsFromScript(file, source)

    let opts =
      rawOptions.OtherOptions
      |> ensureCorrectFSharpCore
      |> ensureCorrectVersions

    return { rawOptions with OtherOptions = opts }
#endif

  }

  member __.CheckProjectsInBackgroundForFile (file,options : seq<string * FSharpProjectOptions>) =
    defaultArg (getDependingProjects file options) []
    |> List.iter (checker.CheckProjectInBackground)

  member __.ParseProjectsForFile(file, options : seq<string * FSharpProjectOptions> ) =
    let project = options |> Seq.tryFind (fun (k,_) -> k = file)
    match project with
    | None -> async {return ResultOrString.Error "Project for current file not found"}
    | Some (name, option) ->
      async {
        let projs = defaultArg (getDependingProjects file options) []
        let! results =
          projs
          |> Seq.map checker.ParseAndCheckProject
          |> Async.Parallel
        let! currentResult =  checker.ParseAndCheckProject option
        let res = [| yield currentResult; yield! results |]
        return Ok res
      }

  member __.GetBackgroundCheckResultsForFileInProject =
    checker.GetBackgroundCheckResultsForFileInProject

  member __.FileChecked =
    checker.FileChecked

  member __.ParseFile =
    checker.ParseFile

  member __.ParseAndCheckFileInProject(filePath, version, source, options) =
    async {
      let fixedFilePath = fixFileName filePath
      let! res = Async.Catch (checker.ParseAndCheckFileInProject (fixedFilePath, version, source, options, null))
      return
          match res with
          | Choice1Of2 x -> Ok x
          | Choice2Of2 e -> ResultOrString.Error e.Message
    }

  member __.TryGetRecentCheckResultsForFile(file, options, ?source) =
    checker.TryGetRecentCheckResultsForFile(file, options, ?source=source)
    |> Option.map (fun (pr, cr, _) -> ParseAndCheckResults (pr, cr, entityCache))

  member x.GetProjectOptions verbose (projectFileName: SourceFilePath) =
    if not (File.Exists projectFileName) then
        Error (GenericError(sprintf "File '%s' does not exist" projectFileName))
    else
        match projectFileName with
        | NetCoreProjectJson -> ProjectCrackerProjectJson.load projectFileName
        | NetCoreSdk -> ProjectCrackerDotnetSdk.load projectFileName
#if NO_PROJECTCRACKER
        | Net45 -> ProjectCrackerDotnetSdk.loadVerboseSdk projectFileName
        | Unsupported -> Error (GenericError(sprintf "Project file '%s' not supported" projectFileName))
#else
        | Net45 -> ProjectCrackerVerbose.load ensureCorrectFSharpCore projectFileName verbose
        | Unsupported -> ProjectCrackerVerbose.load ensureCorrectFSharpCore projectFileName verbose
#endif

  member __.GetUsesOfSymbol (file, options : (SourceFilePath * FSharpProjectOptions) seq, symbol : FSharpSymbol) = async {
    let projects = getDependingProjects file options
    return!
      match projects with
      | None -> async {return [||]}
      | Some projects -> async {
        let! res =
          projects
          |> Seq.map (fun (opts) -> async {
              let! res = checker.ParseAndCheckProject opts
              return! res.GetUsesOfSymbol symbol
            })
          |> Async.Parallel
        return res |> Array.collect id }
  }

  member __.GetDeclarations (fileName, source, options, version) = async {
    let! parseResult = checker.ParseFile(fileName, source, options)
    return parseResult.GetNavigationItems().Declarations
  }
