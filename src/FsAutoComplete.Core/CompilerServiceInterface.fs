namespace FsAutoComplete

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open Utils
open FSharp.Compiler.Range
open FSharp.Compiler
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
type FindDeclarationResult =
    | ExternalDeclaration of Decompiler.ExternalContentPosition
    | Range of FSharp.Compiler.Range.range

type ParseAndCheckResults
    (
        parseResults: FSharpParseFileResults,
        checkResults: FSharpCheckFileResults,
        entityCache: EntityCache
    ) =

  member __.TryGetMethodOverrides (lines: LineStr[]) (pos: pos) = async {
    // Find the number of `,` in the current signature
    let commas, _, _ =
      let lineCutoff = pos.Line - 6
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
      match loop 0 0 (prevPos(pos.Line, pos.Column)) with
      | _, 1, 1 -> 0, pos.Line, pos.Column
      | newPos -> newPos
    let testPos = mkPos pos.Line (pos.Column - 1)
    // Get the parameter locations
    let paramLocations = parseResults.FindNoteworthyParamInfoLocations pos
    match paramLocations with
    | None -> return ResultOrString.Error "Could not find parameter locations"
    | Some nwpl ->
      let names = nwpl.LongId
      let lidEnd = nwpl.LongIdEndLocation
      let! meth = checkResults.GetMethods(lidEnd.Line, lidEnd.Column, "", Some names)
      return Ok(meth, commas) }

  member __.TryFindDeclaration (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return ResultOrString.Error "Could not find ident at this location"
    | Some(col, identIsland) ->

      let! declarations = checkResults.GetDeclarationLocation(pos.Line, col, lineStr, identIsland, false)

      match declarations with
      | FSharpFindDeclResult.DeclNotFound _ ->
        return ResultOrString.Error "Could not find declaration"
      | FSharpFindDeclResult.DeclFound range when range.FileName.EndsWith(Range.rangeStartup.FileName) -> return ResultOrString.Error "Could not find declaration"
      | FSharpFindDeclResult.DeclFound range ->
        return Ok (FindDeclarationResult.Range range)
      | FSharpFindDeclResult.ExternalDecl (assembly, externalSym) ->
        match Decompiler.tryFindExternalDeclaration checkResults (assembly, externalSym) with
        | Ok extDec -> return ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration extDec)
        | Error err -> return ResultOrString.Error (sprintf "External declaration not resolved: %A" err)
    }

  member __.TryFindTypeDeclaration (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None ->
      return Error "Cannot find ident at this location"
    | Some(col,identIsland) ->
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)
      match symbol with
      | None ->
        return Error "Cannot find symbol at this locaion"
      | Some sym ->
        let r =
          match sym with
          | SymbolUse.Field f -> Some f.FieldType.TypeDefinition.DeclarationLocation
          | SymbolUse.Val v -> v.FullTypeSafe |> Option.map (fun f -> f.TypeDefinition.DeclarationLocation)
          | SymbolUse.Entity (e, _) -> Some e.DeclarationLocation
          | SymbolUse.Parameter p -> Some p.Type.TypeDefinition.DeclarationLocation
          | SymbolUse.TypeAbbreviation t -> Some t.DeclarationLocation
          | SymbolUse.Property p -> p.FullTypeSafe |> Option.map (fun f -> f.TypeDefinition.DeclarationLocation)
          | _ -> None
        match r with
        | Some r when File.Exists r.FileName ->
          return (Ok r)
        | _ ->
          return Error "No type information for the symbol at this location"

  }

  member __.TryGetToolTip (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
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
               | Some tip ->
                Ok tip
               | None ->
                ResultOrString.Error "No tooltip information"
            | _ ->
              ResultOrString.Error "No tooltip information"
        | _ ->
          Ok(tip)
  }

  member __.TryGetToolTipEnhanced (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return Error "Cannot find ident for tooltip"
    | Some(col,identIsland) ->

      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)

      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) && symbol.IsNone ->
          match identIsland with
          | [ident] ->
             let keyword = KeywordList.tryGetKeywordDescription ident
                           |> Option.map (fun desc -> FSharpToolTipText [FSharpToolTipElement.Single(ident, FSharpXmlDoc.Text desc)])
             match keyword with
             | Some tip ->
              return Ok (tip, ident, "", None)
             | None ->
              return Error "No tooltip information"
          | _ ->
            return Error "No tooltip information"
      | _ ->
      match symbol with
      | None ->
        return Error "No tooltip information"
      | Some symbol ->

        match SignatureFormatter.getTooltipDetailsFromSymbolUse symbol with
        | None ->
          return Error "No tooltip information"
        | Some (signature, footer) ->
            let typeDoc = getTypeIfConstructor symbol.Symbol |> Option.map (fun n -> n.XmlDocSig)
            return Ok (tip, signature, footer, typeDoc)
  }

  member __.TryGetFormattedDocumentation (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return Error "Cannot find ident"
    | Some(col,identIsland) ->

      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)

      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) && symbol.IsNone ->
          match identIsland with
          | [ident] ->
             let keyword = KeywordList.tryGetKeywordDescription ident
                           |> Option.map (fun desc -> FSharpToolTipText [FSharpToolTipElement.Single(ident, FSharpXmlDoc.Text desc)])
             match keyword with
             | Some tip ->
              return Ok (Some tip, None, (ident, (DocumentationFormatter.emptyTypeTip)), "", "")
             | None ->
              return Error "No tooltip information"
          | _ ->
            return Error "No documentation information"
      | _ ->
      match symbol with
      | None ->
        return Error "No documentation information"
      | Some symbol ->
        match DocumentationFormatter.getTooltipDetailsFromSymbolUse symbol with
        | None ->
          return Error "No documentation information"
        | Some (signature, footer, cn) ->
            match symbol with
            | SymbolUse.TypeAbbreviation symbol ->
              return Ok (None, Some (symbol.GetAbbriviatedParent().XmlDocSig, symbol.GetAbbriviatedParent().Assembly.FileName |> Option.getOrElse ""), signature, footer, cn)
            | _ ->
              return Ok (Some tip, None, signature, footer, cn)
  }

  member x.TryGetFormattedDocumentationForSymbol (xmlSig: string) (assembly: string) = async {
    let entities = x.GetAllEntities false
    let ent =
      entities |> List.tryFind (fun e ->
        let check = (e.Symbol.XmlDocSig = xmlSig && e.Symbol.Assembly.SimpleName = assembly)
        if not check then
          match e.Symbol with
          | FSharpEntity (_, abrvEnt, _) ->
            abrvEnt.XmlDocSig = xmlSig && abrvEnt.Assembly.SimpleName = assembly
          | _ -> false
        else
          true
      )
    let ent =
      match ent with
      | Some ent -> Some ent
      | None ->
        entities |> List.tryFind (fun e ->
          let check = (e.Symbol.XmlDocSig = xmlSig)
          if not check then
            match e.Symbol with
            | FSharpEntity (_, abrvEnt, _) ->
              abrvEnt.XmlDocSig = xmlSig
            | _ -> false
          else
            true
        )

    let symbol =
      match ent with
      | Some ent -> Some ent.Symbol
      | None ->
        entities |> List.tryPick (fun e ->
          match e.Symbol with
          | FSharpEntity (ent, _, _) ->
            match ent.MembersFunctionsAndValues |> Seq.tryFind (fun f -> f.XmlDocSig = xmlSig) with
            | Some e -> Some (e :> FSharpSymbol)
            | None ->
              match  ent.FSharpFields |> Seq.tryFind (fun f -> f.XmlDocSig = xmlSig) with
              | Some e -> Some (e :> FSharpSymbol)
              | None -> None
          | _ ->
            None
        )

    match symbol with
    | None -> return Error "No matching symbol information"
    | Some symbol ->
      match DocumentationFormatter.getTooltipDetailsFromSymbol symbol with
      | None ->
        return Error "No tooltip information"
      | Some (signature, footer, cn) ->
          return Ok (symbol.XmlDocSig, symbol.Assembly.FileName |> Option.getOrElse "", symbol.XmlDoc |> Seq.toList , signature, footer, cn)
  }

  member __.TryGetSymbolUse (pos: pos) (lineStr: LineStr) =
    async {
        match Lexer.findLongIdents(pos.Column - 1, lineStr) with
        | None ->
          return (ResultOrString.Error "No ident at this location")
        | Some(colu, identIsland) ->

        let! symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
        match symboluse with
        | None ->
          return (ResultOrString.Error "No symbol information found")
        | Some symboluse ->

        let! symboluses = checkResults.GetUsesOfSymbolInFile symboluse.Symbol
        return Ok (symboluse, symboluses) }

  member __.TryGetSignatureData (pos: pos) (lineStr: LineStr) =
    async {
        match Lexer.findLongIdents(pos.Column - 1, lineStr) with
        | None ->
          return (ResultOrString.Error "No ident at this location")
        | Some(colu, identIsland) ->

        let! symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
        match symboluse with
        | None ->
          return (ResultOrString.Error "No symbol information found")
        | Some symboluse ->
          let fsym = symboluse.Symbol
          match fsym with
          | :? FSharpMemberOrFunctionOrValue as symbol ->
            let typ = symbol.ReturnParameter.Type.Format symboluse.DisplayContext
            if symbol.IsPropertyGetterMethod then
                return Ok(typ, [], [])
            else
              let parms =
                symbol.CurriedParameterGroups
                |> Seq.map (Seq.map (fun p -> p.DisplayName, p.Type.Format symboluse.DisplayContext) >> Seq.toList )
                |> Seq.toList
              let generics =
                symbol.GenericParameters
                |> Seq.map (fun generic ->
                    generic.Name
                )
                |> Seq.toList
              // Abstract members and abstract member overrides with one () parameter seem have a list with an empty list
              // as parameters.
              match parms with
              | [ [] ] when symbol.IsMember && (not symbol.IsPropertyGetterMethod) ->
                return Ok(typ, [ [ ("unit", "unit") ] ], [])
              | _ ->
                return Ok(typ, parms, generics)
          | _ ->
            return (ResultOrString.Error "Not a member, function or value" )
    }

  member __.TryGetF1Help (pos: pos) (lineStr: LineStr) =
    async {
        match Lexer.findLongIdents(pos.Column - 1, lineStr) with
        | None -> return (ResultOrString.Error "No ident at this location")
        | Some(colu, identIsland) ->

        let! help = checkResults.GetF1Keyword(pos.Line, colu, lineStr, identIsland)
        match help with
        | None -> return (ResultOrString.Error "No symbol information found")
        | Some hlp -> return Ok hlp}

  member __.TryGetCompletions (pos: pos) (lineStr: LineStr) filter (getAllSymbols : unit -> AssemblySymbol list) = async {
    try
      let longName = FSharp.Compiler.QuickParse.GetPartialLongNameEx(lineStr, pos.Column - 2)
      let residue = longName.PartialIdent

      let getAllSymbols() =
        getAllSymbols()
        |> List.filter (fun entity -> entity.FullName.Contains "." && not (PrettyNaming.IsOperatorName entity.Symbol.DisplayName))

      let token = Lexer.getSymbol pos.Line (pos.Column - 1) lineStr SymbolLookupKind.Simple [||]
      match token with
      | Some k when k.Kind = Other -> return None
      | Some k when k.Kind = Operator  -> return None
      | Some k when k.Kind = Keyword  -> return None
      | _ ->

      let! results = checkResults.GetDeclarationListInfo(Some parseResults, pos.Line, lineStr, longName, getAllSymbols)

      let getKindPriority = function
        | CompletionItemKind.CustomOperation -> -1
        | CompletionItemKind.Property -> 0
        | CompletionItemKind.Field -> 1
        | CompletionItemKind.Method (isExtension = false) -> 2
        | CompletionItemKind.Event -> 3
        | CompletionItemKind.Argument -> 4
        | CompletionItemKind.Other -> 5
        | CompletionItemKind.Method (isExtension = true) -> 6

      let sortedDeclItems =
          results.Items
          |> Array.sortWith (fun x y ->
              let mutable n = (not x.IsResolved).CompareTo(not y.IsResolved)
              if n <> 0 then n else
                  n <- (getKindPriority x.Kind).CompareTo(getKindPriority y.Kind)
                  if n <> 0 then n else
                      n <- (not x.IsOwnMember).CompareTo(not y.IsOwnMember)
                      if n <> 0 then n else
                          n <- StringComparer.OrdinalIgnoreCase.Compare(x.Name, y.Name)
                          if n <> 0 then n else
                            x.MinorPriority.CompareTo(y.MinorPriority))
      let decls =
        match filter with
        | Some "StartsWith" -> [| for d in sortedDeclItems do if d.Name.StartsWith(residue, StringComparison.InvariantCultureIgnoreCase) then yield d |]
        | Some "Contains" -> [| for d in sortedDeclItems do if d.Name.IndexOf(residue, StringComparison.InvariantCultureIgnoreCase) >= 0 then yield d |]
        | _ -> sortedDeclItems



      let shouldKeywords = decls.Length > 0 && not results.IsForType && not results.IsError && List.isEmpty longName.QualifyingIdents
      return Some (decls, residue, shouldKeywords)
    with :? TimeoutException -> return None
  }

  member __.GetAllEntities (publicOnly: bool) : AssemblySymbol list =
      try
        let res = [
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
            let contentType = if publicOnly then Public else Full
            let content = AssemblyContentProvider.getAssemblyContent entityCache.Locking contentType fileName signatures
            yield! content
        ]
        res
      with
      | _ -> []

  member __.GetAllSymbolUsesInFile () = checkResults.GetAllUsesOfAllSymbolsInFile()

  member __.GetSemanticClassification = checkResults.GetSemanticClassification None
  member __.GetAST = parseResults.ParseTree
  member __.GetCheckResults = checkResults
  member __.GetParseResults = parseResults
  member __.FileName = parseResults.FileName

type Version = int

type FSharpCompilerServiceChecker(backgroundServiceEnabled) =
  let checker =
    FSharpChecker.Create(
      projectCacheSize = (if backgroundServiceEnabled then 3 else 200),
      keepAllBackgroundResolutions = not backgroundServiceEnabled,
      keepAssemblyContents = false,
      suggestNamesForErrors = true)

  do checker.ImplicitlyStartBackgroundWork <- not backgroundServiceEnabled

  do checker.BeforeBackgroundFileCheck.Add ignore
  let fixFileName path =
    if (try Path.GetFullPath path |> ignore; true with _ -> false) then path
    else
        match Environment.OSVersion.Platform with
        | PlatformID.Unix
        | PlatformID.MacOSX -> Environment.GetEnvironmentVariable "HOME"
        | _ -> Environment.ExpandEnvironmentVariables "%HOMEDRIVE%%HOMEPATH%"
        </> Path.GetFileName path

  let entityCache = EntityCache()

  let mutable disableInMemoryProjectReferences = false

  let clearProjectReferecnes (opts: FSharpProjectOptions) =
    if disableInMemoryProjectReferences then {opts with ReferencedProjects = [||]} else opts

  let fsxBinder = Dotnet.ProjInfo.Workspace.FCS.FsxBinder(NETFrameworkInfoProvider.netFWInfo, checker)

  let logDebug fmt =
    if Debug.verbose then Debug.print "[FSharpChecker] Current Queue Length: %d" checker.CurrentQueueLength
    Debug.print fmt

  member __.CreateFCSBinder(netFwInfo: Dotnet.ProjInfo.Workspace.NetFWInfo, loader: Dotnet.ProjInfo.Workspace.Loader) =
    Dotnet.ProjInfo.Workspace.FCS.FCSBinder(netFwInfo, loader, checker)

  member __.DisableInMemoryProjectReferences
    with get() = disableInMemoryProjectReferences
    and set(value) = disableInMemoryProjectReferences <- value

  member __.GetDependingProjects (file: FilePath) (options : seq<string * FSharpProjectOptions>) =
    let project = options |> Seq.tryFind (fun (k,_) -> k.ToUpperInvariant() = file.ToUpperInvariant())
    project |> Option.map (fun (_, option) ->
      option, [
        yield! options
               |> Seq.map snd
               |> Seq.distinctBy (fun o -> o.ProjectFileName)
               |> Seq.filter (fun o -> o.ReferencedProjects |> Array.map (fun (_,v) -> Path.GetFullPath v.ProjectFileName) |> Array.contains option.ProjectFileName )
      ])

  member __.GetProjectOptionsFromScript(file, source) = async {
#if NETCORE_FSI
    logDebug "[Opts] Getting options for script file %s" file
    let! (projOptions, _) = checker.GetProjectOptionsFromScript(file, SourceText.ofString source, useSdkRefs = true, assumeDotNetFramework = false, useFsiAuxLib = true)
    logDebug "[Opts] Resolved optiosn - %A" projOptions
#else
    let targetFramework = NETFrameworkInfoProvider.latestInstalledNETVersion ()
    let! projOptions = fsxBinder.GetProjectOptionsFromScriptBy(targetFramework, file, source)
#endif
    match FakeSupport.detectFakeScript file with
    | None -> 
      logDebug "[Opts] %s is not a FAKE script" file
      return projOptions
    | Some (detectionInfo) ->
      logDebug "[Opts] %s is a FAKE script" file
      try
        let otherOpts = FakeSupport.getProjectOptions detectionInfo
        logDebug "[Opts] Discovered FAKE options - %A" otherOpts
        return { projOptions with OtherOptions = otherOpts }
      with e ->
        logDebug "[Opts] Error in FAKE script support: %O" e
        return projOptions
  }

  member __.GetBackgroundCheckResultsForFileInProject(fn, opt) =
    logDebug "[Checker] GetBackgroundCheckResultsForFileInProject - %s" fn
    let opt = clearProjectReferecnes opt
    checker.GetBackgroundCheckResultsForFileInProject(fn, opt)
    |> Async.map (fun (pr,cr) ->  ParseAndCheckResults (pr, cr, entityCache))

  member __.FileChecked =
    checker.FileChecked

  member __.ParseFile(fn, source, fpo) =
    logDebug "[Checker] ParseFile - %s" fn
    let source = SourceText.ofString source
    checker.ParseFile(fn, source, fpo)

  member __.ParseAndCheckFileInProject(filePath, version, source, options) =
    async {
      let opName = sprintf "ParseAndCheckFileInProject - %s" filePath
      logDebug "[Checker] %s" opName
      let source = SourceText.ofString source
      let options = clearProjectReferecnes options
      let fixedFilePath = fixFileName filePath
      let! res = Async.Catch (checker.ParseAndCheckFileInProject (fixedFilePath, version, source, options, userOpName = opName))
      return
          match res with
          | Choice1Of2 (p,c)->
            let parseErrors = p.Errors |> Array.map (fun p -> p.Message)
            match c with
            | FSharpCheckFileAnswer.Aborted ->
              logDebug "[Checker] %s completed with errors %A" opName (List.ofArray p.Errors)
              ResultOrString.Error (sprintf "Check aborted (%A). Errors: %A" c parseErrors)
            | FSharpCheckFileAnswer.Succeeded(c) ->
              Ok (ParseAndCheckResults(p,c, entityCache))
          | Choice2Of2 e -> ResultOrString.Error e.Message
    }

  member __.TryGetRecentCheckResultsForFile(file, options, ?source) =
    logDebug "[Checker] TryGetRecentCheckResultsForFile - %s" file
    let source = source |> Option.map SourceText.ofString
    let options = clearProjectReferecnes options
    checker.TryGetRecentCheckResultsForFile(file, options, ?sourceText=source)
    |> Option.map (fun (pr, cr, _) -> ParseAndCheckResults (pr, cr, entityCache))

  member x.GetUsesOfSymbol (file, options : (SourceFilePath * FSharpProjectOptions) seq, symbol : FSharpSymbol) = async {
    logDebug "[Checker] GetUsesOfSymbol - %s" file
    let projects = x.GetDependingProjects file options
    return!
      match projects with
      | None -> async {return [||]}
      | Some (p, projects) -> async {
        let! res =
          [yield p; yield! projects ]
          |> Seq.map (fun (opts) -> async {
              let opts = clearProjectReferecnes opts
              let! res = checker.ParseAndCheckProject opts
              return! res.GetUsesOfSymbol symbol
            })
          |> Async.Parallel
        return res |> Array.collect id }
  }

  member __.GetDeclarations (fileName, source, options, version) = async {
    logDebug "[Checker] GetDeclarations - %s" fileName
    let source = SourceText.ofString source
    let! parseResult = checker.ParseFile(fileName, source, options)
    return parseResult.GetNavigationItems().Declarations
  }

  member __.Compile = checker.Compile

  member internal x.GetFSharpChecker() = checker
