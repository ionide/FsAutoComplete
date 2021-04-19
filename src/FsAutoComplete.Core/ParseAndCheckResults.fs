namespace FsAutoComplete

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open Utils
open FSharp.Compiler.Text
open FSharp.Compiler
open FsAutoComplete.Logging
open FSharp.UMX

[<RequireQualifiedAccess>]
type FindDeclarationResult =
    | ExternalDeclaration of Decompiler.ExternalContentPosition
    | Range of Range
    /// The declaration refers to a file.
    | File of string

type ParseAndCheckResults
    (
        parseResults: FSharpParseFileResults,
        checkResults: FSharpCheckFileResults,
        entityCache: EntityCache
    ) =

  let logger = LogProvider.getLoggerByName "ParseAndCheckResults"

  member __.TryFindDeclaration (pos: Pos) (lineStr: LineStr) = async {
    // try find identifier first
    let! identResult = __.TryFindIdentifierDeclaration pos lineStr
    match identResult with
    | Ok r -> return Ok r
    | Error identErr ->
    // then #load directive
    let! loadResult = __.TryFindLoadDirectiveSource pos lineStr
    match loadResult with
    | Ok r -> return Ok r
    | Error _ -> return Error identErr
  }

  member __.TryFindLoadDirectiveSource (pos: Pos) (lineStr: LineStr) = async {
    let tryGetFullPath fileName =
      try
        // use the parsed file name directory as base path
        let basePath = Path.GetDirectoryName(UMX.untag __.FileName)
        Some (Path.Combine(basePath, fileName))
      with
      | :? ArgumentException -> None
      | :? PathTooLongException -> None
      | :? NotSupportedException -> None

    let result =
      InteractiveDirectives.tryParseLoad lineStr pos.Column
      |> Option.bind tryGetFullPath

    match result with
    | Some file -> return Ok (FindDeclarationResult.File file)
    | None -> return Error "load directive not recognized"
  }

  member __.TryFindIdentifierDeclaration (pos: Pos) (lineStr: LineStr) =
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> async.Return (ResultOrString.Error "Could not find ident at this location")
    | Some(col, identIsland) ->
      let identIsland = Array.toList identIsland
      let declarations = checkResults.GetDeclarationLocation(pos.Line, col, lineStr, identIsland, preferFlag = false)

      let decompile assembly externalSym =
        match Decompiler.tryFindExternalDeclaration checkResults (assembly, externalSym) with
        | Ok extDec -> ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration extDec)
        | Error(Decompiler.FindExternalDeclarationError.ReferenceHasNoFileName assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' missing file name" assy.SimpleName)
        | Error(Decompiler.FindExternalDeclarationError.ReferenceNotFound assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' not found" assy)
        | Error(Decompiler.FindExternalDeclarationError.DecompileError (Decompiler.Exception(symbol, file, exn))) ->
          Error (sprintf "Error while decompiling symbol '%A' in file '%s': %s\n%s" symbol file exn.Message exn.StackTrace)

      /// these are all None because you can't easily get the source file from the external symbol information here.
      let tryGetSourceRangeForSymbol (sym: FSharpExternalSymbol): (string<NormalizedRepoPathSegment> * int * int) option =
        match sym with
        | FSharpExternalSymbol.Type name -> None
        | FSharpExternalSymbol.Constructor(typeName, args) -> None
        | FSharpExternalSymbol.Method(typeName, name, paramSyms, genericArity) -> None
        | FSharpExternalSymbol.Field(typeName, name) -> None
        | FSharpExternalSymbol.Event(typeName, name) -> None
        | FSharpExternalSymbol.Property(typeName, name) -> None

      // attempts to manually discover symbol use and externalsymbol information for a range that doesn't exist in a local file
      // bugfix/workaround for FCS returning invalid declfound for f# members.
      let tryRecoverExternalSymbolForNonexistentDecl (rangeInNonexistentFile: Range): ResultOrString<string<LocalPath> * string<NormalizedRepoPathSegment>> =
        match Lexer.findLongIdents(pos.Column - 1, lineStr) with
        | None -> ResultOrString.Error (sprintf "Range for nonexistent file found, no ident found: %s" rangeInNonexistentFile.FileName)
        | Some (col, identIsland) ->
          let identIsland = Array.toList identIsland
          let symbolUse = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)
          match symbolUse with
          | None -> ResultOrString.Error (sprintf "Range for nonexistent file found, no symboluse found: %s" rangeInNonexistentFile.FileName)
          | Some sym ->
            match sym.Symbol.Assembly.FileName with
            | Some fullFilePath ->
              Ok (UMX.tag<LocalPath> fullFilePath, UMX.tag<NormalizedRepoPathSegment> rangeInNonexistentFile.FileName)
            | None ->
              ResultOrString.Error (sprintf "Assembly '%s' declaring symbol '%s' has no location on disk" sym.Symbol.Assembly.QualifiedName sym.Symbol.DisplayName)

      async {
        match declarations with
        | FSharpFindDeclResult.DeclNotFound reason ->
          let elaboration =
            match reason with
            | FSharpFindDeclFailureReason.NoSourceCode -> "No source code was found for the declaration"
            | FSharpFindDeclFailureReason.ProvidedMember m -> sprintf "Go-to-declaration is not available for Type Provider-provided member %s" m
            | FSharpFindDeclFailureReason.ProvidedType t -> sprintf "Go-to-declaration is not available from Type Provider-provided type %s" t
            | FSharpFindDeclFailureReason.Unknown r -> r
          return ResultOrString.Error (sprintf "Could not find declaration. %s" elaboration)
        | FSharpFindDeclResult.DeclFound range when range.FileName.EndsWith(Range.rangeStartup.FileName) -> return ResultOrString.Error "Could not find declaration"
        | FSharpFindDeclResult.DeclFound range when System.IO.File.Exists range.FileName ->
          let rangeStr = range.ToString()
          logger.info (Log.setMessage "Got a declresult of {range} that supposedly exists" >> Log.addContextDestructured "range" rangeStr)
          return Ok (FindDeclarationResult.Range range)
        | FSharpFindDeclResult.DeclFound rangeInNonexistentFile ->
          let range = rangeInNonexistentFile.ToString()
          logger.warn (Log.setMessage "Got a declresult of {range} that doesn't exist" >> Log.addContextDestructured "range" range)
          match tryRecoverExternalSymbolForNonexistentDecl rangeInNonexistentFile with
          | Ok (assemblyFile, sourceFile) ->
            match! Sourcelink.tryFetchSourcelinkFile assemblyFile sourceFile with
            | Ok localFilePath ->
              return ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration { File = UMX.untag localFilePath; Line = rangeInNonexistentFile.StartLine; Column = rangeInNonexistentFile.StartColumn })
            | Error reason ->
              return ResultOrString.Error (sprintf "%A" reason)
          | Error e -> return Error e
        | FSharpFindDeclResult.ExternalDecl (assembly, externalSym) ->
          // not enough info on external symbols to get a range-like thing :(
          match tryGetSourceRangeForSymbol externalSym with
          | Some (sourceFile, line, column) ->
            match! Sourcelink.tryFetchSourcelinkFile (UMX.tag<LocalPath> assembly) sourceFile with
            | Ok localFilePath ->
              return ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration { File = UMX.untag localFilePath; Line = line; Column = column })
            | Error reason ->
              logger.info (Log.setMessage "no sourcelink info for {assembly}, decompiling instead" >> Log.addContextDestructured "assembly" assembly)
              return decompile assembly externalSym
          | None ->
            return decompile assembly externalSym
    }

  member __.TryFindTypeDeclaration (pos: Pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None ->
      return Error "Cannot find ident at this location"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      let symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)
      match symbol with
      | None ->
        return Error "Cannot find symbol at this location"
      | Some sym ->

        let tryGetTypeDef (t: FSharpType option) =
          t |> Option.bind (fun t -> if t.HasTypeDefinition then Some t.TypeDefinition else None)

        let rec tryGetSource (ty: FSharpEntity option) = async {
            match ty |> Option.map (fun ty -> ty, ty.DeclarationLocation) with
            | Some (_, loc) when File.Exists loc.FileName ->
                return Ok (FindDeclarationResult.Range loc)
            | Some (ty, loc) ->
                match ty.Assembly.FileName with
                | Some dllFile ->
                    let dllFile = UMX.tag<LocalPath> dllFile
                    let sourceFile = UMX.tag<NormalizedRepoPathSegment> loc.FileName
                    let! source = Sourcelink.tryFetchSourcelinkFile dllFile sourceFile
                    match source with
                    | Ok localFilePath ->
                        return Ok (FindDeclarationResult.ExternalDeclaration { File = UMX.untag localFilePath; Line = loc.StartLine; Column = loc.StartColumn })
                    | Error _ ->
                        return! tryDecompile ty
                | None ->
                    return! tryDecompile ty
            | None ->
                return Error "No type information for the symbol at this location"
          }
        and tryDecompile (ty: FSharpEntity) = async {
            match ty.TryFullName with
            | Some fullName ->
                let externalSym = FSharpExternalSymbol.Type fullName
                // from TryFindIdentifierDeclaration
                let decompile assembly externalSym =
                  match Decompiler.tryFindExternalDeclaration checkResults (assembly, externalSym) with
                  | Ok extDec -> ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration extDec)
                  | Error(Decompiler.FindExternalDeclarationError.ReferenceHasNoFileName assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' missing file name" assy.SimpleName)
                  | Error(Decompiler.FindExternalDeclarationError.ReferenceNotFound assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' not found" assy)
                  | Error(Decompiler.FindExternalDeclarationError.DecompileError (Decompiler.Exception(symbol, file, exn))) ->
                      Error (sprintf "Error while decompiling symbol '%A' in file '%s': %s\n%s" symbol file exn.Message exn.StackTrace)

                return decompile ty.Assembly.SimpleName externalSym
            | None ->
                // might be abbreviated type (like string)
                return!
                  if ty.IsFSharpAbbreviation then Some ty.AbbreviatedType else None
                  |> tryGetTypeDef
                  |> tryGetSource
          }

        let ty =
          match sym with
          | SymbolUse.Field f -> Some f.FieldType |> tryGetTypeDef
          | SymbolUse.Constructor c -> c.DeclaringEntity
          | SymbolUse.Property p when p.IsPropertyGetterMethod ->
              Some p.ReturnParameter.Type |> tryGetTypeDef
          | SymbolUse.Val v -> v.FullTypeSafe |> tryGetTypeDef
          | SymbolUse.Entity (e, _) -> Some e
          | SymbolUse.UnionCase c -> Some c.ReturnType |> tryGetTypeDef
          | SymbolUse.Parameter p -> Some p.Type |> tryGetTypeDef
          | _ -> None

        return! tryGetSource ty
  }

  member __.TryGetToolTip (pos: Pos) (lineStr: LineStr) =
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> ResultOrString.Error "Cannot find ident for tooltip"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) ->
          match identIsland with
          | [ident] ->
             match KeywordList.keywordTooltips.TryGetValue ident with
             | true, tip ->
                Ok tip
             | _ ->
                ResultOrString.Error "No tooltip information"
          | _ ->
            ResultOrString.Error "No tooltip information"
      | _ ->
        Ok(tip)

  member __.TryGetToolTipEnhanced (pos: Pos) (lineStr: LineStr) =
    match Lexer.findLongIdents(pos.Column, lineStr) with
    | None -> Error "Cannot find ident for tooltip"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      let symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)

      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) && symbol.IsNone ->
          match identIsland with
          | [ident] ->
             match KeywordList.keywordTooltips.TryGetValue ident with
             | true, tip ->
                Ok (tip, ident, "", None)
             | _ ->
                Error "No tooltip information"
          | _ ->
            Error "No tooltip information"
      | _ ->
        match symbol with
        | None ->
          Error "No tooltip information"
        | Some symbol ->

          match SignatureFormatter.getTooltipDetailsFromSymbolUse symbol with
          | None ->
            Error "No tooltip information"
          | Some (signature, footer) ->
              let typeDoc = getTypeIfConstructor symbol.Symbol |> Option.map (fun n -> n.XmlDocSig)
              Ok (tip, signature, footer, typeDoc)

  member __.TryGetFormattedDocumentation (pos: Pos) (lineStr: LineStr) =
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> Error "Cannot find ident"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      let symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)

      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) && symbol.IsNone ->
          match identIsland with
          | [ident] ->
             match KeywordList.keywordTooltips.TryGetValue ident with
             | true, tip ->
                Ok (Some tip, None, (ident, (DocumentationFormatter.emptyTypeTip)), "", "")
             | _ ->
                Error "No tooltip information"
          | _ ->
            Error "No documentation information"
      | _ ->
      match symbol with
      | None ->
        Error "No documentation information"
      | Some symbol ->
        match DocumentationFormatter.getTooltipDetailsFromSymbolUse symbol with
        | None ->
          Error "No documentation information"
        | Some (signature, footer, cn) ->
            match symbol with
            | SymbolUse.TypeAbbreviation symbol ->
              Ok (None, Some (symbol.GetAbbreviatedParent().XmlDocSig, symbol.GetAbbreviatedParent().Assembly.FileName |> Option.defaultValue ""), signature, footer, cn)
            | _ ->
              Ok (Some tip, None, signature, footer, cn)

  member x.TryGetFormattedDocumentationForSymbol (xmlSig: string) (assembly: string) =
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
    | None -> Error "No matching symbol information"
    | Some symbol ->
      match DocumentationFormatter.getTooltipDetailsFromSymbol symbol with
      | None ->
        Error "No tooltip information"
      | Some (signature, footer, cn) ->
          Ok (symbol.XmlDocSig, symbol.Assembly.FileName |> Option.defaultValue "", symbol.XmlDoc |> Seq.toList , signature, footer, cn)

  member __.TryGetSymbolUse (pos: Pos) (lineStr: LineStr): FSharpSymbolUse option =
    match Lexer.findLongIdents(pos.Column, lineStr) with
    | None -> None
    | Some(colu, identIsland) ->
      let identIsland = Array.toList identIsland
      checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)

  member x.TryGetSymbolUseAndUsages (pos: Pos) (lineStr: LineStr) =
    let symboluse = x.TryGetSymbolUse pos lineStr
    match symboluse with
    | None ->
      ResultOrString.Error "No symbol information found"
    | Some symboluse ->
      let symboluses = checkResults.GetUsesOfSymbolInFile symboluse.Symbol
      Ok (symboluse, symboluses)

  member __.TryGetSignatureData (pos: Pos) (lineStr: LineStr) =
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None ->
      ResultOrString.Error "No ident at this location"
    | Some(colu, identIsland) ->

      let identIsland = Array.toList identIsland
      let symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
      match symboluse with
      | None ->
        ResultOrString.Error "No symbol information found"
      | Some symboluse ->
        let fsym = symboluse.Symbol
        match fsym with
        | :? FSharpMemberOrFunctionOrValue as symbol ->
          let typ = symbol.ReturnParameter.Type.Format (symboluse.DisplayContext.WithPrefixGenericParameters())
          if symbol.IsPropertyGetterMethod then
              Ok(typ, [], [])
          else
            let parms =
              symbol.CurriedParameterGroups
              |> Seq.map (Seq.map (fun p -> p.DisplayName, p.Type.Format (symboluse.DisplayContext.WithPrefixGenericParameters())) >> Seq.toList )
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
              Ok(typ, [ [ ("unit", "unit") ] ], [])
            | _ ->
              Ok(typ, parms, generics)
        | :? FSharpField as symbol ->
          let typ = symbol.FieldType.Format symboluse.DisplayContext
          Ok(typ, [], [])
        | _ ->
          ResultOrString.Error "Not a member, function or value"

  member __.TryGetF1Help (pos: Pos) (lineStr: LineStr) =
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> ResultOrString.Error "No ident at this location"
    | Some(colu, identIsland) ->

      let identIsland = Array.toList identIsland
      let help = checkResults.GetF1Keyword(pos.Line, colu, lineStr, identIsland)
      match help with
      | None -> ResultOrString.Error "No symbol information found"
      | Some hlp -> Ok hlp

  member __.TryGetCompletions (pos: Pos) (lineStr: LineStr) filter (getAllSymbols : unit -> AssemblySymbol list) = async {
    try
      let longName = FSharp.Compiler.SourceCodeServices.QuickParse.GetPartialLongNameEx(lineStr, pos.Column - 1)
      let residue = longName.PartialIdent
      logger.info (Log.setMessage "TryGetCompletions - long name: {longName}" >> Log.addContextDestructured "longName" longName)

      let getAllSymbols() =
        getAllSymbols()
        |> List.filter (fun entity -> entity.FullName.Contains "." && not (PrettyNaming.IsOperatorName entity.Symbol.DisplayName))

      let token = Lexer.getSymbol pos.Line (pos.Column - 1) lineStr SymbolLookupKind.Simple [||]
      logger.info (Log.setMessage "TryGetCompletions - token: {token}" >> Log.addContextDestructured "token" token)
      let isEmpty = longName.QualifyingIdents.IsEmpty && String.IsNullOrWhiteSpace longName.PartialIdent && longName.LastDotPos.IsNone

      match token with
      | Some k when k.Kind = Other && not isEmpty -> return None
      | Some k when k.Kind = Operator  -> return None
      | Some k when k.Kind = Keyword  -> return None
      | _ ->

      let results = checkResults.GetDeclarationListInfo(Some parseResults, pos.Line, lineStr, longName, getAllSymbols)

      let getKindPriority = function
        | FSharpCompletionItemKind.CustomOperation -> -1
        | FSharpCompletionItemKind.Property -> 0
        | FSharpCompletionItemKind.Field -> 1
        | FSharpCompletionItemKind.Method (isExtension = false) -> 2
        | FSharpCompletionItemKind.Event -> 3
        | FSharpCompletionItemKind.Argument -> 4
        | FSharpCompletionItemKind.Other -> 5
        | FSharpCompletionItemKind.Method (isExtension = true) -> 6

      let decls =
        match filter with
        | Some "StartsWith" ->
          results.Items
          |> Array.filter (fun d -> d.Name.StartsWith(residue, StringComparison.InvariantCultureIgnoreCase))
        | Some "Contains" ->
          results.Items
          |> Array.filter (fun d -> d.Name.IndexOf(residue, StringComparison.InvariantCultureIgnoreCase) >= 0)
        | _ -> results.Items

      let sortedDecls =
          decls
          |> Array.sortWith (fun x y ->
              let transformKind (item: FSharpDeclarationListItem) =
                if item.Kind = FSharpCompletionItemKind.Field && item.Glyph = FSharpGlyph.Method then
                  FSharpCompletionItemKind.Method false
                elif item.Kind = FSharpCompletionItemKind.Argument && item.Glyph = FSharpGlyph.Property then
                  FSharpCompletionItemKind.Property
                else
                  item.Kind

              let mutable n = (not x.IsResolved).CompareTo(not y.IsResolved)
              if n <> 0 then n else
                  n <- (getKindPriority <| transformKind x).CompareTo(getKindPriority <| transformKind y)
                  if n <> 0 then n else
                      n <- (not x.IsOwnMember).CompareTo(not y.IsOwnMember)
                      if n <> 0 then n else
                          n <- StringComparer.OrdinalIgnoreCase.Compare(x.Name, y.Name)
                          if n <> 0 then n else
                            x.MinorPriority.CompareTo(y.MinorPriority))

      let shouldKeywords = sortedDecls.Length > 0 && not results.IsForType && not results.IsError && List.isEmpty longName.QualifyingIdents
      return Some (sortedDecls, residue, shouldKeywords)
    with :? TimeoutException -> return None
  }

  member __.GetAllEntities (publicOnly: bool) : AssemblySymbol list =
      try
        let res = [
          yield! AssemblyContentProvider.getAssemblySignatureContent AssemblyContentType.Full checkResults.PartialAssemblySignature
          let ctx = checkResults.ProjectContext
          let assembliesByFileName =
            ctx.GetReferencedAssemblies()
            |> List.groupBy (fun asm -> asm.FileName)
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

  // member this.GetExpandedType (pos: Pos) =
  //   match parseResults.ParseTree with
  //   | Some input ->
  //     AstTraversal.Traverse(pos, input, {
  //       new AstTraversal.AstVisitorBase<_>() with
  //     })
  //   | None -> None


  member __.GetAST = parseResults.ParseTree
  member __.GetCheckResults = checkResults
  member __.GetParseResults = parseResults
  member __.FileName: string<LocalPath> = UMX.tag parseResults.FileName
