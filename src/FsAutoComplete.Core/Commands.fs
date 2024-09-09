namespace FsAutoComplete

open System
open System.IO
open Fantomas.Client.Contracts
open FsAutoComplete.Logging
open FsAutoComplete.UnionPatternMatchCaseGenerator
open FsAutoComplete.RecordStubGenerator
open Utils
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open Ionide.ProjInfo
open Ionide.ProjInfo.ProjectSystem
open FsToolkit.ErrorHandling
open FSharp.Analyzers
open FSharp.UMX
open FSharp.Compiler.Tokenization
open SymbolLocation
open FSharp.Compiler.Symbols
open System.Collections.Generic
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text.Range

[<RequireQualifiedAccess>]
type LocationResponse<'a> = Use of 'a

[<RequireQualifiedAccess>]
type HelpText =
  | Simple of symbol: string * text: string
  | Full of symbol: string * tip: ToolTipText * textEdits: CompletionNamespaceInsert option

[<RequireQualifiedAccess>]
type CoreResponse<'a> =
  | InfoRes of text: string
  | ErrorRes of text: string
  | Res of 'a

[<RequireQualifiedAccess>]
type FormatDocumentResponse =
  | Formatted of source: IFSACSourceText * formatted: string
  | FormattedRange of source: IFSACSourceText * formatted: string * range: FormatSelectionRange
  | UnChanged
  | Ignored
  | ToolNotPresent
  | Error of string

/// Represents a desired change to a given file
type DocumentEdit =
  { InsertPosition: pos
    InsertText: string }

module private Result =
  let ofCoreResponse (r: CoreResponse<'a>) =
    match r with
    | CoreResponse.Res a -> Ok(Some a)
    | CoreResponse.InfoRes _ -> Ok None
    | CoreResponse.ErrorRes msg -> Error msg


module AsyncResult =

  let inline mapErrorRes ar : Async<CoreResponse<'a>> = AsyncResult.foldResult id CoreResponse.ErrorRes ar

  let recoverCancellationGeneric (ar: Async<Result<'t, exn>>) recoverInternal =
    AsyncResult.foldResult id recoverInternal ar

  let recoverCancellation (ar: Async<Result<CoreResponse<'t>, exn>>) =
    recoverCancellationGeneric ar (sprintf "Request cancelled (exn was %A)" >> CoreResponse.InfoRes)

  let recoverCancellationIgnore (ar: Async<Result<unit, exn>>) = ar |> AsyncResult.foldResult id (ignore<exn>)

[<RequireQualifiedAccess>]
type NotificationEvent =
  | ParseError of errors: FSharpDiagnostic[] * file: string<LocalPath> * version: int
  | Workspace of ProjectSystem.ProjectResponse
  | AnalyzerMessage of messages: FSharp.Analyzers.SDK.Message[] * file: string<LocalPath> * version: int
  | UnusedOpens of file: string<LocalPath> * opens: Range[] * version: int
  // | Lint of file: string<LocalPath> * warningsWithCodes: Lint.EnrichedLintWarning list
  | UnusedDeclarations of file: string<LocalPath> * decls: range[] * version: int
  | SimplifyNames of file: string<LocalPath> * names: SimplifyNames.SimplifiableRange[] * version: int
  | UnnecessaryParentheses of file: string<LocalPath> * ranges: range[] * version: int
  | Canceled of errorMessage: string
  | FileParsed of string<LocalPath>
  | TestDetected of file: string<LocalPath> * tests: TestAdapter.TestAdapterEntry<range>[]

module Commands =
  open System.Collections.Concurrent
  let fantomasLogger = LogProvider.getLoggerByName "Fantomas"
  let commandsLogger = LogProvider.getLoggerByName "Commands"

  let addFile (fsprojPath: string) fileVirtPath =
    async {
      try
        let dir = Path.GetDirectoryName fsprojPath
        let newFilePath = Path.Combine(dir, fileVirtPath)
        let fileInfo = FileInfo(newFilePath)

        // Ensure the destination directory exist
        if not fileInfo.Directory.Exists then
          fileInfo.Directory.Create()

        (File.Open(newFilePath, FileMode.OpenOrCreate)).Close()

        let result = FsProjEditor.addFile fsprojPath fileVirtPath

        match result with
        | Ok() -> return CoreResponse.Res()
        | Error msg -> return CoreResponse.ErrorRes msg
      with ex ->
        return CoreResponse.ErrorRes ex.Message
    }

  let addFileAbove (fsprojPath: string) (fileVirtPath: string) newFileName =
    async {
      try
        let dir = Path.GetDirectoryName fsprojPath
        let virtPathDir = Path.GetDirectoryName fileVirtPath

        let newFilePath = Path.Combine(dir, virtPathDir, newFileName)
        let fileInfo = FileInfo(newFilePath)

        // Ensure the destination directory exist
        if not fileInfo.Directory.Exists then
          fileInfo.Directory.Create()

        (File.Open(newFilePath, FileMode.OpenOrCreate)).Close()

        let newVirtPath = Path.Combine(virtPathDir, newFileName)
        let result = FsProjEditor.addFileAbove fsprojPath fileVirtPath newVirtPath

        match result with
        | Ok() -> return CoreResponse.Res()
        | Error msg -> return CoreResponse.ErrorRes msg

      with ex ->
        return CoreResponse.ErrorRes ex.Message
    }

  let addFileBelow (fsprojPath: string) (fileVirtPath: string) newFileName =
    async {
      try
        let dir = Path.GetDirectoryName fsprojPath
        let virtPathDir = Path.GetDirectoryName fileVirtPath

        let newFilePath = Path.Combine(dir, virtPathDir, newFileName)
        let fileInfo = FileInfo(newFilePath)

        // Ensure the destination directory exist
        if not fileInfo.Directory.Exists then
          fileInfo.Directory.Create()

        (File.Open(newFilePath, FileMode.OpenOrCreate)).Close()

        let newVirtPath = Path.Combine(virtPathDir, newFileName)
        let result = FsProjEditor.addFileBelow fsprojPath fileVirtPath newVirtPath

        match result with
        | Ok() -> return CoreResponse.Res()
        | Error msg -> return CoreResponse.ErrorRes msg
      with ex ->
        return CoreResponse.ErrorRes ex.Message
    }

  let renameFile (fsprojPath: string) (oldFileVirtualPath: string) (newFileName: string) =
    async {
      try
        let dir = Path.GetDirectoryName fsprojPath
        let oldFilePath = Path.Combine(dir, oldFileVirtualPath)
        let oldFileInfo = FileInfo(oldFilePath)

        let newFilePath = Path.Combine(oldFileInfo.Directory.FullName, newFileName)

        File.Move(oldFilePath, newFilePath)

        let newVirtPath =
          Path.Combine(Path.GetDirectoryName oldFileVirtualPath, newFileName)

        FsProjEditor.renameFile fsprojPath oldFileVirtualPath newVirtPath
        return CoreResponse.Res()
      with ex ->
        return CoreResponse.ErrorRes ex.Message
    }

  let removeFile fsprojPath fileVirtPath =
    async {
      FsProjEditor.removeFile fsprojPath fileVirtPath
      return CoreResponse.Res()
    }

  let addExistingFile fsprojPath fileVirtPath =
    async {
      let result = FsProjEditor.addExistingFile fsprojPath fileVirtPath

      match result with
      | Ok() -> return CoreResponse.Res()
      | Error msg -> return CoreResponse.ErrorRes msg
    }

  let getRangesAtPosition (getParseResultsForFile: _ -> Async<Result<FSharpParseFileResults, _>>) file positions =
    asyncResult {
      let! ast = getParseResultsForFile file
      return positions |> List.map (FoldingRange.getRangesAtPosition ast.ParseTree)
    }

  let scopesForFile
    (getParseResultsForFile: _ -> Async<Result<IFSACSourceText * FSharpParseFileResults, _>>)
    (file: string<LocalPath>)
    =
    asyncResult {

      let! (text, ast) = getParseResultsForFile file

      let ranges = Structure.getOutliningRanges (text.Lines) ast.ParseTree

      return ranges
    }

  let docForText (lines: IFSACSourceText) (tyRes: ParseAndCheckResults) : Document =
    { LineCount = lines.GetLineCount()
      FullName = tyRes.FileName // from the compiler, assumed safe
      GetText = fun _ -> string lines
      GetLineText0 = fun i -> (lines :> ISourceText).GetLineString i
      GetLineText1 = fun i -> (lines :> ISourceText).GetLineString(i - 1) }

  let getAbstractClassStub
    tryFindAbstractClassExprInBufferAtPos
    writeAbstractClassStub
    (tyRes: ParseAndCheckResults)
    (objExprRange: Range)
    (lines: IFSACSourceText)
    (lineStr: LineStr)
    =
    asyncResult {
      let! abstractClass =
        tryFindAbstractClassExprInBufferAtPos objExprRange.Start lines
        |> Async.map (Result.ofOption (fun _ -> CoreResponse.InfoRes "Abstract class at position not found"))

      let! inserts =
        writeAbstractClassStub tyRes lines lineStr abstractClass
        |> AsyncResult.ofOption (fun _ -> CoreResponse.InfoRes "Didn't need to write an abstract class")

      return CoreResponse.Res inserts
    }

  let getRecordStub
    tryFindRecordDefinitionFromPos
    (tyRes: ParseAndCheckResults)
    (pos: Position)
    (lines: IFSACSourceText)
    =
    async {
      let doc = docForText lines tyRes
      let! res = tryFindRecordDefinitionFromPos pos doc

      match res with
      | None -> return CoreResponse.InfoRes "Record at position not found"
      | Some(recordEpr, (Some recordDefinition), insertionPos) ->
        if shouldGenerateRecordStub recordEpr recordDefinition then
          let result = formatRecord insertionPos "$1" recordDefinition recordEpr.FieldExprList
          return CoreResponse.Res(result, insertionPos.InsertionPos)
        else
          return CoreResponse.InfoRes "Record at position not found"
      | _ -> return CoreResponse.InfoRes "Record at position not found"
    }

  let getNamespaceSuggestions (tyRes: ParseAndCheckResults) (pos: Position) (line: LineStr) =
    async {
      match Lexer.findLongIdents (uint32 pos.Column, line) with
      | None -> return CoreResponse.InfoRes "Ident not found"
      | Some(_, idents) ->
        match ParsedInput.GetEntityKind(pos, tyRes.GetParseResults.ParseTree) with
        | None -> return CoreResponse.InfoRes "EntityKind not found"
        | Some entityKind ->

          let symbol =
            Lexer.getSymbol (uint32 pos.Line) (uint32 pos.Column) line SymbolLookupKind.Fuzzy [||]

          match symbol with
          | None -> return CoreResponse.InfoRes "Symbol at position not found"
          | Some sym ->


            let entities = tyRes.GetAllEntities true

            let isAttribute = entityKind = EntityKind.Attribute

            let entities =
              entities
              |> List.filter (fun e ->
                match entityKind, (e.Kind LookupType.Fuzzy) with
                | EntityKind.Attribute, EntityKind.Attribute
                | EntityKind.Type, (EntityKind.Type | EntityKind.Attribute)
                | EntityKind.FunctionOrValue _, _ -> true
                | EntityKind.Attribute, _
                | _, EntityKind.Module _
                | EntityKind.Module _, _
                | EntityKind.Type, _ -> false)

            let maybeUnresolvedIdents =
              idents |> Array.map (fun ident -> { Ident = ident; Resolved = false })

            let entities =
              entities
              |> List.collect (fun e ->
                [ yield e.TopRequireQualifiedAccessParent, e.AutoOpenParent, e.Namespace, e.CleanedIdents
                  if isAttribute then
                    let lastIdent = e.CleanedIdents.[e.CleanedIdents.Length - 1]

                    if
                      (e.Kind LookupType.Fuzzy) = EntityKind.Attribute
                      && lastIdent.EndsWith("Attribute", StringComparison.Ordinal)
                    then
                      yield
                        e.TopRequireQualifiedAccessParent,
                        e.AutoOpenParent,
                        e.Namespace,
                        e.CleanedIdents
                        |> Array.replace (e.CleanedIdents.Length - 1) (lastIdent.Substring(0, lastIdent.Length - 9)) ])

            let createEntity =
              ParsedInput.TryFindInsertionContext
                pos.Line
                tyRes.GetParseResults.ParseTree
                maybeUnresolvedIdents
                OpenStatementInsertionPoint.Nearest

            let word = sym.Text

            let candidates = entities |> Seq.collect createEntity |> Seq.toList

            let openNamespace =
              candidates
              |> List.choose (fun (entity, ctx) ->
                entity.Namespace |> Option.map (fun ns -> ns, entity.FullDisplayName, ctx))
              |> List.groupBy (fun (ns, _, _) -> ns)
              |> List.map (fun (ns, xs) ->
                ns,
                xs
                |> List.map (fun (_, name, ctx) -> name, ctx)
                |> List.distinctBy (fun (name, _) -> name)
                |> List.sortBy fst)
              |> List.collect (fun (ns, names) ->
                let multipleNames =
                  match names with
                  | [] -> false
                  | [ _ ] -> false
                  | _ -> true

                names |> List.map (fun (name, ctx) -> ns, name, ctx, multipleNames))

            let qualifySymbolActions =
              candidates
              |> List.map (fun (entity, _) -> entity.FullRelativeName, entity.Qualifier)
              |> List.distinct
              |> List.sort

            return CoreResponse.Res(word, openNamespace, qualifySymbolActions)
    }

  let getUnionPatternMatchCases
    tryFindUnionDefinitionFromPos
    (tyRes: ParseAndCheckResults)
    (pos: Position)
    (lines: ISourceText)
    =
    async {

      let doc =
        { Document.LineCount = lines.Length
          FullName = tyRes.FileName
          GetText = fun _ -> string lines
          GetLineText0 = fun i -> lines.GetLineString i
          GetLineText1 = fun i -> lines.GetLineString(i - 1) }

      let! res = tryFindUnionDefinitionFromPos pos doc

      match res with
      | None -> return CoreResponse.InfoRes "Union at position not found"
      | Some(patMatchExpr, unionTypeDefinition, insertionPos) ->

        if shouldGenerateUnionPatternMatchCases patMatchExpr unionTypeDefinition then
          let result = formatMatchExpr insertionPos "$1" patMatchExpr unionTypeDefinition

          return CoreResponse.Res(result, insertionPos.InsertionPos)
        else
          return CoreResponse.InfoRes "Union at position not found"
    }

  let formatSelection
    (tryGetFileCheckerOptionsWithLines: _ -> Async<Result<IFSACSourceText, _>>)
    (formatSelectionAsync: _ -> System.Threading.Tasks.Task<FantomasResponse>)
    (file: string<LocalPath>)
    (rangeToFormat: FormatSelectionRange)
    : Async<Result<FormatDocumentResponse, string>> =
    asyncResult {
      try
        let filePath = (UMX.untag file)
        let! text = tryGetFileCheckerOptionsWithLines file
        let currentCode = string text

        let! fantomasResponse =
          formatSelectionAsync
            { SourceCode = currentCode
              FilePath = filePath
              Config = None
              Range = rangeToFormat }

        match fantomasResponse with
        | { Code = 1
            Content = Some code
            SelectedRange = Some range } ->
          fantomasLogger.debug (Log.setMessage (sprintf "Fantomas daemon was able to format selection in \"%A\"" file))
          return FormatDocumentResponse.FormattedRange(text, code, range)
        | { Code = 2 } ->
          fantomasLogger.debug (Log.setMessage (sprintf "\"%A\" did not change after formatting" file))
          return FormatDocumentResponse.UnChanged
        | { Code = 3; Content = Some error } ->
          fantomasLogger.error (Log.setMessage (sprintf "Error while formatting \"%A\"\n%s" file error))
          return FormatDocumentResponse.Error(sprintf "Formatting failed!\n%A" fantomasResponse)
        | { Code = 4 } ->
          fantomasLogger.debug (Log.setMessage (sprintf "\"%A\" was listed in a .fantomasignore file" file))
          return FormatDocumentResponse.Ignored
        | { Code = 6 } -> return FormatDocumentResponse.ToolNotPresent
        | _ ->
          fantomasLogger.warn (
            Log.setMessage (
              sprintf
                "Fantomas daemon was unable to format \"%A\", due to unexpected result code %i\n%A"
                file
                fantomasResponse.Code
                fantomasResponse
            )
          )

          return FormatDocumentResponse.Error(sprintf "Formatting failed!\n%A" fantomasResponse)
      with ex ->
        fantomasLogger.warn (
          Log.setMessage "Errors while formatting file, defaulting to previous content. Error message was {message}"
          >> Log.addContextDestructured "message" ex.Message
          >> Log.addExn ex
        )

        return! Core.Error ex.Message
    }

  let formatDocument
    (tryGetFileCheckerOptionsWithLines: _ -> Async<Result<IFSACSourceText, _>>)
    (formatDocumentAsync: _ -> System.Threading.Tasks.Task<FantomasResponse>)
    (file: string<LocalPath>)
    : Async<Result<FormatDocumentResponse, string>> =
    asyncResult {
      try
        let filePath = (UMX.untag file)
        let! text = tryGetFileCheckerOptionsWithLines file
        let currentCode = string text

        let! fantomasResponse =
          formatDocumentAsync
            { SourceCode = currentCode
              FilePath = filePath
              Config = None
              Cursor = None }

        match fantomasResponse with
        | { Code = 1; Content = Some code } ->
          fantomasLogger.debug (Log.setMessage (sprintf "Fantomas daemon was able to format \"%A\"" file))
          return FormatDocumentResponse.Formatted(text, code)
        | { Code = 2 } ->
          fantomasLogger.debug (Log.setMessage (sprintf "\"%A\" did not change after formatting" file))
          return FormatDocumentResponse.UnChanged
        | { Code = 3; Content = Some error } ->
          fantomasLogger.error (Log.setMessage (sprintf "Error while formatting \"%A\"\n%s" file error))
          return FormatDocumentResponse.Error(sprintf "Formatting failed!\n%A" fantomasResponse)
        | { Code = 4 } ->
          fantomasLogger.debug (Log.setMessage (sprintf "\"%A\" was listed in a .fantomasignore file" file))
          return FormatDocumentResponse.Ignored
        | { Code = 6 } -> return FormatDocumentResponse.ToolNotPresent
        | _ ->
          fantomasLogger.warn (
            Log.setMessage (
              sprintf
                "Fantomas daemon was unable to format \"%A\", due to unexpected result code %i\n%A"
                file
                fantomasResponse.Code
                fantomasResponse
            )
          )

          return FormatDocumentResponse.Error(sprintf "Formatting failed!\n%A" fantomasResponse)
      with ex ->
        fantomasLogger.warn (
          Log.setMessage "Errors while formatting file, defaulting to previous content. Error message was {message}"
          >> Log.addContextDestructured "message" ex.Message
          >> Log.addExn ex
        )

        return! Core.Error ex.Message
    }

  let symbolImplementationProject
    getProjectOptions
    getUsesOfSymbol
    getAllProjects
    (tyRes: ParseAndCheckResults)
    (pos: Position)
    lineStr
    =

    let filterSymbols symbols =
      if Utils.isSignatureFile (UMX.untag tyRes.FileName) then
        let implFile = Utils.toFSharpFile (UMX.untag tyRes.FileName)
        symbols |> Array.filter (fun (su: FSharpSymbolUse) -> su.FileName = implFile)
      else
        symbols
        |> Array.where (fun (su: FSharpSymbolUse) ->
          su.IsFromDispatchSlotImplementation
          || (su.IsFromType
              && not (tyRes.GetParseResults.IsTypeAnnotationGivenAtPosition(su.Range.Start))))

    async {
      match tyRes.TryGetSymbolUseAndUsages pos lineStr with
      | Ok(sym, usages) ->
        let fsym = sym.Symbol

        if fsym.IsPrivateToFile then
          return CoreResponse.Res(LocationResponse.Use(sym, filterSymbols usages))
        else if fsym.IsInternalToProject then
          let! opts = getProjectOptions tyRes.FileName
          let! symbols = getUsesOfSymbol (tyRes.FileName, [ UMX.untag tyRes.FileName, opts ], sym.Symbol)
          return CoreResponse.Res(LocationResponse.Use(sym, filterSymbols symbols))
        else
          let! projs = getAllProjects ()
          let! symbols = getUsesOfSymbol (tyRes.FileName, projs, sym.Symbol)
          let symbols = filterSymbols symbols
          return CoreResponse.Res(LocationResponse.Use(sym, filterSymbols symbols))
      | Error e -> return CoreResponse.ErrorRes e
    }

  let typesig (tyRes: ParseAndCheckResults) (pos: Position) lineStr = tyRes.TryGetToolTip pos lineStr

  // Calculates pipeline hints for now as in fSharp/pipelineHint with a bit of formatting on the hints
  let inlineValues (contents: IFSACSourceText) (tyRes: ParseAndCheckResults) : Async<(pos * String)[]> =
    asyncResult {
      // Debug.waitForDebuggerAttached "AdaptiveServer"
      let getSignatureAtPos pos =
        option {
          let! lineStr = contents.GetLine pos

          let! tip = tyRes.TryGetToolTip pos lineStr

          return TipFormatter.extractGenericParameters tip
        }
        |> Option.defaultValue []

      let areTokensCommentOrWhitespace (tokens: FSharpTokenInfo list) =
        tokens
        |> List.exists (fun token ->
          token.CharClass <> FSharpTokenCharKind.Comment
          && token.CharClass <> FSharpTokenCharKind.WhiteSpace
          && token.CharClass <> FSharpTokenCharKind.LineComment)
        |> not

      let getStartingPipe =
        function
        | y :: _ when y.TokenName.ToUpper() = "INFIX_BAR_OP" -> Some y
        | x :: y :: _ when x.TokenName.ToUpper() = "WHITESPACE" && y.TokenName.ToUpper() = "INFIX_BAR_OP" -> Some y
        | _ -> None

      let folder (lastExpressionLine, lastExpressionLineWasPipe, acc) (currentIndex, currentTokens) =
        let isCommentOrWhitespace = areTokensCommentOrWhitespace currentTokens

        let isPipe = getStartingPipe currentTokens

        match isCommentOrWhitespace, isPipe with
        | true, _ -> lastExpressionLine, lastExpressionLineWasPipe, acc
        | false, Some pipe ->
          currentIndex, true, (lastExpressionLine, lastExpressionLineWasPipe, currentIndex, pipe) :: acc
        | false, None -> currentIndex, false, acc

      // Signature looks like <T> is Async<unit>
      let inline removeSignPrefix (s: String) = s.Split(" is ") |> Array.tryLast |> Option.defaultValue ""

      let hints =
        Array.init ((contents: ISourceText).GetLineCount()) (fun line -> (contents: ISourceText).GetLineString line)
        |> Array.map (Lexer.tokenizeLine [||])
        |> Array.mapi (fun currentIndex currentTokens -> currentIndex, currentTokens)
        |> Array.fold folder (0, false, [])
        |> (fun (_, _, third) -> third |> Array.ofList)
        |> Array.Parallel.map (fun (lastExpressionLine, lastExpressionLineWasPipe, currentIndex, pipeToken) ->
          let pipePos = Position.fromZ currentIndex pipeToken.RightColumn
          let prevLinePos = Position.fromZ lastExpressionLine 70 //We dont have the column on the previous line. So err to the right and let the client display in the right position
          let gens = getSignatureAtPos pipePos

          if lastExpressionLineWasPipe then
            let allS = gens |> List.tryLast |> Option.defaultValue "" |> removeSignPrefix
            [| (pipePos, allS) |]
          else
            match gens with
            | [ currentS ] -> [| (pipePos, removeSignPrefix currentS) |]
            | [ prevS; currentS ] ->
              [| (prevLinePos, removeSignPrefix prevS)
                 (pipePos, removeSignPrefix currentS) |]
            | _ ->
              let allS = gens |> Seq.intersperse "; " |> Seq.reduce (+)
              [| (pipePos, allS) |])

      return (Array.concat hints)
    }
    |> AsyncResult.foldResult id (fun _ -> [||])


  let pipelineHints (tryGetFileSource: _ -> Async<Result<IFSACSourceText, _>>) (tyRes: ParseAndCheckResults) =
    asyncResult {
      // Debug.waitForDebuggerAttached "AdaptiveServer"
      let! contents = tryGetFileSource tyRes.FileName

      let getSignatureAtPos pos =
        option {
          let! lineStr = contents.GetLine pos

          let! tip = tyRes.TryGetToolTip pos lineStr

          return TipFormatter.extractGenericParameters tip
        }
        |> Option.defaultValue []

      let areTokensCommentOrWhitespace (tokens: FSharpTokenInfo list) =
        tokens
        |> List.exists (fun token ->
          token.CharClass <> FSharpTokenCharKind.Comment
          && token.CharClass <> FSharpTokenCharKind.WhiteSpace
          && token.CharClass <> FSharpTokenCharKind.LineComment)
        |> not

      let getStartingPipe =
        function
        | y :: _ when y.TokenName.ToUpper() = "INFIX_BAR_OP" -> Some y
        | x :: y :: _ when x.TokenName.ToUpper() = "WHITESPACE" && y.TokenName.ToUpper() = "INFIX_BAR_OP" -> Some y
        | _ -> None

      let folder (lastExpressionLine, lastExpressionLineWasPipe, acc) (currentIndex, currentTokens) =
        let isCommentOrWhitespace = areTokensCommentOrWhitespace currentTokens

        let isPipe = getStartingPipe currentTokens

        match isCommentOrWhitespace, isPipe with
        | true, _ -> lastExpressionLine, lastExpressionLineWasPipe, acc
        | false, Some pipe ->
          currentIndex, true, (lastExpressionLine, lastExpressionLineWasPipe, currentIndex, pipe) :: acc
        | false, None -> currentIndex, false, acc

      let hints =
        Array.init ((contents: ISourceText).GetLineCount()) (fun line -> (contents: ISourceText).GetLineString line)
        |> Array.map (Lexer.tokenizeLine [||])
        |> Array.mapi (fun currentIndex currentTokens -> currentIndex, currentTokens)
        |> Array.fold folder (0, false, [])
        |> (fun (_, _, third) -> third |> Array.ofList)
        |> Array.Parallel.map (fun (lastExpressionLine, lastExpressionLineWasPipe, currentIndex, pipeToken) ->
          let pipePos = Position.fromZ currentIndex pipeToken.RightColumn
          let gens = getSignatureAtPos pipePos

          let previousNonPipeLine =
            if lastExpressionLineWasPipe then
              None
            else
              Some lastExpressionLine

          currentIndex, previousNonPipeLine, gens)

      return CoreResponse.Res hints
    }
    |> AsyncResult.bimap id (fun _ -> CoreResponse.InfoRes "Couldn't find file content")

  let calculateNamespaceInsert
    currentAst
    (decl: DeclarationListItem)
    (pos: Position)
    getLine
    : CompletionNamespaceInsert option =
    let getLine (p: Position) = getLine p |> Option.defaultValue ""

    let idents = decl.FullName.Split '.'

    decl.NamespaceToOpen
    |> Option.bind (fun n ->
      (currentAst ())
      |> Option.map (fun ast ->
        ParsedInput.FindNearestPointToInsertOpenDeclaration (pos.Line) ast idents OpenStatementInsertionPoint.Nearest)
      |> Option.map (fun ic ->
        //TODO: unite with `CodeFix/ResolveNamespace`
        //TODO: Handle Nearest AND TopLevel. Currently it's just Nearest (vs. ResolveNamespace -> TopLevel) (#789)

        let detectIndentation (line: string) = line |> Seq.takeWhile ((=) ' ') |> Seq.length

        // adjust line
        let pos =
          match ic.ScopeKind with
          | ScopeKind.Namespace ->
            // for namespace `open` isn't created close at namespace,
            // but instead on first member
            // -> move `open` closer to namespace
            // this only happens when there are no other `open`

            // from insert position go up until first open OR namespace
            ic.Pos.IncLine().LinesToBeginning()
            |> Seq.tryFind (fun l ->
              let lineStr = getLine l
              // namespace MUST be top level -> no indentation
              lineStr.StartsWith("namespace ", StringComparison.Ordinal))
            |> function
              // move to the next line below "namespace"
              | Some l -> l.IncLine()
              | None -> ic.Pos
          | _ -> ic.Pos

        // adjust column
        let pos =
          match pos with
          | Pos(1, _) -> pos
          | Pos(l, 0) ->
            let prev = getLine (pos.DecLine())
            let indentation = detectIndentation prev

            if indentation <> 0 then
              // happens when there are already other `open`s
              Position.mkPos l indentation
            else
              pos
          | Pos(_, _) -> pos

        { Namespace = n
          Position = pos
          Scope = ic.ScopeKind }))

  let symbolUseWorkspaceAux
    (getDeclarationLocation: FSharpSymbolUse * IFSACSourceText -> Async<SymbolDeclarationLocation option>)
    (findReferencesForSymbolInFile: (string<LocalPath> * CompilerProjectOption * FSharpSymbol) -> Async<Range seq>)
    (tryGetFileSource: string<LocalPath> -> Async<ResultOrString<IFSACSourceText>>)
    (tryGetProjectOptionsForFsproj: string<LocalPath> -> Async<CompilerProjectOption option>)
    (getAllProjectOptions: unit -> Async<CompilerProjectOption seq>)
    (includeDeclarations: bool)
    (includeBackticks: bool)
    (errorOnFailureToFixRange: bool)
    (text: IFSACSourceText)
    (tyRes: ParseAndCheckResults)
    (symbolUse: FSharpSymbolUse)
    : Async<Result<(FSharpSymbol * IDictionary<string<LocalPath>, Range[]>), string>> =
    asyncResult {
      let symbol = symbolUse.Symbol

      let symbolNameCore = symbol.DisplayNameCore

      let tryAdjustRanges (text: IFSACSourceText, ranges: seq<Range>) =
        let ranges = ranges |> Seq.map (fun range -> range.NormalizeDriveLetterCasing())

        if errorOnFailureToFixRange then
          ranges
          |> Seq.map (fun range ->
            Tokenizer.tryFixupRange (symbolNameCore, range, text, includeBackticks)
            |> Result.ofVOption (fun _ -> $"Cannot adjust range"))
          |> Seq.sequenceResultM
          |> Result.map (Seq.toArray)
        else
          ranges
          |> Seq.map (fun range ->
            Tokenizer.tryFixupRange (symbolNameCore, range, text, includeBackticks)
            |> ValueOption.defaultValue range)
          |> Seq.toArray
          |> Ok

      let! declLoc = getDeclarationLocation (symbolUse, text)

      match declLoc with
      // local symbol -> all uses are inside `text`
      // Note: declarations in script files are currently always local!
      | Some SymbolDeclarationLocation.CurrentDocument ->
        let! ct = Async.CancellationToken
        let symbolUses = tyRes.GetCheckResults.GetUsesOfSymbolInFile(symbol, ct)

        let symbolUses: _ seq =
          if includeDeclarations then
            symbolUses
          else
            symbolUses |> Seq.filter (fun u -> not u.IsFromDefinition)

        let ranges = symbolUses |> Seq.map (fun u -> u.Range)
        // Note: tryAdjustRanges is designed to only be able to fail iff `errorOnFailureToFixRange` is `true`
        let! ranges = tryAdjustRanges (text, ranges)
        let ranges = dict [ (text.FileName, Seq.toArray ranges) ]

        return (symbol, ranges)
      | scope ->
        let projectsToCheck: Async<CompilerProjectOption list> =
          async {
            match scope with
            | Some(SymbolDeclarationLocation.Projects(projects (*isLocalForProject=*) , true)) -> return projects
            | Some(SymbolDeclarationLocation.Projects(projects (*isLocalForProject=*) , false)) ->

              let! resolvedProjects =
                [ for project in projects do
                    yield Async.singleton (Some project)

                    yield!
                      project.ReferencedProjectsPath
                      |> List.map (fun p -> Utils.normalizePath p |> tryGetProjectOptionsForFsproj) ]
                |> Async.parallel75


              return
                resolvedProjects
                |> Array.choose id
                |> Array.toList
                |> List.distinctBy (fun x -> x.ProjectFileName)
            | _ (*None*) ->
              // symbol is declared external -> look through all F# projects
              // (each script (including untitled) has its own project -> scripts get checked too. But only once they are loaded (-> inside `state`))
              let! allOptions = getAllProjectOptions ()
              return allOptions |> Seq.distinctBy (fun x -> x.ProjectFileName) |> Seq.toList
          }

        let tryAdjustRanges (file: string<LocalPath>, ranges: Range[]) =
          async {
            match! tryGetFileSource file with
            | Error _ when errorOnFailureToFixRange -> return Error $"Cannot get source of '{file}'"
            | Error _ -> return Ok ranges
            | Ok text ->
              return
                tryAdjustRanges (text, ranges)
                // Note: `Error` only possible when `errorOnFailureToFixRange`
                |> Result.mapError (fun _ -> $"Cannot adjust ranges in file '{file}'")
          }

        let isDeclLocation =
          if includeDeclarations then
            // not actually used
            fun _ -> false
          else
            symbol |> Symbol.getIsDeclaration

        let dict = ConcurrentDictionary()

        /// Adds References of `symbol` in `file` to `dict`
        ///
        /// `Error` iff adjusting ranges failed (including cannot get source) and `errorOnFailureToFixRange`. Otherwise always `Ok`
        let tryFindReferencesInFile (file: string<LocalPath>, project: CompilerProjectOption) =
          async {
            if dict.ContainsKey file then
              return Ok()
            else
              let! references = findReferencesForSymbolInFile (file, project, symbol)

              let references =
                if includeDeclarations then
                  references
                else
                  references |> Seq.filter (not << isDeclLocation)

              let references = references |> Seq.toArray

              // Note: this check is important!
              // otherwise `tryAdjustRanges` tries to get source for files like `AssemblyInfo.fs`
              //   (which fails -> error if `errorOnFailureToFixRange`)
              if references |> Array.isEmpty then
                return Ok()
              else
                let! ranges = tryAdjustRanges (file, references)

                match ranges with
                | Error msg when errorOnFailureToFixRange -> return Error msg
                | Error _ ->
                  dict.TryAdd(file, references) |> ignore
                  return Ok()
                | Ok ranges ->
                  dict.TryAdd(file, ranges) |> ignore
                  return Ok()
          }
          |> AsyncResult.catch string
          |> Async.map (function
            | Ok() -> Ok()
            | Error e ->
              commandsLogger.info (
                Log.setMessage "tryFindReferencesInFile failed: {error}"
                >> Log.addContextDestructured "error" e
              )

              if errorOnFailureToFixRange then Error e else Ok())

        let iterProjects (projects: CompilerProjectOption seq) =
          // should:
          // * check files in parallel
          // * stop when error occurs
          // -> `Async.Choice`: executes in parallel, returns first `Some`
          // -> map `Error` to `Some` for `Async.Choice`, afterwards map `Some` back to `Error`
          [ for project in projects do
              for file in project.SourceFilesTagged do

                async {
                  match! tryFindReferencesInFile (file, project) with
                  | Ok _ -> return None
                  | Error err -> return Some err

                } ]
          |> Async.parallel75
          |> Async.Ignore<_>
        // |> Async.map (function
        //   | None -> Ok()
        //   | Some err -> Error err)

        let! projects = projectsToCheck
        do! iterProjects projects

        return (symbol, dict)
    }

  /// * `includeDeclarations`:
  ///   if `false` only returns usage locations and excludes declarations
  ///   * Note: if `true` you can still separate usages and declarations from each other
  ///     with `Symbol.partitionInfoDeclarationsAndUsages`
  /// * `includeBackticks`:
  ///   if `true` returns ranges including existing backticks, otherwise without:
  ///   `let _ = ``my value`` + 42`
  ///   * `true`: ` ``my value`` `
  ///   * `false`: `my value`
  /// * `errorOnFailureToFixRange`:
  ///   Ranges returned by FCS don't just span the actual identifier, but include Namespace, Module, Type: `System.String.IsNullOrEmpty`
  ///   These ranges gets adjusted to include just the concrete identifier (`IsNullOrEmpty`)
  ///   * If `false` and range cannot be adjust, the original range gets used.
  ///     * When results are more important than always exact range
  ///       -> for "Find All References"
  ///   * If `true`: Instead of using the source range, this function instead returns an Error
  ///     * When exact ranges are required
  ///       -> for "Rename"
  let symbolUseWorkspace
    (getDeclarationLocation: FSharpSymbolUse * IFSACSourceText -> Async<SymbolDeclarationLocation option>)
    (findReferencesForSymbolInFile: (string<LocalPath> * CompilerProjectOption * FSharpSymbol) -> Async<Range seq>)
    (tryGetFileSource: string<LocalPath> -> Async<ResultOrString<IFSACSourceText>>)
    (tryGetProjectOptionsForFsproj: string<LocalPath> -> Async<CompilerProjectOption option>)
    (getAllProjectOptions: unit -> Async<CompilerProjectOption seq>)
    (includeDeclarations: bool)
    (includeBackticks: bool)
    (errorOnFailureToFixRange: bool)
    pos
    lineStr
    (text: IFSACSourceText)
    (tyRes: ParseAndCheckResults)
    : Async<Result<(IDictionary<string<LocalPath>, Range[]>), string>> =
    asyncResult {
      let multipleSymbols = tyRes.TryGetSymbolUses pos lineStr
      let result = Dictionary<string<LocalPath>, Range[]>()

      for symbolUse in multipleSymbols do
        let! symbolResult =
          symbolUseWorkspaceAux
            getDeclarationLocation
            findReferencesForSymbolInFile
            tryGetFileSource
            tryGetProjectOptionsForFsproj
            getAllProjectOptions
            includeDeclarations
            includeBackticks
            errorOnFailureToFixRange
            text
            tyRes
            symbolUse

        for KeyValue(k, v) in snd symbolResult do
          if result.ContainsKey k then
            result.[k] <- [| yield! result.[k]; yield! v |]
          else
            result.Add(k, v)

      return result
    }

  /// Puts `newName` into backticks if necessary.
  ///
  ///
  /// Also does very basic validation of `newName`:
  /// * Must be valid operator name when operator
  let adjustRenameSymbolNewName pos lineStr (tyRes: ParseAndCheckResults) (newName: string) =
    asyncResult {
      let! symbolUse =
        tyRes.TryGetSymbolUse pos lineStr
        |> Result.ofOption (fun _ -> "Nothing to rename")

      match symbolUse with
      | SymbolUse.Operator _ ->
        // different validation rules
        // and no backticks for operator
        if PrettyNaming.IsOperatorDisplayName newName then
          return newName
        else
          return! Error $"'%s{newName}' is not a valid operator name!"
      | _ ->
        //ENHANCEMENT: more validation like check upper case start for types

        // `IsIdentifierName` doesn't work with backticks
        // -> only check if no backticks
        let newBacktickedName = newName |> PrettyNaming.NormalizeIdentifierBackticks

        if
          newBacktickedName.StartsWith("``", StringComparison.Ordinal)
          && newBacktickedName.EndsWith("``", StringComparison.Ordinal)
        then
          return newBacktickedName
        elif PrettyNaming.IsIdentifierName newName then
          return newName
        else
          return! Error $"'%s{newName}' is not a valid identifier name!"
    }

  /// `Error` if renaming is invalid at specified `pos`.
  /// Otherwise range of identifier at specified `pos`
  ///
  /// Note:
  /// Rename for Active Patterns is disabled:
  /// Each case is its own identifier and complete Active Pattern name isn't correctly handled by FCS
  ///
  /// Note:
  /// Rename for Active Pattern Cases is disabled:
  /// `SymbolUseWorkspace` returns ranges for ALL Cases of that Active Pattern instead of just the single case
  let renameSymbolRange
    (getDeclarationLocation: FSharpSymbolUse * IFSACSourceText -> Async<SymbolDeclarationLocation option>)
    (includeBackticks: bool)
    pos
    lineStr
    (text: IFSACSourceText)
    (tyRes: ParseAndCheckResults)
    =
    asyncResult {
      let! symbolUse =
        tyRes.TryGetSymbolUse pos lineStr
        |> Result.ofOption (fun _ -> "Nothing to rename")

      let! _ =
        // None: external symbol -> not under our control -> cannot rename
        getDeclarationLocation (symbolUse, text)
        |> AsyncResult.ofOption (fun _ -> "Must be declared inside current workspace, but is external.")

      do!
        match symbolUse with
        | SymbolUse.ActivePattern _ ->
          // Active Pattern is not supported:
          // ```fsharp
          // let (|Even|Odd|) v = if v % 2 = 0 then Even else Odd
          // match 42 with
          // | Even -> ()
          // | Odd -> ()
          // let _ = (|Even|Odd|) 42
          // ```
          // ->
          // `(|Even|Odd|)` at usage is own symbol -- NOT either Even or Odd (depending on pos)
          // -> Rename at that location renames complete `(|Even|Odd|)` -- but not individual usages
          Error "Renaming of Active Patterns is not supported"
        | SymbolUse.ActivePatternCase _ ->
          // Active Pattern Case is not supported:
          // ```fsharp
          // let (|Even|Odd|) v = if v % 2 = 0 then Even else Odd
          // match 42 with
          // | Even -> ()
          // | Odd -> ()
          // ```
          // -> `Even` -> finds all occurrences of `Odd` too -> get renamed too...
          //Enhancement: Handle: Exclude cases that don't match symbol at pos
          Error "Renaming of Active Pattern Cases is currently not supported"
        | _ -> Ok()

      let symbol = symbolUse.Symbol
      let nameCore = symbol.DisplayNameCore

      let! range =
        Tokenizer.tryFixupRange (nameCore, symbolUse.Range, text, includeBackticks)
        |> Result.ofVOption (fun _ -> "Cannot correctly isolate range of identifier")

      return (symbol, nameCore, range)
    }

  // given an enveloping range and the sub-ranges it overlaps, split out the enveloping range into a
  // set of range segments that are non-overlapping with the children
  let segmentRanges (parentRange: Range) (childRanges: Range[]) : Range[] =
    let firstSegment =
      Range.mkRange parentRange.FileName parentRange.Start childRanges.[0].Start

    let lastSegment =
      Range.mkRange parentRange.FileName (Array.last childRanges).End parentRange.End // from end of last child to end of parent
    // now we can go pairwise, emitting a new range for the area between each end and start
    let innerSegments =
      childRanges
      |> Array.pairwise
      |> Array.map (fun (left, right) -> Range.mkRange parentRange.FileName left.End right.Start)

    [|
       // note that the first and last segments can be zero-length.
       // in that case we should not emit them because it confuses the
       // encoding algorithm
       if Position.posEq firstSegment.Start firstSegment.End then
         ()
       else
         firstSegment
       yield! innerSegments
       if Position.posEq lastSegment.Start lastSegment.End then
         ()
       else
         lastSegment |]

  // TODO: LSP technically does now know how to handle overlapping, nested and multiline ranges, but
  // as of 3 February 2021 there are no good examples of this that I've found, so we still do this
  /// because LSP doesn't know how to handle overlapping/nested ranges, we have to de-dupe them here
  let scrubRanges (highlights: SemanticClassificationItem array) : SemanticClassificationItem array =
    let startToken =
      fun (m: SemanticClassificationItem) -> m.Range.StartLine, m.Range.StartColumn

    highlights
    |> Array.sortBy startToken
    |> Array.groupBy (fun m -> m.Range.StartLine)
    |> Array.collect (fun (_, highlights) ->

      // split out any ranges that contain other ranges on this line into the non-overlapping portions of that range
      let expandParents (p: SemanticClassificationItem) =
        let children =
          highlights
          |> Array.except [ p ]
          |> Array.choose (fun child ->
            if Range.rangeContainsRange p.Range child.Range then
              Some child.Range
            else
              None)

        match children with
        | [||] -> [| p |]
        | children ->
          let sortedChildren =
            children |> Array.sortBy (fun r -> r.Start.Line, r.Start.Column)

          segmentRanges p.Range sortedChildren
          |> Array.map (fun subRange -> SemanticClassificationItem((subRange, p.Type)))

      highlights |> Array.collect expandParents)
    |> Array.sortBy startToken



  let analyzerHandler
    (
      client: SDK.Client<SDK.EditorAnalyzerAttribute, SDK.EditorContext>,
      file: string<LocalPath>,
      content: ISourceText,
      pt,
      tast,
      checkFileResults: FSharpCheckFileResults
    ) =
    let ctx: SDK.EditorContext =
      { FileName = UMX.untag file
        SourceText = content
        ParseFileResults = pt
        CheckFileResults = Some checkFileResults
        TypedTree = Some tast
        CheckProjectResults = None }

    let extractResultsFromAnalyzer (r: SDK.AnalysisResult) =
      match r.Output with
      | Ok results ->
        Loggers.analyzers.info (
          Log.setMessage "Analyzer {analyzer} returned {count} diagnostics for file {file}"
          >> Log.addContextDestructured "analyzer" r.AnalyzerName
          >> Log.addContextDestructured "count" results.Length
          >> Log.addContextDestructured "file" (UMX.untag file)
        )

        results
      | Error e ->
        Loggers.analyzers.error (
          Log.setMessage "Analyzer {analyzer} errored while processing {file}: {message}"
          >> Log.addContextDestructured "analyzer" r.AnalyzerName
          >> Log.addContextDestructured "file" (UMX.untag file)
          >> Log.addContextDestructured "message" e.Message
          >> Log.addExn e
        )

        []

    async {
      try
        let! r = client.RunAnalyzersSafely ctx
        return r |> List.collect extractResultsFromAnalyzer |> List.toArray
      with ex ->
        Loggers.analyzers.error (
          Log.setMessage "Error while processing analyzers for {file}: {message}"
          >> Log.addContextDestructured "message" ex.Message
          >> Log.addExn ex
          >> Log.addContextDestructured "file" file
        )

        return [||]
    }

type Commands() =

  static member DotnetNewList() =
    async {
      let results = DotnetNewTemplate.installedTemplates ()
      return CoreResponse.Res results
    }


  static member DotnetNewRun
    (templateShortName: string)
    (name: string option)
    (output: string option)
    (parameterStr: (string * obj) list)
    =
    async {
      let! results = DotnetCli.dotnetNew templateShortName name output parameterStr
      return CoreResponse.Res results
    }

  static member DotnetAddProject (toProject: string) (reference: string) =
    async {
      let! result = DotnetCli.dotnetAddProject toProject reference
      return CoreResponse.Res result
    }

  static member DotnetRemoveProject (fromProject: string) (reference: string) =
    async {
      let! result = DotnetCli.dotnetRemoveProject fromProject reference
      return CoreResponse.Res result
    }

  static member DotnetSlnAdd (sln: string) (project: string) =
    async {
      let! result = DotnetCli.dotnetSlnAdd sln project
      return CoreResponse.Res result
    }

  static member FsProjMoveFileUp (fsprojPath: string) (fileVirtPath: string) =
    async {
      FsProjEditor.moveFileUp fsprojPath fileVirtPath
      return CoreResponse.Res()
    }

  static member FsProjMoveFileDown (fsprojPath: string) (fileVirtPath: string) =
    async {
      FsProjEditor.moveFileDown fsprojPath fileVirtPath
      return CoreResponse.Res()
    }


  static member CompilerLocation(checker: FSharpCompilerServiceChecker) =
    CoreResponse.Res(Environment.fsc, Environment.fsi, Some "", checker.GetDotnetRoot())

  static member FormattedDocumentation (tyRes: ParseAndCheckResults) (pos: Position) lineStr =
    tyRes.TryGetFormattedDocumentation pos lineStr
    |> Result.bimap CoreResponse.Res CoreResponse.ErrorRes

  static member FormattedDocumentationForSymbol (tyRes: ParseAndCheckResults) (xmlSig: string) (assembly: string) =
    tyRes.TryGetFormattedDocumentationForSymbol xmlSig assembly
    |> Result.bimap CoreResponse.Res CoreResponse.ErrorRes

  static member SignatureData (tyRes: ParseAndCheckResults) (pos: Position) lineStr =
    tyRes.TryGetSignatureData pos lineStr
    |> Result.bimap CoreResponse.Res CoreResponse.ErrorRes

  static member Help (tyRes: ParseAndCheckResults) (pos: Position) lineStr =
    tyRes.TryGetF1Help pos lineStr
    |> Result.bimap CoreResponse.Res CoreResponse.ErrorRes

  /// for a given member, use its signature information to generate placeholder XML documentation strings.
  /// calculates the required indent and gives the position to insert the text.
  static member GenerateXmlDocumentation(tyRes: ParseAndCheckResults, triggerPosition: Position, lineStr: LineStr) =
    asyncResult {
      let tryGetFirstAttributeLine (synAttributes: SynAttributes) =
        synAttributes
        |> List.collect (fun a -> a.Attributes)
        |> function
          | [] -> None
          | attributes ->
            attributes
            |> Seq.minBy (fun a -> a.Range.StartLine)
            |> fun attr -> Some attr.Range.StartLine

      let longIdentContainsPos (longIdent: LongIdent) (pos: FSharp.Compiler.Text.pos) =
        longIdent
        |> List.tryFind (fun i -> rangeContainsPos i.idRange pos)
        |> Option.isSome

      let isLowerAstElemWithEmptyPreXmlDoc input pos : Option<bool * Option<int>> =
        SyntaxTraversal.Traverse(
          pos,
          input,
          { new SyntaxVisitorBase<_>() with
              member _.VisitBinding(_, defaultTraverse, synBinding) =
                match synBinding with
                | SynBinding(attributes = attributes; xmlDoc = xmlDoc; valData = valData) as s when
                  rangeContainsPos s.RangeOfBindingWithoutRhs pos && xmlDoc.IsEmpty
                  ->
                  match valData with
                  | SynValData(memberFlags = Some({ MemberKind = SynMemberKind.PropertyGet }))
                  | SynValData(memberFlags = Some({ MemberKind = SynMemberKind.PropertySet }))
                  | SynValData(memberFlags = Some({ MemberKind = SynMemberKind.PropertyGetSet })) -> None
                  | _ -> Some(false, tryGetFirstAttributeLine attributes)
                | _ -> defaultTraverse synBinding

              member _.VisitComponentInfo(_, synComponentInfo) =
                match synComponentInfo with
                | SynComponentInfo(attributes = attributes; longId = longId; xmlDoc = xmlDoc) when
                  longIdentContainsPos longId pos && xmlDoc.IsEmpty
                  ->
                  Some(false, tryGetFirstAttributeLine attributes)
                | _ -> None

              member _.VisitRecordDefn(_, fields, _) =
                let isInLine c =
                  match c with
                  | SynField(attributes = attributes; xmlDoc = xmlDoc; idOpt = Some ident) when
                    rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty
                    ->
                    Some(false, tryGetFirstAttributeLine attributes)
                  | _ -> None

                fields |> List.tryPick isInLine

              member _.VisitUnionDefn(_, cases, _) =
                let isInLine c =
                  match c with
                  | SynUnionCase(attributes = attributes; xmlDoc = xmlDoc; ident = (SynIdent(ident = ident))) when
                    rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty
                    ->
                    Some(false, tryGetFirstAttributeLine attributes)
                  | _ -> None

                cases |> List.tryPick isInLine

              member _.VisitEnumDefn(_, cases, _) =
                let isInLine b =
                  match b with
                  | SynEnumCase(attributes = attributes; xmlDoc = xmlDoc; ident = (SynIdent(ident = ident))) when
                    rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty
                    ->
                    Some(false, tryGetFirstAttributeLine attributes)
                  | _ -> None

                cases |> List.tryPick isInLine

              member _.VisitLetOrUse(_, _, defaultTraverse, bindings, _) =
                let isInLine b =
                  match b with
                  | SynBinding(attributes = attributes; xmlDoc = xmlDoc) as s when
                    rangeContainsPos s.RangeOfBindingWithoutRhs pos && xmlDoc.IsEmpty
                    ->
                    Some(false, tryGetFirstAttributeLine attributes)
                  | _ -> defaultTraverse b

                bindings |> List.tryPick isInLine

              member _.VisitExpr(_, _, defaultTraverse, expr) = defaultTraverse expr } // needed for nested let bindings
        )

      let isModuleOrNamespaceOrAutoPropertyWithEmptyPreXmlDoc input pos : Option<bool * Option<int>> =
        SyntaxTraversal.Traverse(
          pos,
          input,
          { new SyntaxVisitorBase<_>() with

              member _.VisitModuleOrNamespace(_, synModuleOrNamespace) =
                match synModuleOrNamespace with
                | SynModuleOrNamespace(attribs = attributes; longId = longId; xmlDoc = xmlDoc; kind = kind) when
                  kind = SynModuleOrNamespaceKind.NamedModule
                  && longIdentContainsPos longId pos
                  && xmlDoc.IsEmpty
                  ->
                  Some(false, tryGetFirstAttributeLine attributes)
                | SynModuleOrNamespace(decls = decls) ->

                  let rec findNested decls =
                    decls
                    |> List.tryPick (fun d ->
                      match d with
                      | SynModuleDecl.NestedModule(moduleInfo = moduleInfo; decls = decls) ->
                        match moduleInfo with
                        | SynComponentInfo(attributes = attributes; longId = longId; xmlDoc = xmlDoc) when
                          longIdentContainsPos longId pos && xmlDoc.IsEmpty
                          ->
                          Some(false, tryGetFirstAttributeLine attributes)
                        | _ -> findNested decls
                      | SynModuleDecl.Types(typeDefns = typeDefns) ->
                        typeDefns
                        |> List.tryPick (fun td ->
                          match td with
                          | SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(_, members, _)) ->
                            members
                            |> List.tryPick (fun m ->
                              match m with
                              | SynMemberDefn.AutoProperty(attributes = attributes; ident = ident; xmlDoc = xmlDoc) when
                                rangeContainsPos ident.idRange pos && xmlDoc.IsEmpty
                                ->
                                Some(true, tryGetFirstAttributeLine attributes)
                              | SynMemberDefn.GetSetMember(
                                  memberDefnForSet = Some(SynBinding(
                                    attributes = attributes
                                    xmlDoc = xmlDoc
                                    headPat = SynPat.LongIdent(longDotId = longDotId)))) when
                                rangeContainsPos longDotId.Range pos && xmlDoc.IsEmpty
                                ->
                                Some(true, tryGetFirstAttributeLine attributes)
                              | SynMemberDefn.GetSetMember(
                                  memberDefnForGet = Some(SynBinding(
                                    attributes = attributes
                                    xmlDoc = xmlDoc
                                    headPat = SynPat.LongIdent(longDotId = longDotId)))) when
                                rangeContainsPos longDotId.Range pos && xmlDoc.IsEmpty
                                ->
                                Some(true, tryGetFirstAttributeLine attributes)
                              | _ -> None)
                          | _ -> None)
                      | _ -> None)

                  findNested decls }
        )

      let isAstElemWithEmptyPreXmlDoc input pos =
        match isLowerAstElemWithEmptyPreXmlDoc input pos with
        | Some(isAutoProperty, firstAttrLine) -> Some(isAutoProperty, firstAttrLine)
        | _ -> isModuleOrNamespaceOrAutoPropertyWithEmptyPreXmlDoc input pos

      let trimmed = lineStr.TrimStart(' ')
      let indentLength = lineStr.Length - trimmed.Length
      let indentString = String.replicate indentLength " "

      match isAstElemWithEmptyPreXmlDoc tyRes.GetAST triggerPosition with
      | None -> return None
      | Some(isAutoProperty, firstAttrLine) ->

        let signatureData =
          Commands.SignatureData tyRes triggerPosition lineStr |> Result.ofCoreResponse

        let summarySection = "/// <summary></summary>"

        let parameterSection (name, _type) = $"/// <param name=\"%s{name}\"></param>"

        let genericArg name = $"/// <typeparam name=\"'%s{name}\"></typeparam>"

        let returnsSection = "/// <returns></returns>"

        let formattedXmlDoc =
          seq {
            yield summarySection

            match signatureData with
            | Ok(Some(_, memberParameters, genericParameters)) ->
              match memberParameters with
              | [] -> ()
              | _ when isAutoProperty -> () // no parameter section for auto properties
              | parameters ->
                yield!
                  parameters
                  |> List.concat
                  |> List.mapi (fun _index parameter -> parameterSection parameter)

              match genericParameters with
              | [] -> ()
              | generics -> yield! generics |> List.mapi (fun _index generic -> genericArg generic)

              yield returnsSection
            | _ -> ()
          }
          |> Seq.map (fun s -> indentString + s)
          |> String.concat Environment.NewLine
          |> fun s -> s + Environment.NewLine // need a newline at the very end

        // always insert at the start of the line, because we've prepended the indent to the start of the summary section
        let insertPosLine = firstAttrLine |> Option.defaultValue triggerPosition.Line
        let insertPosition = Position.mkPos insertPosLine 0

        return
          Some
            { InsertPosition = insertPosition
              InsertText = formattedXmlDoc }
    }

  static member InlayHints(text, tyRes: ParseAndCheckResults, range, ?showTypeHints, ?showParameterHints) =
    let hintConfig: Core.InlayHints.HintConfig =
      { ShowTypeHints = defaultArg showTypeHints true
        ShowParameterHints = defaultArg showParameterHints true }

    FsAutoComplete.Core.InlayHints.provideHints (text, tyRes, range, hintConfig)
