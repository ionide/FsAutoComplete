namespace FsAutoComplete.Lsp

open System
open System.IO
open FsAutoComplete
open FsAutoComplete.Core
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsAutoComplete.Logging
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types.LspResult
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Ionide.ProjInfo.ProjectSystem
open System.Reactive
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsAutoComplete.Adaptive
open FsAutoComplete.LspHelpers

open FSharp.Control.Reactive
open FsToolkit.ErrorHandling
open FsAutoComplete.Telemetry
open FsAutoComplete.Utils.Tracing
open FSharp.UMX

open FSharp.Compiler.Text
open CliWrap
open CliWrap.Buffered
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService

open FSharp.Data.Adaptive
open Ionide.ProjInfo
open System.Diagnostics
open IcedTasks
open System.Threading.Tasks
open FsAutoComplete.FCSPatches
open Helpers

#nowarn "44" // we create CompletionItems here that have two deprecated properties. Since records always have to set each property....

type AdaptiveFSharpLspServer
  (workspaceLoader: IWorkspaceLoader, lspClient: FSharpLspClient, sourceTextFactory: ISourceTextFactory) =

  let mutable lastFSharpDocumentationTypeCheck: ParseAndCheckResults option = None

  let logger = LogProvider.getLoggerFor<AdaptiveFSharpLspServer> ()

  let fantomasLogger = LogProvider.getLoggerByName "Fantomas"
  let fantomasService: FantomasService = new LSPFantomasService() :> FantomasService

  let thisType = typeof<AdaptiveFSharpLspServer>

  let disposables = new Disposables.CompositeDisposable()

  let state = new AdaptiveState(lspClient, sourceTextFactory, workspaceLoader)

  do disposables.Add(state)

  [<return: Struct>]
  let rec (|Cancelled|_|) (e: exn) =
    match e with
    | :? TaskCanceledException -> ValueSome()
    | :? OperationCanceledException -> ValueSome()
    | :? System.AggregateException as aex ->
      if aex.InnerExceptions.Count = 1 then
        (|Cancelled|_|) aex.InnerException
      else
        ValueNone
    | _ -> ValueNone

  let returnException e =
    match e with
    | Cancelled -> LspResult.requestCancelled
    | e -> LspResult.internalError (string e)


  let getFilePathAndPosition (p: ITextDocumentPositionParams) =
    let filePath = p.GetFilePath() |> Utils.normalizePath
    let pos = p.GetFcsPos()
    filePath, pos



  member private x.handleSemanticTokens (filePath: string<LocalPath>) range : AsyncLspResult<SemanticTokens option> =
    asyncResult {

      let! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr
      let r = tyRes.GetCheckResults.GetSemanticClassification(range)
      let filteredRanges = Commands.scrubRanges r

      let lspTypedRanges =
        filteredRanges
        |> Array.map (fun item ->
          let ty, mods = ClassificationUtils.map item.Type
          struct (fcsRangeToLsp item.Range, ty, mods))

      match encodeSemanticHighlightRanges lspTypedRanges with
      | None -> return None
      | Some encoded -> return (Some { Data = encoded; ResultId = None }) // TODO: provide a resultId when we support delta ranges
    }

  member __.HandleFormatting
    (
      fileName: string<LocalPath>,
      action: unit -> Async<Result<FormatDocumentResponse, string>>,
      handlerFormattedDoc: (IFSACSourceText * string) -> TextEdit[],
      handleFormattedRange: (IFSACSourceText * string * FormatSelectionRange) -> TextEdit[]
    ) : Async<LspResult<option<_>>> =
    asyncResult {
      let tags = [ SemanticConventions.fsac_sourceCodePath, box fileName ]
      use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

      try


        let! res = action () |> AsyncResult.ofStringErr

        let rootPath = state.RootPath

        match res with
        | (FormatDocumentResponse.Formatted(sourceText, formatted)) ->
          let result = handlerFormattedDoc (sourceText, formatted)

          return (Some(result))
        | (FormatDocumentResponse.FormattedRange(sourceText, formatted, range)) ->
          let result = handleFormattedRange (sourceText, formatted, range)

          return (Some(result))
        | FormatDocumentResponse.Ignored ->
          let fileName = UMX.untag fileName |> Path.GetFileName

          do!
            lspClient.WindowShowMessage
              { Type = MessageType.Info
                Message = (sprintf "\"%s\" is ignored by a .fantomasignore file." fileName) }

          return None
        | FormatDocumentResponse.UnChanged -> return None
        | FormatDocumentResponse.ToolNotPresent ->
          let actions =
            [| if Option.isSome rootPath then
                 { Title = "Install locally" }
                 { Title = "Install globally" } |]

          let! response =
            lspClient.WindowShowMessageRequest
              { Type = MessageType.Warning
                Message = "No Fantomas install was found."
                Actions = Some actions }

          match response with
          | (Some { Title = "Install locally" }) ->
            do!
              rootPath
              |> Option.map (fun rootPath ->
                async {
                  let dotConfig = Path.Combine(rootPath, ".config", "dotnet-tools.json")

                  if not (File.Exists dotConfig) then
                    let! result =
                      Cli
                        .Wrap("dotnet")
                        .WithArguments("new tool-manifest")
                        .WithWorkingDirectory(rootPath)
                        .ExecuteBufferedAsync()
                        .Task
                      |> Async.AwaitTask

                    if result.ExitCode <> 0 then
                      fantomasLogger.warn (
                        Log.setMessage (sprintf "Unable to create a new tool manifest in %s" rootPath)
                      )
                  else
                    let dotConfigContent = File.ReadAllText dotConfig

                    if dotConfigContent.Contains("fantomas") then
                      // uninstall a older, non-compatible version of fantomas
                      let! result =
                        Cli
                          .Wrap("dotnet")
                          .WithArguments("tool uninstall fantomas")
                          .WithWorkingDirectory(rootPath)
                          .ExecuteBufferedAsync()
                          .Task
                        |> Async.AwaitTask

                      if result.ExitCode <> 0 then
                        fantomasLogger.warn (
                          Log.setMessage (
                            sprintf "Unable to uninstall a non compatible version of fantomas in %s" rootPath
                          )
                        )

                  let! result =
                    Cli
                      .Wrap("dotnet")
                      .WithArguments("tool install fantomas")
                      .WithWorkingDirectory(rootPath)
                      .ExecuteBufferedAsync()
                      .Task
                    |> Async.AwaitTask

                  if result.ExitCode = 0 then
                    fantomasLogger.info (Log.setMessage (sprintf "fantomas was installed locally at %A" rootPath))

                    do!
                      lspClient.WindowShowMessage
                        { Type = MessageType.Info
                          Message = "fantomas was installed locally" }

                    fantomasService.ClearCache()
                  else
                    fantomasLogger.warn (
                      Log.setMessage (sprintf "Unable to install a compatible version of fantomas in %s" rootPath)
                    )
                }

              )
              |> Option.defaultValue (async { return () })
          | (Some { Title = "Install globally" }) ->
            let! result =
              Cli
                .Wrap("dotnet")
                .WithArguments("tool install -g fantomas")
                .ExecuteBufferedAsync()
                .Task
              |> Async.AwaitTask

            if result.ExitCode = 0 then
              fantomasLogger.info (Log.setMessage "fantomas was installed globally")

              do!
                lspClient.WindowShowMessage
                  { Type = MessageType.Info
                    Message = "fantomas was installed globally" }

              fantomasService.ClearCache()
            else
              fantomasLogger.warn (Log.setMessage "Unable to install a compatible version of fantomas globally")
          | _ -> ()

          return! LspResult.internalError "Fantomas install not found."
        | (FormatDocumentResponse.Error ex) -> return! LspResult.internalError ex
      with e ->
        trace |> Tracing.recordException e
        logger.error (Log.setMessage "HandleFormatting Request Errored {p}" >> Log.addExn e)
        return! returnException e
    }

  member __.ScriptFileProjectOptions = state.ScriptFileProjectOptions.Publish

  member private x.logUnimplementedRequest<'t, 'u>
    (
      argValue: 't,
      [<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string
    ) =
    logger.info (
      Log.setMessage $"{caller} request: {{params}}"
      >> Log.addContextDestructured "params" argValue
    )

    Helpers.notImplemented<'u>

  member private x.logIgnoredNotification<'t>
    (
      argValue: 't,
      [<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string
    ) =
    logger.info (
      Log.setMessage $"{caller} request: {{params}}"
      >> Log.addContextDestructured "params" argValue
    )

    Helpers.ignoreNotification

  interface IFSharpLspServer with
    override x.Shutdown() = (x :> System.IDisposable).Dispose() |> async.Return

    override _.Initialize(p: InitializeParams) =
      asyncResult {
        let tags = [ "InitializeParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (Log.setMessage "Initialize Request {p}" >> Log.addContextDestructured "p" p)

          let c =
            p.InitializationOptions
            |> Option.bind (fun options -> if options.HasValues then Some options else None)
            |> Option.map Server.deserialize<FSharpConfigDto>
            |> Option.map FSharpConfig.FromDto
            |> Option.defaultValue FSharpConfig.Default

          logger.info (
            Log.setMessage "Initialization options {items}"
            >> Log.addContextDestructured "items" c
          )

          let inlineValueToggle: InlineValueOptions option =
            match c.InlineValues.Enabled with
            | Some true -> Some { ResolveProvider = Some false }
            | Some false -> None
            | None -> None

          let workspaceRoot =
            match p.WorkspaceFolders with
            | None
            | Some [||] ->
              failwith
                "Unable to start LSP server - no workspacePaths are available and we do not support the deprecated RootPath and RootUri options."
            | Some folders -> Some(Path.FileUriToLocalPath folders[0].Uri)

          let projs =
            match workspaceRoot, c.AutomaticWorkspaceInit with
            // if no workspace root available, or one is available but we don't want to automatically
            // initialize the workspace, then we don't have any projects to initialize.
            // in this case we need the client to call WorkspacePeek/WorkspaceLoad explicitly
            | None, _
            | _, false -> WorkspaceChosen.NotChosen
            // otherwise, try to infer the workspace from the automatic workspace information
            | Some actualRootPath, true ->
              let peeks =
                WorkspacePeek.peek
                  actualRootPath
                  c.WorkspaceModePeekDeepLevel
                  (c.ExcludeProjectDirectories |> List.ofArray)
                |> List.map Workspace.mapInteresting
                |> List.sortByDescending (fun x ->
                  match x with
                  | CommandResponse.WorkspacePeekFound.Solution sln -> Workspace.countProjectsInSln sln
                  | CommandResponse.WorkspacePeekFound.Directory _ -> -1)

              logger.info (
                Log.setMessage "Choosing from interesting items {items}"
                >> Log.addContextDestructured "items" peeks
              )

              match peeks with
              | [] -> []
              | [ CommandResponse.WorkspacePeekFound.Directory projs ] -> projs.Fsprojs
              | CommandResponse.WorkspacePeekFound.Solution sln :: _ ->
                sln.Items |> List.collect Workspace.foldFsproj |> List.map fst
              | _ -> []
              |> List.map (Utils.normalizePath)
              |> HashSet.ofList
              |> WorkspaceChosen.Projs

          transact (fun () ->
            state.RootPath <- workspaceRoot
            state.ClientCapabilities <- p.Capabilities
            lspClient.ClientCapabilities <- p.Capabilities

            state.DiagnosticCollections.ClientSupportsDiagnostics <-
              match p.Capabilities with
              | Some { TextDocument = Some { PublishDiagnostics = Some _ } } -> true
              | _ -> false

            state.Config <- c
            state.WorkspacePaths <- projs)

          let defaultSettings =
            { Helpers.defaultServerCapabilities with
                TextDocumentSync =
                  Helpers.defaultServerCapabilities.TextDocumentSync
                  |> Option.map (fun x ->
                    { x with
                        Change = Some TextDocumentSyncKind.Incremental })
                InlineValueProvider = inlineValueToggle }

          return
            { InitializeResult.Default with
                Capabilities = defaultSettings }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "Initialize Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.Initialized(p: InitializedParams) =
      async {
        let tags = [ "InitializedParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (Log.setMessage "Initialized request {p}" >> Log.addContextDestructured "p" p)
          let! _ = state.ParseAllFiles()
          return ()
        with e ->

          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "Initialized Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) =
      async {
        let tags = [ "DidOpenTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDidOpen Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath

          do! state.OpenDocument(filePath, doc.Text, doc.Version)

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidOpen Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
      async {
        let tags = [ "DidCloseTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDidClose Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let doc = p.TextDocument
          do! state.ForgetDocument doc.Uri
          return ()

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidClose Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
      async {
        let tags = [ "DidChangeTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDidChange Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath
          do! state.ChangeDocument(filePath, p)

          return ()
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidChange Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return ()
      }

    override __.TextDocumentDidSave(p: DidSaveTextDocumentParams) =
      async {
        let tags = [ "DidSaveTextDocumentParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try

          logger.info (
            Log.setMessage "TextDocumentDidSave Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let doc = p.TextDocument
          let filePath = doc.GetFilePath() |> Utils.normalizePath

          do! state.SaveDocument(filePath, p.Text)

          do! lspClient.CodeLensRefresh()

          logger.info (
            Log.setMessage "TextDocumentDidSave Request Finished: {params}"
            >> Log.addContextDestructured "params" p
          )

          return ()
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDidSave Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

        return ()
      }

    override __.TextDocumentCompletion(p: CompletionParams) =
      asyncResult {
        let tags = [ "CompletionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentCompletion Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p

          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr

          if volatileFile.Source.Length = 0 then
            return None // An empty file has empty completions. Otherwise we would error down there
          else

            let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr

            if lineStr.StartsWith("#", StringComparison.Ordinal) then
              let completionList =
                { IsIncomplete = false
                  Items = KeywordList.hashSymbolCompletionItems p.Position
                  ItemDefaults = None }


              return! success (Some completionList)
            else
              let config = state.Config

              let rec retryAsyncOption (delay: TimeSpan) timesLeft handleError action =
                async {
                  match! action with
                  | Ok x -> return Ok x
                  | Error e when timesLeft >= 0 ->
                    let nextAction = handleError e
                    do! Async.Sleep(delay)
                    return! retryAsyncOption delay (timesLeft - 1) handleError nextAction
                  | Error e -> return Error e
                }

              let getCompletions forceGetTypeCheckResultsStale =
                asyncResult {

                  let! volatileFile = state.GetOpenFileOrRead filePath

                  let! lineStr =
                    volatileFile.Source
                    |> tryGetLineStr pos
                    |> Result.mapError ErrorMsgUtils.formatLineLookErr
                  //TODO ⮝⮝⮝ good candidate for better error model -- review!

                  // TextDocumentCompletion will sometimes come in before TextDocumentDidChange
                  // This will require the trigger character to be at the place VSCode says it is
                  // Otherwise we'll fail here and our retry logic will come into place
                  do!
                    match p.Context with
                    | Some({ triggerKind = CompletionTriggerKind.TriggerCharacter } as context) ->
                      volatileFile.Source.TryGetChar pos = context.triggerCharacter
                    | _ -> true
                    |> Result.requireTrue $"TextDocumentCompletion was sent before TextDocumentDidChange"

                  // Special characters like parentheses, brackets, etc. require a full type check
                  let isSpecialChar = Option.exists (Char.IsLetterOrDigit >> not)

                  let previousCharacter = volatileFile.Source.TryGetChar(FcsPos.subtractColumn pos 1)

                  let! typeCheckResults =
                    if isSpecialChar previousCharacter then
                      state.GetOpenFileTypeCheckResults filePath
                    else
                      forceGetTypeCheckResultsStale filePath

                  let getAllSymbols () =
                    if config.ExternalAutocomplete then
                      typeCheckResults.GetAllEntities true
                    else
                      []

                  let! (decls, residue, shouldKeywords) =
                    Debug.measure "TextDocumentCompletion.TryGetCompletions" (fun () ->
                      typeCheckResults.TryGetCompletions pos lineStr None getAllSymbols
                      |> AsyncResult.ofOption (fun () -> "No TryGetCompletions results"))

                  do! Result.requireNotEmpty "Should not have empty completions" decls

                  return Some(decls, residue, shouldKeywords, typeCheckResults, getAllSymbols, volatileFile)
                }

              let handleError e =
                match e with
                | "Should not have empty completions" ->
                  // If we don't get any completions, assume we need to wait for a full typecheck
                  getCompletions state.GetOpenFileTypeCheckResults
                | _ -> getCompletions state.GetOpenFileTypeCheckResultsCached

              let getCodeToInsert (d: DeclarationListItem) =
                match d.NamespaceToOpen with
                | Some no when config.FullNameExternalAutocomplete -> sprintf "%s.%s" no d.NameInCode
                | _ -> d.NameInCode

              let textEdit text pos : U2<TextEdit, _> =
                U2.First(
                  { Range = { Start = pos; End = pos }
                    NewText = text }
                )

              let createCompletionItem (config: FSharpConfig) insertPos (id: int) (d: DeclarationListItem) =
                let code = getCodeToInsert d

                /// The `label` for completion "System.Math.Ceiling" will be displayed as "Ceiling (System.Math)". This is to bias the viewer towards the member name,
                /// with the namespace being less-important. The `filterText` is the text that will be used to filter the list of completions as the user types.
                /// Prepending the member name to the filter text makes it so that the text the user is mot likely typing catches more relevant members at the head of the list.
                /// e.f. "CeilingSystem.Math.Ceiling" means that the user typing `ceiling` will catch all of the members named ceiling that are in the available namespaces
                let label, filterText =
                  match d.NamespaceToOpen with
                  | Some no when config.FullNameExternalAutocomplete ->
                    sprintf "%s (%s)" d.NameInList no, d.NameInList + code
                  | Some no -> sprintf "%s (open %s)" d.NameInList no, d.NameInList
                  | None -> d.NameInList, d.NameInList

                { CompletionItem.Create(d.NameInList) with
                    Data = Some(JValue(d.FullName))
                    Kind = (state.GlyphToCompletionKind) d.Glyph
                    TextEdit = Some(textEdit code insertPos)
                    SortText = Some(sprintf "%06d" id)
                    FilterText = Some filterText
                    Label = label }

              match!
                retryAsyncOption
                  (TimeSpan.FromMilliseconds(15.))
                  100
                  handleError
                  (getCompletions state.GetOpenFileTypeCheckResultsCached)
                |> AsyncResult.ofStringErr
              with
              | None -> return! success (None)
              | Some(decls, _, shouldKeywords, typeCheckResults, _, volatileFile) ->

                return!
                  Debug.measure "TextDocumentCompletion.TryGetCompletions success"
                  <| fun () ->
                    [ for d in decls do
                        d.FullName, (d, pos, filePath, volatileFile.Source.GetLine, typeCheckResults.GetAST) ]

                    |> state.UpdateAutocompleteItems
                    |> ignore<bool>

                    let includeKeywords = config.KeywordsAutocomplete && shouldKeywords

                    let items = decls |> Array.mapi (createCompletionItem config p.Position)

                    let its =
                      if not includeKeywords then
                        items
                      else
                        Array.append items (KeywordList.keywordCompletionItems p.Position)

                    let completionList =
                      { IsIncomplete = false
                        Items = its
                        ItemDefaults = None }

                    success (Some completionList)

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentCompletion Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.CompletionItemResolve(ci: CompletionItem) =
      let config = state.Config

      let mapHelpText (ci: CompletionItem) (text: HelpText) =
        match text with
        | HelpText.Simple(symbolName, text) ->
          let d = Documentation.Markup(markdown text)

          { ci with
              Detail = Some symbolName
              Documentation = Some d }
        | HelpText.Full(_name, tip, additionalEdit) ->
          let (si, comment) = TipFormatter.formatCompletionItemTip tip

          let edits, label =
            match additionalEdit with
            | None -> None, ci.Label
            | Some { Namespace = ns; Position = fcsPos } ->
              let text =
                let indentation = String(' ', fcsPos.Column)
                $"{indentation}open {ns}\n"

              let insertPos =
                { (fcsPos |> fcsPosToLsp) with
                    Character = 0 }

              Some
                [| { TextEdit.NewText = text
                     TextEdit.Range = { Start = insertPos; End = insertPos } } |],
              $"{ci.Label} (open {ns})"

          let d = Documentation.Markup(markdown comment)

          { ci with
              Detail = Some si
              Documentation = Some d
              AdditionalTextEdits = edits
              Label = label }

      let helpText sym =
        match KeywordList.keywordDescriptions.TryGetValue sym with
        | true, s -> CoreResponse.Res(HelpText.Simple(sym, s))
        | _ ->
          match KeywordList.hashDirectives.TryGetValue sym with
          | true, s -> CoreResponse.Res(HelpText.Simple(sym, s))
          | _ ->
            let sym =
              if
                sym.StartsWith("``", StringComparison.Ordinal)
                && sym.EndsWith("``", StringComparison.Ordinal)
              then
                sym.TrimStart([| '`' |]).TrimEnd([| '`' |])
              else
                sym

            match state.GetAutoCompleteByDeclName sym with
            | None -> //Isn't in sync filled cache, we don't have result
              CoreResponse.ErrorRes(sprintf "No help text available for symbol '%s'" sym)
            | Some(decl, _pos, _fn, _, _) -> //Is in sync filled cache, try to get results from async filled caches or calculate if it's not there

              let tip = decl.Description

              let n =
                match state.GetAutoCompleteNamespacesByDeclName sym with
                | Some s when not config.FullNameExternalAutocomplete -> Some s
                | _ -> None

              CoreResponse.Res(HelpText.Full(sym, tip, n))


      asyncResult {
        let tags = [ "CompletionItem", box ci ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "CompletionItemResolve Request: {params}"
            >> Log.addContextDestructured "params" ci
          )

          return!
            match ci.Data with
            | None -> LspResult.internalError "No FullName"
            | Some fullName ->
              helpText (fullName.ToString())
              |> Result.ofCoreResponse
              |> Result.bimap
                (function
                | None -> ci
                | Some text -> mapHelpText ci text)
                (fun _ -> ci)
              |> success

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "CompletionItemResolve Request Errored {p}"
            >> Log.addContextDestructured "p" ci
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentSignatureHelp(p: SignatureHelpParams) =
      asyncResult {
        let tags = [ "SignatureHelpParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentSignatureHelp Request: {params}"
            >> Log.addContextDestructured "params" p
          )


          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr



          let charAtCaret = p.Context |> Option.bind (fun c -> c.TriggerCharacter)

          match!
            SignatureHelp.getSignatureHelpFor (tyRes, pos, volatileFile.Source, charAtCaret, None)
            |> AsyncResult.ofStringErr
          with
          | None ->
            ()
            return! success None
          | Some sigHelp ->
            // sigHelpKind <- Some sigHelp.SigHelpKind

            let sigs =
              sigHelp.Methods
              |> Array.map (fun m ->
                let (signature, comment) = TipFormatter.formatPlainTip m.Description

                let parameters =
                  m.Parameters
                  |> Array.map (fun p ->
                    { ParameterInformation.Label = U2.First p.ParameterName
                      Documentation = Some(Documentation.String p.CanonicalTypeTextForSorting) })

                let d = Documentation.Markup(markdown comment)

                { SignatureInformation.Label = signature
                  Documentation = Some d
                  Parameters = Some parameters
                  ActiveParameter = None })

            let res =
              { Signatures = sigs
                ActiveSignature = sigHelp.ActiveOverload
                ActiveParameter = sigHelp.ActiveParameter }

            return! success (Some res)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSignatureHelp Request: {params}"
            >> Log.addContextDestructured "params" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentHover(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentHover Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResultsCached filePath |> AsyncResult.ofStringErr

          match tyRes.TryGetToolTipEnhanced pos lineStr with
          | Some tooltipResult ->
            logger.info (
              Log.setMessage "TryGetToolTipEnhanced : {params}"
              >> Log.addContextDestructured "params" tooltipResult
            )

            let formatCommentStyle =
              let config = state.Config

              if config.TooltipMode = "full" then
                TipFormatter.FormatCommentStyle.FullEnhanced
              else if config.TooltipMode = "summary" then
                TipFormatter.FormatCommentStyle.SummaryOnly
              else
                TipFormatter.FormatCommentStyle.Legacy

            let md text : MarkupContent =
              { Kind = MarkupKind.Markdown
                Value = text }

            match TipFormatter.tryFormatTipEnhanced tooltipResult.ToolTipText formatCommentStyle with
            | TipFormatter.TipFormatterResult.Success tooltipInfo ->

              // Display the signature as a code block

              let fsharpCode s = "```fsharp\n" + s + "\n```"

              let signature =
                tooltipResult.Signature |> TipFormatter.prepareSignature |> fsharpCode

              // Display each footer line as a separate line
              let footerLines = tooltipResult.Footer |> TipFormatter.prepareFooterLines

              let contents =
                [| signature
                   tooltipInfo.DocComment
                   match tooltipResult.SymbolInfo with
                   | TryGetToolTipEnhancedResult.Keyword _ -> ()
                   | TryGetToolTipEnhancedResult.Symbol symbolInfo ->
                     TipFormatter.renderShowDocumentationLink
                       tooltipInfo.HasTruncatedExamples
                       symbolInfo.XmlDocSig
                       symbolInfo.Assembly
                   yield! footerLines |]

              let response =
                { Contents = contents |> String.join Environment.NewLine |> md |> MarkupContent
                  Range = None }

              return (Some response)

            | TipFormatter.TipFormatterResult.Error error ->
              let contents = md $"<Note>\n {error}"

              let response =
                { Contents = MarkupContent contents
                  Range = None }

              return (Some response)

            | TipFormatter.TipFormatterResult.None -> return None

          | None -> return None

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentHover Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentPrepareRename p =
      asyncResult {
        logger.info (
          Log.setMessage "TextDocumentOnPrepareRename Request: {params}"
          >> Log.addContextDestructured "params" p
        )

        let (filePath, pos) = getFilePathAndPosition p
        let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
        let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
        let! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

        let! (_, _, range) =
          Commands.renameSymbolRange state.GetDeclarationLocation false pos lineStr volatileFile.Source tyRes
          |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

        return range |> fcsRangeToLsp |> PrepareRenameResult.Range |> Some
      }

    override x.TextDocumentRename(p: RenameParams) =
      asyncResult {
        let tags = [ "RenameParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentRename Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          // validate name and surround with backticks if necessary
          let! newName =
            Commands.adjustRenameSymbolNewName pos lineStr tyRes p.NewName
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          // safety check: rename valid?
          let! _ =
            Commands.renameSymbolRange state.GetDeclarationLocation false pos lineStr volatileFile.Source tyRes
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          let! ranges =
            state.SymbolUseWorkspace(true, true, true, pos, lineStr, volatileFile.Source, tyRes)
            |> AsyncResult.mapError (fun msg -> JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, msg))

          let! documentChanges =
            ranges
            |> Seq.map (fun kvp ->
              async {
                let edits =
                  kvp.Value
                  |> Array.map (fun range ->
                    let range = fcsRangeToLsp range
                    { Range = range; NewText = newName })

                let file: string<LocalPath> = kvp.Key

                let! version =
                  async {
                    let! file = state.GetOpenFileOrRead file
                    return file |> Option.ofResult |> Option.map (fun (f) -> f.Version)
                  }

                return
                  { TextDocument =
                      { Uri = Path.FilePathToUri(UMX.untag file)
                        Version = version }
                    Edits = edits }
              })
            |> Async.parallel75


          return
            state.ClientCapabilities
            |> Option.map (fun clientCapabilities -> WorkspaceEdit.Create(documentChanges, clientCapabilities))

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentRename Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentDefinition(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDefinition Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr

          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr
          let! decl = tyRes.TryFindDeclaration pos lineStr |> AsyncResult.ofStringErr
          return decl |> findDeclToLspLocation |> GotoResult.Single |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDefinition Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentTypeDefinition(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentTypeDefinition Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p

          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr
          let! decl = tyRes.TryFindTypeDeclaration pos lineStr |> AsyncResult.ofStringErr
          return decl |> findDeclToLspLocation |> GotoResult.Single |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentTypeDefinition Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentReferences(p: ReferenceParams) =
      asyncResult {
        let tags = [ "ReferenceParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentReferences Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          let! usages =
            state.SymbolUseWorkspace(true, true, false, pos, lineStr, volatileFile.Source, tyRes)
            |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

          let references =
            usages.Values |> Seq.collect (Seq.map fcsRangeToLspLocation) |> Seq.toArray

          return Some references
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentReferences Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentDocumentHighlight(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDocumentHighlight Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          match
            tyRes.TryGetSymbolUseAndUsages pos lineStr
            |> Result.bimap CoreResponse.Res CoreResponse.InfoRes
          with
          | CoreResponse.InfoRes _msg -> return None
          | CoreResponse.ErrorRes msg -> return! LspResult.internalError msg
          | CoreResponse.Res(_symbol, uses) ->
            return
              uses
              |> Array.map (fun s ->
                { DocumentHighlight.Range = fcsRangeToLsp s.Range
                  Kind = None })
              |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDocumentHighlight Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

      }

    override x.TextDocumentImplementation(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentImplementation Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          logger.info (
            Log.setMessage "TextDocumentImplementation Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let getProjectOptions file = state.GetProjectOptionsForFile file |> AsyncResult.bimap id failwith //? Should we fail here?

          let getUsesOfSymbol (filePath, opts: _ list, symbol: FSharpSymbol) =
            state.GetUsesOfSymbol(filePath, opts, symbol)

          let getAllProjects () =
            state.GetFilesToProject()
            |> Async.map (
              Array.map (fun (file, proj) -> UMX.untag file, proj.FSharpProjectOptions)
              >> Array.toList
            )

          let! res =
            Commands.symbolImplementationProject getProjectOptions getUsesOfSymbol getAllProjects tyRes pos lineStr
            |> AsyncResult.ofCoreResponse

          match res with
          | None -> return None
          | Some res ->
            let ranges: FSharp.Compiler.Text.Range[] =
              match res with
              | LocationResponse.Use(_, uses) -> uses |> Array.map (fun u -> u.Range)

            let mappedRanges = ranges |> Array.map fcsRangeToLspLocation

            match mappedRanges with
            | [||] -> return None
            | [| single |] -> return Some(GotoResult.Single single)
            | multiple -> return Some(GotoResult.Multiple multiple)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentImplementation Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    /// This is mostly used to power the per-document @-based searching in VSCode. Open the Command Palette, type a query starting with @SOME_MEMBER_NAME, and quickly go to that member in the current file
    override __.TextDocumentDocumentSymbol(p: DocumentSymbolParams) =
      asyncResult {
        let tags = [ "DocumentSymbolParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentDocumentSymbol Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

          match! state.GetDeclarations fn with
          | Some decls ->
            return
              decls
              |> Array.map (getDocumentSymbol state.GlyphToSymbolKind)
              |> U2.Second
              |> Some
          | None -> return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentDocumentSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    /// This is used to power the "Go to symbol in workspace" feature in VSCode. Open the Command Palette, type a query starting with #SOME_MEMBER_NAME, and quickly go to that member in the entire workspace
    override __.WorkspaceSymbol(symbolRequest: WorkspaceSymbolParams) =
      asyncResult {
        let tags = [ "WorkspaceSymbolParams", box symbolRequest ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkspaceSymbol Request: {params}"
            >> Log.addContextDestructured "params" symbolRequest
          )

          let glyphToSymbolKind = state.GlyphToSymbolKind

          let! decls = state.GetAllDeclarations()

          let res =
            decls
            |> Array.collect (fun (p, ns) ->
              let uri = Path.LocalPathToUri p

              ns
              |> Array.collect (fun n -> getWorkspaceSymbols uri glyphToSymbolKind n (applyQuery symbolRequest.Query)))

          return Some(U2.Second res)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkspaceSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" symbolRequest
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentFormatting(p: DocumentFormattingParams) =
      asyncResult {
        let tags = [ "DocumentFormattingParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          let doc = p.TextDocument
          let fileName = doc.GetFilePath() |> Utils.normalizePath

          let action () =
            logger.info (
              Log.setMessage "TextDocumentFormatting Request: {params}"
              >> Log.addContextDestructured "params" p
            )

            let tryGetFileCheckerOptionsWithLines file = state.GetOpenFileSource file
            let formatDocumentAsync x = fantomasService.FormatDocumentAsync x
            Commands.formatDocument tryGetFileCheckerOptionsWithLines formatDocumentAsync fileName

          let handlerFormattedDoc (sourceText: IFSACSourceText, formatted: string) =
            let range =
              let zero = { Line = 0; Character = 0 }
              let lastPos = sourceText.LastFilePosition

              { Start = zero
                End = fcsPosToLsp lastPos }

            [| { Range = range; NewText = formatted } |]

          return! x.HandleFormatting(fileName, action, handlerFormattedDoc, (fun (_, _, _) -> [||]))
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentFormatting Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) =
      asyncResult {
        let tags = [ "DocumentRangeFormattingParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          let doc = p.TextDocument
          let fileName = doc.GetFilePath() |> Utils.normalizePath

          let action () =
            logger.info (
              Log.setMessage "TextDocumentRangeFormatting Request: {params}"
              >> Log.addContextDestructured "params" p
            )

            let range =
              FormatSelectionRange(
                p.Range.Start.Line + 1,
                p.Range.Start.Character,
                p.Range.End.Line + 1,
                p.Range.End.Character
              )

            let tryGetFileCheckerOptionsWithLines file = state.GetOpenFileSource file
            let formatSelectionAsync x = fantomasService.FormatSelectionAsync x
            Commands.formatSelection tryGetFileCheckerOptionsWithLines formatSelectionAsync fileName range


          let handlerFormattedRangeDoc (_sourceText: IFSACSourceText, formatted: string, range: FormatSelectionRange) =
            let range =
              { Start =
                  { Line = range.StartLine - 1
                    Character = range.StartColumn }
                End =
                  { Line = range.EndLine - 1
                    Character = range.EndColumn } }

            [| { Range = range; NewText = formatted } |]


          return! x.HandleFormatting(fileName, action, (fun (_, _) -> [||]), handlerFormattedRangeDoc)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentRangeFormatting Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override x.TextDocumentCodeAction(codeActionParams: CodeActionParams) =
      asyncResult {
        let tags = [ "CodeActionParams", box codeActionParams ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentCodeAction Request: {params}"
            >> Log.addContextDestructured "params" codeActionParams
          )

          let (fixes: Async<Result<Fix list, string>[]>) =
            state.Codefixes
            |> Array.map (fun codeFix ->
              async {
                try
                  return! codeFix codeActionParams
                with e ->
                  logger.error (
                    Log.setMessage "Exception in CodeFix: {error}"
                    >> Log.addContextDestructured "error" (e.ToString())
                  )

                  return Ok []
              })
            |> Async.parallel75

          let! fixes = fixes
          let (actions: Fix list[], errors: string[]) = Array.partitionResults fixes
          let actions = actions |> List.concat

          if errors.Length <> 0 then
            logger.trace (
              Log.setMessage
                "Errors while processing code action request for {file} with {context} at {range}: {errors}"
              >> Log.addContextDestructured "file" codeActionParams.TextDocument.Uri
              >> Log.addContextDestructured "context" codeActionParams.Context
              >> Log.addContextDestructured "range" codeActionParams.Range
              >> Log.addContextDestructured "errors" errors
            )

          let tryGetFileVersion filePath =
            async {
              let! foo = state.GetOpenFileOrRead filePath
              return foo |> Option.ofResult |> Option.map (fun (f) -> f.Version)
            }

          let! clientCapabilities =
            state.ClientCapabilities
            |> Result.ofOption (fun () -> "ClientCapabilities not available")
            |> Result.ofStringErr

          match actions with
          | [] -> return None
          | actions ->
            let! fixes =
              actions
              |> List.map (CodeAction.OfFix tryGetFileVersion clientCapabilities)
              |> Async.parallel75

            return Some(fixes |> Array.map U2.Second)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentCodeAction Request Errored {p}"
            >> Log.addContextDestructured "p" codeActionParams
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.TextDocumentCodeLens(p: CodeLensParams) =
      asyncResult {
        let tags = [ "CodeLensParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentCodeLens Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath

          match! state.GetDeclarations(fn) with
          | None -> return None
          | Some decls ->
            let config = state.Config

            let res =
              [| if config.LineLens.Enabled <> "replaceCodeLens" then
                   if config.CodeLenses.Signature.Enabled then
                     yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "signature")
                 if config.CodeLenses.References.Enabled then
                   yield! decls |> Array.collect (getCodeLensInformation p.TextDocument.Uri "reference") |]

            return Some res
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentCodeLens Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.CodeLensResolve(p: CodeLens) =
      // JB:TODO see how to reuse existing code
      logger.info (
        Log.setMessage "CodeLensResolve Request: {params}"
        >> Log.addContextDestructured "params" p
      )

      let handler (f) (arg: CodeLens) : Async<LspResult<CodeLens>> =
        asyncResult {

          let tags = [ "CodeLens", box p ]
          use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

          let pos = protocolPosToPos arg.Range.Start

          let data = arg.Data.Value.ToObject<string[]>()

          let filePath = Path.FileUriToLocalPath data.[0] |> Utils.normalizePath

          try
            let! tyRes = state.GetOpenFileTypeCheckResultsCached filePath |> AsyncResult.ofStringErr


            logger.info (
              Log.setMessage "CodeLensResolve - Cached typecheck results now available for {file}."
              >> Log.addContextDestructured "file" filePath
            )

            let! (sourceText: IFSACSourceText) = state.GetOpenFileSource filePath |> AsyncResult.ofStringErr
            let! lineStr = sourceText |> tryGetLineStr pos |> Result.lineLookupErr

            let typ = data.[1]
            let! r = Async.Catch(f arg pos tyRes sourceText lineStr typ filePath)

            match r with
            | Choice1Of2(r: LspResult<CodeLens option>) ->
              match r with
              | Ok(Some r) -> return r
              | _ -> return Unchecked.defaultof<_>
            | Choice2Of2 e ->
              logger.error (
                Log.setMessage "CodeLensResolve - Child operation failed for {file}"
                >> Log.addContextDestructured "file" filePath
                >> Log.addExn e
              )

              let title = if typ = "signature" then "" else "0 References"

              let codeLens =
                { p with
                    Command =
                      Some
                        { Title = title
                          Command = ""
                          Arguments = None } }

              return codeLens
          with e ->
            trace |> Tracing.recordException e

            logger.error (
              Log.setMessage "CodeLensResolve - Operation failed on {file}"
              >> Log.addContextDestructured "file" filePath
              >> Log.addExn e
            )

            return! returnException e
        }

      let writePayload (sourceFile: string<LocalPath>, triggerPos: pos, usageLocations: range[]) =
        Some
          [| JToken.FromObject(Path.LocalPathToUri sourceFile)
             JToken.FromObject(fcsPosToLsp triggerPos)
             JToken.FromObject(usageLocations |> Array.map fcsRangeToLspLocation) |]

      handler
        (fun p pos tyRes sourceText lineStr typ file ->
          async {
            if typ = "signature" then
              match
                Debug.measure "TryGetSignatureData" (fun () ->
                  tyRes.TryGetSignatureData pos lineStr
                  |> Result.bimap CoreResponse.Res CoreResponse.ErrorRes)
              with
              | CoreResponse.InfoRes msg
              | CoreResponse.ErrorRes msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error on file {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                return { p with Command = None } |> Some |> success
              | CoreResponse.Res(typ, parms, _) ->
                let formatted = SignatureData.formatSignature typ parms

                let cmd =
                  { Title = formatted
                    Command = ""
                    Arguments = None }

                return { p with Command = Some cmd } |> Some |> success
            elif typ = "reference" then
              let! uses =
                state.SymbolUseWorkspace(false, true, false, pos, lineStr, sourceText, tyRes)
                |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

              match uses with
              | Error msg ->
                logger.error (
                  Log.setMessage "CodeLensResolve - error getting symbol use for {file}"
                  >> Log.addContextDestructured "file" file
                  >> Log.addContextDestructured "error" msg
                )

                return
                  success (
                    Some
                      { p with
                          Command =
                            Some
                              { Title = ""
                                Command = ""
                                Arguments = None } }
                  )

              | Ok uses ->
                let allUses = uses.Values |> Array.concat

                let cmd =
                  if Array.isEmpty allUses then
                    { Title = "0 References"
                      Command = ""
                      Arguments = None }
                  else
                    { Title = $"%d{allUses.Length} References"
                      Command = "fsharp.showReferences"
                      Arguments = writePayload (file, pos, allUses) }

                return { p with Command = Some cmd } |> Some |> success
            else
              logger.error (
                Log.setMessage "CodeLensResolve - unknown type {file} - {typ}"
                >> Log.addContextDestructured "file" file
                >> Log.addContextDestructured "typ" typ
              )

              return { p with Command = None } |> Some |> success
          })
        p

    override __.WorkspaceDidChangeWatchedFiles(p: DidChangeWatchedFilesParams) =
      async {

        let tags = [ "DidChangeWatchedFilesParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkspaceDidChangeWatchedFiles Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          for c in p.Changes do
            if c.Type = FileChangeType.Deleted then
              do! state.ForgetDocument c.Uri
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkspaceDidChangeWatchedFiles Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )
      }

    override __.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) =
      async {
        let tags = [ "DidChangeConfigurationParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkspaceDidChangeConfiguration Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let dto = p.Settings |> Server.deserialize<FSharpConfigRequest>

          dto.FSharp
          |> Option.iter (fun fsharpConfig ->
            let c = state.Config
            let c = c.AddDto fsharpConfig
            state.Config <- c)

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkspaceDidChangeConfiguration Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )
      }

    override __.TextDocumentFoldingRange(rangeP: FoldingRangeParams) =
      asyncResult {
        let tags = [ "FoldingRangeParams", box rangeP ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentFoldingRange Request: {params}"
            >> Log.addContextDestructured "params" rangeP
          )

          let file = rangeP.TextDocument.GetFilePath() |> Utils.normalizePath

          let getParseResultsForFile file =
            asyncResult {
              let! sourceText = state.GetOpenFileSource file
              and! parseResults = state.GetParseResults file
              return sourceText, parseResults
            }

          let! scopes = Commands.scopesForFile getParseResultsForFile file |> AsyncResult.ofStringErr
          return scopes |> Seq.map Structure.toFoldingRange |> Set.ofSeq |> List.ofSeq |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentFoldingRange Request Errored {p}"
            >> Log.addContextDestructured "p" rangeP
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.TextDocumentSelectionRange(selectionRangeP: SelectionRangeParams) =
      asyncResult {
        let tags = [ "SelectionRangeParams", box selectionRangeP ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentSelectionRange Request: {params}"
            >> Log.addContextDestructured "params" selectionRangeP
          )

          let rec mkSelectionRanges =
            function
            | [] -> None
            | r :: xs ->
              Some
                { Range = fcsRangeToLsp r
                  Parent = mkSelectionRanges xs }

          let file = selectionRangeP.TextDocument.GetFilePath() |> Utils.normalizePath

          let poss = selectionRangeP.Positions |> Array.map protocolPosToPos |> Array.toList

          let getParseResultsForFile file =
            asyncResult {
              let! parseResults = state.GetParseResults file
              return parseResults
            }

          let! ranges =
            Commands.getRangesAtPosition getParseResultsForFile file poss
            |> AsyncResult.ofStringErr

          return ranges |> List.choose mkSelectionRanges |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSelectionRange Request Errored {p}"
            >> Log.addContextDestructured "p" selectionRangeP
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentSemanticTokensFull(p: SemanticTokensParams) : AsyncLspResult<SemanticTokens option> =
      asyncResult {
        let tags = [ "SemanticTokensParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentSemanticTokensFull request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
          return! x.handleSemanticTokens fn None

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSemanticTokensFull Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }



    override x.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams) : AsyncLspResult<SemanticTokens option> =
      asyncResult {
        let tags = [ "SemanticTokensRangeParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentSemanticTokensRange request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let fn = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let fcsRange = protocolRangeToRange (UMX.untag fn) p.Range
          return! x.handleSemanticTokens fn (Some fcsRange)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentSemanticTokensRange Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentInlayHint(p: InlayHintParams) : AsyncLspResult<InlayHint[] option> =
      asyncResult {
        let tags = [ "InlayHintParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentInlayHint Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr

          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          let fcsRange = protocolRangeToRange (UMX.untag filePath) p.Range
          let config = state.Config

          let! hints =
            Commands.InlayHints(
              volatileFile.Source,
              tyRes,
              fcsRange,
              showTypeHints = config.InlayHints.typeAnnotations,
              showParameterHints = config.InlayHints.parameterNames
            )

          let getTooltip (h: InlayHints.Hint) =
            let hideTypeAnnotations = "fsharp.inlayHints.hideTypeAnnotations"
            let hideParameterNames = "fsharp.inlayHints.hideParameterNames"
            let hideAll = "fsharp.inlayHints.hideAll"
            let setToToggle = "fsharp.inlayHints.setToToggle"
            let disableLongTooltip = "fsharp.inlayHints.disableLongTooltip"

            if config.InlayHints.disableLongTooltip then
              h.Tooltip |> Option.map (InlayHintTooltip.String)
            else
              let lines = ResizeArray()

              let hideCommand =
                match h.Kind with
                | InlayHints.HintKind.Type -> hideTypeAnnotations
                | InlayHints.HintKind.Parameter -> hideParameterNames

              lines.Add $"To hide these hints, [click here](command:{hideCommand})."
              lines.Add $"To hide *ALL* hints, [click here](command:{hideAll})."

              // We don't have access to generic VSCode config so we don't know what inlay hints mode is used
              // if not isSetToToggle && toggleSupported then
              lines.Add
                $"Hints can also be hidden by default, and shown when Ctrl/Cmd+Alt is pressed. To do this, [click here](command:{setToToggle})."

              lines.Add $"Finally, to dismiss this long tooltip forever, [click here](command:{disableLongTooltip})."

              h.Tooltip
              |> Option.iter (fun t ->
                lines.Add ""
                lines.Add(t))

              String.concat "\n" lines |> markdown |> InlayHintTooltip.Markup |> Some

          let hints: InlayHint[] =
            hints
            |> Array.map (fun h ->
              { Position = fcsPosToLsp h.Pos
                Label = InlayHintLabel.String h.Text
                Kind =
                  match h.Kind with
                  | InlayHints.HintKind.Type -> Types.InlayHintKind.Type
                  | InlayHints.HintKind.Parameter -> Types.InlayHintKind.Parameter
                  |> Some
                TextEdits =
                  match h.Insertions with
                  | None -> None
                  | Some insertions ->
                    insertions
                    |> Array.map (fun insertion ->
                      { Range = fcsPosToProtocolRange insertion.Pos
                        NewText = insertion.Text })
                    |> Some
                Tooltip = getTooltip h
                PaddingLeft =
                  match h.Kind with
                  | InlayHints.HintKind.Type -> Some true
                  | _ -> None
                PaddingRight =
                  match h.Kind with
                  | InlayHints.HintKind.Parameter -> Some true
                  | _ -> None
                Data = None })

          return (Some hints)
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentInlayHint Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentInlineValue(p: InlineValueParams) =
      asyncResult {
        let tags = [ "InlineValueParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "TextDocumentInlineValue Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr


          let! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          let _fcsRange = protocolRangeToRange (UMX.untag filePath) p.Range

          let! pipelineHints = Commands.inlineValues volatileFile.Source tyRes

          let hints =
            pipelineHints
            |> Array.map (fun (pos, lineHints) ->
              { InlineValueText.Range = fcsPosToProtocolRange pos
                Text = lineHints }
              |> InlineValue.InlineValueText)
            |> Some

          return hints
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "TextDocumentInlineValue Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    //unsupported -- begin
    override x.CodeActionResolve(p) = x.logUnimplementedRequest p

    override x.DocumentLinkResolve(p) = x.logUnimplementedRequest p

    override x.Exit() = x.logIgnoredNotification (())

    override x.InlayHintResolve p = x.logUnimplementedRequest p

    override x.TextDocumentDocumentColor p = x.logUnimplementedRequest p

    override x.TextDocumentColorPresentation p = x.logUnimplementedRequest p

    override x.TextDocumentDocumentLink p = x.logUnimplementedRequest p

    override x.TextDocumentOnTypeFormatting p = x.logUnimplementedRequest p

    override x.TextDocumentSemanticTokensFullDelta p = x.logUnimplementedRequest p

    override x.TextDocumentWillSave p = x.logIgnoredNotification p

    override x.TextDocumentWillSaveWaitUntil p = x.logUnimplementedRequest p

    override x.WorkspaceDidChangeWorkspaceFolders p = x.logIgnoredNotification p

    override x.WorkspaceDidCreateFiles p = x.logIgnoredNotification p

    override x.WorkspaceDidDeleteFiles p = x.logIgnoredNotification p

    override x.WorkspaceDidRenameFiles p = x.logIgnoredNotification p

    override x.WorkspaceExecuteCommand p = x.logUnimplementedRequest p

    override x.WorkspaceWillCreateFiles p = x.logUnimplementedRequest p

    override x.WorkspaceWillDeleteFiles p = x.logUnimplementedRequest p

    override x.WorkspaceWillRenameFiles p = x.logUnimplementedRequest p

    override x.CallHierarchyIncomingCalls(p: CallHierarchyIncomingCallsParams) =
      asyncResult {
        // IncomingCalls is a recursive "Find All References".
        let tags = [ "CallHierarchyIncomingCalls", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "CallHierarchyIncomingCalls Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let filePath = Path.FileUriToLocalPath p.Item.Uri |> Utils.normalizePath
          let pos = protocolPosToPos p.Item.SelectionRange.Start
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.lineLookupErr
          // Incoming file may not be "Opened" so we need to force a typecheck
          let! tyRes = state.GetTypeCheckResultsForFile filePath |> AsyncResult.ofStringErr


          let locationToCallHierarchyItem (loc: Location) =
            asyncOption {

              // Don't process ourselves
              if p.Item.SelectionRange.Start = loc.Range.Start then
                do! None

              let fn = loc.Uri |> Path.FileUriToLocalPath |> Utils.normalizePath

              let! parseResults = state.GetParseResults fn |> Async.map Result.toOption

              let! (fullBindingRange, glyph, bindingIdents) =
                parseResults.TryRangeOfNameOfNearestOuterBindingOrMember(protocolPosToPos loc.Range.Start)

              // We only want to use the last identifiers range because if we have a member like `self.MyMember`
              // F# Find Usages only works with the last identifier's range so we want to use `MyMember`.
              let! endRange = bindingIdents |> Seq.tryLast

              // However we still want to display that whole name.
              let name = bindingIdents |> Seq.map (fun x -> x.idText) |> String.concat "."

              let retVals =
                { From =
                    { Name = name
                      Kind = (state.GlyphToSymbolKind) glyph |> Option.defaultValue SymbolKind.Method
                      Tags = None
                      Detail = Some(sprintf $"From {Path.GetFileName(UMX.untag fn)}")
                      Uri = loc.Uri
                      Range = fcsRangeToLsp fullBindingRange
                      SelectionRange = fcsRangeToLsp endRange.idRange
                      Data = None }
                  FromRanges = [| loc.Range |] }

              return retVals
            }

          let! usages =
            state.SymbolUseWorkspace(true, true, false, pos, lineStr, volatileFile.Source, tyRes)
            |> AsyncResult.mapError (JsonRpc.Error.InternalErrorMessage)

          let! references =
            usages.Values
            |> Seq.collect (Seq.map fcsRangeToLspLocation)
            |> Seq.toArray
            |> Array.map locationToCallHierarchyItem
            |> Async.parallel75
            |> Async.map (Array.choose id)

          return Some references
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "CallHierarchyIncomingCalls Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

      }



    override x.TextDocumentPrepareCallHierarchy(p: CallHierarchyPrepareParams) =
      asyncResult {
        let tags = [ "CallHierarchyPrepareParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "CallHierarchyPrepareParams Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) =
            { new ITextDocumentPositionParams with
                member __.TextDocument = p.TextDocument
                member __.Position = p.Position }
            |> getFilePathAndPosition

          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = tryGetLineStr pos volatileFile.Source |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          let! decl = tyRes.TryFindDeclaration pos lineStr |> AsyncResult.ofStringErr

          let! lexedResult =
            Lexer.getSymbol pos.Line pos.Column lineStr SymbolLookupKind.Fuzzy [||]
            |> Result.ofOption (fun () -> "No symbol found")
            |> Result.ofStringErr

          let location = findDeclToLspLocation decl

          let returnValue =
            [| { Name = lexedResult.Text
                 Kind = SymbolKind.Function
                 Tags = None
                 Detail = None
                 Uri = location.Uri
                 Range = location.Range
                 SelectionRange = location.Range
                 Data = None } |]

          return Some returnValue
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "CallHierarchyPrepareParams Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.TextDocumentPrepareTypeHierarchy p = x.logUnimplementedRequest p

    override x.TypeHierarchySubtypes p = x.logUnimplementedRequest p

    override x.TypeHierarchySupertypes p = x.logUnimplementedRequest p

    override x.TextDocumentDeclaration p = x.logUnimplementedRequest p

    override x.TextDocumentDiagnostic p = x.logUnimplementedRequest p

    override x.TextDocumentLinkedEditingRange p = x.logUnimplementedRequest p

    override x.TextDocumentMoniker p = x.logUnimplementedRequest p

    override x.WorkspaceDiagnostic p = x.logUnimplementedRequest p

    override x.WorkspaceSymbolResolve p = x.logUnimplementedRequest p

    //unsupported -- end

    // -----------------
    // FSharp Operations
    // -----------------

    override x.FSharpLiterateRequest(p: FSharpLiterateRequest) =
      logger.info (
        Log.setMessage "FSharpLiterateRequest Request: {params}"
        >> Log.addContextDestructured "params" p
      )

      Helpers.notImplemented

    override x.FSharpSignature(p: TextDocumentPositionParams) =
      asyncResult {

        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpSignature Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr

          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr
          let tip = Commands.typesig tyRes pos lineStr

          return
            tip
            |> Option.map (fun tip -> { Content = CommandResponse.typeSig FsAutoComplete.JsonSerializer.writeJson tip })
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpSignature Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpSignatureData(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpSignatureData Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let pos =
            FSharp.Compiler.Text.Position.mkPos (p.Position.Line) (p.Position.Character + 2)

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr

          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr
          let! (typ, parms, generics) = tyRes.TryGetSignatureData pos lineStr |> Result.ofStringErr

          return
            Some
              { Content = CommandResponse.signatureData FsAutoComplete.JsonSerializer.writeJson (typ, parms, generics) }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpSignatureData Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override x.FSharpDocumentationGenerator(p: OptionallyVersionedTextDocumentPositionParams) =
      asyncResult {
        let tags = [ "OptionallyVersionedTextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDocumentationGenerator Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr

          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          match!
            Commands.GenerateXmlDocumentation(tyRes, pos, lineStr)
            |> AsyncResult.ofStringErr
          with
          | None -> return ()
          | Some { InsertPosition = insertPos
                   InsertText = text } ->

            let edit: ApplyWorkspaceEditParams =
              { Label = Some "Generate Xml Documentation"
                Edit =
                  { DocumentChanges =
                      Some
                        [| { TextDocument =
                               { Uri = p.TextDocument.Uri
                                 Version = Some p.TextDocument.Version }
                             Edits =
                               [| { Range = fcsPosToProtocolRange insertPos
                                    NewText = text } |] } |]
                    Changes = None } }

            let! _ = lspClient.WorkspaceApplyEdit edit
            return ()

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDocumentationGenerator Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpLineLens(p: ProjectParms) =
      asyncResult {
        let tags = [ "ProjectParms", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpLineLense Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let fn = p.Project.GetFilePath() |> Utils.normalizePath

          match! state.GetDeclarations fn with
          | None -> return! LspResult.internalError $"No declarations found for {fn}"
          | Some decls ->
            let decls = decls |> Array.map (fun d -> d, fn)

            return Some { Content = CommandResponse.declarations FsAutoComplete.JsonSerializer.writeJson decls }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpLineLense Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpWorkspaceLoad(p: WorkspaceLoadParms) =
      asyncResult {
        let tags = [ "WorkspaceLoadParms", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpWorkspaceLoad Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let projs =
            p.TextDocuments
            |> Array.map (fun t -> t.GetFilePath() |> Utils.normalizePath)
            |> HashSet.ofArray

          transact (fun () -> state.WorkspacePaths <- (WorkspaceChosen.Projs projs))
          let! _ = state.ParseAllFiles()

          return { Content = CommandResponse.workspaceLoad FsAutoComplete.JsonSerializer.writeJson true }

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpWorkspaceLoad Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

      }


    override __.FSharpWorkspacePeek(p: WorkspacePeekRequest) =
      asyncResult {
        let tags = [ "WorkspacePeekRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpWorkspacePeek Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let res =
            WorkspacePeek.peek p.Directory p.Deep (p.ExcludedDirs |> List.ofArray)
            |> CoreResponse.Res

          let res =
            match res with
            | CoreResponse.InfoRes msg
            | CoreResponse.ErrorRes msg -> LspResult.internalError msg
            | CoreResponse.Res found ->
              { Content = CommandResponse.workspacePeek FsAutoComplete.JsonSerializer.writeJson found }
              |> success

          return! res
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpWorkspacePeek Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpProject(p: ProjectParms) =
      asyncResult {
        let tags = [ "ProjectParms", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpProject Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let paths = state.WorkspacePaths

          transact (fun () ->
            state.WorkspacePaths <-
              match paths with
              | WorkspaceChosen.Projs ps ->
                p.Project.GetFilePath()
                |> Utils.normalizePath
                |> ps.Add
                |> WorkspaceChosen.Projs
              | WorkspaceChosen.NotChosen ->
                p.Project.GetFilePath()
                |> Utils.normalizePath
                |> HashSet.single
                |> WorkspaceChosen.Projs)

          return! Helpers.notImplemented
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpWorkspacePeek Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e

      }

    override __.FSharpFsdn(p: FsdnRequest) =
      logger.info (
        Log.setMessage "FSharpFsdn Request: {params}"
        >> Log.addContextDestructured "params" p
      )
      // Hasn't been online for a long time
      Helpers.notImplemented

    override __.FSharpDotnetNewList(p: DotnetNewListRequest) =
      asyncResult {
        let tags = [ "DotnetNewListRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetNewList Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          match! Commands.DotnetNewList() |> AsyncResult.ofCoreResponse with
          | Some funcs ->
            return Some { Content = CommandResponse.dotnetnewlist FsAutoComplete.JsonSerializer.writeJson funcs }
          | None -> return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetNewList Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetNewRun(p: DotnetNewRunRequest) =
      asyncResult {
        let tags = [ "DotnetNewRunRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetNewRun Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.DotnetNewRun p.Template p.Name p.Output []
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error> // mapping unit option to unit

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetNewRun Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetAddProject(p: DotnetProjectRequest) =
      asyncResult {
        let tags = [ "DotnetProjectRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetAddProject Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          if p.Target <> p.Reference then
            do!
              Commands.DotnetAddProject p.Target p.Reference
              |> AsyncResult.ofCoreResponse
              |> AsyncResult.ignore<unit option, JsonRpc.Error> // mapping unit option to unit

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetAddProject Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetRemoveProject(p: DotnetProjectRequest) =
      asyncResult {
        let tags = [ "DotnetProjectRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetRemoveProject Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.DotnetRemoveProject p.Target p.Reference
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetRemoveProject Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FSharpDotnetSlnAdd(p: DotnetProjectRequest) =
      asyncResult {
        let tags = [ "DotnetProjectRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDotnetSlnAdd Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.DotnetSlnAdd p.Target p.Reference
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDotnetSlnAdd Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpHelp(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpHelp Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          match! Commands.Help tyRes pos lineStr |> Result.ofCoreResponse with
          | Some t -> return Some { Content = CommandResponse.help FsAutoComplete.JsonSerializer.writeJson t }
          | None -> return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpHelp Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpDocumentation(p: TextDocumentPositionParams) =
      asyncResult {
        let tags = [ "TextDocumentPositionParams", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDocumentation Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let (filePath, pos) = getFilePathAndPosition p
          let! volatileFile = state.GetOpenFileOrRead filePath |> AsyncResult.ofStringErr
          let! lineStr = volatileFile.Source |> tryGetLineStr pos |> Result.lineLookupErr
          and! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr
          lastFSharpDocumentationTypeCheck <- Some tyRes

          match! Commands.FormattedDocumentation tyRes pos lineStr |> Result.ofCoreResponse with
          | Some(tip, xml, signature, footer, xmlKey) ->
            return
              Some
                { Content =
                    CommandResponse.formattedDocumentation
                      JsonSerializer.writeJson
                      {| Tip = tip
                         XmlSig = xml
                         Signature = signature
                         Footer = footer
                         XmlKey = xmlKey |} }
          | None -> return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDocumentation Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.FSharpDocumentationSymbol(p: DocumentationForSymbolRequest) =
      asyncResult {
        let tags = [ "DocumentationForSymbolRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpDocumentationSymbol Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let! tyRes =
            lastFSharpDocumentationTypeCheck
            |> Result.ofOption (fun () -> $"No typecheck results from FSharpDocumentation")
            |> Result.ofStringErr

          match!
            Commands.FormattedDocumentationForSymbol tyRes p.XmlSig p.Assembly
            |> Result.ofCoreResponse
          with
          | None -> return None
          | Some(xml, assembly, xmlDoc, signature, footer, xmlKey) ->

            return
              { Content =
                  CommandResponse.formattedDocumentationForSymbol
                    JsonSerializer.writeJson
                    {| Xml = xml
                       Assembly = assembly
                       XmlDoc = xmlDoc
                       Signature = signature
                       Footer = footer
                       XmlKey = xmlKey |} }
              |> Some
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpDocumentationSymbol Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.LoadAnalyzers(path) =
      async {

        use trace = fsacActivitySource.StartActivityForType(thisType)

        logger.info (
          Log.setMessage "LoadAnalyzers Request: {params}"
          >> Log.addContextDestructured "params" path
        )

        try
          // since the analyzer state handling code is in `updateConfig`, re-trigger it here
          state.LoadAnalyzers()

          return LspResult.success ()
        with e ->
          trace |> Tracing.recordException e
          Loggers.analyzers.error (Log.setMessage "Loading failed" >> Log.addExn e)
          return LspResult.success ()
      }

    override x.FSharpPipelineHints(p: FSharpPipelineHintRequest) =
      asyncResult {
        let tags = [ "FSharpPipelineHintRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FSharpPipelineHints Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let filePath = p.TextDocument.GetFilePath() |> Utils.normalizePath
          let! tyRes = state.GetOpenFileTypeCheckResults filePath |> AsyncResult.ofStringErr

          match!
            Commands.pipelineHints state.GetOpenFileSource tyRes
            |> AsyncResult.ofCoreResponse
          with
          | None -> return None
          | Some res ->
            return Some { Content = CommandResponse.pipelineHint FsAutoComplete.JsonSerializer.writeJson res }
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FSharpPipelineHints Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FsProjMoveFileUp(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjMoveFileUp Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.FsProjMoveFileUp p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjMoveFileUp Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override __.FsProjMoveFileDown(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjMoveFileDown Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.FsProjMoveFileDown p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjMoveFileDown Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override __.FsProjAddFileAbove(p: DotnetFile2Request) =
      asyncResult {
        let tags = [ "DotnetFile2Request", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddFileAbove Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.addFileAbove p.FsProj p.FileVirtualPath p.NewFile
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddFileAbove Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FsProjAddFileBelow(p: DotnetFile2Request) =
      asyncResult {
        let tags = [ "DotnetFile2Request", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddFileBelow Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.addFileBelow p.FsProj p.FileVirtualPath p.NewFile
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddFileBelow Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override __.FsProjRenameFile(p: DotnetRenameFileRequest) =
      asyncResult {
        let tags = [ "DotnetRenameFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjRenameFile Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.renameFile p.FsProj p.OldFileVirtualPath p.NewFileName
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjRenameFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }


    override __.FsProjAddFile(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddFile Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.addFile p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override _.FsProjRemoveFile(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjRemoveFile Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          let fullPath = Path.Combine(Path.GetDirectoryName p.FsProj, p.FileVirtualPath)

          do!
            Commands.removeFile p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          let fileUri = Path.FilePathToUri fullPath
          do! state.ForgetDocument fileUri
          // diagnosticCollections.ClearFor fileUri

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjRemoveFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override _.FsProjAddExistingFile(p: DotnetFileRequest) =
      asyncResult {
        let tags = [ "DotnetFileRequest", box p ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "FsProjAddExistingFile Request: {params}"
            >> Log.addContextDestructured "params" p
          )

          do!
            Commands.addExistingFile p.FsProj p.FileVirtualPath
            |> AsyncResult.ofCoreResponse
            |> AsyncResult.ignore<unit option, JsonRpc.Error>

          return None
        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "FsProjAddExistingFile Request Errored {p}"
            >> Log.addContextDestructured "p" p
            >> Log.addExn e
          )

          return! returnException e
      }

    override x.Dispose() = disposables.Dispose()

    member this.WorkDoneProgressCancel(progressParams: WorkDoneProgressCancelParams) : Async<unit> =
      async {

        let tags = [ "ProgressToken", box progressParams.token ]
        use trace = fsacActivitySource.StartActivityForType(thisType, tags = tags)

        try
          logger.info (
            Log.setMessage "WorkDoneProgressCancel Request: {params}"
            >> Log.addContextDestructured "params" progressParams.token
          )

        with e ->
          trace |> Tracing.recordException e

          logger.error (
            Log.setMessage "WorkDoneProgressCancel Request Errored {p}"
            >> Log.addContextDestructured "token" progressParams.token
            >> Log.addExn e
          )

        return ()
      }

    member this.CallHierarchyOutgoingCalls
      (_arg1: CallHierarchyOutgoingCallsParams)
      : AsyncLspResult<CallHierarchyOutgoingCall array option> =
      AsyncLspResult.notImplemented

module AdaptiveFSharpLspServer =

  open StreamJsonRpc

  let createRpc (handler: IJsonRpcMessageHandler) : JsonRpc =
    let rec (|HandleableException|_|) (e: exn) =
      match e with
      | :? LocalRpcException -> Some()
      | :? TaskCanceledException -> Some()
      | :? OperationCanceledException -> Some()
      | :? Newtonsoft.Json.JsonSerializationException -> Some()
      | :? System.AggregateException as aex ->
        if aex.InnerExceptions.Count = 1 then
          (|HandleableException|_|) aex.InnerException
        else
          None
      | _ -> None

    let _strategy = StreamJsonRpcTracingStrategy(Tracing.fsacActivitySource)

    let (|Flatten|_|) (e: exn) =
      match e with
      | :? AggregateException as aex ->
        let aex = aex.Flatten()

        if aex.InnerExceptions.Count = 1 then
          Some aex.InnerException
        else
          Some e
      | _ -> Some e

    let strategy = StreamJsonRpcTracingStrategy(Tracing.fsacActivitySource)

    { new JsonRpc(handler, ActivityTracingStrategy = strategy) with
        member this.IsFatalException(ex: Exception) =
          match ex with
          | HandleableException -> false
          | _ -> true

        member this.CreateErrorDetails(request: Protocol.JsonRpcRequest, ex: Exception) =
          let isSerializable = this.ExceptionStrategy = ExceptionProcessing.ISerializable

          match ex with
          | Flatten(:? Newtonsoft.Json.JsonSerializationException as ex) ->

            let data: obj = if isSerializable then ex else Protocol.CommonErrorData(ex)

            Protocol.JsonRpcError.ErrorDetail(
              Code = Protocol.JsonRpcErrorCode.ParseError,
              Message = ex.Message,
              Data = data
            )
          | _ -> base.CreateErrorDetails(request, ex)

    }

  let startCore toolsPath workspaceLoaderFactory sourceTextFactory =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    let requestsHandlings =
      (defaultRequestHandlings (): Map<string, ServerRequestHandling<IFSharpLspServer>>)
      |> Map.add "fsharp/signature" (serverRequestHandling (fun s p -> s.FSharpSignature(p)))
      |> Map.add "fsharp/signatureData" (serverRequestHandling (fun s p -> s.FSharpSignatureData(p)))
      |> Map.add "fsharp/documentationGenerator" (serverRequestHandling (fun s p -> s.FSharpDocumentationGenerator(p)))
      |> Map.add "fsharp/lineLens" (serverRequestHandling (fun s p -> s.FSharpLineLens(p)))
      |> Map.add "fsharp/workspaceLoad" (serverRequestHandling (fun s p -> s.FSharpWorkspaceLoad(p)))
      |> Map.add "fsharp/workspacePeek" (serverRequestHandling (fun s p -> s.FSharpWorkspacePeek(p)))
      |> Map.add "fsharp/project" (serverRequestHandling (fun s p -> s.FSharpProject(p)))
      |> Map.add "fsharp/fsdn" (serverRequestHandling (fun s p -> s.FSharpFsdn(p)))
      |> Map.add "fsharp/dotnetnewlist" (serverRequestHandling (fun s p -> s.FSharpDotnetNewList(p)))
      |> Map.add "fsharp/dotnetnewrun" (serverRequestHandling (fun s p -> s.FSharpDotnetNewRun(p)))
      |> Map.add "fsharp/dotnetaddproject" (serverRequestHandling (fun s p -> s.FSharpDotnetAddProject(p)))
      |> Map.add "fsharp/dotnetremoveproject" (serverRequestHandling (fun s p -> s.FSharpDotnetRemoveProject(p)))
      |> Map.add "fsharp/dotnetaddsln" (serverRequestHandling (fun s p -> s.FSharpDotnetSlnAdd(p)))
      |> Map.add "fsharp/f1Help" (serverRequestHandling (fun s p -> s.FSharpHelp(p)))
      |> Map.add "fsharp/documentation" (serverRequestHandling (fun s p -> s.FSharpDocumentation(p)))
      |> Map.add "fsharp/documentationSymbol" (serverRequestHandling (fun s p -> s.FSharpDocumentationSymbol(p)))
      |> Map.add "fsharp/loadAnalyzers" (serverRequestHandling (fun s p -> s.LoadAnalyzers(p)))
      // |> Map.add "fsharp/fsharpLiterate" (serverRequestHandling (fun s p -> s.FSharpLiterate(p) ))
      |> Map.add "fsharp/pipelineHint" (serverRequestHandling (fun s p -> s.FSharpPipelineHints(p)))
      |> Map.add "fsproj/moveFileUp" (serverRequestHandling (fun s p -> s.FsProjMoveFileUp(p)))
      |> Map.add "fsproj/moveFileDown" (serverRequestHandling (fun s p -> s.FsProjMoveFileDown(p)))
      |> Map.add "fsproj/addFileAbove" (serverRequestHandling (fun s p -> s.FsProjAddFileAbove(p)))
      |> Map.add "fsproj/addFileBelow" (serverRequestHandling (fun s p -> s.FsProjAddFileBelow(p)))
      |> Map.add "fsproj/addFile" (serverRequestHandling (fun s p -> s.FsProjAddFile(p)))
      |> Map.add "fsproj/addExistingFile" (serverRequestHandling (fun s p -> s.FsProjAddExistingFile(p)))
      |> Map.add "fsproj/renameFile" (serverRequestHandling (fun s p -> s.FsProjRenameFile(p)))
      |> Map.add "fsproj/removeFile" (serverRequestHandling (fun s p -> s.FsProjRemoveFile(p)))

    let adaptiveServer lspClient =
      let loader = workspaceLoaderFactory toolsPath
      new AdaptiveFSharpLspServer(loader, lspClient, sourceTextFactory) :> IFSharpLspServer

    Ionide.LanguageServerProtocol.Server.start requestsHandlings input output FSharpLspClient adaptiveServer createRpc

  let start (startCore: unit -> LspCloseReason) =
    let logger = LogProvider.getLoggerByName "Startup"

    try
      let result = startCore ()

      logger.info (
        Log.setMessage "Start - Ending LSP mode with {reason}"
        >> Log.addContextDestructured "reason" result
      )

      int result
    with ex ->
      logger.error (Log.setMessage "Start - LSP mode crashed" >> Log.addExn ex)

      3
