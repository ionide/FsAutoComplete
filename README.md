
# FsAutoComplete

[![NuGet version (FsAutoComplete)](https://img.shields.io/nuget/v/FsAutoComplete.svg?style=flat-square)](https://www.nuget.org/packages/FsAutoComplete/)

The `FsAutoComplete` project (`FSAC`) provides a backend service for rich editing or intellisense features for editors.

It can be hosted using the Language Server Protocol.

Currently, it is used by the following extensions:

* [Emacs](https://github.com/fsharp/emacs-fsharp-mode)
* [Neovim](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#fsautocomplete)
* [Vim](https://github.com/fsharp/vim-fsharp)
* [Visual Studio Code](https://github.com/ionide/ionide-vscode-fsharp)
* [Sublime Text](https://lsp.sublimetext.io/language_servers/#f)
* [Zed](https://github.com/nathanjcollins/zed-fsharp)

And it can be used with the following editors, by simply installing FsAutoComplete directly: 
`dotnet tool install --global fsautocomplete`

* [Kate](https://kate-editor.org/)
* [Flow](https://flow-control.dev/)
* [Helix](https://helix-editor.com/)

It is based on:

* [FSharp.Compiler.Service](https://github.com/fsharp/FSharp.Compiler.Service/) for F# language info.
* [Ionide.ProjInfo](https://github.com/ionide/proj-info) for project/solution management.
* [FSharpLint](https://github.com/fsprojects/FSharpLint/) for the linter feature.
* [Fantomas](https://github.com/fsprojects/fantomas) for F# code formatting.

## Building and testing

Requirements:

* .NET SDK, see [global.json](global.json) for the exact version.
   Minimum: >= 8.0, Recommended: >= 10.0 

1. Restore dotnet tools to install local Paket `dotnet tool restore`
2. Build FSAC with `dotnet build`

### DevContainer

The repository additionally provides DevContainer definition that can be used with VSCode's Remote Containers extension - use it to get stable development environment

### Creating a new code fix

Checkout [this guide](./docs/Creating%20a%20new%20code%20fix.md) to start with a new code fix.

## Releasing

* Update CHANGELOG.md with the release notes from the current release in the `Unreleased` section. Use section headings like `Added`, `Fixed`, etc from keepachangelog.com.
* For individual section items in the Changelog, use headings like `BUGFIX`, `FEATURE`, and `ENHANCEMENT` followed by a link to the PR with the PR title.
* Run the `Promote` FAKE target via the `Promote` target to create the appropriate release version from the current `Unreleased` section and stamp the date, as well as create a commit and tag for this promotion
* push this commit and tag to main
* the CI pipeline will publish a release from the tag.


## OpenTelemetry

FsAutocomplete is using [System.Diagnostics.Activity](https://learn.microsoft.com/en-us/dotnet/core/diagnostics/distributed-tracing-instrumentation-walkthroughs) to create traces.

To export traces, run [Jaeger](https://www.jaegertracing.io/)

```bash
docker run -d --name jaeger \
  -e COLLECTOR_ZIPKIN_HOST_PORT=9411 \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 6831:6831/udp \
  -p 6832:6832/udp \
  -p 5778:5778 \
  -p 16686:16686 \
  -p 4317:4317 \
  -p 4318:4318 \
  -p 14250:14250 \
  -p 14268:14268 \
  -p 14269:14269 \
  -p 9411:9411 \
  jaegertracing/all-in-one:latest
```
Then configure your [environment](https://opentelemetry.io/docs/concepts/sdk-configuration/otlp-exporter-configuration/)

```bash
OTEL_EXPORTER_OTLP_ENDPOINT = "http://localhost:4317"
```

Start FsAutocomplete by `dotnet fsautocomplete --otel-exporter-enabled`.

Or by `code .` with setting `"FSharp.fsac.fsacArgs": ["--otel-exporter-enabled"]`.
(If you also want to observe fsc traces, use the `FSharp.notifications` settings.)

Do some actions like opening documents, saving, getting tooltips, etc.

Then open `http://localhost:16686/` to inspect traces.


## Communication protocol

FsAutoComplete supports [LSP](https://microsoft.github.io/language-server-protocol/) as a communication protocol.

### Supported LSP endpoints

* `initialize`
* `textDocument/didOpen`
* `textDocument/didChange`
* `textDocument/didSave`
* `textDocument/hover`
* `textDocument/completion` & `completionItem/resolve`
* `textDocument/prepareRename` & `textDocument/rename`
* `textDocument/definition`
* `textDocument/typeDefinition`
* `textDocument/implementation`
* `textDocument/codeAction`:
  * Remove unused `open`
  * Resolve namespace/module
  * Replace unused symbol with `_`
  * Fix typo based on error message
  * Remove redundant qualifier
  * Add missing `new` keyword for `IDisposable`
  * Generate cases for all DU case in pattern matching
  * Generate empty interface implementation
  * Fixes suggested by [FSharpLint](https://github.com/fsprojects/FSharpLint)
* `textDocument/codeLens` & `codeLens/resolve`:
  * signature Code Lenses
  * reference number Code Lenses
* `textDocument/formatting` - powered by [fantomas](https://github.com/fsprojects/fantomas)
* `textDocument/rangeFormatting` - powered by [fantomas](https://github.com/fsprojects/fantomas)
* `textDocument/references`
* `textDocument/documentHighlight`
* `textDocument/signatureHelp`
* `textDocument/documentSymbol`
* `textDocument/inlayHint`
* `textDocument/inlineValue`
* `textDocument/foldingRange`
* `textDocument/selectionRange`
* `textDocument/semanticTokens/full`
* `textDocument/semanticTokens/range`
* `callHierarchy/prepareCallHierarchy`
* `callHierarchy/incomingCalls`
* `workspace/didChangeWatchedFiles`
* `workspace/didChangeConfiguration`
* `workspace/symbol`

### Custom endpoints

Custom endpoints are using (for messages body) `PlainNotification` type and string format serialized with exactly same serialization format as old JSON protocol

* `fsharp/signature` - accepts `TextDocumentPositionParams`, returns signature of symbol at given position as a formatted string
* `fsharp/signatureData` - accepts `TextDocumentPositionParams`, returns signature of symbol at given position as DTO
* `fsharp/lineLens` - accepts `ProjectParms` (`Project` filed contain F# file path), returns locations where LineLenses should be displayed
* `fsharp/compilerLocation` - no input, returns paths to FCS, FSI and MsBuild
* `fsharp/compile` - accepts `ProjectParms`, tries to compile project, returns list of errors and exit status code
* `fsharp/workspacePeek` - accepts `WorkspacePeekRequest`, returns list of possible workspaces (resolved solution files, or list of projects if there are no solution files)
* `fsharp/workspaceLoad` - accepts `WorkspaceLoadParms`, loads given list of projects in the background, partial result notified by `fsharp/notifyWorkspace` notification
* `fsharp/project`  - accepts a `ProjectParms` object (which points to a single project by URI) and loads that project into the current session
* ~~`fsharp/fsdn`~~ - **NOT IMPLEMENTED** - previously queried FSDN for function signatures (FSDN service is offline)
* `fsharp/f1Help` - accepts `TextDocumentPositionParams`, returns URL to MSDN documentation for symbol at given position
* `fsharp/documentation` - accepts `TextDocumentPositionParams`, returns documentation data about symbol at given position, used for InfoPanel
* `fsharp/documentationSymbol` - accepts `DocumentationForSymbolReuqest`, returns documentation data about given symbol from given assembly, used for InfoPanel
* `fsproj/moveFileUp` - accepts `DotnetFileRequest`, move the file down of 1 line in the project file
* `fsproj/moveFileDown` - accepts `DotnetFileRequest`, move the file up of 1 line in the project file
* `fsproj/addFileAbove` - accepts `DotnetFile2Request`, create the file if needed and add it above the reference file in the project if not already present
* `fsproj/addFileBelow` - accepts `DotnetFile2Request`, create the file if needed and add it below the reference file in the project if not already present
* `fsproj/addFile` - accepts `DotnetFileRequest`, create the file if needed and add it to the project if not already present
* `fsproj/addExistingFile` - accepts `DotnetFileRequest`, add existing file to a project if not already present
* `fsproj/removeFile` - accepts `DotnetFileRequest`, remove the file from the project
* `fsproj/renameFile` - accepts `DotnetRenameFileRequest`, rename the file from the project

### Supported LSP notifications

* `window/showMessage`
* `window/logMessage`
* `textDocument/publishDiagnostics`

### Custom notifications

* `fsharp/notifyWorkspace` - notification for workspace/solution/project loading events
* `fsharp/notifyWorkspacePeek` - notification for initial workspace peek

### Additional startup options

* `--state-directory dir` - a workspace-specific directory for keeping language server states.
* `--verbose` - passing this flag enables additional logging being printed out in `stderr`
* `DOTNET_ROOT` - setting this environment variable will set the dotnet SDK root, which is used when finding references for FSX scripts.

### Initialization options

Options that should be send as `initializationOptions` as part of `initialize` request.

* `AutomaticWorkspaceInit` - setting it to `true` will start Workspace Loading without need to run `fsharp/workspacePeek` and `fsharp/workspaceLoad` commands. It will always choose top workspace from the found list - all projects in workspace if 0 `.sln` files are found, `.sln` file if 1 `.sln` file was found, `.sln` file with most projects if multiple `.sln` files were found. It's designed to be used in clients that doesn't allow to create custom UI for selecting workspaces.

### Settings

* `FSharp.keywordsAutocomplete` - provides keywords in autocomplete list, recommended default value: `true`
* `FSharp.ExternalAutocomplete` - provides autocomplete for symbols from not opened namespaces/modules, insert `open` on accept, recommended default value: `false`
* `FSharp.Linter` - enables FSharpLint integration, provides additional warnings and code action fixes, recommended default value: `true`
* `FSharp.UnionCaseStubGeneration` - enables code action to generate pattern matching cases, recommended default value: `true`
* `FSharp.UnionCaseStubGenerationBody` - defines dummy body used by pattern matching generator, recommended default value: `"failwith \"Not Implemented\""`
* `FSharp.RecordStubGeneration` - enables code action to generate record stub, recommended default value: `true`
* `FSharp.RecordStubGenerationBody` - defines dummy body used by record stub generator, recommended default value: `"failwith \"Not Implemented\""`
* `FSharp.InterfaceStubGeneration` - enables code action to generate interface stub, recommended default value: `true`
* `FSharp.InterfaceStubGenerationObjectIdentifier` - defines object identifier used by interface stub generator,recommended default value: `"this"`
* `FSharp.InterfaceStubGenerationMethodBody` - defines dummy body used by interface stub generator, recommended default value: `"failwith \"Not Implemented\""`
* `FSharp.UnusedOpensAnalyzer` - enables unused `open` detections, recommended default value: `true`
* `FSharp.UnusedDeclarationsAnalyzer` - enables unused symbol detection, recommended default value: `true`
* `FSharp.UseSdkScripts` - enables the use of .Net Core SDKs for script file type-checking and evaluation, otherwise the .Net Framework reference lists will be used. Recommended default value: `true`. Current default value: `true`
* `FSharp.SimplifyNameAnalyzer` - enables simplify name analyzer and remove redundant qualifier quick fix, recommended default value: `false`
* `FSharp.ResolveNamespaces` - enables resolve namespace quick fix (add `open` if symbol is from not yet opened module/namespace), recommended default value: `true`
* `FSharp.EnableReferenceCodeLens` - enables reference count code lenses, recommended default value: `true`
* `FSharp.dotNetRoot` - sets the root path for finding dotnet SDK references. Primarily used for FSX Scripts. Default value: operating-system dependent. On windows, `C:\Program Files\dotnet`; on Unix, `/usr/local/share/dotnet`
* Extra parameters for FSI: use only `FSharp.FSIExtraParameters` on its own *or* a combination of `FSharp.FSIExtraInteractiveParameters` and `FSharp.FSIExtraSharedParameters`. The former is expected to be deprecated in favor of the second two. See #1210 for more detail. FSAC will send a warning if you mix usage improperly.
  * `FSharp.FSIExtraParameters` - an array of additional runtime arguments that are passed to FSI. These are used when typechecking scripts to ensure that typechecking has the same context as your FSI instances.  An example would be to set the following parameters to enable Preview features (like opening static classes) for typechecking.
  * `FSharp.FSIExtraInteractiveParameters` - currently unused by FSAC, but available to editor plugins for interactive `dotnet fsi` parameters that are not shared by the compiler. Future intentions are to manage the interpreter from FSAC, at which point FSAC will utilize this parameter. [Check this reference for parameters that are interactive-only or shared with the compiler](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-interactive-options).
  * `FSharp.FSIExtraSharedParameters` - an array of additional runtime arguments that are passed to FSI; specifically parameters that are shared with the compiler. These are used when typechecking scripts to ensure that typechecking has the same context as your FSI instances.  An example would be to set the following parameters to enable Preview features (like opening static classes) for typechecking. [Check this reference for parameters that are interactive-only or shared with the compiler](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-interactive-options).

    ```json
        "FSharp.fsiExtraSharedParameters": ["--langversion:preview"]
        "FSharp.fsiExtraInteractiveParameters": ["--readline-"]
    ```

## Maintainers

The maintainers of this repository are:

* [Krzysztof Cieślak](http://github.com/Krzysztof-Cieslak)
* [Chester Husk](http://github.com/baronfel)
* [Jimmy Byrd](https://github.com/TheAngryByrd)

Previous maintainers:

* [Robin Neatherway](https://github.com/rneatherway)
* [Steffen Forkmann](http://github.com/forki)
* [Karl Nilsson](http://github.com/kjnilsson)
* [Enrico Sada](http://github.com/enricosada)
