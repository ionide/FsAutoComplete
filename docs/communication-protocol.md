
# Communication Protocol

FsAutoComplete supports [LSP](https://microsoft.github.io/language-server-protocol/) as its communication protocol.

## Startup Options

The following command-line flags and environment variables are available when starting FsAutoComplete:

* `--state-directory <dir>` - a workspace-specific directory for keeping language server state.
* `--verbose` - enables additional logging printed to `stderr`.
* `--otel-exporter-enabled` - enables OpenTelemetry trace export (see [opentelemetry.md](./opentelemetry.md)).
* `DOTNET_ROOT` - sets the dotnet SDK root used when finding references for FSX scripts.

## Initialization Options

The following options may be sent as `initializationOptions` in the `initialize` request:

* `AutomaticWorkspaceInit` - setting to `true` starts workspace loading automatically, without requiring `fsharp/workspacePeek` and `fsharp/workspaceLoad` calls. It always picks the top workspace from the discovered list: all projects if no `.sln` files are found; the single `.sln` file if exactly one is found; the `.sln` with the most projects if multiple are found. Designed for clients that don't support custom workspace-selection UI.

## Supported LSP Endpoints

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
  * Generate cases for all DU cases in pattern matching
  * Generate empty interface implementation
  * Fixes suggested by [FSharpLint](https://github.com/fsprojects/FSharpLint)
* `textDocument/codeLens` & `codeLens/resolve`:
  * Signature code lenses
  * Reference-count code lenses
* `textDocument/formatting` - powered by [Fantomas](https://github.com/fsprojects/fantomas)
* `textDocument/rangeFormatting` - powered by [Fantomas](https://github.com/fsprojects/fantomas)
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

## Custom Endpoints

Custom endpoints use the `PlainNotification` type for message bodies, serialized as strings using the same JSON serialization format as the old JSON protocol.

* `fsharp/signature` - accepts `TextDocumentPositionParams`; returns the signature of the symbol at the given position as a formatted string.
* `fsharp/signatureData` - accepts `TextDocumentPositionParams`; returns the signature of the symbol at the given position as a DTO.
* `fsharp/lineLens` - accepts `ProjectParms` (`Project` field contains an F# file path); returns locations where line lenses should be displayed.
* `fsharp/compilerLocation` - no input; returns paths to FCS, FSI, and MSBuild.
* `fsharp/compile` - accepts `ProjectParms`; tries to compile the project and returns a list of errors and exit status code.
* `fsharp/workspacePeek` - accepts `WorkspacePeekRequest`; returns a list of possible workspaces (resolved solution files, or a list of projects if no solution files exist).
* `fsharp/workspaceLoad` - accepts `WorkspaceLoadParms`; loads the given list of projects in the background. Partial results are notified via the `fsharp/notifyWorkspace` notification.
* `fsharp/project` - accepts a `ProjectParms` object (pointing to a single project by URI) and loads that project into the current session.
* ~~`fsharp/fsdn`~~ - **NOT IMPLEMENTED** — previously queried FSDN for function signatures (FSDN service is offline).
* `fsharp/f1Help` - accepts `TextDocumentPositionParams`; returns a URL to MSDN documentation for the symbol at the given position.
* `fsharp/documentation` - accepts `TextDocumentPositionParams`; returns documentation data about the symbol at the given position, used for the Info Panel.
* `fsharp/documentationSymbol` - accepts `DocumentationForSymbolRequest`; returns documentation data about the given symbol from the given assembly, used for the Info Panel.
* `fsproj/moveFileUp` - accepts `DotnetFileRequest`; moves the file up one line in the project file.
* `fsproj/moveFileDown` - accepts `DotnetFileRequest`; moves the file down one line in the project file.
* `fsproj/addFileAbove` - accepts `DotnetFile2Request`; creates the file if needed and adds it above the reference file in the project (if not already present).
* `fsproj/addFileBelow` - accepts `DotnetFile2Request`; creates the file if needed and adds it below the reference file in the project (if not already present).
* `fsproj/addFile` - accepts `DotnetFileRequest`; creates the file if needed and adds it to the project (if not already present).
* `fsproj/addExistingFile` - accepts `DotnetFileRequest`; adds an existing file to a project (if not already present).
* `fsproj/removeFile` - accepts `DotnetFileRequest`; removes the file from the project.
* `fsproj/renameFile` - accepts `DotnetRenameFileRequest`; renames the file in the project.

## Supported LSP Notifications

* `window/showMessage`
* `window/logMessage`
* `textDocument/publishDiagnostics`

## Custom Notifications

* `fsharp/notifyWorkspace` - notification for workspace/solution/project loading events.
* `fsharp/notifyWorkspacePeek` - notification for the initial workspace peek.

## Settings

* `FSharp.keywordsAutocomplete` - provides keywords in the autocomplete list. Recommended default: `true`.
* `FSharp.ExternalAutocomplete` - provides autocomplete for symbols from unopened namespaces/modules, inserting `open` on accept. Recommended default: `false`.
* `FSharp.Linter` - enables FSharpLint integration for additional warnings and code-action fixes. Recommended default: `true`.
* `FSharp.UnionCaseStubGeneration` - enables the code action to generate pattern-matching cases. Recommended default: `true`.
* `FSharp.UnionCaseStubGenerationBody` - defines the dummy body used by the pattern-matching generator. Recommended default: `"failwith \"Not Implemented\""`.
* `FSharp.RecordStubGeneration` - enables the code action to generate record stubs. Recommended default: `true`.
* `FSharp.RecordStubGenerationBody` - defines the dummy body used by the record-stub generator. Recommended default: `"failwith \"Not Implemented\""`.
* `FSharp.InterfaceStubGeneration` - enables the code action to generate interface stubs. Recommended default: `true`.
* `FSharp.InterfaceStubGenerationObjectIdentifier` - defines the object identifier used by the interface-stub generator. Recommended default: `"this"`.
* `FSharp.InterfaceStubGenerationMethodBody` - defines the dummy body used by the interface-stub generator. Recommended default: `"failwith \"Not Implemented\""`.
* `FSharp.UnusedOpensAnalyzer` - enables unused `open` detection. Recommended default: `true`.
* `FSharp.UnusedDeclarationsAnalyzer` - enables unused-symbol detection. Recommended default: `true`.
* `FSharp.UseSdkScripts` - enables .NET Core SDKs for script file type-checking and evaluation; otherwise .NET Framework reference lists are used. Recommended default: `true`.
* `FSharp.SimplifyNameAnalyzer` - enables the simplify-name analyzer and removes redundant-qualifier quick fix. Recommended default: `false`.
* `FSharp.ResolveNamespaces` - enables the resolve-namespace quick fix (adds `open` when a symbol is from an unopened module/namespace). Recommended default: `true`.
* `FSharp.EnableReferenceCodeLens` - enables reference-count code lenses. Recommended default: `true`.
* `FSharp.dotNetRoot` - sets the root path for finding dotnet SDK references (primarily for FSX scripts). Default: OS-dependent — `C:\Program Files\dotnet` on Windows, `/usr/local/share/dotnet` on Unix.
* FSI extra parameters — use **either** `FSharp.FSIExtraParameters` on its own, **or** the combination of `FSharp.FSIExtraInteractiveParameters` and `FSharp.FSIExtraSharedParameters`. The former is expected to be deprecated in favour of the latter two. FSAC will emit a warning if usage is mixed (see [#1210](https://github.com/ionide/FsAutoComplete/issues/1210) for details).
  * `FSharp.FSIExtraParameters` - an array of additional runtime arguments passed to FSI, used when type-checking scripts so that type-checking has the same context as your FSI instances.
  * `FSharp.FSIExtraInteractiveParameters` - currently unused by FSAC, but available to editor plugins for interactive-only `dotnet fsi` parameters. [See the reference for interactive-only vs shared parameters.](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-interactive-options)
  * `FSharp.FSIExtraSharedParameters` - an array of additional runtime arguments passed to FSI; specifically parameters shared with the compiler. Used for type-checking scripts. [See the reference for interactive-only vs shared parameters.](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-interactive-options)

  Example:

  ```json
  "FSharp.fsiExtraSharedParameters": ["--langversion:preview"],
  "FSharp.fsiExtraInteractiveParameters": ["--readline-"]
  ```
