[![Build Status](https://dev.azure.com/fsautocomplete/fsautocomplete/_apis/build/status/fsharp.FsAutoComplete?branchName=master)](https://dev.azure.com/fsautocomplete/fsautocomplete/_build/latest?definitionId=1&branchName=master)

# FsAutoComplete

The `FsAutoComplete` project (`FSAC`) provides a backend service for rich editing or 'intellisense' features for editors.

It can be hosted using the Language Server Protocol.

Currently it is used by:

* [Emacs](https://github.com/fsharp/emacs-fsharp-mode)
* [Vim](https://github.com/fsharp/vim-fsharp)
* [Visual Studio Code](https://github.com/ionide/ionide-vscode-fsharp)

It's based on:

- [FSharp.Compiler.Service](https://github.com/fsharp/FSharp.Compiler.Service/) for F# language info.
- [Dotnet.ProjInfo](https://github.com/enricosada/dotnet-proj-info/) for project/sln management.
- [FSharpLint](https://github.com/fsprojects/FSharpLint/) for the linter feature.

## Required software

FsAutoComplete can run on .NET/mono or .NET Core.

### FSAC .NET

* on windows: [Microsoft Build Tools 2015](https://www.microsoft.com/en-us/download/details.aspx?id=48159)
* on unix/mac: Required: Mono >= 5.12, Recommended: Mono >= 5.18

### FSAC .NET Core

* .NET Core Sdk
* on unix/mac: Required: Mono >= 5.12, Recommended: Mono >= 5.18

## Building and testing

Requirements:

- .NET Core Sdk, see [global.json](global.json) for the exact version.
- Mono 5.18 on unix/osx
- Microsoft Build Tools 2013

There is a [FAKE script](build.fsx) who can be invoked with `build.cmd`/`build.sh`.

- To build fsautocomplete binaries in `~/bin` directory, do run `build LocalRelease`
- To build, run all tests and create packages, do run `build All`


## Communication protocol

FsAutoComplete supports [LSP](https://microsoft.github.io/language-server-protocol/) as a communication protocol. 

#### Supported LSP endpoints

* `initialize`
* `textDocument/didOpen`
* `textDocument/didChange`
* `textDocument/didSave`
* `textDocument/hover`
* `textDocument/completion` & `completionItem/resolve`
* `textDocument/rename`
* `textDocument/definition`
* `textDocument/typeDefinition`
* `textDocument/implementation`
* `textDocument/codeAction`:
  - Remove unused `open`
  - Resolve namespace/module
  - Replace unused symbol with `_`
  - Fix typo based on error message
  - Remove redundant qualifier
  - Add missing `new` keyword for `IDisposable`
  - Generate cases for all DU case in pattern matching
  - Generate empty interface implementation
  - Fixes suggested by [FSharpLint](https://github.com/fsprojects/FSharpLint)
* `textDocument/codeLens` & `codeLens/resolve`:
  - signature Code Lenses
  - reference number Code Lenses
* `textDocument/formatting` - powered by [fantomas](https://github.com/fsprojects/fantomas)
* `textDocument/references`
* `textDocument/documentHighlight`
* `textDocument/signatureHelp`
* `textDocument/documentSymbol`
* `workspace/didChangeWatchedFiles`
* `workspace/didChangeConfiguration`
* `workspace/symbol`

**Custom endpoints:**

Custom endpoints are using (for messages body) `PlainNotification` type and string format serialized with exactly same serialization format as old JSON protocol

* `fsharp/signature` - accepts `TextDocumentPositionParams`, returns signature of symbol at given position as formated string
* `fsharp/signatureData` - accepts `TextDocumentPositionParams`, returns signature of symbol at given position as DTO
* `fsharp/lineLens` - accepts `ProjectParms` (`Project` filed contain F# file path), returns locations where LineLenses should be displayed
* `fsharp/compilerLocation` - no input, returns paths to FCS, FSI and MsBuild
* `fsharp/compile` - accepts `ProjectParms`, tries to compile project, returns list of errors and exit status code
* `fsharp/workspacePeek` - accepts `WorkspacePeekRequest`, returns list of possible workspaces (resolved solution files, or list of projects if there are no solution files)
* `fsharp/workspaceLoad` - accepts `WorkspaceLoadParms`, loads given list of projects in the background, partial result notified by `fsharp/notifyWorkspace` notification
* `fsharp/project` - accepts `ProjectParms`, loads given project
* `fsharp/fsdn` - accepts `ProjectParms` (`Project` filed contain query string), query FSDN and returns list of functions
* `fsharp/f1Help` - accepts `TextDocumentPositionParams`, returns URL to MSDN documentation for symbol at given position
* `fsharp/documentation` - accepts `TextDocumentPositionParams`, returns documentation data about symbol at given position, used for InfoPanel
* `fsharp/documentationSymbol` - accepts `DocumentationForSymbolReuqest`, returns documentation data about given symbol from given assembly, used for InfoPanel

#### Supported LSP notifications

* `window/showMessage`
* `window/logMessage`
* `textDocument/publishDiagnostics`

**Custom notifications:**

* `fsharp/notifyWorkspace` - notification for workspace/solution/project loading events
* `fsharp/notifyWorkspacePeek` - notification for initial workspace peek

#### Additional startup options:

* `--background-service-enabled` - passing this flag enables background service feature, increasing FSAC responsiveness by moving some of the operations (especially background type checking) to other process. It results in increased memory usage. Used by default in Ionide.
* `--verbose` - passing this flag enables additional logging being printed out in `stderr`
* `DOTNET_ROOT` - setting this environment variable will set the dotnet SDK root, which is used when finding references for FSX scripts.

#### Initialization options:

Options that should be send as `initializationOptions` as part of `initialize` request.

* `AutomaticWorkspaceInit` - setting it to `true` will start Workspace Loading without need to run `fsharp/workspacePeek` and `fsharp/workspaceLoad` commands. It will always choose top workspace from the found list - all projects in workspace if 0 `.sln` files are found, `.sln` file if 1 `.sln` file was found, `.sln` file with most projects if multiple `.sln` files were found. It's designed to be used in clients that doesn't allow to create custom UI for selecting workspaces.

#### Settings:

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
* `FSharp.UseSdkScripts` - enables the use of .Net Core SDKs for script file type-checking and evaluation, otherwise the .Net Framework reference lists will be used. Recommended default value: `true`. Current default value: `false`
* `FSharp.SimplifyNameAnalyzer` - enables simplify name analyzer and remove redundant qualifier quick fix, recommended default value: `false`
* `FSharp.ResolveNamespaces` - enables resolve namespace quick fix (add `open` if symbol is from not yet opened module/namespace), recommended default value: `true`
* `FSharp.EnableReferenceCodeLens` - enables reference count code lenses, recommended default value: `true` if `--background-service-enabled` is used by default, `false` otherwise
* `FSharp.dotNetRoot` - sets the root path for finding dotnet SDK references. Primarily used for FSX Scripts. Default value: operating-system dependent. On windows, `C:\Program Files\dotnet`; on Unix, `/usr/local/share/dotnet`

## Troubleshooting

### FileWatcher exceptions

You may see a stack trace finishing with `System.IO.IOException: kqueue() error at init, error code = ’0’`. This is due to a limitation in the number of filehandles that the Mono file watchers can keep open. Restarting FsAutoComplete or the hosting editor should help. If not, try setting `export MONO_MANAGED_WATCHER=disabled` in your `~/.bash_profile`. Note that on OSX, this setting will only take effect if you launch emacs from the terminal.


## Maintainers

The maintainers of this repository are:

- [Steffen Forkmann](http://github.com/forki)
- [Karl Nilsson](http://github.com/kjnilsson)
- [Enrico Sada](http://github.com/enricosada)
- [Krzysztof Cieślak](http://github.com/Krzysztof-Cieslak)

The primary maintainer for this repository is [Enrico Sada](http://github.com/enricosada)

Previous maintainers:

- [Robin Neatherway](https://github.com/rneatherway)
