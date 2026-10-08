# FsAutoComplete Copilot Instructions

## Project Overview

FsAutoComplete (FSAC) is a Language Server Protocol (LSP) backend service that provides rich editing and intellisense features for F# development. It serves as the core engine behind F# support in various editors including Visual Studio Code (Ionide), Emacs, Neovim, Vim, Sublime Text, and Zed.

## Supported Editors

FsAutoComplete currently provides F# support for:
- **Visual Studio Code** (via [Ionide](https://github.com/ionide/ionide-vscode-fsharp))
- **Emacs** (via [emacs-fsharp-mode](https://github.com/fsharp/emacs-fsharp-mode))
- **Neovim** (via [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#fsautocomplete))
- **Vim** (via [vim-fsharp](https://github.com/fsharp/vim-fsharp))
- **Sublime Text** (via [LSP package](https://lsp.sublimetext.io/language_servers/#f))
- **Zed** (via [zed-fsharp](https://github.com/nathanjcollins/zed-fsharp))

## Architecture

### Core Components

- **FsAutoComplete.Core**: Contains the core functionality, including:
  - F# compiler service interfaces
  - Code generation and refactoring utilities
  - Symbol resolution and type checking
  - Signature formatting and documentation
  - File system abstractions

- **FsAutoComplete**: Main LSP server implementation with:
  - LSP protocol handlers and endpoints
  - Code fixes and quick actions
  - Parser for LSP requests/responses
  - Program entry point

- **FsAutoComplete.Logging**: Centralized logging infrastructure

### Key Dependencies

- **FSharp.Compiler.Service** (>= 43.9.300): Core F# compiler APIs for language analysis
- **Ionide.ProjInfo** (>= 0.71.2): Project and solution file parsing, with separate packages for FCS integration and project system
- **FSharpLint.Core**: Code linting and static analysis
- **Fantomas.Client** (>= 0.9): F# code formatting
- **Microsoft.Build** (>= 17.2): MSBuild integration for project loading
- **Serilog** (>= 2.10.0): Structured logging infrastructure
- **Language Server Protocol**: Communication with editors

#### Target Frameworks
- **netstandard2.0 & netstandard2.1**: For broader compatibility
- **net8.0 & net9.0**: For latest .NET features and performance

## Development Workflow

### Building the Project

Requirements:
- .NET SDK (see `global.json` for exact version - minimum >= 6.0, recommended >= 7.0)

```bash
# Restore .NET tools (including Paket)
dotnet tool restore

# Build the entire solution
dotnet build

# Run tests: see "Running Tests" below

# Format code
dotnet fantomas src/ test/
```

#### Development Environment Options
- **DevContainer**: Use with VSCode's Remote Containers extension for stable development environment
- **Gitpod**: Web-based VSCode IDE available at https://gitpod.io/#https://github.com/fsharp/fsautocomplete

### Project Dependencies

This project uses **Paket** for dependency management instead of NuGet directly:
- Dependencies are declared in `paket.dependencies`
- Lock file is `paket.lock`
- Each project has its own `paket.references` file

### Code Organization

#### Code Fixes
- Located in `src/FsAutoComplete/CodeFixes/`
- Each code fix is typically a separate F# module
- Follow the pattern: analyze issue → generate fix → apply transformation
- **Scaffolding**: Use `dotnet fsi build.fsx -- -p ScaffoldCodeFix YourCodeFixName` to create new code fixes
- This generates implementation file, signature file, and unit test, plus updates registration files
- Examples include: `ImplementInterface.fs`, `GenerateUnionCases.fs`, `AddMissingEqualsToTypeDefinition.fs`

#### LSP Endpoints
- Standard LSP endpoints in `src/FsAutoComplete/LspServers/`
- Key server files: `AdaptiveFSharpLspServer.fs`, `AdaptiveServerState.fs`, `ProjectWorkspace.fs`
- Custom F#-specific endpoints prefixed with `fsharp/`
- Request/response types in `CommandResponse.fs`
- Interface definitions in `IFSharpLspServer.fs`

#### Testing
- Main test suite in `test/FsAutoComplete.Tests.Lsp/`
- Tests organized by feature area (CompletionTests, CodeFixTests, etc.)
- Uses F# testing frameworks with custom helpers in `Helpers.fs`
- Test cases often in `TestCases/` subdirectories

## F# Language Conventions

### Coding Style
- Follow F# community conventions
- Use `fantomas` for code formatting (configured in the project)
- Prefer immutable data structures and functional programming patterns
- Use explicit type annotations where they improve clarity

### Module Organization
- One primary type/feature per file
- Use `.fs` and `.fsi` pairs for public APIs
- Organize related functionality into modules
- Follow naming conventions: `CamelCase` for types, `camelCase` for values

### Error Handling
- Use F# Result types for error handling where appropriate
- Use FsToolkit.ErrorHandling for railway-oriented programming
- Prefer explicit error types over generic exceptions

## LSP Implementation Details

### Supported Standard LSP Features
- `textDocument/completion` with `completionItem/resolve`
- `textDocument/hover`, `textDocument/definition`, `textDocument/references`
- `textDocument/codeAction`, `textDocument/codeLens`
- `textDocument/formatting` (via Fantomas)
- `textDocument/rename`, `textDocument/signatureHelp`
- Workspace management and file watching

### Custom F# Extensions
- `fsharp/signature`: Get formatted signature at position
- `fsharp/compile`: Compile project and return diagnostics  
- `fsharp/workspacePeek`: Discover available projects/solutions
- `fsharp/workspaceLoad`: Load specific projects
- `fsproj/addFile`, `fsproj/removeFile`: Project file manipulation
- `fsharp/documentationForSymbol`: Get documentation for symbols
- `fsharp/f1Help`: F1 help functionality
- `fsharp/fsi`: F# Interactive integration

## Testing Guidelines

### Test Structure
- Tests are organized by feature area
- Use descriptive test names that explain the scenario
- Include both positive and negative test cases
- Test with realistic F# code examples

### Adding New Tests
1. Identify the appropriate test file (e.g., `CompletionTests.fs` for completion features)
2. Follow existing patterns for test setup and assertions
3. Use the helpers in `Helpers.fs` for common operations
4. Include edge cases and error conditions
5. Run the tests you touched as described in "Running Tests"
6. Remove focused test markers before submitting PRs (they cause CI failures)
7. Do not delete tests without permission.

### Running Tests
Build one target framework, then run the test executable directly with a filter:

```bash
dotnet build test/FsAutoComplete.Tests.Lsp -f net10.0
test/FsAutoComplete.Tests.Lsp/bin/Debug/net10.0/FsAutoComplete.Tests.Lsp --filter-test-list "Document Highlighting"
```

- The tests need a .NET SDK with the same major version as the framework they run on, like CI. When `global.json` resolves another SDK, the run stops with one failing test whose message is the `dotnet new globaljson` command that pins a matching SDK in `test/FsAutoComplete.Tests.Lsp` (that file is gitignored).
- `--filter-test-list` and `--filter-test-case` match a substring of the test list or case name, `--filter` takes a slash-separated path, and `--list-tests` prints names without running anything.
- Each test runs once per compiler (BackgroundCompiler and TransparentCompiler). While prototyping, run one with `USE_TRANSPARENT_COMPILER=BackgroundCompiler` (or `TransparentCompiler`). Run both before the work is done.
- Without a filter, the executable runs the full suite: the LSP test groups run four at a time, then the groups that change process-wide state (the current directory, environment variables) one after another. `--parallel-workers` changes the four. CI schedules the tests the same way, with one `dotnet test` per framework (`dotnet fsi build.fsx -- -p test-ci:net10.0` does it locally, from a Release build). `dotnet test` does not call the executable's `main`, so the run timeout below and the list of tests that did not pass at the end apply only to the executable.
- When you run the executable, the whole run is cancelled after `TEST_TIMEOUT_MINUTES` (default 10).
- A failure like "Timeout waiting for latest diagnostics" can mean a busy machine rather than a bug. Rerun with `FSAC_TEST_DEFAULT_TIMEOUT=120000`, as CI does, before debugging. That variable is the per-test limit in milliseconds (default 60000), and the waits for server notifications use it when set (10 seconds otherwise).
- Pass `-f` to `dotnet test` as well. The pin serves one target framework, so a run over all of them fails the others.

### Test Data
- Sample F# projects in `TestCases/` directories
- Use minimal, focused examples that demonstrate specific features
- Avoid overly complex test scenarios that are hard to debug

## Performance Considerations

### Memory Management
- Be mindful of memory usage in long-running language server scenarios
- Dispose of compiler service resources appropriately
- Use caching judiciously to balance performance and memory

### Responsiveness
- LSP operations should be fast and non-blocking
- Use async/await patterns for I/O operations
- Consider cancellation tokens for long-running operations

## Debugging and Telemetry

### OpenTelemetry Integration
- Tracing is available with `--otel-exporter-enabled` flag
- Use Jaeger for trace visualization during development
- Activity tracing helps debug performance issues

### Logging
- Structured logging via Serilog
- Use appropriate log levels (Debug, Info, Warning, Error)
- Include relevant context in log messages

## Common Patterns

### Working with FCS (F# Compiler Service)
- Always work with `FSharpCheckFileResults` and `FSharpParseFileResults`
- Handle both parsed and typed ASTs appropriately
- Be aware of file dependencies and project context

### LSP Request Handling
- Validate input parameters
- Handle exceptions gracefully
- Return appropriate error responses for invalid requests
- Use proper JSON serialization

### Code Generation
- Use the F# AST utilities in `TypedAstUtils.fs` and `UntypedAstUtils.fs`
- Consider both syntactic and semantic correctness
- Test generated code compiles and has expected behavior

## Contributing Guidelines

### Before Submitting Changes
1. Ensure all tests pass: `dotnet test`
2. Run code formatting: `dotnet fantomas src/ test/`
3. ALWAYS add test cases for changes, run them and report results in PRs
4. Verify the solution builds cleanly
5. Test your changes with a real F# project if possible

### Code Review Focus Areas
- Correctness of F# language analysis
- Performance impact on language server operations
- Compatibility with different F# project types
- LSP protocol compliance
- Test coverage for new features

## Resources

### Core Documentation
- [FsAutoComplete GitHub Repository](https://github.com/ionide/FsAutoComplete)
- [LSP Specification](https://microsoft.github.io/language-server-protocol/)
- [F# Compiler Service Documentation](https://fsharp.github.io/FSharp.Compiler.Service/)

### F# Development Guidelines
- [F# Style Guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/)
- [F# Formatting Guidelines](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting)
- [F# Component Design Guidelines](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/component-design-guidelines)

### Project-Specific Guides
- [Creating a New Code Fix Guide](./docs/Creating%20a%20new%20code%20fix.md)
- [Ionide.ProjInfo Documentation](https://github.com/ionide/proj-info)
- [Fantomas Configuration](https://fsprojects.github.io/fantomas/)

### Related Tools
- [FSharpLint](https://github.com/fsprojects/FSharpLint/) - Static analysis tool
- [Paket](https://fsprojects.github.io/Paket/) - Dependency management
- [FAKE](https://fake.build/) - Build automation (used for scaffolding)
