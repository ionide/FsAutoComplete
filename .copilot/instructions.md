# FsAutoComplete Copilot Instructions

## Project Overview

FsAutoComplete (FSAC) is a Language Server Protocol (LSP) backend service that provides rich editing and intellisense features for F# development. It serves as the core engine behind F# support in various editors including Visual Studio Code (Ionide), Emacs, Neovim, Vim, Sublime Text, and Zed.

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

- **FSharp.Compiler.Service**: Core F# compiler APIs for language analysis
- **Ionide.ProjInfo**: Project and solution file parsing
- **FSharpLint**: Code linting and static analysis
- **Fantomas**: F# code formatting
- **Language Server Protocol**: Communication with editors

## Development Workflow

### Building the Project

```bash
# Restore .NET tools (including Paket)
dotnet tool restore

# Build the entire solution
dotnet build

# Run tests
dotnet test
```

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
- Use the `build.fsx` script's `ScaffoldCodeFix` module to create new code fixes

#### LSP Endpoints
- Standard LSP endpoints in `src/FsAutoComplete/LspServers/`
- Custom F#-specific endpoints prefixed with `fsharp/`
- Request/response types in `CommandResponse.fs`

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
- `fsproj/addFile`, `fsproj/removeFile`: Project file manipulation

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
3. Verify the solution builds cleanly
4. Test your changes with a real F# project if possible

### Code Review Focus Areas
- Correctness of F# language analysis
- Performance impact on language server operations
- Compatibility with different F# project types
- LSP protocol compliance
- Test coverage for new features

## Resources

- [LSP Specification](https://microsoft.github.io/language-server-protocol/)
- [F# Compiler Service Documentation](https://fsharp.github.io/FSharp.Compiler.Service/)
- [F# Style Guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/)
- [Creating a New Code Fix Guide](./docs/Creating%20a%20new%20code%20fix.md)