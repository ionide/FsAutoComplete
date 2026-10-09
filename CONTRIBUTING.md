# Contributing

## Building and Testing

Requirements:

* .NET SDK — see [global.json](global.json) for the exact version. Minimum: >= 8.0, Recommended: >= 10.0

```bash
# Restore .NET tools (includes local Paket)
dotnet tool restore

# Build the solution
dotnet build

# Run all tests
dotnet test

# Run a specific test project
dotnet test -f net8.0 ./test/FsAutoComplete.Tests.Lsp/FsAutoComplete.Tests.Lsp.fsproj

# Format code
dotnet fantomas src/ test/
```

### DevContainer

The repository provides a DevContainer definition that can be used with VSCode's Remote Containers extension — use it to get a stable, reproducible development environment.

### Creating a New Code Fix

See [docs/Creating a new code fix.md](./docs/Creating%20a%20new%20code%20fix.md) for a step-by-step guide.

## Releasing

The newest version in `CHANGELOG.md` drives the release. Do not create tags by hand.

* Add a new version section to `CHANGELOG.md` (for example, `## [0.85.0] - 2026-10-09`) with the release notes. Use section headings (`Added`, `Fixed`, etc.) from [keepachangelog.com](https://keepachangelog.com/).
* For individual items in the changelog, use headings like `BUGFIX`, `FEATURE`, and `ENHANCEMENT` followed by a link to the PR and the PR title.
* Merge the change into `main`.

When `main` gets a `CHANGELOG.md` whose newest version has no GitHub release yet (for example, `v0.85.0`), the [Release workflow](.github/workflows/release.yml) starts a release job. That job pushes the package to NuGet (with trusted publishing) and creates the GitHub release and its tag, with the changelog section as notes and the package attached. If a release fails part way, rerun it with "Run workflow" on `main`.

To see what a release would do without publishing, run `dotnet fsi build.fsx -- -p Release --dry-run`.
