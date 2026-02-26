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

* Update `CHANGELOG.md` with the release notes for the current release in the `Unreleased` section. Use section headings (`Added`, `Fixed`, etc.) from [keepachangelog.com](https://keepachangelog.com/).
* For individual items in the changelog, use headings like `BUGFIX`, `FEATURE`, and `ENHANCEMENT` followed by a link to the PR and the PR title.
* Run the `Promote` FAKE target to create the appropriate release version from the current `Unreleased` section, stamp the date, and create a commit and tag for the promotion.
* Push the commit and tag to `main`.
* The CI pipeline will publish a release from the tag.
