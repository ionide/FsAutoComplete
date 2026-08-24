# Contributing

FsAutoComplete is primarily maintained through agentic development under human guidance. Maintainers
discuss proposed work and then direct coding agents to make the complete change, including
implementation, tests, documentation, and other affected files.

## Start With an Issue

We generally prefer contributions as [GitHub issues] rather than pull requests. Search for an
existing report first. Issues may include proposed changes, patches, or links to forks or branches.
Maintainers may refine the scope and assign the issue to an agent to implement and validate the
complete change.

## Repo Assist

[Repo Assist] is an automated AI assistant that runs regularly in this repository. It may triage or
respond to issues, investigate bugs, suggest improvements, and attempt implementations as draft pull
requests. Its work is identified as automated and remains subject to human review; Repo Assist does
not merge pull requests or make final maintenance decisions.

Maintainers can invoke Repo Assist with `/repo-assist <instructions>` for a specific agentic task,
such as investigating an issue, preparing a fix, adding tests, or updating documentation.

## Pull Requests

Every pull request must have a matching issue that has been discussed with the maintainers. Link the
pull request to that issue and keep it focused. Maintainers may close a pull request and use the issue
as the basis for an agent-produced implementation instead; the submitted analysis and code remain
valuable inputs to that work.

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

[GitHub issues]: https://github.com/ionide/FsAutoComplete/issues
[Repo Assist]: https://github.com/githubnext/agentics/blob/main/docs/repo-assist.md
