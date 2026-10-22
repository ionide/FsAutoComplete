---
name: update-fcs-nightly
description: Judge whether a newer FCS nightly is worth taking, then move the nightly branch to it, port FSAC to what changed, and get the tests green with both compilers.
---

# Update the nightly FCS

The `nightly` branch builds FsAutoComplete (FSAC) against FSharp.Compiler.Service (FCS) and FSharp.Core built from
dotnet/fsharp `main`. You rebase it on the latest FSAC `main`, move it to the newest nightly FCS, port FSAC to what FCS
changed, and end with a green suite under both compilers. Its main use is to catch transparent compiler changes before
they ship, so a failure under one compiler only is the most valuable thing you can find.

## How nightly is wired

- The main paket group stays on the stable FCS. Every FSharp.Analyzers.SDK release pins FSharp.Core to an exact
  version, so the nightly packages cannot enter that group.
- The `FcsNightly` group in `paket.dependencies` downloads the nightly FCS and FSharp.Core from a dotnet/dotnet feed
  (`dotnet12` and later). Their versions are also in `FcsNightlyVersion` and `FSharpCoreNightlyVersion` in
  `Directory.Build.props`. `scripts/bump.fsx` keeps the two in sync.
- The `UseFcsNightly` target in `Directory.Build.targets` swaps the nightly assemblies in for every project that
  resolves FSharp.Compiler.Service.
- The main group pins a System.* package where the nightly FCS references a newer version than net8.0 and net9.0
  ship (System.Reflection.Metadata). The build shows the next one as an MSB3277 version conflict.
- `nightly` is FSAC `main` plus one commit per earlier update (`git log main..nightly`). Each message records the API
  ports and behaviour changes of that update. Read them before you port.
- Every update rebases `nightly`, so publishing it is a force push. The `nightly` ruleset on ionide/FsAutoComplete
  blocks force pushes and deletion for everyone but repository admins.

## Steps

### 1. Judge whether the newest nightly is worth it

```
dotnet fsi .agents/skills/update-fcs-nightly/scripts/bump.fsx --dry-run
```

When it prints `is already the newest`, tell the user and stop. When the user named a version, pass it as the last
argument. Otherwise keep the `dotnet/fsharp changes` link: it is the change log for this update.

List the commits that touch the checker, in the range of the link and on dotnet/fsharp `main` after its end. The
second list is merged but in no nightly yet: the feed lags `main` by days. The version suffix is the build date
(`26521` is 2026, then month × 50 + day: 21 October), not the date of the last commit in it.

```
.agents/skills/update-fcs-nightly/scripts/service-commits.sh <from> <to>
.agents/skills/update-fcs-nightly/scripts/service-commits.sh <to> main
gh api repos/dotnet/fsharp/compare/<to>...main --jq '.commits[].commit.message | split("\n")[0]' | rg FCSMinorVersion
```

The script filters on files, not titles: a title like "Completion: respect EditorBrowsableAttribute" hides an `.fsi`
change. Sort each commit by the files it lists:

- `TransparentCompiler.*`, `FSharpProjectSnapshot.fs`: transparent compiler.
- `BackgroundCompiler.*`, `IncrementalBuild.*`: background compiler.
- `service.*`, `FSharpCheckerResults.*`, `Caches.*` and the rest, or files of both lists above: both compilers.
- Any `.fsi`: an API port to expect.

Read the PR of each one (`gh pr view <number> -R dotnet/fsharp --json title,author,body`) for what it fixes and any
numbers it measured. Report to the user:

- the old and new version, and the `dotnet/fsharp changes` link;
- the transparent compiler changes, then the changes to both compilers or to the background compiler only, one line
  each with the PR number and what it fixes;
- the API ports to expect: the `.fsi` files changed;
- the same kinds of change merged on `main` but in no nightly yet, and an `FCSMinorVersion` bump, which moves the
  next nightly to a new minor version;
- a recommendation: update now, or wait for the named pending PRs.

Done when the user has decided. Go on to step 2 only on their go-ahead.

### 2. Rebase on the latest main

Fetch the remote that points at ionide/FsAutoComplete, check out `nightly`, and rebase it on `<remote>/main`. Resolve
conflicts commit by commit:

- `paket.lock`: take the side of the rebase base (`git checkout --ours paket.lock`), then run `dotnet paket install`
  to add the commit's `FcsNightly` group again, and stage the result.
- Every other file: keep both main's change and the commit's change. When main reformatted the file, write the
  commit's change in main's new layout.

Done when the rebase has finished, `git log HEAD..<remote>/main` is empty, and the working tree is clean.

### 3. Bump

Run the command of step 1 without `--dry-run`, then:

```
dotnet paket install
dotnet restore --force
```

The forced restore matters: after a lock file change, restore can report "up-to-date" and keep the old package
graph in `obj/`.

Done when the `FcsNightly` group in `paket.lock` has the new versions and the main group did not change.

### 4. Build

`dotnet build` at the root builds every target framework and the tests, with warnings as errors. Port FSAC to each
error: find the FCS change in the commit range (a dotnet/fsharp clone with `git log -S`, or the `.fsi` files at the
new commit) and use the new FCS API.

Done when the build has 0 errors and 0 warnings, and
`src/FsAutoComplete/bin/Debug/net10.0/FSharp.Compiler.Service.dll` is byte-identical (`cmp`) to
`~/.nuget/packages/fsharp.compiler.service/<version>/lib/netstandard2.0/FSharp.Compiler.Service.dll`.

### 5. Test

Run the full net10.0 suite as "Running Tests" in `AGENTS.md` describes, once with
`USE_TRANSPARENT_COMPILER=TransparentCompiler` and once with `BackgroundCompiler`. Send each run to its own log file:
tests that expect exceptions log long stack traces, and the result is the closing `EXPECTO!` line. Triage every
failure:

- **Under both compilers, the same failure**: an FCS behaviour change. Find the commit in the range. Adapt FSAC when
  its behaviour should stay; update the test when the FCS change is an improvement and the test's intent still holds.
  Name the dotnet/fsharp PR in a comment at the change.
- **Under one compiler only**: a transparent compiler (or background compiler) change, the finding this branch
  exists for. Leave the test as it is, find the suspect commits in the range, and report it.

Done when both `EXPECTO!` lines report `0 failed, 0 errored`, or every remaining failure is a reported one-compiler
finding.

### 6. Format

`dotnet fantomas .` from the repository root. `.fantomasignore` skips the test case projects.

### 7. Commit and report

The update is one new commit on top of the rebased ones. Its message names the old and new version and the
`dotnet/fsharp changes` link, and lists the API ports and the behaviour changes with their dotnet/fsharp PR numbers
(or says that FSAC needed none).

Report to the user: the old and new version, the `dotnet/fsharp changes` link, the API ports, the behaviour changes
with their dotnet/fsharp PR numbers, the test counts per compiler, every one-compiler finding, and what is left.
Publishing is left to a repository admin: `git push --force-with-lease <remote> nightly`.
