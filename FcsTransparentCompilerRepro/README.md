# FCS Bug: FSharpChecker.FindBackgroundReferencesInFile with FSharpProjectSnapshot Returns 0 for Active Patterns

## Summary

`FSharpChecker.FindBackgroundReferencesInFile` returns 0 references for active pattern symbols when called with the `FSharpProjectSnapshot` overload.

The `FSharpProjectOptions` overload works correctly, as does `FSharpCheckFileResults.GetUsesOfSymbolInFile`.

## Affected API

```fsharp
// ❌ BROKEN - Returns 0 for active patterns:
member FSharpChecker.FindBackgroundReferencesInFile(
    fileName: string, 
    projectSnapshot: FSharpProjectSnapshot,
    symbol: FSharpSymbol,
    ?userOpName: string) : Async<seq<range>>

// ✅ WORKS - Returns correct counts:
member FSharpChecker.FindBackgroundReferencesInFile(
    fileName: string,
    options: FSharpProjectOptions,
    symbol: FSharpSymbol,
    ?canInvalidateProject: bool,
    ?fastCheck: bool,
    ?userOpName: string) : Async<seq<range>>

// ✅ WORKS - Always returns correct counts:
member FSharpCheckFileResults.GetUsesOfSymbolInFile(
    symbol: FSharpSymbol,
    ?cancellationToken: CancellationToken) : FSharpSymbolUse[]
```

## Environment

- FSharp.Compiler.Service: 43.10.100
- .NET: 8.0+

## To Reproduce

```bash
cd FcsTransparentCompilerRepro
dotnet run
```

## Expected Behavior

All three APIs should return consistent reference counts for active pattern symbols.

## Actual Behavior

| API | Active Pattern References |
|-----|--------------------------|
| `FSharpChecker.FindBackgroundReferencesInFile(file, FSharpProjectOptions, ...)` | ✅ Correct |
| `FSharpChecker.FindBackgroundReferencesInFile(file, FSharpProjectSnapshot, ...)` | ❌ Returns 0 |
| `FSharpCheckFileResults.GetUsesOfSymbolInFile(symbol, ...)` | ✅ Correct |

## Sample Output

```
======================================================================
Testing with BackgroundCompiler (using FSharpProjectOptions)
======================================================================

Results for BackgroundCompiler:
Symbol                    CheckFileResults.GetUses       Checker.FindBackgroundRefs
------------------------------------------------------------------------------------------
(|IsOneOfChoice|_|)                              4                              4 ✅
(|StrStartsWithOneOf|_|)                         1                              1 ✅
(|StrStartsWith|_|)                              5                              5 ✅

======================================================================
Testing with TransparentCompiler (using FSharpProjectSnapshot)
======================================================================

Results for TransparentCompiler:
Symbol                    CheckFileResults.GetUses       Checker.FindBackgroundRefs
------------------------------------------------------------------------------------------
(|IsOneOfChoice|_|)                              4                              0 ⚠️
(|StrStartsWithOneOf|_|)                         1                              0 ⚠️
(|StrStartsWith|_|)                              5                              0 ⚠️

❌ BUG CONFIRMED: FSharpProjectSnapshot overload returns 0 for active patterns
```

## Impact

This bug affects FsAutoComplete's "Find All References" feature for active patterns when using TransparentCompiler mode (the default). References may be missed.

## Workaround

Use `FSharpCheckFileResults.GetUsesOfSymbolInFile` on each file's check results instead of `FSharpChecker.FindBackgroundReferencesInFile` when:
- Using TransparentCompiler (`useTransparentCompiler = true`)
- The symbol is an active pattern

## Test Files

The reproduction uses 3 F# source files:
- **Patterns.fs**: Defines inline struct active patterns including `(|IsOneOfChoice|_|)`
- **Module1.fs**: Uses the patterns with `open Patterns`
- **Module2.fs**: Uses the patterns with qualified access

## Original Discovery

This issue was discovered in FsAutoComplete tests:
- Test file: `test/FsAutoComplete.Tests.Lsp/FindReferencesTests.fs` - `activePatternProjectTests`
- Test project: `test/FsAutoComplete.Tests.Lsp/TestCases/FindReferences/ActivePatternProject/`
