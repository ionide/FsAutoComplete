# FSAC's Microsoft.Testing.Platform client

This is an implementation dependency of FSAC, not a separately published package.
`FsAutoComplete.Core/MtpWrapper.fs` uses the F# wrapper to discover and run tests.
The sibling `FsAutoComplete.TestingPlatform.Client.Protocol` project compiles the
internal C# types from Microsoft's source-only client package and grants this
assembly access through `InternalsVisibleTo`.

The Microsoft source package is pinned to 2.4.0. When updating it, rebuild all
supported frameworks and run `FsAutoComplete.Tests.TestExplorer` plus the LSP
`TestExplorerTests`. Check the FSAC tool package contains both client assemblies.

The F# sources were imported from
[Partas.Testing](https://github.com/shayanhabibi/Partas.Testing/tree/648d27901c65b6b8703b5953f311bc74a2d4d317/src/Partas.TestingPlatform.Client)
(MIT, shayanhabibi). Their namespace and default client name were changed for
FSAC; the session and protocol behavior is preserved. See `LICENSE` for the
original license. Microsoft's sources retain their own license in the NuGet
package and are restored at build time rather than copied into this repository.
