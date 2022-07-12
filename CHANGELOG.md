# Changelog

## [0.55.0] - 2022-07-12

### Added

* [Support for LSP 3.17 InlayHints](https://github.com/fsharp/FsAutoComplete/pull/943)

### Fixed

* [Codelens for -1 reference no longer shown](https://github.com/fsharp/FsAutoComplete/pull/965)
* [Remove backticks for signatures in signature help](https://github.com/fsharp/FsAutoComplete/pull/964)
* [Tons of bugs and enhancements to InlayHints](https://github.com/fsharp/FsAutoComplete/pull/957) (thanks @Booksbaum!)
* [Renames and reference counts are more accurate](https://github.com/fsharp/FsAutoComplete/pull/945)

### Changed

* [Use the parent dotnet binary to set the toolspath](https://github.com/fsharp/FsAutoComplete/pull/958)

### Removed

* [FAKE Integration](https://github.com/fsharp/FsAutoComplete/pull/961)
* Custom InlayHints - (fsharp/inlayHints, removed in favor of LSP inlayHints)

## [0.54.0] - 2022-05-29

### Fixed

* [IndexOutOfBounds exceptions that took down the process](https://github.com/fsharp/FsAutoComplete/pull/938) (thanks @BooksBaum!)

### Changed

* [Update Ionide.LanguageServerProtocol to get new types and fixes](https://github.com/fsharp/FsAutoComplete/pull/948) (thanks @BooksBaum!)
* [Enable several features to be used with untitled/unsaved files](https://github.com/fsharp/FsAutoComplete/pull/949) (thanks @BooksBaum!)
  * Shift+F1 help, Info Panel, Pipeline Hints, and Line Lens all work now for unsaved/untitled loose files
  * This required an API change to the `fsharp/fileParsed` notification - it now returns a URI instead of a string
  * This required an API change to the `fsharp/pipelineHint` request - is is now `{ TextDocument: TextDocumentIdentifier }`

## [0.53.2] - 2022-05-13

### Added
* [Update Fantomas.Client to use new fantomas alpha if present](https://github.com/fsharp/FsAutoComplete/pull/938) (thanks @nojaf!)

## [0.53.1] - 2022-05-01

### Changed

* [Alter logic for showing inlay hints to show fewer hints on parameters](https://github.com/fsharp/FsAutoComplete/pull/9350) (thanks @Booksbaum!)

## [0.53.0] - 2022-04-29

### Added

* [New Codefix: rename parameter to match signature file](https://github.com/fsharp/FsAutoComplete/pull/917) (thanks @Booksbaum!)
* [Config toggles for both kinds of code lenses](https://github.com/fsharp/FsAutoComplete/pull/931)

### Changed

* [Don't trigger inlay hints for typed bindings](https://github.com/fsharp/FsAutoComplete/pull/922)
* [Updated to Ionide.LanguageServerProtocol 0.4.0](https://github.com/fsharp/FsAutoComplete/pull/933)
* [Trigger fewer inlay hints for certain kinds of parameters](https://github.com/fsharp/FsAutoComplete/pull/932)

### Fixed

* Don't hardcode state file to my personal user directory
* [Don't generate state file in an OS-specific way](https://github.com/fsharp/FsAutoComplete/pull/927)
* [ImplementInterface code fix unification and improvements](https://github.com/fsharp/FsAutoComplete/pull/929) (thanks @Booksbaum!)
* [More trigger locations and behavior fixes for the Add Explicit Type to Parameter CodeFix](https://github.com/fsharp/FsAutoComplete/pull/926) (thanks @Booksbaum!)

## [0.52.1] - 2020-04-16

### Changed

* [Updated proj-info to get support for C#/VB projects, as well as .NET SDK workload support](https://github.com/fsharp/FsAutoComplete/pull/920)

## [0.52.0] - 2020-04-14

### Added

* [New notification - `fsharp/testDetected`. This notification is fired per-file when tests are detected for the current file. The data in the payload can be used to run individual tests or groups of tests.](https://github.com/fsharp/FsAutoComplete/pull/893)
* [New endpoint - `fsharp/inlayHints](https://github.com/fsharp/FsAutoComplete/pull/907). This provides support for type annotation and parameter name inlay hints.
* [New codefix - convert erroring single-quoted interpolations to triple-quoted interpolations](https://github.com/fsharp/FsAutoComplete/pull/910)
* [New command-line argument - `--state-directory`. Specified a folder to store workspace-specific FSAC data.](https://github.com/fsharp/FsAutoComplete/pull/913)


### Changed

* [Update to .NET 6](https://github.com/fsharp/FsAutoComplete/pull/903) (Thanks @dsyme!)
* [Update to FCS 41.0.3](https://github.com/fsharp/FsAutoComplete/pull/890)
* [Update to Ionide.ProjInfo 0.58.2 to get fixes around the project loader loop](https://github.com/fsharp/FsAutoComplete/pull/904), [project cache](https://github.com/ionide/proj-info/pull/139), and [legacy project support](https://github.com/ionide/proj-info/pull/131)
* [Completions for types are much better now](https://github.com/fsharp/FsAutoComplete/pull/908) (thanks @tboby!)
* [Completions triggers on the first typed character](https://github.com/fsharp/FsAutoComplete/pull/909) (thanks @tboby!)
* [New CLI Parser with support for auto-completion and nicer help](https://github.com/fsharp/FsAutoComplete/pull/888)

### Fixed

* [Record stub generation works again](https://github.com/fsharp/FsAutoComplete/pull/905)
* The fsautocomplete.netcore.zip file that was previously added to the release announcement on GitHub is back again.
* [Several corner cases around code fixes and many LSP server endpoints](https://github.com/fsharp/FsAutoComplete/pull/911) ([part 2](https://github.com/fsharp/FsAutoComplete/pull/915)) (Thanks @Booksbaum!)

## [0.51.0] - 2022-03-13

### Fixed

* [No longer cause SignatureHelp errors due to errors in text navigation](https://github.com/fsharp/FsAutoComplete/pull/894)

### Added

* [New Codefix: Convert positional DU patterns to named patterns](https://github.com/fsharp/FsAutoComplete/pull/895)

## [0.50.1] - 2022-03-12

### Fixed

* [Fix textDocument/publishDiagnostics sometimes not getting sent](https://github.com/fsharp/FsAutoComplete/pull/887) (Thanks @Booksbaum!)
* [Fix completions in the middle of lines](https://github.com/fsharp/FsAutoComplete/pull/892)

## [0.50.0] - 2022-01-23

### Added

* New release process driven by this Changelog

### Changed

* [Update Fantomas.Client to prefer stable versions](https://github.com/fsharp/FsAutoComplete/pull/880) (Thanks @nojaf)
* [Moved to use the Ionide.LanguageServerProtocol shared nuget package](https://github.com/fsharp/FsAutoComplete/pull/875)

### Fixed
* [Sourcelink's go-to-definition works better on windows for deterministic paths](https://github.com/fsharp/FsAutoComplete/pull/878)
* [Fix missing commas in Info Panel generic type signatures](https://github.com/fsharp/FsAutoComplete/pull/870) (Thanks @jcmrva!)
* [Fix off-by-1 error in the negation-to-subtraction codefix](https://github.com/fsharp/FsAutoComplete/pull/882) (Thanks @jasiozet!)

## [0.49.5] - 2021-12-01

### Added

## [0.49.4] - 2021-11-20

### Added
* BUGFIX: [Fix background service](https://github.com/fsharp/FsAutoComplete/pull/858)
* BUGFIX: [Fix File System](https://github.com/fsharp/FsAutoComplete/pull/860)

## [0.49.3] - 2021-11-19

### Added
* ENHANCEMENT: [Better handling of file typechecking after FCS 40 update](https://github.com/fsharp/FsAutoComplete/pull/857)
* BUGFIX: [Fix regression in cross-project support after FCS 40 update in proj-info](https://github.com/fsharp/FsAutoComplete/pull/857)

## [0.49.2] - 2021-11-16

### Added
* BUGFIX: [Fix probing for dotnet binary locations in the dotnet tool](https://github.com/fsharp/FsAutoComplete/pull/854)

## [0.49.1] - 2021-11-14

### Added
* BUGFIX: [Fix stuck code lenses](https://github.com/fsharp/FsAutoComplete/pull/852) (thanks @beauvankirk!)

## [0.49.0] - 2021-10-29

### Added
* FEATURE: [Support .Net 6 and F# 6](https://github.com/fsharp/FsAutoComplete/pull/846)

## [0.48.2] - 2021-10-27

### Added
* BUGFIX: [Fix Fantomas.Client reference in the fsautocomplete dotnet tool](https://github.com/fsharp/FsAutoComplete/pull/844)

## [0.48.1] - 2021-10-24

### Added
* BUGFIX: [Bump Fantomas.Client to 0.3.1](https://github.com/fsharp/FsAutoComplete/pull/842) (thanks @nojaf!)

## [0.48.0] - 2021-10-23

### Added
* BUGFIX: [update handling of langword and crefs in see xmldoc nodes](https://github.com/fsharp/FsAutoComplete/pull/838)
* BUGFIX: [handle href elements on a, see, and xref xml doc comments](https://github.com/fsharp/FsAutoComplete/pull/839)
* FEATURE: [Use user's managed Fantomas dotnet tool instead of embedding directly into FSAC](https://github.com/fsharp/FsAutoComplete/pull/836) (thanks @nojaf!)

## [0.47.2] - 2021-09-09

### Added
* BUGFIX: [Fix dotnet template rendering on non-english locales](https://github.com/fsharp/FsAutoComplete/pull/826) (thanks @jmiven)
* ENHANCEMENT: [Don't provide completions or tooltips for string literals of all kinds](https://github.com/fsharp/FsAutoComplete/pull/830)
This allows for other extensions to provide completions/hover tooltips for these strings when configured to do so

## [0.47.1] - 2021-08-04

### Added
* BUGFIX: [Handle exceptions from fantomas a bit more safely](https://github.com/fsharp/FsAutoComplete/pull/823)

## [0.47.0] - 2021-07-25

### Added
* BUGFIX: [Fix loading of dotnet new templates](https://github.com/fsharp/FsAutoComplete/pull/815) (thanks @Happypig375)
* BUGFIX: [Fix datatype for workspace/applyEdit request](https://github.com/fsharp/FsAutoComplete/pull/816)
* ENHANCEMENT: [Update Fantomas to 4.5.0 stable](https://github.com/fsharp/FsAutoComplete/pull/813) (thanks @nojaf)
* ENHANCEMENT: [Enable running on .net 6 via rollForward](https://github.com/fsharp/FsAutoComplete/pull/818)
NOTE: if you have both 5.0 and 6.0 SDKs installed, you _must_ launch fsautocomplete by passing the `--fx-version` argument to the dotnet CLI. See [the cli docs](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet) for more details.

## [0.46.7] - 2021-06-29

### Added
* ENHANCEMENT: [Make the RemoveUnusedBinding codefix work for parameters as well as nested bindings](https://github.com/fsharp/FsAutoComplete/pull/812)

## [0.46.6] - 2021-06-27

### Added
* ENHANCEMENT: [Make the Unused Value analyzer suggest single-underscore discards](https://github.com/fsharp/FsAutoComplete/pull/795) (thanks @pblasucci)
* ENHANCEMENT: [Add new 'Add explicit type annotation' codefix](https://github.com/fsharp/FsAutoComplete/pull/807)
This works for parameters, but not function-typed parameters
* BUGFIX: [Align with LSP protocol around command fields](https://github.com/fsharp/FsAutoComplete/commit/a3f5564ea579767f40cf673595db1efbcf755d85)
Fixes an issue in Ionide-vim (thanks @cannorin)

## [0.46.5] - 2021-06-21

### Added
* ENHANCEMENT: [Add diagnostic code links to Compiler, Linter, and Analyzer diagnostics](https://github.com/fsharp/FsAutoComplete/pull/804)

## [0.46.4] - 2021-06-18

### Added
* ENHANCEMENT: [Reenable FSharpLint linting](https://github.com/fsharp/FsAutoComplete/pull/799)

## [0.46.3] - 2021-06-17

### Added
* ENHANCEMENT: [Update Fantomas dependency to latest prerelease](https://github.com/fsharp/FsAutoComplete/pull/798)

## [0.46.2] - 2021-06-13

### Added
* BUGFIX: fix the dotnet tool packaging to include a missing dependency for code formatting
* BUGFIX: [fix indentation and insert position for unopened namespaces](https://github.com/fsharp/FsAutoComplete/pull/788) (Thanks @Booksbaum)
* ENHANCEMENT: [Render parameters that are functions with parens for readability](https://github.com/fsharp/FsAutoComplete/pull/785)

## [0.46.1] - 2021-06-09

### Added
* Publish the dotnet tool fsautocomplete to nuget. It can be installed with `dotnet tool install fsautocomplete`.

## [0.46.0] - 2021-05-15

### Added
* [Improve memory usage by reducing string array allocations](https://github.com/fsharp/FsAutoComplete/pull/777)
* [Fix fsharp/signature off-by-ones](https://github.com/fsharp/FsAutoComplete/pull/782) (Thanks @mhoogendoorn)
* [Fix analyzer usage](https://github.com/fsharp/FsAutoComplete/pull/783)
* [Add new codefixes](https://github.com/fsharp/FsAutoComplete/pull/784)
* Add missing self-identifier to instance member
* Refactor `typeof<'t>.Name` to `nameof('t)`

## [0.45.4] - 2021-04-30

### Added
* Fix returned tokens in `textDocument/semanticTokens/full` and `textDocument/semanticTokens/range` to no longer return zero-length tokens.

## [0.45.3] - 2021-04-23

### Added
* Improve edgecase detection when
  * finding declarations
  * finding type definitions
  * getting symbol usages
  * checking for inclusion in a file

## [0.45.2] - 2021-04-18

### Added
* Improve overload detection in `textDocument/signatureHelp` for methods

## [0.45.1] - 2021-04-18

### Added
* Fix regression in `textDocument/completion` introduced in 0.45.0

## [0.45.0] - 2021-04-17

### Added
* Update Unused Binding CodeFix to handle more cases
* Enable faster typechecking when signature files are present for a module
  * Happens transparently, but is mutually exclusive with analyzers.
* Refactors around tooltip signature generation
* Fix the display of units of measure in tooltips (`float<m/s>` instead of `float<MeasureInverse<MeasureProduct<.....>>>`)
* Much better experience for signature help for function applications and method calls
* Update the Generate Abstract Class CodeFix to work for abstract classes that aren't defined in F#

## [0.44.0] - 2021-03-15

### Added
* Update to Ionide.ProjInfo 0.51 to prevent workspace init deadlocks

## [0.43.0] - 2021-03-15

### Added
* Fantomas updated to 4.4 stable
* FCS 39 update
* More codefixes!
* Fixed serialization of the FormattingOptions type to prevent server crashes
* Performance enhancements for the BackgroundService

## [0.42.0] - 2021-02-03

### Added
* Many large changes, .Net 5 is required now
* Support for LSP semantic highlighting
* Fantomas upgrade to 4.4.0-beta-003
* FCS 38.0.2 upgrade
* Use Ionide.ProjInfo for the project system instead of the oen built into this repo
* Use local hosted msbuild to crack projects instead of managing builds ourselves

## [0.41.1] - 2020-03-23

### Added
* Fix `PublishDiagnosticsCapabilities` type [#574](https://github.com/fsharp/FsAutoComplete/pull/574) by [@Gastove](https://github.com/Gastove)
* Set defaultDotNetSDKRoot on Linux correctly [#576](https://github.com/fsharp/FsAutoComplete/pull/576) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)

## [0.41.0] - 2020-03-10

### Added
* Rework documentation parser [#446](https://github.com/fsharp/FsAutoComplete/issues/446) by [@MangelMaxime](https://github.com/MangelMaxime)
* Update FAKE integration [#566](https://github.com/fsharp/FsAutoComplete/issues/566) by [@baronfel](https://github.com/baronfel)
* Update FSharp.Analyzers.SDK to 0.4 [#568](https://github.com/fsharp/FsAutoComplete/issues/568) by [@baronfel](https://github.com/baronfel)

## [0.40.1] - 2020-02-28

### Added
* Update to FCS 34.1 ( + all other deps) [#552](https://github.com/fsharp/FsAutoComplete/issues/556) by [@baronfel](https://github.com/baronfel)

## [0.40.0] - 2020-02-19

### Added
* Move Fantomas formatting to Core project [#553](https://github.com/fsharp/FsAutoComplete/issues/553) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
* Fix return type in signatures in documentation formatter [#554](https://github.com/fsharp/FsAutoComplete/issues/554) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
* Work around build infrastructure by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
* Allows analyzer paths to be absolute [#555](https://github.com/fsharp/FsAutoComplete/issues/555) by [@Zaid-Ajaj](https://github.com/Zaid-Ajaj)
* Update FSI references version-finding algorithm to probe packs dir as well as runtimes dir [#556](https://github.com/fsharp/FsAutoComplete/issues/556) by [@baronfel](https://github.com/baronfel)
* Update FSharp.Analyzers.SDK to 0.3.0 and make them available only in .Net Core build [#557](https://github.com/fsharp/FsAutoComplete/issues/557) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)

## [0.39.0]

### Added

## [0.38.2]

### Added

## [0.38.1] - 2019-04-16

### Added
* fix packaging of zip releases [#373](https://github.com/fsharp/FsAutoComplete/issues/373) by [@TOTBWF](https://github.com/TOTBWF)

## [0.38.0] - 2019-04-10

### Added
* upgrade to `FSharp.Compiler.Service` v28.0.0
* upgrade to `FSharpLint.Core` v0.10.8
* include symbolcache `runtimeconfig.json` and `deps.json` to .net core binaries
* add `default.win32manifest` to .net core binaries
* fix to allow run with only .NET Core Runtime 3 installed (previously v2.x was required) [#364](https://github.com/fsharp/FsAutoComplete/issues/364)
* add go-to-implementation command (`symbolimplementation`)

## [0.37.0] - 2019-02-28

### Added
* upgrade to `FSharp.Compiler.Service` v27.0.1
* upgrade to `FSharpLint.Core` v0.10.7

## [0.36.0] - 2019-02-20

### Added
* upgrade to `FSharp.Compiler.Service` v26.0.1 (#338)
* upgrade to `FSharpLint.Core` v0.10.5

## [0.35.0] - 2019-02-19

### Added
* new project parser for old fsproj/fsx based on `Dotnet.ProjInfo`, enabled by default in .NET Core FSAC
* add unused declarations diagnostic
* add simplified names analyzer
* add unused opens analyzer
* styling for XmlDocs and tooltips
* add find type declaration command
* adds autocomplete for external (from unopened namespaces and modules) symbols, provides information where and what `open` statements should be inserted
* add workspaceLoad async command
* add notifications (project loading, etc). In http mode, using websocket
* add generic parameters to tooltips
* include keywords in autocomplete only when needed
* don't autocomplete for comments, strings etc
* add project cache
* watch file changes, to trigger project reloading
* implement record stub generator (#297)
* add background and persistent symbol cache out of process
* use dnspy libs to navigate to decompiled files for external libs (#299)
* fsac .NET runs as 64 bit exe
* add description for '=' symbol (#312)
* fix autocomplete for literal values (#316)
* support keywords in helptext command
* add interface stub generator (#327)
* support `FSharp.Analyzers.SDK` analyzer
* upgrade to `FSharp.Compiler.Service` v25.0.1
* upgrade to `Dotnet.ProjInfo` v0.31.0
* upgrade to `FSharpLint.Core` v0.10.4

## [0.34.0] - 2017-09-13

### Added
* support mixed dotnet langs projects (#173)
* add detailed errors info (#175)
* add hostPID command line arg (#190)
* add workspace peek command (#191)
* fix ci, .net core 2.0 RTM, normalize fsprojs (#197)
* fix linter crash (#206)
* single console app (#212)

## [0.33.0] - 2017-06-13

### Added
* add sdk 2.0 support (#166)

## [0.32.0] - 2017-04-16

### Added
* .NET Core project support

## [0.31.1] - 2017-02-07

### Added
* Allow for inconsistent casing of "Fsharp" when detecting: #149.

## [0.31.0] - 2017-01-27

### Added
* Improvements from downstream ionide fork:
  - support msbuild15, same as preview2
  - Add Background checking
  - Performance updates for find usages
  - Implement GetNamespaceSuggestions
  - Update FSharpLint version
  - Optimize GetNamespaceSuggestions
  - Optimize GetDeclarations
  - Add endpoint for F1 Help
  - ... and more!
* (Some of these features only exposed currently via HTTP interface)

## [0.30.2] - 2016-10-31

### Added
* Add parse errors, tooltips for keywords, and signatures for constructors: #135.

## [0.30.1] - 2016-10-31

### Added
* Invalid release, ignore.

## [0.30.0] - 2016-10-30

### Added
* Add EnclosingEntity and IsAbstract to Declaration contract: #129.
* Merge Ionide changes (#123):
  - Glyphs
  - Update dependencies
  - Lint settings
  - Keyword completion

## [0.29.0] - 2016-07-12

### Added
* Add command for all declarations in known projects: #117.
* cache ProjectResponse, invalidate it if project file's last write time changed: #116.
* Add command to parse all known projects: #115.
* Merge Ionide changes (#110):
  - Naive support for project.json (this probably will be dropped in futture but let's have it now)
  - Better (file) paths normalization across different features
  - Resolve scripts to latest .Net on Windows
  - Make completion faster on Suave
  - Depend on F# 4 (FSharp.Core deployed with application) instead of 4.3.1
* Fix Symboluseproject: #104.

## [0.28.0] - 2016-04-05

### Added
* Backwards-incompatible: Make completions faster by not requiring a parse on each request: #99
* Add `SymbolUseProject` command: #101.
* Add typesig command, that doesn't get Comment data: #73
* Add extraction of xmldoc from other assemblies (from .xml files).

## [0.27.4] - 2016-02-18

### Added
* Normalize paths to source files from projects: #94.

## [0.27.3] - 2016-02-08

### Added
* Set MinThreads to avoid deadlocks on Mono < 4.2.2: #92.

## [0.27.2] - 2016-02-05

### Added
* Upgrade to FCS 2.0.0.4 to fix project cracking with spaces in paths: #91.

## [0.27.1] - 2016-01-26

### Added
* Upgrade to FCS 2.0.0.3 to fix VS2015 project cracking: #89.

## [0.27.0] - 2015-12-08

### Added
* Upgrade to FCS 2.0.0.0-beta and add project cracking verbosity option: #86.
* Add FSharpLint support: #83.

## [0.26.1] - 2015-10-23

### Added
* Switch to depend on FSharp.Core 4.3.1.0: #81.
* Don't output a BOM to standard out: #82

## [0.26.0] - 2015-10-20

### Added
* Fix for uncompiled referenced projects: #78.
* Backwards-incompatible: Framework no longer returned in `project` response.

## [0.25.1] - 2015-10-16

### Added
* Add App.config to FsAutoComplete.Suave release: #76.
* Also for fsautocomplete.exe.

## [0.25.0] - 2015-10-14

### Added
* Add Suave hosting for FSAC: #74.
* Backwards-incompatible: return GlyphName rather than code in
* Declarations message: #75.

## [0.24.1] - 2015-09-11

### Added
* Fix StackOverflowException and encoding issue: #70.

## [0.24.0] - 2015-09-04

### Added
* Backwards-incompatible: do not format help text, leave that to the client, which allows the display to be more semantic. #63 (due to @Krzysztof-Cieslak)

## [0.23.1] - 2015-09-02

### Added
* Fix MSBuild v14 support on non-English systems by avoiding attempting to load *.resources.dll (patch from @ryun).

## [0.23.0] - 2015-08-14

### Added
* Add a new `colorizations <true|false>` command to enable/disable asynchronous provision of colorization information following a parse: #60 (Fixes #44).
*  Newest FSharp.Core is used for type-checking scripts and for projects that do not reference FSharp.Core. Supports F# 3.0, 3.1 and 4.0: #59.
*  If MSBuild v12 is not available, instead try load MSBuild v14. This, together with the previous point, adds support for VS2015-only Windows installs: #57. Fixes: #12 #21 #23 #25 #54.
*  Backwards-incompatible: `compilerlocation` command has changed. Now provides path to best version of msbuild, fsc and fsi on Windows: #23.

## [0.22.0] - 2015-08-06

### Added
* Backwards-incompatible: Symbol use command now includes FileName rather than Filename

## [0.21.1] - 2015-08-06

### Added
* Reduce timeout message from 'error' to 'info'

## [0.21.0] - 2015-08-04

### Added
* Update to FCS 1.4.X (support for F# 4.0): #52
* Automatically reparse F# project files if they are changed on disk: #47

## [0.20.1] - 2015-07-30

### Added
* Fix exception in `symboluse` command: #46.

## [0.20.0] - 2015-07-28

### Added
* Backwards-incompatible changes:
  * Update helptext command to return { Name = ""; Text = "" }. Fixes #35.
  * `project` command response now has 'null' for OutputFile and TargetFramework if a value cannot be determined.
* FSharp.CompilerBinding removed, and used parts absorbed. Fixes #17.
* ScriptCheckerOptions fetched with no timeout, and also stores them. Fixes #18, #28.
* If a .fs file is not in a loaded project, produce an incomplete typecheck environment for it to give basic results.
* Update parsing of project options to include ProjectReferences. Fixes #39.
* Separate parsing of commands, main command loop, and formatting of response message into separate modules.

## [0.19.0] - 2015-06-30

### Added
* Add symboluse command - https://github.com/fsharp/FsAutoComplete/pull/34
* Breaking change: all columns returned are now 1-based. Format of error locations has also changed to be more consistent with other formats.
* Add param completion command - https://github.com/fsharp/FsAutoComplete/pull/30

## [0.18.2] - 2015-06-13

### Added
* Update to FCS 0.0.90 (fix referencing PCL projects) - https://github.com/fsharp/FsAutoComplete/pull/26

## [0.18.1] - 2015-06-09

### Added
* Prevent test assemblies from being included in release archives by avoiding forcing the output directory.

## [0.18.0] - 2015-06-03

### Added
* Adjust for 1-based column indexing - https://github.com/fsharp/FSharp.AutoComplete/pull/13
  * Note that this was previously the intended behaviour, but column indexes were treated as 0-based. Ensure that both line and column indexes sent in commands are 1-based.

## [0.17.0] - 2015-05-31

### Added
* Completion filtering - https://github.com/fsharp/FSharp.AutoComplete/pull/10

## [0.16.0] - 2015-05-28

### Added
* Implement multiple unsaved file checking - https://github.com/fsharp/FSharp.AutoComplete/pull/8

## [0.15.0] - 2015-05-20

### Added
* Add Glyphs to completion responses - https://github.com/fsharp/FSharp.AutoComplete/pull/1
