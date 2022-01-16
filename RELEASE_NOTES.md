### New in 0.49.6 (Released 2022/01/16)

* ENHANCEMENT: [Update Fantomas.Client to prefer stable versions](https://github.com/fsharp/FsAutoComplete/pull/880)
* BUGFIX: [Sourcelink's go-to-definition works better on windows for deterministic paths](https://github.com/fsharp/FsAutoComplete/pull/878)

### New in 0.49.5 (Released 2021/12/01)

### New in 0.49.4 (Released 2021/11/20)
* BUGFIX: [Fix background service](https://github.com/fsharp/FsAutoComplete/pull/858)
* BUGFIX: [Fix File System](https://github.com/fsharp/FsAutoComplete/pull/860)

### New in 0.49.3 (Released 2021/11/19)
* ENHANCEMENT: [Better handling of file typechecking after FCS 40 update](https://github.com/fsharp/FsAutoComplete/pull/857)
* BUGFIX: [Fix regression in cross-project support after FCS 40 update in proj-info](https://github.com/fsharp/FsAutoComplete/pull/857)

### New in 0.49.2 (Released 2021/11/16)

* BUGFIX: [Fix probing for dotnet binary locations in the dotnet tool](https://github.com/fsharp/FsAutoComplete/pull/854)

### New in 0.49.1 (Released 2021/11/14)

* BUGFIX: [Fix stuck code lenses](https://github.com/fsharp/FsAutoComplete/pull/852) (thanks @beauvankirk!)

### 0.49.0 - 8.11.2021

* FEATURE: [Support .Net 6 and F# 6](https://github.com/fsharp/FsAutoComplete/pull/846)

### New in 0.48.2 (Released 2021/10/27)

* BUGFIX: [Fix Fantomas.Client reference in the fsautocomplete dotnet tool](https://github.com/fsharp/FsAutoComplete/pull/844)

### New in 0.48.1 (Released 2021/10/24)

* BUGFIX: [Bump Fantomas.Client to 0.3.1](https://github.com/fsharp/FsAutoComplete/pull/842) (thanks @nojaf!)

### New in 0.48.0 (Released 2021/10/23)

* BUGFIX: [update handling of langword and crefs in see xmldoc nodes](https://github.com/fsharp/FsAutoComplete/pull/838)
* BUGFIX: [handle href elements on a, see, and xref xml doc comments](https://github.com/fsharp/FsAutoComplete/pull/839)
* FEATURE: [Use user's managed Fantomas dotnet tool instead of embedding directly into FSAC](https://github.com/fsharp/FsAutoComplete/pull/836) (thanks @nojaf!)

### New in 0.47.2 (Released 2021/09/09)

* BUGFIX: [Fix dotnet template rendering on non-english locales](https://github.com/fsharp/FsAutoComplete/pull/826) (thanks @jmiven)
* ENHANCEMENT: [Don't provide completions or tooltips for string literals of all kinds](https://github.com/fsharp/FsAutoComplete/pull/830)
  * This allows for other extensions to provide completions/hover tooltips for these strings when configured to do so

### New in 0.47.1 (Released 2021/08/04)

* BUGFIX: [Handle exceptions from fantomas a bit more safely](https://github.com/fsharp/FsAutoComplete/pull/823)

### New in 0.47.0 (Released 2021/07/25)

* BUGFIX: [Fix loading of dotnet new templates](https://github.com/fsharp/FsAutoComplete/pull/815) (thanks @Happypig375)
* BUGFIX: [Fix datatype for workspace/applyEdit request](https://github.com/fsharp/FsAutoComplete/pull/816)
* ENHANCEMENT: [Update Fantomas to 4.5.0 stable](https://github.com/fsharp/FsAutoComplete/pull/813) (thanks @nojaf)
* ENHANCEMENT: [Enable running on .net 6 via rollForward](https://github.com/fsharp/FsAutoComplete/pull/818)
  * NOTE: if you have both 5.0 and 6.0 SDKs installed, you _must_ launch fsautocomplete by passing the `--fx-version` argument to the dotnet CLI. See [the cli docs](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet) for more details.

### New in 0.46.7 (Released 2021/06/29)

* ENHANCEMENT: [Make the RemoveUnusedBinding codefix work for parameters as well as nested bindings](https://github.com/fsharp/FsAutoComplete/pull/812)

### New in 0.46.6 (Released 2021/06/27)

* ENHANCEMENT: [Make the Unused Value analyzer suggest single-underscore discards](https://github.com/fsharp/FsAutoComplete/pull/795) (thanks @pblasucci)
* ENHANCEMENT: [Add new 'Add explicit type annotation' codefix](https://github.com/fsharp/FsAutoComplete/pull/807)
  * This works for parameters, but not function-typed parameters
* BUGFIX: [Align with LSP protocol around command fields](https://github.com/fsharp/FsAutoComplete/commit/a3f5564ea579767f40cf673595db1efbcf755d85)
  * Fixes an issue in Ionide-vim (thanks @cannorin)
### New in 0.46.5 (Released 2021/06/21)

* ENHANCEMENT: [Add diagnostic code links to Compiler, Linter, and Analyzer diagnostics](https://github.com/fsharp/FsAutoComplete/pull/804)

### New in 0.46.4 (Released 2021/06/18)

* ENHANCEMENT: [Reenable FSharpLint linting](https://github.com/fsharp/FsAutoComplete/pull/799)

### New in 0.46.3 (Released 2021/06/17)

* ENHANCEMENT: [Update Fantomas dependency to latest prerelease](https://github.com/fsharp/FsAutoComplete/pull/798)

### New in 0.46.2 (Released 2021/06/13)

* BUGFIX: fix the dotnet tool packaging to include a missing dependency for code formatting
* BUGFIX: [fix indentation and insert position for unopened namespaces](https://github.com/fsharp/FsAutoComplete/pull/788) (Thanks @Booksbaum)
* ENHANCEMENT: [Render parameters that are functions with parens for readability](https://github.com/fsharp/FsAutoComplete/pull/785)

### New in 0.46.1 (Released 2021/06/09)

* Publish the dotnet tool fsautocomplete to nuget. It can be installed with `dotnet tool install fsautocomplete`.

### New in 0.46.0 (Released 2021/05/15)

* [Improve memory usage by reducing string array allocations](https://github.com/fsharp/FsAutoComplete/pull/777)
* [Fix fsharp/signature off-by-ones](https://github.com/fsharp/FsAutoComplete/pull/782) (Thanks @mhoogendoorn)
* [Fix analyzer usage](https://github.com/fsharp/FsAutoComplete/pull/783)
* [Add new codefixes](https://github.com/fsharp/FsAutoComplete/pull/784)
  * Add missing self-identifier to instance member
  * Refactor `typeof<'t>.Name` to `nameof('t)`

### New in 0.45.4 (Released 2021/04/30)

* Fix returned tokens in `textDocument/semanticTokens/full` and `textDocument/semanticTokens/range` to no longer return zero-length tokens.

### New in 0.45.3 (Released 2021/04/23)

* Improve edgecase detection when
  * finding declarations
  * finding type definitions
  * getting symbol usages
  * checking for inclusion in a file

### New in 0.45.2 (Released 2021/04/18)

* Improve overload detection in `textDocument/signatureHelp` for methods

### New in 0.45.1 (Released 2021/04/18)

* Fix regression in `textDocument/completion` introduced in 0.45.0

### New in 0.45.0 (Released 2021/04/17)

* Update Unused Binding CodeFix to handle more cases
* Enable faster typechecking when signature files are present for a module
  * Happens transparently, but is mutually exclusive with analyzers.
* Refactors around tooltip signature generation
* Fix the display of units of measure in tooltips (`float<m/s>` instead of `float<MeasureInverse<MeasureProduct<.....>>>`)
* Much better experience for signature help for function applications and method calls
* Update the Generate Abstract Class CodeFix to work for abstract classes that aren't defined in F#

### New in 0.44.0 (Released 2021/03/15)

* Update to Ionide.ProjInfo 0.51 to prevent workspace init deadlocks

### New in 0.43.0 (Released 2021/03/15)

* Fantomas updated to 4.4 stable
* FCS 39 update
* More codefixes!
* Fixed serialization of the FormattingOptions type to prevent server crashes
* Performance enhancements for the BackgroundService

### New in 0.42.0 (Released 2021/02/03)

* Many large changes, .Net 5 is required now
* Support for LSP semantic highlighting
* Fantomas upgrade to 4.4.0-beta-003
* FCS 38.0.2 upgrade
* Use Ionide.ProjInfo for the project system instead of the oen built into this repo
* Use local hosted msbuild to crack projects instead of managing builds ourselves

### New in 0.41.1 (Released 2020/03/23)

* Fix `PublishDiagnosticsCapabilities` type [#574](https://github.com/fsharp/FsAutoComplete/pull/574) by [@Gastove](https://github.com/Gastove)
* Set defaultDotNetSDKRoot on Linux correctly [#576](https://github.com/fsharp/FsAutoComplete/pull/576) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)

### New in 0.41.0 (Released 2020/03/10)

* Rework documentation parser [#446](https://github.com/fsharp/FsAutoComplete/issues/446) by [@MangelMaxime](https://github.com/MangelMaxime)
* Update FAKE integration [#566](https://github.com/fsharp/FsAutoComplete/issues/566) by [@baronfel](https://github.com/baronfel)
* Update FSharp.Analyzers.SDK to 0.4 [#568](https://github.com/fsharp/FsAutoComplete/issues/568) by [@baronfel](https://github.com/baronfel)

### New in 0.40.1 (Released 2020/02/28)

* Update to FCS 34.1 ( + all other deps) [#552](https://github.com/fsharp/FsAutoComplete/issues/556) by [@baronfel](https://github.com/baronfel)

### New in 0.40.0 (Released 2020/02/19)

* Move Fantomas formatting to Core project [#553](https://github.com/fsharp/FsAutoComplete/issues/553) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
* Fix return type in signatures in documentation formatter [#554](https://github.com/fsharp/FsAutoComplete/issues/554) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
* Work around build infrastructure by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
* Allows analyzer paths to be absolute [#555](https://github.com/fsharp/FsAutoComplete/issues/555) by [@Zaid-Ajaj](https://github.com/Zaid-Ajaj)
* Update FSI references version-finding algorithm to probe packs dir as well as runtimes dir [#556](https://github.com/fsharp/FsAutoComplete/issues/556) by [@baronfel](https://github.com/baronfel)
* Update FSharp.Analyzers.SDK to 0.3.0 and make them available only in .Net Core build [#557](https://github.com/fsharp/FsAutoComplete/issues/557) by [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)

### 0.39.0

### 0.38.2

### New in 0.38.1 (Released 2019/04/16)

* fix packaging of zip releases [#373](https://github.com/fsharp/FsAutoComplete/issues/373) by [@TOTBWF](https://github.com/TOTBWF)

### New in 0.38.0 (Released 2019/04/10)

* upgrade to `FSharp.Compiler.Service` v28.0.0
* upgrade to `FSharpLint.Core` v0.10.8
* include symbolcache `runtimeconfig.json` and `deps.json` to .net core binaries
* add `default.win32manifest` to .net core binaries
* fix to allow run with only .NET Core Runtime 3 installed (previously v2.x was required) [#364](https://github.com/fsharp/FsAutoComplete/issues/364)
* add go-to-implementation command (`symbolimplementation`)

### New in 0.37.0 (Released 2019/02/28)

* upgrade to `FSharp.Compiler.Service` v27.0.1
* upgrade to `FSharpLint.Core` v0.10.7

### New in 0.36.0 (Released 2019/02/20)

* upgrade to `FSharp.Compiler.Service` v26.0.1 (#338)
* upgrade to `FSharpLint.Core` v0.10.5

### New in 0.35.0 (Released 2019/02/19)

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

### New in 0.34.0 (Released 2017/09/13)

* support mixed dotnet langs projects (#173)
* add detailed errors info (#175)
* add hostPID command line arg (#190)
* add workspace peek command (#191)
* fix ci, .net core 2.0 RTM, normalize fsprojs (#197)
* fix linter crash (#206)
* single console app (#212)

### New in 0.33.0 (Released 2017/06/13)

* add sdk 2.0 support (#166)

### New in 0.32.0 (Released 2017/04/16)

* .NET Core project support

### New in 0.31.1 (Released 2017/02/07)

* Allow for inconsistent casing of "Fsharp" when detecting: #149.

### New in 0.31.0 (Released 2017/01/27)

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

(Some of these features only exposed currently via HTTP interface)

### New in 0.30.2 (Released 2016/10/31)

* Add parse errors, tooltips for keywords, and signatures for
  constructors: #135.

### New in 0.30.1 (Released 2016/10/31)

* Invalid release, ignore.

### New in 0.30.0 (Released 2016/10/30)

* Add EnclosingEntity and IsAbstract to Declaration contract: #129.
* Merge Ionide changes (#123):
  - Glyphs
  - Update dependencies
  - Lint settings
  - Keyword completion

### New in 0.29.0 (Released 2016/07/12)

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

### New in 0.28.0 (Released 2016/04/05)

* Backwards-incompatible: Make completions faster by not requiring a parse on each request: #99
* Add `SymbolUseProject` command: #101.
* Add typesig command, that doesn't get Comment data: #73
* Add extraction of xmldoc from other assemblies (from .xml files).

### New in 0.27.4 (Released 2016/02/18)

* Normalize paths to source files from projects: #94.

### New in 0.27.3 (Released 2016/02/08)

* Set MinThreads to avoid deadlocks on Mono < 4.2.2: #92.

### New in 0.27.2 (Released 2016/02/05)

* Upgrade to FCS 2.0.0.4 to fix project cracking with spaces in paths: #91.

### New in 0.27.1 (Released 2016/01/26)

* Upgrade to FCS 2.0.0.3 to fix VS2015 project cracking: #89.

### New in 0.27.0 (Released 2015/12/08)

* Upgrade to FCS 2.0.0.0-beta and add project cracking verbosity option: #86.
* Add FSharpLint support: #83.

### New in 0.26.1 (Released 2015/10/23)

* Switch to depend on FSharp.Core 4.3.1.0: #81.
* Don't output a BOM to standard out: #82

### New in 0.26.0 (Released 2015/10/20)

* Fix for uncompiled referenced projects: #78.
* Backwards-incompatible: Framework no longer returned in `project` response.

### New in 0.25.1 (Released 2015/10/16)

* Add App.config to FsAutoComplete.Suave release: #76.
* Also for fsautocomplete.exe.

### New in 0.25.0 (Released 2015/10/14)

* Add Suave hosting for FSAC: #74.
* Backwards-incompatible: return GlyphName rather than code in
  Declarations message: #75.

### New in 0.24.1 (Released 2015/09/11)

* Fix StackOverflowException and encoding issue: #70.

### New in 0.24.0 (Released 2015/09/04)

* Backwards-incompatible: do not format help text, leave that to the
  client, which allows the display to be more semantic. #63 (due to
  @Krzysztof-Cieslak)

### New in 0.23.1 (Released 2015/09/02)

* Fix MSBuild v14 support on non-English systems by avoiding
  attempting to load *.resources.dll (patch from @ryun).

### New in 0.23.0 (Released 2015/08/14)

* Add a new `colorizations <true|false>` command to enable/disable asynchronous
  provision of colorization information following a parse: #60 (Fixes #44).
* Newest FSharp.Core is used for type-checking scripts and for projects that
  do not reference FSharp.Core. Supports F# 3.0, 3.1 and 4.0: #59.
* If MSBuild v12 is not available, instead try load MSBuild v14. This, together
  with the previous point, adds support for VS2015-only Windows installs: #57.
  Fixes: #12 #21 #23 #25 #54.
* Backwards-incompatible: `compilerlocation` command has changed. Now provides
  path to best version of msbuild, fsc and fsi on Windows: #23.

### New in 0.22.0 (Released 2015/08/06)

* Backwards-incompatible: Symbol use command now includes FileName rather than Filename

### New in 0.21.1 (Released 2015/08/06)

* Reduce timeout message from 'error' to 'info'

### New in 0.21.0 (Released 2015/08/04)

* Update to FCS 1.4.X (support for F# 4.0): #52
* Automatically reparse F# project files if they are changed on disk: #47

### New in 0.20.1 (Released 2015/07/30)

* Fix exception in `symboluse` command: #46.

### New in 0.20.0 (Released 2015/07/28)

Backwards-incompatible changes:

* Update helptext command to return { Name = ""; Text = "" }. Fixes #35.
* `project` command response now has 'null' for OutputFile and
  TargetFramework if a value cannot be determined.

Other changes:

* FSharp.CompilerBinding removed, and used parts absorbed. Fixes #17.
* ScriptCheckerOptions fetched with no timeout, and also stores them.
  Fixes #18, #28.
* If a .fs file is not in a loaded project, produce an incomplete
  typecheck environment for it to give basic results.
* Update parsing of project options to include ProjectReferences. Fixes #39.
* Separate parsing of commands, main command loop, and formatting of
  response message into separate modules.

### New in 0.19.0 (Released 2015/06/30)

* Add symboluse command - https://github.com/fsharp/FsAutoComplete/pull/34
  Breaking change: all columns returned are now 1-based. Format of error
  locations has also changed to be more consistent with other formats.
* Add param completion command - https://github.com/fsharp/FsAutoComplete/pull/30

### New in 0.18.2 (Released 2015/06/13)

* Update to FCS 0.0.90 (fix referencing PCL projects) - https://github.com/fsharp/FsAutoComplete/pull/26

### New in 0.18.1 (Released 2015/06/09)

* Prevent test assemblies from being included in release archives by
  avoiding forcing the output directory.

### New in 0.18.0 (Released 2015/06/03)

* Adjust for 1-based column indexing - https://github.com/fsharp/FSharp.AutoComplete/pull/13
  Note that this was previously the intended behaviour, but column
  indexes were treated as 0-based. Ensure that both line and column
  indexes sent in commands are 1-based.

### New in 0.17.0 (Released 2015/05/31)

* Completion filtering - https://github.com/fsharp/FSharp.AutoComplete/pull/10

### New in 0.16.0 (Released 2015/05/28)

* Implement multiple unsaved file checking - https://github.com/fsharp/FSharp.AutoComplete/pull/8

### New in 0.15.0 (Released 2015/05/20)

* Add Glyphs to completion responses - https://github.com/fsharp/FSharp.AutoComplete/pull/1
