#### 0.30.2 - 31.10.2016

* Add parse errors, tooltips for keywords, and signatures for
  constructors: #135.

#### 0.30.1 - 31.10.2016

* Invalid release, ignore.

#### 0.30.0 - 30.10.2016

* Add EnclosingEntity and IsAbstract to Declaration contract: #129.
* Merge Ionide changes (#123):
  - Glyphs
  - Update dependencies
  - Lint settings
  - Keyword completion

#### 0.29.0 - 12.07.2016

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

#### 0.28.0 - 05.04.2016

* Backwards-incompatible: Make completions faster by not requiring a parse on each request: #99
* Add `SymbolUseProject` command: #101.
* Add typesig command, that doesn't get Comment data: #73
* Add extraction of xmldoc from other assemblies (from .xml files).

#### 0.27.4 - 18.02.2016

* Normalize paths to source files from projects: #94.

#### 0.27.3 - 08.02.2016

* Set MinThreads to avoid deadlocks on Mono < 4.2.2: #92.

#### 0.27.2 - 05.02.2016

* Upgrade to FCS 2.0.0.4 to fix project cracking with spaces in paths: #91.

#### 0.27.1 - 26.01.2016

* Upgrade to FCS 2.0.0.3 to fix VS2015 project cracking: #89.

#### 0.27.0 - 08.12.2015

* Upgrade to FCS 2.0.0.0-beta and add project cracking verbosity option: #86.
* Add FSharpLint support: #83.

#### 0.26.1 - 23.10.2015

* Switch to depend on FSharp.Core 4.3.1.0: #81.
* Don't output a BOM to standard out: #82

#### 0.26.0 - 20.10.2015

* Fix for uncompiled referenced projects: #78.
* Backwards-incompatible: Framework no longer returned in `project` response.

#### 0.25.1 - 16.10.2015

* Add App.config to FsAutoComplete.Suave release: #76.
* Also for fsautocomplete.exe.

#### 0.25.0 - 14.10.2015

* Add Suave hosting for FSAC: #74.
* Backwards-incompatible: return GlyphName rather than code in
  Declarations message: #75.

#### 0.24.1 - 11.09.2015

* Fix StackOverflowException and encoding issue: #70.

#### 0.24.0 - 04.09.2015

* Backwards-incompatible: do not format help text, leave that to the
  client, which allows the display to be more semantic. #63 (due to
  @Krzysztof-Cieslak)

#### 0.23.1 - 02.09.2015

* Fix MSBuild v14 support on non-English systems by avoiding
  attempting to load *.resources.dll (patch from @ryun).

#### 0.23.0 - 14.08.2015

* Add a new `colorizations <true|false>` command to enable/disable asynchronous
  provision of colorization information following a parse: #60 (Fixes #44).
* Newest FSharp.Core is used for type-checking scripts and for projects that
  do not reference FSharp.Core. Supports F# 3.0, 3.1 and 4.0: #59.
* If MSBuild v12 is not available, instead try load MSBuild v14. This, together
  with the previous point, adds support for VS2015-only Windows installs: #57.
  Fixes: #12 #21 #23 #25 #54.
* Backwards-incompatible: `compilerlocation` command has changed. Now provides
  path to best version of msbuild, fsc and fsi on Windows: #23.

#### 0.22.0 - 06.08.2015

* Backwards-incompatible: Symbol use command now includes FileName rather than Filename

#### 0.21.1 - 06.08.2015

* Reduce timeout message from 'error' to 'info'

#### 0.21.0 - 04.08.2015

* Update to FCS 1.4.X (support for F# 4.0): #52
* Automatically reparse F# project files if they are changed on disk: #47

#### 0.20.1 - 30.07.2015

* Fix exception in `symboluse` command: #46. 

#### 0.20.0 - 28.07.2015

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

#### 0.19.0 - 30.06.2015

* Add symboluse command - https://github.com/fsharp/FsAutoComplete/pull/34
  Breaking change: all columns returned are now 1-based. Format of error
  locations has also changed to be more consistent with other formats.
* Add param completion command - https://github.com/fsharp/FsAutoComplete/pull/30

#### 0.18.2 - 13.06.2015

* Update to FCS 0.0.90 (fix referencing PCL projects) - https://github.com/fsharp/FsAutoComplete/pull/26

#### 0.18.1 - 09.06.2015

* Prevent test assemblies from being included in release archives by
  avoiding forcing the output directory.

#### 0.18.0 - 03.06.2015

* Adjust for 1-based column indexing - https://github.com/fsharp/FSharp.AutoComplete/pull/13
  Note that this was previously the intended behaviour, but column
  indexes were treated as 0-based. Ensure that both line and column
  indexes sent in commands are 1-based.
  
#### 0.17.0 - 31.05.2015

* Completion filtering - https://github.com/fsharp/FSharp.AutoComplete/pull/10

#### 0.16.0 - 28.05.2015

* Implement multiple unsaved file checking - https://github.com/fsharp/FSharp.AutoComplete/pull/8

#### 0.15.0 - 20.05.2015

* Add Glyphs to completion responses - https://github.com/fsharp/FSharp.AutoComplete/pull/1
