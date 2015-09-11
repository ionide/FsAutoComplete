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
