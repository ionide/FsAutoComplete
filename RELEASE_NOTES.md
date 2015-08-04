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
