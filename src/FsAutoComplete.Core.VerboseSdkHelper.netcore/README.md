# Why this is needed?

Both `Dotnet.ProjInfo` and `Fsharp.Compiler.Service` define an `TaskItem` class and others interfaces, to redefine some msbuild classes (in `FCS` is done under `FX_RESHAPED_MSBUILD` compiler define)

That make not possibile to build `Fsc.fs` (the `Fsc` task) as is inside `FsAutoComplete.Core`, because compiler may pick the wrong `TaskItem` class.
Reuse `Fsc.fs` as is, is important for maintanability, so can be aligned to `FCS` as needed.

For example, if to `fsc` invocation is passed first the reference to `FSharp.Compiler.Service.dll` and after `Dotnet.ProjInfo.Helpers.dll`, than works. The inverse not. So is not deterministic.
