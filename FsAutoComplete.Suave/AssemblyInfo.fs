namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsAutoComplete.Suave")>]
[<assembly: AssemblyProductAttribute("FsAutoComplete.Suave")>]
[<assembly: AssemblyDescriptionAttribute("A command line tool for interfacing with FSharp.Compiler.Service over a pipe.")>]
[<assembly: AssemblyVersionAttribute("0.24.1")>]
[<assembly: AssemblyFileVersionAttribute("0.24.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.24.1"
