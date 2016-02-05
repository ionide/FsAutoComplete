namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsAutoComplete")>]
[<assembly: AssemblyProductAttribute("FsAutoComplete")>]
[<assembly: AssemblyDescriptionAttribute("A command line tool for interfacing with FSharp.Compiler.Service over a pipe.")>]
[<assembly: AssemblyVersionAttribute("0.27.2")>]
[<assembly: AssemblyFileVersionAttribute("0.27.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.27.2"
