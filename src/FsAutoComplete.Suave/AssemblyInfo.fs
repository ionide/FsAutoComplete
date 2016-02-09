namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsAutoComplete.Suave")>]
[<assembly: AssemblyProductAttribute("FsAutoComplete.Suave")>]
[<assembly: AssemblyDescriptionAttribute("A Suave web server for interfacing with FSharp.Compiler.Service over a HTTP.")>]
[<assembly: AssemblyVersionAttribute("0.27.3")>]
[<assembly: AssemblyFileVersionAttribute("0.27.3")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.27.3"
