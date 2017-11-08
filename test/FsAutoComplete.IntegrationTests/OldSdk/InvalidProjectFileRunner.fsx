#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

let normalizedOutputRemoveToolsList s =
  // FCS v17 added a different output, with the list of avaiable tools directories who is not
  // normalized by test suite default function
  // like:
  //  Available tools: ["C:/Program Files (x86)/MSBuild/12.0/bin";\n "C:/Program Files (x86)/MSBuild/14.0/bin";\n "C:/WINDOWS/Microsoft.NET/Framework/v2.0.50727";\n "C:/WINDOWS/Microsoft.NET/Framework/v3.5";\n "C:/Windows/Microsoft.NET/Framework/v4.0.30319"]. Message
  System.Text.RegularExpressions.Regex.Replace(s,
              """\\\"(.|\n|\r)*\][\.Message]""",
              "<absolute path lists>]")

let invalidprojectfileJson = outputJsonForRuntime "invalidprojectfile.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete invalidprojectfileJson

let doIt () =
  let p = new FsAutoCompleteWrapper()

  p.project "sample3/l1/Test1.fsproj"
  p.parse "sample3/l1/Module1.fs"

  p.send "quit\n"
  p.finalOutput ()
  |> writeNormalizedOutputWith normalizedOutputRemoveToolsList invalidprojectfileJson

doIt ()
