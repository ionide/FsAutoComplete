#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

Diagnostics.Process.Start("dotnet", "restore sample1/c1")

let p = new FsAutoCompleteWrapper()

p.project "sample1/c1/c1.fsproj"
p.parse "sample1/c1/Program.fs"

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

