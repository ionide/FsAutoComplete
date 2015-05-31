#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "nosuchproject.txt"

let p = new FSharpAutoCompleteWrapper()

p.project "NoSuchProject.fsproj"
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "nosuchproject.txt"

