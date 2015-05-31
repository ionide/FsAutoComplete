#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.txt"

let p = new FSharpAutoCompleteWrapper()

p.project "Test1.fsproj"
p.parse "Program.fs"
p.completion "Program.fs" 6 14
p.parse "Script.fsx"
p.completion "Script.fsx" 6 14
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.txt"

