#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.project "Test1.fsproj"
p.parse "Program.fs"
p.completion "Program.fs" 6 14
p.parse "Script.fsx"
p.completion "Script.fsx" 6 14
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

