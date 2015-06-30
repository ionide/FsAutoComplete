#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "completewithoutproject.txt"

let p = new FsAutoCompleteWrapper()

p.parse "Project/Script.fsx"
p.completion "Project/Script.fsx" 6 14
p.parse "Project/Program.fs"
p.completion "Project/Program.fs" 4 23
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "completewithoutproject.txt"

