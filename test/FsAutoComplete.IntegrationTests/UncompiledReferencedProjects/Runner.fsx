#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()
 
p.project "MultiProject1.fsproj"
p.parse "MultiProject1.fs"
p.tooltip "MultiProject1.fs" 5 20
p.tooltip "MultiProject1.fs" 5 34

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

