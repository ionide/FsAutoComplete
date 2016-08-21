#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.parse "Script.fsx"
p.lint "Script.fsx"
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"
