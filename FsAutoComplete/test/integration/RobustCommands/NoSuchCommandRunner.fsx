#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "nosuchcommand.txt"

let p = new FsAutoCompleteWrapper()

p.send "BadCommand\n"
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "nosuchcommand.txt"

