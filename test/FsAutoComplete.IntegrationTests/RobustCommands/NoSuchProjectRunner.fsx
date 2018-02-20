#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "nosuchproject.json"

let p = new FsAutoCompleteWrapper()

p.project "NoSuchProject.fsproj"
p.quit()
p.finalOutput ()
|> writeNormalizedOutput "nosuchproject.json"

