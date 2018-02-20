#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "workspacepeek.json"

let p = new FsAutoCompleteWrapper()

p.workspacepeek __SOURCE_DIRECTORY__ 2

p.quit()

p.finalOutput ()
|> writeNormalizedOutput "workspacepeek.json"

