#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

let completenosuchfileJson = "completenosuchfile.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete completenosuchfileJson

let p = new FsAutoCompleteWrapper()

p.project "Project/Test1.fsproj"
p.completion "NoSuchFile.fs" "whatever" 1 1
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput completenosuchfileJson

