#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

let completebadpositionJson = outputJsonForRuntime "completebadposition.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete completebadpositionJson

let p = new FsAutoCompleteWrapper()

p.project "Project/Test1.fsproj"
p.parse "Project/Program.fs"
p.completion "Project/Program.fs" "whatever" 50 1
p.completion "Project/Program.fs" "module X =" 1 100
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput completebadpositionJson

