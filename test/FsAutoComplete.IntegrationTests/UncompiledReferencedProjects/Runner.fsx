#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

let outputJson = outputJsonForRuntime "output.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete outputJson

let p = new FsAutoCompleteWrapper()
 
p.project "MultiProject1.fsproj"
p.parse "MultiProject1.fs"
p.tooltip "MultiProject1.fs" "let p = (Project1A.x1, Project1B.b)" 5 20
p.tooltip "MultiProject1.fs" "let p = (Project1A.x1, Project1B.b)" 5 34

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput outputJson

