#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

let outputJson = "output.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete outputJson

let p = new FsAutoCompleteWrapper()


p.project "Test1.fsproj"
p.parse "Program.fs"
p.parse "Script.fsx"
p.completion "Program.fs" "let val2 = X.func 2" 6 14
p.completion "Script.fsx" "let val2 = X.func 2" 6 14
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput outputJson

