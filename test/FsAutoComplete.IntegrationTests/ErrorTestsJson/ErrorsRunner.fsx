#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()


p.project "Test1.fsproj"
p.parse "Program.fs"
p.parse "Script.fsx"
p.completion "Program.fs" "let val2 = X.func 2" 6 14
p.completion "Script.fsx" "let val2 = X.func 2" 6 14
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

