#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "completewithoutproject.json"

let p = new FsAutoCompleteWrapper()

p.parse "Project/Script.fsx"
p.completion "Project/Script.fsx" "let val2 = X.func 2" 6 14
p.parse "Project/Program.fs"
p.completion "Project/Program.fs" "let testval = FileTwo.NewObjectType()" 4 23
p.quit()
p.finalOutput ()
|> writeNormalizedOutput "completewithoutproject.json"

