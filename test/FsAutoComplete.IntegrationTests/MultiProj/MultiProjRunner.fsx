#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

let outputJson = "output.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete outputJson

let p = new FsAutoCompleteWrapper()

p.project "Proj1/Proj1.fsproj"
p.parse "Proj1/Ops.fs"
p.parse "Proj1/Program.fs"
p.completionFilter "Proj1/Program.fs" "let val3 = testval.Terrific val2" 8 21 "StartsWith"
p.completion "Proj1/Program.fs" "let testval = FileTwo.NewObjectType()" 4 23
p.completion "Proj1/Program.fs" "let val2 = X.func 2" 6 14
p.completion "Proj1/Program.fs" "let val4 : FileTwo.NewObjectType = testval" 10 20

p.project "Proj2/Proj2.fsproj"
p.parse "Proj2/Core.fs"
p.parse "Proj2/Program.fs"
p.completionFilter "Proj2/Program.fs" "let val3 = testval.Terrifying val2" 8 21 "StartsWith"
p.completion "Proj2/Program.fs" "let testval = FileTwo.NewObjectType()" 4 23
p.completion "Proj2/Program.fs" "let val2 = X.func 2" 6 14
p.completion "Proj2/Program.fs" "let val4 : FileTwo.NewObjectType = testval" 10 20

p.parse "Proj1/Ops.fs"
p.parse "Proj1/Program.fs"
p.completionFilter "Proj1/Program.fs" "let val3 = testval.Terrific val2" 8 21 "StartsWith"
p.completion "Proj1/Program.fs" "let testval = FileTwo.NewObjectType()" 4 23
p.completion "Proj1/Program.fs" "let val2 = X.func 2" 6 14
p.completion "Proj1/Program.fs" "let val4 : FileTwo.NewObjectType = testval" 10 20

p.parse "Proj2/Core.fs"
p.parse "Proj2/Program.fs"
p.completionFilter "Proj2/Program.fs" "let val3 = testval.Terrific val2" 8 21 "StartsWith"
p.completion "Proj2/Program.fs" "let testval = FileTwo.NewObjectType()" 4 23
p.completion "Proj2/Program.fs" "let val2 = X.func 2" 6 14
p.completion "Proj2/Program.fs" "let val4 : FileTwo.NewObjectType = testval" 10 20

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput outputJson

