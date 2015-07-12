#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.project "Proj1/Proj1.fsproj"
p.parse "Proj1/Ops.fs"
p.parse "Proj1/Program.fs"
p.completion "Proj1/Program.fs" 8 20
p.completion "Proj1/Program.fs" 4 23
p.completion "Proj1/Program.fs" 6 14
p.completion "Proj1/Program.fs" 10 20

p.project "Proj2/Proj2.fsproj"
p.parse "Proj2/Core.fs"
p.parse "Proj2/Program.fs"
p.completion "Proj2/Program.fs" 8 20
p.completion "Proj2/Program.fs" 4 23
p.completion "Proj2/Program.fs" 6 14
p.completion "Proj2/Program.fs" 10 20

p.parse "Proj1/Ops.fs"
p.parse "Proj1/Program.fs"
p.completion "Proj1/Program.fs" 8 20
p.completion "Proj1/Program.fs" 4 23
p.completion "Proj1/Program.fs" 6 14
p.completion "Proj1/Program.fs" 10 20

p.parse "Proj2/Core.fs"
p.parse "Proj2/Program.fs"
p.completion "Proj2/Program.fs" 8 20
p.completion "Proj2/Program.fs" 4 23
p.completion "Proj2/Program.fs" 6 14
p.completion "Proj2/Program.fs" 10 20

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

