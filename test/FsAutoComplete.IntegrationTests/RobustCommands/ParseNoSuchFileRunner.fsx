#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "parsenosuchfile.json"

let p = new FsAutoCompleteWrapper()

p.project "Project/Test1.fsproj"
p.send "parse \"NoSuchFile.fs\"\nBla bla bla\n<<EOF>>\n"
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "parsenosuchfile.json"

