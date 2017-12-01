#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.parse "Script.fsx"
p.send "colorizations true\n"
p.parse "Script.fsx"
p.send "colorizations false\n"
p.parse "Script.fsx"
p.quit()
p.finalOutput ()
|> writeNormalizedOutput "output.json"

