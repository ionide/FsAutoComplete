#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

let includeFile = __SOURCE_DIRECTORY__ + @"..\..\..\..\.paket\load\net45\IntegrationTests\Http.fs.fsx"
printfn "File '%s' content (begin)" includeFile
File.ReadAllLines(includeFile)
|> Array.iter (printfn "%s")
printfn "File content (end)"


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.parse "Script.fsx"
p.send "colorizations true\n"
p.parse "Script.fsx"
p.send "colorizations false\n"
p.parse "Script.fsx"
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"

