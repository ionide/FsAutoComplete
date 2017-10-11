#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

(*
 * This test is a simple sanity check of a basic run of the program.
 * A few completions, files and script.
 *)

let outputJson = outputJsonForRuntime "output.json"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete outputJson

let p = new FsAutoCompleteWrapper()

p.project "Test1.fsproj"
p.waitForLine()
Threading.Thread.Sleep 2000
File.WriteAllBytes("Test1.fsproj", File.ReadAllBytes "Test1.fsproj")
Threading.Thread.Sleep 2000
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput outputJson

