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
p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput outputJson

