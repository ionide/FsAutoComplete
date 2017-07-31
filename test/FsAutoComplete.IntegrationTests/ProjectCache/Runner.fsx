#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

(*
 * This test is a simple sanity check of a basic run of the program.
 * A few completions, files and script.
 *)


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let cacheFile = Path.Combine("obj", "fsac.cache")
File.Delete "output.json"
if File.Exists cacheFile then File.Delete cacheFile


let p = new FsAutoCompleteWrapper()

p.project "Test1.fsproj"
p.waitForLine()
Threading.Thread.Sleep 2000
let msg1, time1 = if File.Exists cacheFile then "cache exists\n", File.GetLastWriteTimeUtc cacheFile else "cache doesn't exist\n", DateTime.MinValue

File.WriteAllBytes("Test1.fsproj", File.ReadAllBytes "Test1.fsproj")
Threading.Thread.Sleep 2000
let time2 = if File.Exists cacheFile then File.GetLastWriteTimeUtc cacheFile else DateTime.MinValue
let msg2 = if time1 < time2 then "cache updated" else "cache not updated"

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"
File.AppendAllText("output.json", msg1)
File.AppendAllText("output.json", msg2)
