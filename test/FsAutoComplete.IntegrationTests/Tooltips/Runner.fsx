#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System

(*
 * This test is a simple sanity check of a basic run of the program.
 * A few completions, files and script.
 *)

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.parse "Script.fsx"
p.tooltip "Script.fsx" "  let funky x = x + 1" 4 10
p.tooltip "Script.fsx" "module CommandResponse =" 8 19
p.tooltip "Script.fsx" "  type ResponseMsg<'T> =" 10 14
p.tooltip "Script.fsx" "  let funky x = x + 1" 4 10
p.tooltip "Script.fsx" "let funct (x : CommandResponse.ResponseMsg<_>) = ()" 19 6
// p.tooltip "Script.fsx" "let x = System.DateTime(123L)" 21 21
p.tooltip "Script.fsx" "type Abcd () =" 24 8
p.tooltip "Script.fsx" "  member __.asdfg = 123" 26 15
p.tooltip "Script.fsx" "  member val bnm = 1 with get, set" 27 15
p.tooltip "Script.fsx" "  member __.cvbnm" 28 15
p.tooltip "Script.fsx" "type Desf =" 35 8
// p.tooltip "Script.fsx" "  member __.b = 234" 36 5
p.tooltip "Script.fsx" "type Qwer =" 38 8
p.tooltip "Script.fsx" "type Gjk =" 43 8
p.tooltip "Script.fsx" "type Gjk =" 43 2
p.tooltip "Script.fsx" "let (>=>) a b = a + b" 48 7
p.tooltip "Script.fsx" "let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd" 50 7
p.tooltip "Script.fsx" "   | Even -> 1" 54 8
p.tooltip "Script.fsx" "let _  = [1..3] |> List.map ((+) 1)" 60 27
// p.tooltip "Script.fsx" "let tasd = System.String.IsNullOrEmpty" 57 23
// p.tooltip "Script.fsx" "let cbfdg = Array.append" 58 15


p.quit()
p.finalOutput ()
|> writeNormalizedOutput "output.json"

