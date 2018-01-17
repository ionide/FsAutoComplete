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

p.project "FindDecl.fsproj"
p.parse "FileTwo.fs"
p.parse "Script.fsx"
p.parse "Program.fs"
p.finddeclaration "Program.fs" "let val2 = X.func 2" 6 16
p.finddeclaration "Program.fs" "let val3 = testval.Terrific val2" 8 20
p.finddeclaration "Program.fs" "    printfn \\\"Hello %d\\\" val2" 24 26
p.finddeclaration "Program.fs" "let val4 : FileTwo.NewObjectType = testval" 10 20
p.finddeclaration "Program.fs" "let val5 = ClassLib.ExternalType.SimpleMethod()" 12 24
p.finddeclaration "Program.fs" "let val5 = ClassLib.ExternalType.SimpleMethod()" 12 36
p.finddeclaration "Program.fs" "let val6 = ClassLib.ExternalType.SimpleMethod(\\\"a\\\", \\\"b\\\")" 14 38
p.finddeclaration "Program.fs" "let val7 = ClassLib.ExternalType.RefParameter(ref lst)" 18 38
p.finddeclaration "Program.fs" "let val8 = ClassLib.ExternalType.Generic(System.Collections.Generic.Dictionary<string, string>())" 20 38

p.finddeclaration "Script.fsx" "let val99 = XA.funky 21" 6 17
p.quit()
p.finalOutput ()
|> writeNormalizedOutput "output.json"
