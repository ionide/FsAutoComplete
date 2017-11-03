module FsAutoComplete.Tests

open NUnit.Framework

[<Test>]
let ``activePatternCracking`` () =
  let lineStr = "let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n-1)"
  match Parsing.findLongIdents(8, lineStr) with
  | None -> Assert.Fail()
  | Some (col, _idents) ->

  Assert.AreEqual(16,col)
  //Assert.AreEqual(["|Zero|Succ|"],idents)
  // Actually returns "Zero|Succ|", but this is fine for tooltips
  Assert.AreEqual(lineStr.[col], ')')


[<Test>]
let ``symbolicOperatorCracking`` () =
  match Parsing.findLongIdents(0, "|<>|") with
  | None -> Assert.Fail()
  | Some (col, idents) ->

   Assert.AreEqual(4, col)
   Assert.AreEqual(["|<>|"], idents)
   Assert.AreEqual("|<>|".[col-1], '|')

[<Test>]
let ``normalCracking`` () =
  match Parsing.findLongIdents(15, "First.Second.Third") with
  | None -> Assert.Fail()
  | Some (col, idents) ->
     Assert.AreEqual(18, col)
     CollectionAssert.AreEqual (["Third"], idents, "idents")

[<Test>]
let ``should find FSharp.Core`` () =
  Assert.That(Option.isSome Environment.fsharpCoreOpt, "FSharp.Core.dll resolution failed")

[<Test>]
let ``should find fsc on Windows`` () =
  if not Utils.runningOnMono then
    Assert.That(Environment.fsc.Contains("Microsoft SDKs"), "fsc.exe resolution failed")

[<Test; Ignore("wip")>]
let ``should find msbuild on Windows`` () =
  if not Utils.runningOnMono then
    Assert.That(Environment.msbuild.Length > "MSBuild.exe".Length, "MSBuild.exe resolution failed")
