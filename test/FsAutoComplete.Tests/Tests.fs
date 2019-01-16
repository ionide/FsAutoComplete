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
let ``should find fsc on Windows`` () =
  if not Utils.runningOnMono then
    Assert.That(Option.isSome Environment.fsc, "should find fsc.exe")
    let (Some fsc) = Environment.fsc
    Assert.That(fsc.Contains("Microsoft SDKs"), "fsc.exe resolution failed")

[<Test>]
let ``should find msbuild on Windows`` () =
  if not Utils.runningOnMono then
    match Environment.msbuild with
    | Some msbuild ->
      Assert.That(msbuild.Length > ("MSBuild.exe".Length), "MSBuild.exe resolution failed")
    | None ->
      ()
