namespace FsAutoComplete.Tests.Tests

open NUnit.Framework
open FsAutoComplete

[<TestFixture>]
type Tests () =

    [<SetUp>]
    member this.Setup () =
        ()


    [<Test>]
    member this. ``activePatternCracking`` () =
      let lineStr = "let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n-1)"
      match Lexer.findLongIdents(8, lineStr) with
      | None -> Assert.Fail()
      | Some (col, _idents) ->

      Assert.AreEqual(16,col)
      //Assert.AreEqual(["|Zero|Succ|"],idents)
      // Actually returns "Zero|Succ|", but this is fine for tooltips
      Assert.AreEqual(lineStr.[col], ')')


    [<Test>]
    member this.``symbolicOperatorCracking`` () =
      match Lexer.findLongIdents(0, "|<>|") with
      | None -> Assert.Fail()
      | Some (col, idents) ->

       Assert.AreEqual(4, col)
       Assert.AreEqual(["|<>|"], idents)
       Assert.AreEqual("|<>|".[col-1], '|')

    [<Test>]
    member this.``normalCracking`` () =
      match Lexer.findLongIdents(15, "First.Second.Third") with
      | None -> Assert.Fail()
      | Some (col, idents) ->
         Assert.AreEqual(18, col)
         CollectionAssert.AreEqual (["Third"], idents, "idents")

    [<Test>]
    [<Ignore("need to make this platform independent")>]
    member this.``should find fsc on Windows`` () =
        match Environment.fsc with
        | None ->
          Assert.Fail("should find fsc.exe")
        | Some fsc ->
          Assert.That(fsc.Contains("Microsoft SDKs"), "fsc.exe resolution failed")

    [<Test>]
    [<Ignore("Need to make this platform independent")>]
    member this.``should find msbuild on Windows`` () =
      if not Utils.runningOnMono then
        match Environment.msbuild with
        | Some msbuild ->
          Assert.That(msbuild.Length > ("MSBuild.exe".Length), "MSBuild.exe resolution failed")
        | None ->
          ()
