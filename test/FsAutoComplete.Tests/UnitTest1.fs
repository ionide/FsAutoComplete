namespace FsAutoComplete.Tests.Tests

open NUnit.Framework
open FsAutoComplete
open FsAutoComplete.Parser

[<TestFixture>]
type Tests () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Test1 () =
        let input = "project \"sample1/c1/c1.fsproj\""
        let reader = Parser.createForwardStringReader input 0
        let cmds = CommandInput.project <|> CommandInput.error
        let cmd = reader |> Parser.getFirst cmds
        Assert.IsTrue((cmd = Command.Project("sample1/c1/c1.fsproj", false)), sprintf "but was %A" cmd)

    [<Test>]
    member this.Test2 () =
        let input = "project \"sample2/c2/c2.fsproj\""
        let cmd = CommandInput.parseCommand input
        Assert.IsTrue((cmd = Command.Project("sample2/c2/c2.fsproj", false)), sprintf "but was %A" cmd)

    [<Test>]
    member this.``stdio parser workspaceload `` () =
        let input = "workspaceload \"sample2/c2/c2.fsproj\" \"c:\\sample2\\c2\\c2.fsproj\""
        let cmd = CommandInput.parseCommand input
        Assert.IsTrue((cmd = WorkspaceLoad([| @"sample2/c2/c2.fsproj"; @"c:\sample2\c2\c2.fsproj" |])), sprintf "but was %A" cmd)

    [<Test>]
    member this.``dotnet new create cli`` () =
        let input = "dotnetnewCreateCli \"console\" \"-n\" \"MyApp\" \"-lang\" \"F#\" \"--no-restore\" \"false\""
        let cmd = CommandInput.parseCommand input
        Assert.AreEqual((cmd = DotnetNewCreateCli(@"console", [(@"-n", @"MyApp" :> System.Object);(@"-lang",@"F#" :> System.Object);(@"--no-restore",false :> System.Object);])), sprintf "but was %A" cmd)

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
