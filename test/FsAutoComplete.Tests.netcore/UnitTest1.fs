namespace FsAutoComplete.Tests.Tests

open NUnit.Framework
open FsAutoComplete
open FsAutoComplete.Parser

[<TestClass>]
type TestClass () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Test1 () =
        let input = "project \"sample1/c1/c1.fsproj\""
        let reader = FsAutoComplete.Parsing.createForwardStringReader input 0
        let cmds = CommandInput.project <|> CommandInput.error
        let cmd = reader |> Parsing.getFirst cmds
        Assert.IsTrue((cmd = Project("sample1/c1/c1.fsproj", false)), sprintf "but was %A" cmd)

    [<Test>]
    member this.Test2 () =
        let input = "project \"sample2/c2/c2.fsproj\""
        let cmd = CommandInput.parseCommand input
        Assert.IsTrue((cmd = Project("sample2/c2/c2.fsproj", false)), sprintf "but was %A" cmd)
