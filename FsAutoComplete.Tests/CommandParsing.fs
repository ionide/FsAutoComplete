namespace FsAutoComplete
open NUnit.Framework

[<TestFixture>]
module CommandParsing = 
    open FsAutoComplete.CommandInput
    open Parser

    type ``parsing tests``() = 
        
        let run p s = match apply p s with | h::_ -> Some h | [] -> None

        [<Test>]
        member x.``parse format no config``() =  
            match run format "format file \"lol.fs\"" with
            | Some (Command.Format(name, conf)) -> 
                Assert.AreEqual("lol.fs", name)
                Assert.IsTrue(Types.FormatConfig.Default = conf)
            | _ -> Assert.Fail("could not parse command")

        [<Test>]
        member x.``parse format config``() =  
            match run format "format file \"lol.fs\" config spaceindent 5" with
            | Some (Command.Format(name, conf)) -> 
                Assert.AreEqual("lol.fs", name)
                Assert.IsTrue({Types.FormatConfig.Default with IndentSpaceNum = 5} = conf)
            | _ -> Assert.Fail("could not parse command")

        [<Test>]
        member x.``parse format range``() =
            match run formatSelection "formatselection file \"lol.fs\" range 2:1-3:19 config spaceindent 5" with
            | Some (Command.FormatSelection(name, range, conf)) -> 
                Assert.AreEqual("lol.fs", name)
                Assert.IsTrue(Types.Range.Create(2,1,3,19) = range)
                Assert.IsTrue({ Types.FormatConfig.Default with IndentSpaceNum = 5} = conf)
            | _ -> Assert.Fail("could not parse command")

        

