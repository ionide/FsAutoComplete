namespace FsAutoComplete
open NUnit.Framework

[<TestFixture>]
module CommandParsing = 
    open FsAutoComplete.CommandInput
    open Parser

    type ``parsing tests``() = 
        
        [<Test>]
        member x.``parse format no config``() =  
            let (Command.Format(name, conf)) = apply format  "format \"lol.fs\"" |> List.head
            Assert.AreEqual("lol.fs", name)
            Assert.IsTrue(Types.FormatConfig.Default = conf)

        [<Test>]
        member x.``parse format config``() =  
            let (Command.Format(name, conf)) = apply format  "format \"lol.fs\" config spaceindent 5" |> List.head
            Assert.AreEqual("lol.fs", name)
            Assert.IsTrue({ Types.FormatConfig.Default with IndentSpaceNum = 5 } = conf)

        [<Test>]
        member x.``parse format range``() =
            let (Command.FormatSelection(name, range, conf)) = apply formatSelection "format \"lol.fs\" range 2:1-3:19" |> List.head
            Assert.AreEqual("lol.fs", name)
            Assert.IsTrue(Types.Range.Create(2,1,3,19) = range)
            Assert.IsTrue(Types.FormatConfig.Default = conf)

        

