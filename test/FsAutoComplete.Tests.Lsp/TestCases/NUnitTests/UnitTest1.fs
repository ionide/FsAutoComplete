module NUnitTests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.Pass()

module Outer =
    module Inner =
        [<TestCase (0)>]
        let Test2 (i: int) =
            Assert.Pass()
