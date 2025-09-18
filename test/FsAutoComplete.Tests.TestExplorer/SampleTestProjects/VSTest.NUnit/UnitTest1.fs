module VSTest.NUnit

open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () = Assert.Pass()

[<Test>]
let Test2 () = Assert.Pass()
