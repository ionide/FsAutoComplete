module Tests

open NUnit.Framework

[<Test>]
let Add () =
    Assert.AreEqual(3, Library.add 1 2)
