module Tests

open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
let ``My theory`` (n: int) = Assert.True(n > 0)

[<Fact>]
let ``Fails`` () = Assert.True(false)

[<Fact(Skip = "Skip me")>]
let ``Skipped`` () = Assert.True(true)

[<Fact>]
let ``Writes to stdout`` () =
  System.Console.WriteLine("Where do I show up in the results")
  Assert.True(true)
