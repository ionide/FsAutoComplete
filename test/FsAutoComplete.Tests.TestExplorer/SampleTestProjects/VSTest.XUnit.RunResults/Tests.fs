module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
  System.Console.WriteLine("Where do I show up in the results")
  Assert.True(true)

[<Fact>]
let ``Fails`` () = Assert.True(false)

[<Fact(Skip = "Skip me")>]
let ``Skipped`` () = Assert.True(true)

[<Fact>]
let ``Exception`` () : unit = failwith "Report as an exception"

[<Fact>]
let ``Expects environment variable`` () : unit =
  Assert.Equal("Set me", Environment.GetEnvironmentVariable("dd586685-08f6-410c-a9f1-84530af117ab"))


module Nested =
  [<Fact>]
  let ``Test 1`` () : unit = ()

  [<Fact>]
  let ``Test 2`` () : unit = ()
