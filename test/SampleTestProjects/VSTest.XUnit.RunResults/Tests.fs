module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
    System.Console.WriteLine("Where do I show up in the results")
    Assert.True(true)

[<Fact>]
let ``Fails`` () =
    Assert.True(false)

[<Fact(Skip= "Skip me")>]
let ``Skipped`` () =
    Assert.True(true)

[<Fact>]
let ``Exception`` () : unit = 
    failwith "Report as an exception"
