module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``Fails`` () =
    Assert.True(false)

[<Fact(Skip= "Skip me")>]
let ``Skipped`` () =
    Assert.True(true)

[<Fact>]
let ``Exception`` () = 
    failwith "Report as an exception"
