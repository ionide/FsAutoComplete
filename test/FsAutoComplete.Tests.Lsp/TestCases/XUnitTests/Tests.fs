module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)

module Inner =
    [<Fact>]
    let ``Other test`` () =
        Assert.True(true)

type NameClash () =
    do ()

module NameClash =
    [<Fact>]
    let ``Clashing test`` () =
        Assert.True(true)