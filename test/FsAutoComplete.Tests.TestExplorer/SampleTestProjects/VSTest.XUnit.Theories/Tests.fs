module Tests

open Xunit

/// xUnit reports every row of a theory under one fully-qualified name, so only the adapter's
/// test case id tells the rows apart. Row 2 fails, so a run of one row shows which row ran.
[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
[<InlineData(3)>]
let ``Row two fails`` (x: int) = Assert.NotEqual(2, x)
