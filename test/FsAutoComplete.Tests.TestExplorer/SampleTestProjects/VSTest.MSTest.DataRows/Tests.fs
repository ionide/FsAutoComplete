namespace Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

/// MSTest reports every data row under one fully-qualified name, so only the adapter's test
/// case id tells the rows apart. Row 2 fails, so a run of one row shows which row ran.
[<TestClass>]
type DataRows() =

  [<TestMethod>]
  [<DataRow(1)>]
  [<DataRow(2)>]
  [<DataRow(3)>]
  member _.RowTwoFails(x: int) = Assert.AreNotEqual(2, x)
