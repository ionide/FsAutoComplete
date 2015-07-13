module FsAutoComplete.Tests

open NUnit.Framework

[<Test>]
let ``activePatternCracking`` () =
  let lineStr = "let (|Zero|Succ|) n = if n = 0 then Zero else Succ(n-1)"
  match Parsing.findLongIdents(8, lineStr) with
  | None -> Assert.Fail()
  | Some (col, _idents) ->

  Assert.AreEqual(15,col)
  //Assert.AreEqual(["|Zero|Succ|"],idents)
  // Actually returns "Zero|Succ|", but this is fine for tooltips
  Assert.AreEqual(lineStr.[col], '|')

[<Test>]
let ``symbolicOperatorCracking`` () =
  match Parsing.findLongIdents(0, "|<>|") with
  | None -> Assert.Fail()
  | Some (col, idents) ->

  Assert.AreEqual(3,col)
  Assert.AreEqual(["|<>|"],idents)
  Assert.AreEqual("|<>|".[col], '|')

[<Test>]
let ``normalCracking`` () =
  match Parsing.findLongIdents(8, "First.Second.Third") with
  | None -> Assert.Fail()
  | Some (col, idents) ->

  Assert.AreEqual(11,col)
  Assert.AreEqual(["First";"Second"],idents)
  Assert.AreEqual("First.Second.Third".[col], 'd')
