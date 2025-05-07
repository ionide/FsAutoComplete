module Tests

open Expecto
open FsAutoComplete.VSTestAdapter

[<Tests>]
let tests =
  testList "Test Discovery" [
    testCase "No projects, no tests" <| fun _ ->
      let expected = []
      let actual = VSTestWrapper.discoverTests [] 
      Expect.equal actual expected ""
  ]
