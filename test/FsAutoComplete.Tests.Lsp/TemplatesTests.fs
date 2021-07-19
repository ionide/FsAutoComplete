module FsAutoComplete.Tests.Templates

open Expecto

let tests() =
  testList "Templates Tests" [
    testCase "Templates are not empty" <| fun () ->
      FsAutoComplete.DotnetNewTemplate.installedTemplates()
      |> Expect.isNonEmpty <| "Templates are empty"
    testCase "Detailed templates are not empty" <| fun () ->
      FsAutoComplete.DotnetNewTemplate.templateDetails()
      |> Expect.isNonEmpty <| "Detailed templates are empty"
  ]
