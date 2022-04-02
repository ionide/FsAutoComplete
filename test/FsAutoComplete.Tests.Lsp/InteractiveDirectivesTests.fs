module FsAutoComplete.Tests.InteractiveDirectivesTests

open Expecto
open FsAutoComplete.InteractiveDirectives

let interactiveDirectivesUnitTests =
  testList
    "interactive directives unit tests"
    [

      testList
        "tryParseLoad"
        [ testCase "happy cases"
          <| fun () ->
               let cases =
                 [ "#load \"script\"", 0, "script"
                   "#load \"script\"", 2, "script"
                   "#load  \"script\" ", 0, "script"
                   "#load @\"script\"", 0, "script"
                   "#load @\"path/to\\script\"", 0, "path/to\\script"
                   "#load \"\"\"script\"\"\"", 0, "script"
                   "#load \"script.fsx\"", 0, "script.fsx" ]

               Expect.all
                 cases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "all are valid cases"

          testCase "invalid directives"
          <| fun () ->
               let cases =
                 [ "load \"script\"", 0
                   "##load \"script\"", 0
                   "#loda \"script\"", 0
                   "#LOAD \"script\"", 0
                   "  #load \"script\"", 1
                   "#load \"script", 0 ]

               Expect.all
                 cases
                 (fun (line, index) ->
                   let actual = tryParseLoad line index
                   actual = None)
                 "all are invalid cases" ] ]
