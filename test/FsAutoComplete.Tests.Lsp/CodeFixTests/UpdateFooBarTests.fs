module private FsAutoComplete.Tests.CodeFixTests.UpdateFooBarTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof UpdateFooBar) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle UpdateFooBar.title

      ftestCaseAsync "first unit test for UpdateFooBar"
      <| CodeFix.check
        server
        "let a$0 b c = ()"
        Diagnostics.acceptAll
        selectCodeFix
        "let Text replaced by UpdateFooBar b c = ()"
    ])
