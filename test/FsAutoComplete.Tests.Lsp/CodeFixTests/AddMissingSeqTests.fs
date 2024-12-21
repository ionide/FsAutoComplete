module private FsAutoComplete.Tests.CodeFixTests.AddMissingSeqTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof AddMissingSeq) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddMissingSeq.title

      ftestCaseAsync "first unit test for AddMissingSeq"
      <| CodeFix.check
        server
        "$0{ 1;10 }"
        Diagnostics.acceptAll
        selectCodeFix
        "seq { 1;10 }"
    ])
