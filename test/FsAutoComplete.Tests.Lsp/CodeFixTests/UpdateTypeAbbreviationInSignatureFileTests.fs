module private FsAutoComplete.Tests.CodeFixTests.UpdateTypeAbbreviationInSignatureFileTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof UpdateTypeAbbreviationInSignatureFile) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle UpdateTypeAbbreviationInSignatureFile.title

      ftestCaseAsync "first unit test for UpdateTypeAbbreviationInSignatureFile"
      <| CodeFix.check
        server
        "let a$0 b c = ()"
        Diagnostics.acceptAll
        selectCodeFix
        "let Text replaced by UpdateTypeAbbreviationInSignatureFile b c = ()"
    ])
