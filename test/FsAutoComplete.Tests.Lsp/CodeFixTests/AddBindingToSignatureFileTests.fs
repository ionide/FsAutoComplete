module private FsAutoComplete.Tests.CodeFixTests.AddBindingToSignatureFileTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof AddBindingToSignatureFile) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddBindingToSignatureFile.title

      ftestCaseAsync "first unit test for AddBindingToSignatureFile"
      <| CodeFix.check
        server
        "let a$0 b c = ()"
        Diagnostics.acceptAll
        selectCodeFix
        "let Text replaced by AddBindingToSignatureFile b c = ()"
    ])
