module private FsAutoComplete.Tests.CodeFixTests.AddTypeAliasToSignatureFileTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof AddTypeAliasToSignatureFile) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddTypeAliasToSignatureFile.title

      ftestCaseAsync "first unit test for AddTypeAliasToSignatureFile"
      <| CodeFix.check
        server
        "let a$0 b c = ()"
        Diagnostics.acceptAll
        selectCodeFix
        "let Text replaced by AddTypeAliasToSignatureFile b c = ()"
    ])
