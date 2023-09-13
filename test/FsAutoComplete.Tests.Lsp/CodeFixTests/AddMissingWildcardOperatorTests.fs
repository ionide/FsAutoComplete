module private FsAutoComplete.Tests.CodeFixTests.AddMissingWildcardOperatorTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof AddMissingWildcardOperator) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddMissingWildcardOperator.title

      ftestCaseAsync "can suggest wildcard pattern for missing match case"
      <| CodeFix.check
        server
        """
        type SomeUnion =
            | First
            | Second

        let testMatch su =
            match su with
            | First -> "hey"
            $0|-> "hello"
        """
        (Diagnostics.expectCode "43")
        selectCodeFix
        """
        type SomeUnion =
            | First
            | Second

        let testMatch su =
            match su with
            | First -> "hey"
            | _ -> "hello"
        """ ])
