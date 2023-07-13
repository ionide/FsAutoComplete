module private FsAutoComplete.Tests.CodeFixTests.ToInterpolatedStringTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  fserverTestList (nameof ToInterpolatedString) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ToInterpolatedString.title

      testCaseAsync "simple integer string format"
      <| CodeFix.check
        server
        """
        let a = sprintf$0 "Hey %i" 3
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let a = $"Hey %i" 3
        """ ])
