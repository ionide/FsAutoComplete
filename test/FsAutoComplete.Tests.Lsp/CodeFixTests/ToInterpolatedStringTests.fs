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
        let a = $"Hey %i{3}"
        """

      testCaseAsync "replace two simple formats"
      <| CodeFix.check
        server
        """
        let name = "pikachu"
        let a = sprintf$0 "Hey you %s %i" name 9000
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let name = "pikachu"
        let a = $"Hey you %s{name} %i{9000}"
        """

      testCaseAsync "leading zeros in format"
      <| CodeFix.check
        server
        """
        printfn $0"%02i" 9
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        printfn $"%02i{9}"
        """

      ])
