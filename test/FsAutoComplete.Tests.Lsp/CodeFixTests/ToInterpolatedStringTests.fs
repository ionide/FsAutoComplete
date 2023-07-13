module private FsAutoComplete.Tests.CodeFixTests.ToInterpolatedStringTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof ToInterpolatedString) state defaultConfigDto None (fun server ->
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
        printf$0 "Hey you %s %i" name 9000
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        let name = "pikachu"
        printf $"Hey you %s{name} %i{9000}"
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

      testCaseAsync "leading + in format"
      <| CodeFix.check
        server
        """
        printfn $0"%+i" 9
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        printfn $"%+i{9}"
        """

      testCaseAsync "leading - in format"
      <| CodeFix.check
        server
        """
        printfn $0"%-i" 9
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        printfn $"%-i{9}"
        """

      testCaseAsync "bool in format"
      <| CodeFix.check
        server
        """
        sprintf$0 "%b" true
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%b{true}"
        """

      testCaseAsync "char in format"
      <| CodeFix.check
        server
        """
        sprintf$0 "%c" 'd'
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%c{'d'}"
        """

      testCaseAsync "int in format but as d"
      <| CodeFix.check
        server
        """
        sprintf$0 "%d" 9001
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%d{9001}"
        """

      testCaseAsync "unsigned int in format"
      <| CodeFix.check
        server
        """
        sprintf$0 "%u" 7L
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%u{7L}"
        """

      testCaseAsync "lowercase hex in format"
      <| CodeFix.check
        server
        """
        sprintf$0 "%x" 12
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%x{12}"
        """

      testCaseAsync "uppercase hex in format"
      <| CodeFix.check
        server
        """
        sprintf$0 "%X" 13
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%X{13}"
        """

      testCaseAsync "unsigned octal number in format"
      <| CodeFix.check
        server
        """
        sprintf$0 "%o" 9
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%o{9}"
        """

      testCaseAsync "binary number in format"
      <| CodeFix.check
        server
        """
        sprintf$0 "%B" 9
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%B{9}"
        """

      testCaseAsync "signed value having the form [-]d.dddde[sign]ddd, lowercase"
      <| CodeFix.check
        server
        """
        sprintf$0 "%e" 9
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%e{9}"
        """

      testCaseAsync "signed value having the form [-]d.dddde[sign]ddd, uppercase"
      <| CodeFix.check
        server
        """
        sprintf$0 "%E" 9
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%E{9}"
        """

      testCaseAsync "signed value having the form [-]dddd.dddd, lowercase"
      <| CodeFix.check
        server
        """
        sprintf$0 "%f" 9.4
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%f{9.4}"
        """

      testCaseAsync "signed value having the form [-]dddd.dddd, uppercase"
      <| CodeFix.check
        server
        """
        sprintf$0 "%F" 9.4
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%F{9.4}"
        """

      testCaseAsync "%g"
      <| CodeFix.check
        server
        """
        sprintf$0 "%g" 9.4
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%g{9.4}"
        """

      testCaseAsync "%G"
      <| CodeFix.check
        server
        """
        sprintf$0 "%G" 9.4
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%G{9.4}"
        """

      testCaseAsync "%M"
      <| CodeFix.check
        server
        """
        sprintf$0 "%M" 9.4M
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%M{9.4M}"
        """

      testCaseAsync "Box anonymous record"
      <| CodeFix.check
        server
        """
        sprintf$0 "%O" {| Foo = 2 |}
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%O{ {| Foo = 2 |} }"
        """

      testCaseAsync "%A"
      <| CodeFix.check
        server
        """
        type Foo = { Meh: int }
        sprintf$0 "%A" { Meh = 2 }
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        type Foo = { Meh: int }
        $"%A{ { Meh = 2 } }"
        """

      testCaseAsync "Don't do %a"
      <| CodeFix.checkNotApplicable
        server
        """
        printfn$0 "%a" (fun _ _ -> ()) 0
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "Don't do %t"
      <| CodeFix.checkNotApplicable
        server
        """
        printfn$0 "%a" x 0
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "Don't do %%"
      <| CodeFix.checkNotApplicable
        server
        """
        printfn$0 "%%"
        """
        Diagnostics.acceptAll
        selectCodeFix

      testCaseAsync "Object expressions needs a space"
      <| CodeFix.check
        server
        """
        sprintf$0 "%A" { new System.IDisposable with member _.Dispose() = () }
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
        $"%A{ { new System.IDisposable with member _.Dispose() = () } }"
        """

      ])
