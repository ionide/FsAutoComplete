module private FsAutoComplete.Tests.CodeFixTests.ExprTypeMismatchTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof ExprTypeMismatch) state defaultConfigDto None (fun server ->
    [ testCaseAsync "Update return type"
      <| CodeFix.check
        server
        "let a b : int = $0\"meh\""
        Diagnostics.acceptAll
        (CodeFix.withTitle "Update int to string")
        "let a b : string = \"meh\""

      testCaseAsync "Wrap constant in Some"
      <| CodeFix.check
        server
        "let a b : int option = 1$0"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Wrap expression in Some")
        "let a b : int option = Some 1"

      testCaseAsync "Wrap expr in Some"
      <| CodeFix.check
        server
        "let a b : bool option = true$0 = false"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Wrap expression in Some")
        "let a b : bool option = Some(true = false)"

      testCaseAsync "Wrap single indent in Some"
      <| CodeFix.check
        server
        "let a b : bool option = let x = true in $0x"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Wrap expression in Some")
        "let a b : bool option = let x = true in Some x"

      testCaseAsync "Replace expression with None"
      <| CodeFix.check
        server
        "let a b : int option = 1$0"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Replace expression with None")
        "let a b : int option = None"

      testCaseAsync "Wrap constant in ValueSome"
      <| CodeFix.check
        server
        "let a b : int voption = 1$0"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Wrap expression in ValueSome")
        "let a b : int voption = ValueSome 1"

      testCaseAsync "Wrap expr in ValueSome"
      <| CodeFix.check
        server
        "let a b : bool voption = true$0 = false"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Wrap expression in ValueSome")
        "let a b : bool voption = ValueSome(true = false)"

      testCaseAsync "Wrap single indent in ValueSome"
      <| CodeFix.check
        server
        "let a b : bool voption = let x = true in $0x"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Wrap expression in ValueSome")
        "let a b : bool voption = let x = true in ValueSome x"

      testCaseAsync "Replace expression with ValueNone"
      <| CodeFix.check
        server
        "let a b : int voption = 1$0"
        Diagnostics.acceptAll
        (CodeFix.withTitle "Replace expression with ValueNone")
        "let a b : int voption = ValueNone" ])
