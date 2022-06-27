module Utils.Tests.CursorbasedTests
open Expecto
open Helpers
open Ionide.LanguageServerProtocol.Types
open Utils.Utils
open Utils.ServerTests
open Utils.CursorbasedTests

module private CodeFix =
  let private expectLength n (fixes: CodeAction[]) =
    // Note: this fails with `failwith` instead of `Expect....` (-> AssertException) to fail `Expect.failure`
    // Expect.hasLength fixes n "Incorrect number of CodeFixes"
    if fixes.Length <> n then
      failwith $"Expected {n} CodeFixes, but was {fixes.Length}"
    fixes
  let private expectAtLeast n (fixes: CodeAction[]) =
    // Expect.isGreaterThanOrEqual (fixes |> Array.length) n "Too few CodeFixes"
    if fixes.Length < n then
      failwith $"Expected at least {n} CodeFixes, but was {fixes.Length}"
    fixes

  let private checkNotApplicableTests server = testList (nameof CodeFix.checkNotApplicable) [
    let expectFailCheckNotApplicable
      server
      beforeWithCursor
      chooseFix
      =
      CodeFix.checkNotApplicable server beforeWithCursor ignore chooseFix
      |> Expect.failure

    testCaseAsync "not applicable when some code actions but no matching one" <|
      CodeFix.checkNotApplicable
        server
        "let a$0 = 42"
        ignore
        (
          CodeFix.withTitle "Add missing '=' to type definition"
          >> expectLength 0
        )
    testCaseAsync "not applicable when no code actions" <|
      CodeFix.checkNotApplicable
        server
        """
        l$0et a = 42
        a + 42
        """
        ignore
        (expectLength 0)
    testCaseAsync "not not applicable when one code action" <|
      expectFailCheckNotApplicable
        server
        "let a$0 = 42"
        (
          (CodeFix.withTitle "Replace with _")
          >> expectLength 1
        )
    testCaseAsync "not not applicable when multiple code actions" <|
      expectFailCheckNotApplicable
        server
        "let a$0 = 42"
        (expectAtLeast 2)
  ]

  let private checkApplicableTests server = testList (nameof CodeFix.checkApplicable) [
    let expectFailCheckApplicable
      server
      beforeWithCursor
      chooseFix
      =
      CodeFix.checkApplicable server beforeWithCursor ignore chooseFix
      |> Expect.failure

    testCaseAsync "applicable when one code action" <|
      CodeFix.checkApplicable
        server
        "let a$0 = 42"
        ignore
        (
          (CodeFix.withTitle "Replace with _")
          >> expectLength 1
        )
    testCaseAsync "not applicable when no code action" <|
      expectFailCheckApplicable
        server
        "let a$0 = 42"
        (
          CodeFix.withTitle "Add missing '=' to type definition"
          >> expectLength 0
        )
    testCaseAsync "not applicable when multiple code actions" <|
      expectFailCheckApplicable
        server
        "let a$0 = 42"
        (
          expectAtLeast 2
        )
  ]

  let private checkTests server = testList (nameof CodeFix.check) [
    let expectFailCheck
      server
      beforeWithCursor
      chooseFix
      expected
      =
      CodeFix.check server beforeWithCursor ignore chooseFix expected
      |> Expect.failure

    testCaseAsync "can get expected output" <|
      CodeFix.check
        server
        "let a$0 = 42"
        ignore
        (
          (CodeFix.withTitle "Replace with _")
          >> expectLength 1
        )
        "let _ = 42"
    testCaseAsync "fails when unexpected output" <|
      expectFailCheck
        server
        "let a$0 = 42"
        (
          (CodeFix.withTitle "Replace with _")
          >> expectLength 1
        )
        "let b = 42"
    testCaseAsync "fails when no code action" <|
      expectFailCheck
        server
        "let a$0 = 42"
        (
          CodeFix.withTitle "Add missing '=' to type definition"
          >> expectLength 0
        )
        "let _ = 42"
    testCaseAsync "fails when multiple code actions" <|
      expectFailCheck
        server
        "let a$0 = 42"
        (
          expectAtLeast 2
        )
        "let _ = 42"
  ]

  let tests state = 
    let config =
      { defaultConfigDto with
          UnusedOpensAnalyzer = Some true
          UnusedDeclarationsAnalyzer = Some true
          SimplifyNameAnalyzer = Some true
      }
    serverTestList (nameof CodeFix) state config None (fun server -> [
      checkNotApplicableTests server
      checkApplicableTests server
      checkTests server
    ])

let tests state = testList (nameof Utils.CursorbasedTests) [
  CodeFix.tests state
]
