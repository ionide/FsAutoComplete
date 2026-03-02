module private FsAutoComplete.Tests.CodeFixTests.IntroduceMissingBindingTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof IntroduceMissingBinding) state defaultConfigDto None (fun server ->
    [ testCaseAsync "introduce binding for undefined identifier"
      <| CodeFix.check
        server
        "let a () =
    myFun$0 ()
    ()"
        Diagnostics.acceptAll
        (CodeFix.withTitle (IntroduceMissingBinding.title "myFun"))
        "let a () =
    let myFun = failwith \"Not Implemented\"
    myFun ()
    ()"

      testCaseAsync "introduce binding preserves indentation"
      <| CodeFix.check
        server
        "let a () =
    let b () =
        myVal$0
    b ()"
        Diagnostics.acceptAll
        (CodeFix.withTitle (IntroduceMissingBinding.title "myVal"))
        "let a () =
    let b () =
        let myVal = failwith \"Not Implemented\"
        myVal
    b ()"

      testCaseAsync "introduce binding at top level"
      <| CodeFix.check
        server
        "let x = myTop$0Level + 1"
        Diagnostics.acceptAll
        (CodeFix.withTitle (IntroduceMissingBinding.title "myTopLevel"))
        "let myTopLevel = failwith \"Not Implemented\"
let x = myTopLevel + 1" ])
