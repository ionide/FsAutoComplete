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
let x = myTopLevel + 1"

      testCaseAsync "does not apply to qualified names"
      <| CodeFix.checkNotApplicable
        server
        "let x = MyModule.$0missingValue"
        Diagnostics.acceptAll
        (CodeFix.withTitle (IntroduceMissingBinding.title "missingValue"))

      testCaseAsync "introduce binding in deeply nested context"
      <| CodeFix.check
        server
        "let a () =
    let b () =
        let c () =
            deeplyNested$0
        c ()
    b ()"
        Diagnostics.acceptAll
        (CodeFix.withTitle (IntroduceMissingBinding.title "deeplyNested"))
        "let a () =
    let b () =
        let c () =
            let deeplyNested = failwith \"Not Implemented\"
            deeplyNested
        c ()
    b ()"

      testCaseAsync "introduce binding used in expression context"
      <| CodeFix.check
        server
        "let result = missingFn$0 42 + 1"
        Diagnostics.acceptAll
        (CodeFix.withTitle (IntroduceMissingBinding.title "missingFn"))
        "let missingFn = failwith \"Not Implemented\"
let result = missingFn 42 + 1" ])
