module private FsAutoComplete.Tests.CodeFixTests.AddMissingSeqTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof AddMissingSeq) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddMissingSeq.title

      testCaseAsync "FS3873 — Adds missing seq before { start..step..finish } top level"
      <| CodeFix.check
        server
        "$0{ 1..10..20 }"
        Diagnostics.acceptAll
        selectCodeFix
        "seq { 1..10..20 }"

      testCaseAsync "FS3873 — Adds missing seq before { start..step..finish } binding"
      <| CodeFix.check
        server
        "let xs = $0{ 1..10..20 }"
        Diagnostics.acceptAll
        selectCodeFix
        "let xs = seq { 1..10..20 }"

      testCaseAsync "FS3873 — Adds missing seq before { start..finish } top level"
      <| CodeFix.check
        server
        "$0{ 1..10 }"
        Diagnostics.acceptAll
        selectCodeFix
        "seq { 1..10 }"

      testCaseAsync "FS3873 — Adds missing seq before { start..finish } binding"
      <| CodeFix.check
        server
        "let xs = $0{ 1..10 }"
        Diagnostics.acceptAll
        selectCodeFix
        "let xs = seq { 1..10 }"

      testCaseAsync "FS0740 — Adds missing seq before { x; y } top level"
      <| CodeFix.check
        server
        "$0{ 1;10 }"
        Diagnostics.acceptAll
        selectCodeFix
        "seq { 1;10 }"

      testCaseAsync "FS0740 — Adds missing seq before { x; y } binding"
      <| CodeFix.check
        server
        "let xs = $0{ 1;10 }"
        Diagnostics.acceptAll
        selectCodeFix
        "let xs = seq { 1;10 }"
    ])
