module private FsAutoComplete.Tests.CodeFixTests.ReplaceLambdaWithDotLambdaTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  serverTestList (nameof ReplaceLambdaWithDotLambda) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle ReplaceLambdaWithDotLambda.titleReplaceToDotLambda

      testCaseAsync "Simple property"
      <| CodeFix.check
        server
        "let x = \"\" |> fun y -> $0y.Length"
        Diagnostics.acceptAll
        selectCodeFix
        "let x = \"\" |> _.Length"

      testCaseAsync "Property of application"
      <| CodeFix.check
        server
        "let a5 : {| Foo : int -> {| X : string |} |} -> string = fun x -> x.$0Foo(5).X"
        Diagnostics.acceptAll
        selectCodeFix
        "let a5 : {| Foo : int -> {| X : string |} |} -> string = _.Foo(5).X"

      testCaseAsync "Application"
      <| CodeFix.check
        server
        "let a6 = [1] |> List.map(fun x -> x$0.ToString())"
        Diagnostics.acceptAll
        selectCodeFix
        "let a6 = [1] |> List.map _.ToString()"

      testCaseAsync "fun x -> x.ToString()"
      <| CodeFix.check
        server
        "let a6 = fun$0 x -> x.ToString()"
        Diagnostics.acceptAll
        selectCodeFix
        "let a6 = _.ToString()"

      // -------------------- from dot lambda to lambda test ----------------------

      let selectCodeFix = CodeFix.withTitle ReplaceLambdaWithDotLambda.titleReplaceToLambda

      testCaseAsync "Simple property (from dot lambda to lambda)"
      <| CodeFix.check
        server
        "let x = \"\" |> $0_.Length"
        Diagnostics.acceptAll
        selectCodeFix
        "let x = \"\" |> (fun _i -> _i.Length)"

      testCaseAsync "Property of application (from dot lambda to lambda)"
      <| CodeFix.check
        server
        "let a5 : {| Foo : int -> {| X : string |} |} -> string = _.$0Foo(5).X"
        Diagnostics.acceptAll
        selectCodeFix
        "let a5 : {| Foo : int -> {| X : string |} |} -> string = (fun _i -> _i.Foo(5).X)"

      testCaseAsync "Application (from dot lambda to lambda)"
      <| CodeFix.check
        server
        "let a6 = [1] |> List.map _$0.ToString()"
        Diagnostics.acceptAll
        selectCodeFix
        "let a6 = [1] |> List.map (fun _i -> _i.ToString())"

      testCaseAsync "fun x -> x.ToString() (from dot lambda to lambda)"
      <| CodeFix.check
        server
        "let a6 = _.ToStri$0ng()"
        Diagnostics.acceptAll
        selectCodeFix
        "let a6 = (fun _i -> _i.ToString())"
      ])
