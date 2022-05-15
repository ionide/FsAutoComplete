module private FsAutoComplete.Tests.CodeFixTests.RenameParamToMatchSignatureTests

open Expecto
open Helpers
open System.IO
open Utils.Utils
open Utils.TextEdit
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix
open Utils.Server
open Utils.CursorbasedTests.CodeFix


let tests state =
  let selectCodeFix expectedName = CodeFix.withTitle (RenameParamToMatchSignature.title expectedName)

  // requires `fsi` and corresponding `fs` file (and a project!)
  // -> cannot use untitled doc
  // -> use existing files, but load with text specified in tests
  let path = Path.Combine(__SOURCE_DIRECTORY__, @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")
  let (fsiFile, fsFile) = ("Code.fsi", "Code.fs")

  serverTestList (nameof RenameParamToMatchSignature) state defaultConfigDto (Some path) (fun server -> [
    let checkWithFsi
      fsiSource
      fsSourceWithCursor
      selectCodeFix
      fsSourceExpected
      = async {
        let fsiSource = fsiSource |> Text.trimTripleQuotation
        let (cursor, fsSource) =
          fsSourceWithCursor
          |> Text.trimTripleQuotation
          |> Cursor.assertExtractRange
        let! (fsiDoc, diags) = server |> Server.openDocumentWithText fsiFile fsiSource
        use fsiDoc = fsiDoc
        Expect.isEmpty diags "There should be no diagnostics in fsi doc"
        let! (fsDoc, diags) = server |> Server.openDocumentWithText fsFile fsSource
        use fsDoc = fsDoc

        do!
          checkFixAt
            (fsDoc, diags)
            (fsSource, cursor)
            (Diagnostics.expectCode "3218")
            selectCodeFix
            (After (fsSourceExpected |> Text.trimTripleQuotation))
      }

    testCaseAsync "can rename parameter in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename parameter with backticks in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: ``my value``: int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "``my value``")
        """
        module Code

        let f ``my value`` = ``my value`` + 1
        """
    testCaseAsync "can rename parameter with backticks in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f ``$0my value`` = ``my value`` + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename all usage in F# function" <|
      checkWithFsi
        """
        module Code

        val f: x: int -> value: int -> y: int -> int
        """
        """
        module Code

        let f x $0v y =
          let a = v + 1
          let b = v * v
          let v = a + b
          v + x * y
        """
        (selectCodeFix "value")
        """
        module Code

        let f x value y =
          let a = value + 1
          let b = value * value
          let v = a + b
          v + x * y
        """
    testCaseAsync "can rename parameter with type in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f ($0v: int) = v + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f (value: int) = value + 1
        """
    testCaseAsync "can rename parameter in constructor" <|
      checkWithFsi
        """
        module Code

        type T =
          new: value: int -> T
        """
        """
        module Code

        type T($0v: int) =
          let _ = v + 3
        """
        (selectCodeFix "value")
        """
        module Code

        type T(value: int) =
          let _ = value + 3
        """
    testCaseAsync "can rename parameter in member" <|
      checkWithFsi
        """
        module Code

        type T =
          new: unit -> T
          member F: value: int -> int
        """
        """
        module Code

        type T() =
          member _.F($0v) = v + 1
        """
        (selectCodeFix "value")
        """
        module Code

        type T() =
          member _.F(value) = value + 1
        """
    testCaseAsync "can rename parameter with ' in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value': int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "value'")
        """
        module Code

        let f value' = value' + 1
        """
    testCaseAsync "can rename parameter with ' in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0v' = v' + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename parameter with ' (not in last place) in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: v'2: int -> int
        """
        """
        module Code

        let f $0value = value + 1
        """
        (selectCodeFix "v'2")
        """
        module Code

        let f v'2 = v'2 + 1
        """
    testCaseAsync "can rename parameter with ' (not in last place) in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0v'2 = v'2 + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    testCaseAsync "can rename parameter with multiple ' in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value'v'2: int -> int
        """
        """
        module Code

        let f $0v = v + 1
        """
        (selectCodeFix "value'v'2")
        """
        module Code

        let f value'v'2 = value'v'2 + 1
        """
    testCaseAsync "can rename parameter with multiple ' in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0value'v'2 = value'v'2 + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    itestCaseAsync "can handle `' and implementation '` in impl name" <|
      checkWithFsi
        """
        module Code

        val f: value: int -> int
        """
        """
        module Code

        let f $0``sig' and implementation 'impl' do not match`` = ``sig' and implementation 'impl' do not match`` + 1
        """
        (selectCodeFix "value")
        """
        module Code

        let f value = value + 1
        """
    //ENHANCEMENT: correctly detect below. Currently: detects sig name `sig`
    itestCaseAsync "can handle `' and implementation '` in sig name" <|
      checkWithFsi
        """
        module Code

        val f: ``sig' and implementation 'impl' do not match``: int -> int
        """
        """
        module Code

        let f $0value = value + 1
        """
        (selectCodeFix "``sig' and implementation 'impl' do not match``")
        """
        module Code

        let f ``sig' and implementation 'impl' do not match`` = ``sig' and implementation 'impl' do not match`` + 1
        """
  ])
