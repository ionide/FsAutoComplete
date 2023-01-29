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
  let path = Path.Combine(File.CurrentDir(), @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")
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

        val f: value1: int -> int
        """
        """
        module Code

        let f $0v1 = v1 + 1
        """
        (selectCodeFix "value1")
        """
        module Code

        let f value1 = value1 + 1
        """
    testCaseAsync "can rename parameter with backticks in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: ``my value2``: int -> int
        """
        """
        module Code

        let f $0v2 = v2 + 1
        """
        (selectCodeFix "``my value2``")
        """
        module Code

        let f ``my value2`` = ``my value2`` + 1
        """
    testCaseAsync "can rename parameter with backticks in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value3: int -> int
        """
        """
        module Code

        let f ``$0my value3`` = ``my value3`` + 1
        """
        (selectCodeFix "value3")
        """
        module Code

        let f value3 = value3 + 1
        """
    testCaseAsync "can rename all usage in F# function" <|
      checkWithFsi
        """
        module Code

        val f: x: int -> value4: int -> y: int -> int
        """
        """
        module Code

        let f x $0v4 y =
          let a = v4 + 1
          let b = v4 * v4
          let v = a + b
          v + x * y
        """
        (selectCodeFix "value4")
        """
        module Code

        let f x value4 y =
          let a = value4 + 1
          let b = value4 * value4
          let v = a + b
          v + x * y
        """
    testCaseAsync "can rename parameter with type in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value5: int -> int
        """
        """
        module Code

        let f ($0v5: int) = v5 + 1
        """
        (selectCodeFix "value5")
        """
        module Code

        let f (value5: int) = value5 + 1
        """
    testCaseAsync "can rename parameter in constructor" <|
      checkWithFsi
        """
        module Code

        type T =
          new: value6: int -> T
        """
        """
        module Code

        type T($0v6: int) =
          let _ = v6 + 3
        """
        (selectCodeFix "value6")
        """
        module Code

        type T(value6: int) =
          let _ = value6 + 3
        """
    testCaseAsync "can rename parameter in member" <|
      checkWithFsi
        """
        module Code

        type T =
          new: unit -> T
          member F: value7: int -> int
        """
        """
        module Code

        type T() =
          member _.F($0v7) = v7 + 1
        """
        (selectCodeFix "value7")
        """
        module Code

        type T() =
          member _.F(value7) = value7 + 1
        """
    testCaseAsync "can rename parameter with ' in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value8': int -> int
        """
        """
        module Code

        let f $0v8 = v8 + 1
        """
        (selectCodeFix "value8'")
        """
        module Code

        let f value8' = value8' + 1
        """
    testCaseAsync "can rename parameter with ' in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value9: int -> int
        """
        """
        module Code

        let f $0v9' = v9' + 1
        """
        (selectCodeFix "value9")
        """
        module Code

        let f value9 = value9 + 1
        """
    testCaseAsync "can rename parameter with ' (not in last place) in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: v10'2: int -> int
        """
        """
        module Code

        let f $0value10 = value10 + 1
        """
        (selectCodeFix "v10'2")
        """
        module Code

        let f v10'2 = v10'2 + 1
        """
    testCaseAsync "can rename parameter with ' (not in last place) in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value11: int -> int
        """
        """
        module Code

        let f $0v11'2 = v11'2 + 1
        """
        (selectCodeFix "value11")
        """
        module Code

        let f value11 = value11 + 1
        """
    testCaseAsync "can rename parameter with multiple ' in signature in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value12'v'2: int -> int
        """
        """
        module Code

        let f $0v12 = v12 + 1
        """
        (selectCodeFix "value12'v'2")
        """
        module Code

        let f value12'v'2 = value12'v'2 + 1
        """
    testCaseAsync "can rename parameter with multiple ' in implementation in F# function" <|
      checkWithFsi
        """
        module Code

        val f: value13: int -> int
        """
        """
        module Code

        let f $0value13'v'2 = value13'v'2 + 1
        """
        (selectCodeFix "value13")
        """
        module Code

        let f value13 = value13 + 1
        """
    itestCaseAsync "can handle `' and implementation '` in impl name" <|
      checkWithFsi
        """
        module Code

        val f: value14: int -> int
        """
        """
        module Code

        let f $0``sig' and implementation 'impl' do not match`` = ``sig' and implementation 'impl' do not match`` + 1
        """
        (selectCodeFix "value14")
        """
        module Code

        let f value14 = value14 + 1
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

        let f $0value15 = value15 + 1
        """
        (selectCodeFix "``sig' and implementation 'impl' do not match``")
        """
        module Code

        let f ``sig' and implementation 'impl' do not match`` = ``sig' and implementation 'impl' do not match`` + 1
        """
  ])
