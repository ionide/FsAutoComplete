module private FsAutoComplete.Tests.CodeFixTests.UpdateValueInSignatureFileTests

open System.IO
open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix
open Utils.Utils
open Utils.TextEdit
open Utils.Server
open Utils.CursorbasedTests.CodeFix

let path = Path.Combine(__SOURCE_DIRECTORY__, @"../TestCases/CodeFixTests/RenameParamToMatchSignature/")
let fsiFile, fsFile = ("Code.fsi", "Code.fs")

let checkWithFsi
  server
  fsiSource
  fsSourceWithCursor
  selectCodeFix
  fsiSourceExpected
  = async {
    let fsiSource = fsiSource |> Text.trimTripleQuotation
    let cursor, fsSource =
      fsSourceWithCursor
      |> Text.trimTripleQuotation
      |> Cursor.assertExtractRange
    let! fsiDoc, diags = server |> Server.openDocumentWithText fsiFile fsiSource
    use fsiDoc = fsiDoc
    Expect.isEmpty diags "There should be no diagnostics in fsi doc"
    let! fsDoc, diags = server |> Server.openDocumentWithText fsFile fsSource
    use fsDoc = fsDoc

    do!
      checkFixAt
        (fsDoc, diags)
        fsiDoc.VersionedTextDocumentIdentifier
        (fsiSource, cursor)
        (Diagnostics.expectCode "34")
        selectCodeFix
        (After (fsiSourceExpected |> Text.trimTripleQuotation))
  }

let tests state =
  serverTestList (nameof UpdateValueInSignatureFile) state defaultConfigDto (Some path) (fun server ->
    [ let selectCodeFix = CodeFix.withTitle UpdateValueInSignatureFile.title

      ftestCaseAsync "first unit test for UpdateValueInSignatureFile"
      <| checkWithFsi
        server
        """
module A

val a: b:int -> int
"""
"""
module A

let a$0 (b:int) (c: string) = 0
"""
        selectCodeFix
        """
module A

val a: b: int -> c: string -> int
"""
    ])
