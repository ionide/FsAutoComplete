module FsAutoComplete.Tests.NestedLanguageTests

open Expecto
open Utils.ServerTests
open Helpers
open Utils.Server
open System
open Ionide.LanguageServerProtocol.Types

type Document with

  member x.NestedLanguages =
    x.Server.Events
    |> Document.typedEvents<FsAutoComplete.Lsp.TextDocumentNestedLanguages> ("fsharp/textDocument/nestedLanguages")
    |> Observable.filter (fun n -> n.TextDocument = x.VersionedTextDocumentIdentifier)

let private getDocumentText (lines: string[]) (ranges: Range array) : string =
  ranges
  |> Array.map (fun r ->
    let startLine = lines.[int r.Start.Line]
    let endLine = lines.[int r.End.Line]

    if r.Start.Line = r.End.Line then
      startLine.Substring(int r.Start.Character, int (r.End.Character - r.Start.Character))
    else
      let start = startLine.Substring(int r.Start.Character)
      let ``end`` = endLine.Substring(0, int r.End.Character)

      let middle =
        lines.[int (r.Start.Line + 1u) .. int (r.End.Line - 1u)]
        |> Array.map (fun l -> l.Trim())

      let middle = String.Join(" ", middle)
      start + middle + ``end``)
  |> String.concat "\n"



let private contentErrorMessage
  (actual: FsAutoComplete.Lsp.NestedLanguage array)
  (expected: FsAutoComplete.Lsp.NestedLanguage array)
  (sourceText: string)
  =
  let builder = System.Text.StringBuilder()
  let lines = sourceText.Split([| '\n'; '\r' |], StringSplitOptions.None)

  builder.AppendLine "Expected nested documents to be equivalent, but found differences"
  |> ignore

  if actual.Length <> expected.Length then
    builder.AppendLine $"Expected %d{expected.Length} nested languages, but found %d{actual.Length}"
    |> ignore
  else
    for (index, (expected, actual)) in Array.zip expected actual |> Array.indexed do
      if expected.Language <> actual.Language then
        builder.AppendLine
          $"Expected document #${index}'s language to be %s{expected.Language}, but was %s{actual.Language}"
        |> ignore

      let expectedText = getDocumentText lines expected.Ranges
      let actualText = getDocumentText lines actual.Ranges

      builder.AppendLine $"Expected document #{index} to be \n\t%s{expectedText}\nbut was\n\t%s{actualText}"
      |> ignore

  builder.ToString()

let hasLanguages name source expectedLanguages server =
  testAsync name {
    let! (doc, diags) = server |> Server.createUntitledDocument source
    Expect.isEmpty diags "no diagnostics"
    let! nestedLanguages = doc.NestedLanguages |> Async.AwaitObservable

    let mappedExpectedLanguages: FsAutoComplete.Lsp.NestedLanguage array =
      expectedLanguages
      |> Array.map (fun (l, rs) ->
        { Language = l
          Ranges =
            rs
            |> Array.map (fun ((sl, sc), (el, ec)) ->
              { Start = { Line = sl; Character = sc }
                End = { Line = el; Character = ec } }) })

    Expect.equal
      nestedLanguages.NestedLanguages
      mappedExpectedLanguages
      (contentErrorMessage nestedLanguages.NestedLanguages mappedExpectedLanguages source)
  }

let tests state =
  testList
    "nested languages"
    [ testList
        "unsupported scenarios"
        // pending because class members don't return attributes in the FCS Parameter API
        [ serverTestList "class member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "BCL type"
                """
            let b = System.UriBuilder("https://google.com")
            """
                [| ("uri", [| (1u, 38u), (1u, 58u) |]) |]
                server

              hasLanguages
                "F#-defined type"
                """
            type Foo() =
              member x.Boo([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let f = new Foo()
            let u = f.Boo("https://google.com")
            """
                [| ("uri", [| (4u, 26u), (4u, 46u) |]) |]
                server ])
          serverTestList "functions" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "interpolated string with format specifier"
                """
            let uri ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>]s: string) = ()
            let u = uri $"https://%b{true}.com"
            """
                [| ("uri", [| (2u, 26u), (2u, 34u); (2u, 42u), (2u, 46u) |]) |]
                server


              // commented out because I can't figure out how to get the new string interpolation working
              //   hasLanguages
              //     "more than triple-quoted interpolated string with format specifier"
              //     """
              // let uri ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>]s: string) = ()
              // let u = uri $$""""https://%b{{true}}.com""""
              // """
              //     [| ("uri", [| (2, 24), (2, 35); (2, 39), (2, 45) |]) |]
              //     server
              ]) ]
      testList
        "FSharp Code"
        [ serverTestList "let bound function member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "normal string value"
                """
            let boo ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let u = boo "https://google.com"
            """
                // note for reader - 24 is the start quote, 44 is the end quote, so we want a doc including 25-43
                [| ("uri", [| (2u, 25u), (2u, 43u) |]) |]
                server

              hasLanguages
                "verbatim string value"
                """
            let boo ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let u = boo @"https://google.com"
            """
                [| ("uri", [| (2u, 26u), (2u, 44u) |]) |]
                server

              hasLanguages
                "triple-quote string value"
                """
            let boo ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let u = boo "https://google.com"
            """
                [| ("uri", [| (2u, 25u), (2u, 43u) |]) |]
                server

              hasLanguages
                "simple interpolated string"
                """
            let uri ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>]s: string) = ()
            let u = uri $"https://{true}.com"
            """
                [| ("uri", [| (2u, 26u), (2u, 34u); (2u, 40u), (2u, 44u) |]) |]
                server

              // commented out because I can't figure out how to get the new string interpolation working
              //   hasLanguages
              //     "triple-quote interpolated string"
              //     """
              // let uri ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>]s: string) = ()
              // let u = uri $\"\"\"https://{true}.com"\"\\"
              // """
              //     [| ("uri", [| (2, 24), (2, 35); (2, 39), (2, 45) |]) |]
              //     server



              // commented out because I can't figure out how to get the new string interpolation working
              //   hasLanguages
              //     "triple-quoted interpolated string with format specifier"
              //     """
              // let uri ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>]s: string) = ()
              // let u = uri $"https://%b{true}.com"
              // """
              //     [| ("uri", [| (2, 24), (2, 35); (2, 39), (2, 45) |]) |]
              //     server

              hasLanguages
                "multiple languages in the same document"
                """
            let html ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("html")>]s: string) = ()
            let sql ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("sql")>]s: string) = ()
            let myWebPage = html "<body>wow</body>"
            let myQuery = sql "select * from accounts where net_worth > 1000000"
            """
                [| ("html", [| (3u, 34u), (3u, 50u) |]); ("sql", [| (4u, 31u), (4u, 79u) |]) |]
                server ]) ] ]
