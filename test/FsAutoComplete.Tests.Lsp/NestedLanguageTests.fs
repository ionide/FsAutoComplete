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

    Expect.equal nestedLanguages.NestedLanguages mappedExpectedLanguages "languages"
  }

let tests state =
  testList
    "nested languages"
    [ testList
        "BCL"
        [ serverTestList "class member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "with single string parameter"
                """
            let b = System.UriBuilder("https://google.com")
            """
                [| ("uri", [| (1u, 38u), (1u, 58u) |]) |]
                server ]) ]
      testList
        "FSharp Code"
        [ serverTestList "class member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "with single string parameter"
                """
            type Foo() =
              member x.Uri([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let f = new Foo()
            let u = f.Uri("https://google.com")
            """
                [| ("uri", [| (5u, 31u), (5u, 51u) |]) |]
                server ])

          serverTestList "let bound function member" state defaultConfigDto None (fun server ->
            [ hasLanguages
                "with single string parameter"
                """
            let foo ([<System.Diagnostics.CodeAnalysis.StringSyntaxAttribute("uri")>] uriString: string) = ()
            let u = foo "https://google.com"
            """
                [| ("uri", [| (5u, 31u), (5u, 51u) |]) |]
                server ]) ]
      ftestList "Sql" [
        serverTestList "loose function" state defaultConfigDto None (fun server -> [
          hasLanguages
                "with single string parameter and string literal"
                """
            let foo (s: string) = ()
            let u = foo "https://google.com"
            """
                [| ("foo", [| (2u, 24u), (2u, 44u) |]) |]
                server

          hasLanguages
                "with single string parameter and interpolated string literal"
                """
            let foo (s: string) = ()
            let u = foo $"https://{true}.com"
            """
                [| ("foo", [| (2u, 24u), (2u, 35u)
                              (2u, 39u), (2u, 45u) |]) |]
                server

          hasLanguages
                "multiple lanuages in the same document"
                """
            let html (s: string) = ()
            let sql (s: string) = ()
            let myWebPage = html "<body>WOWEE</body>"
            let myQuery = sql "select * from accounts where net_worth > 1000000"
            """
                [| ("html", [| (3u, 33u), (3u, 53u) |])
                   ("sql", [| (4u, 30u), (4u, 80u) |]) |]
                server
        ]
        )
      ]]
