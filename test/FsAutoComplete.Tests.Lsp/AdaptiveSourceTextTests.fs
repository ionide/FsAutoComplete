namespace AdaptiveSourceText


module Tests =
  open Expecto
  open FsAutoComplete.Adaptive
  open FSharp.Data.Adaptive

  let mkRange startLine startCol endLine endCol : Ionide.LanguageServerProtocol.Types.Range =  {
    Start = { Line = startLine; Character = startCol }
    End = { Line = endLine; Character = endCol }
  }

  let tests = ftestList "AdaptiveSourceText tests" [

    testList "Parse from string" [

        testCase "Can parse single line" <| fun _ ->
          let text = """let foo = 4"""
          let sourceText = AdaptiveSourceText2("foo.fsx", text)
          let actualText = sourceText.Text |> AVal.force
          Expect.equal actualText text "Incorrect text"
          let actualLines = sourceText.Lines |> AList.count |> AVal.force
          Expect.equal actualLines 1 "Incorrect number of lines"

        testCase "Can parse multiple lines" <| fun _ ->
          let text = "let foo = 4\nlet bar = 5"
          let sourceText = AdaptiveSourceText2("foo.fsx", text)
          let actualText = sourceText.Text |> AVal.force
          Expect.equal actualText text "Incorrect text"
          let actualLines = sourceText.Lines |> AList.count |> AVal.force
          sourceText.Lines |> AList.force |> printfn "actualLines: %A"
          Expect.equal actualLines 2 "Incorrect number of lines"
    ]

    testList "Single Line" [

      testCase "Can insert at end of line" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 11 0 11
        sourceText.ModifyText(range, "2")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let foo = 42" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"


      testCase "Can insert at middle of line" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 4 0 4
        sourceText.ModifyText(range, "o")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let ofoo = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can insert at start of line" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 0 0 0
        sourceText.ModifyText(range, "let ")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let let foo = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can delete character" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 4 0 5
        sourceText.ModifyText(range, "")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let oo = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can insert many characters at end of line" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 11 0 11
        sourceText.ModifyText(range, "222")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let foo = 4222" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can insert many characters of line" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 4 0 4
        sourceText.ModifyText(range, "222")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let 222foo = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can insert many characters at beginning of line" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 0 0 0
        sourceText.ModifyText(range, "222")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "222let foo = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can delete many characters" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 4 0 7
        sourceText.ModifyText(range, "")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let  = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can replace many characters" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 4 0 7
        sourceText.ModifyText(range, "222")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let 222 = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can insert text with newlines" <| fun _ ->
        let text = """let foo = 4"""
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 4 0 4
        sourceText.ModifyText(range, "222\n333")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let 222\n333foo = 4" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 2 "Incorrect number of lines"
    ]
    testList "Multiple Lines" [
      testCase "Can delete many characters" <|
        fun _ ->
          let text = "let foo = 4\nlet bar = 5"
          let sourceText = AdaptiveSourceText2("foo.fsx", text)
          printfn "Lines: %A" (AList.force sourceText.Lines)
          let range = mkRange 0 4 1 10
          sourceText.ModifyText(range, "")
          printfn "Lines: %A" (AList.force sourceText.Lines)
          let actualText = sourceText.Text |> AVal.force
          Expect.equal actualText "let 5" "Incorrect text"
          let actualLines = sourceText.Lines |> AList.count |> AVal.force
          Expect.equal actualLines 1 "Incorrect number of lines"


      testCase "Can delete many characters 2" <|
        fun _ ->
          let text = "let foo = 4\nlet bar = 5\nlet baz = 6"
          let sourceText = AdaptiveSourceText2("foo.fsx", text)
          let range = mkRange 0 4 2 10
          sourceText.ModifyText(range, "")
          let actualText = sourceText.Text |> AVal.force
          Expect.equal actualText "let 6" "Incorrect text"
          let actualLines = sourceText.Lines |> AList.count |> AVal.force
          Expect.equal actualLines 1 "Incorrect number of lines"

      testCase "Can insert at end of line" <| fun _ ->
        let text = "let foo = 4\nlet bar = 5"
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 11 1 11
        sourceText.ModifyText(range, "222")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let foo = 4222" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 1 "Incorrect number of lines"


      testCase "Can insert at end of line 2" <| fun _ ->
        let text = "let foo = 4\nlet bar = 5\nlet baz = 6"
        let sourceText = AdaptiveSourceText2("foo.fsx", text)
        let range = mkRange 0 11 1 11
        sourceText.ModifyText(range, "2")
        let actualText = sourceText.Text |> AVal.force
        Expect.equal actualText "let foo = 42\nlet baz = 6" "Incorrect text"
        let actualLines = sourceText.Lines |> AList.count |> AVal.force
        Expect.equal actualLines 2 "Incorrect number of lines"
  ]
]
