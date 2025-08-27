module FsAutoComplete.Tests.InteractiveDirectivesTests

open Expecto
open FsAutoComplete.InteractiveDirectives
open Helpers.Expecto.ShadowedTimeouts

let interactiveDirectivesUnitTests =
  testList
    "interactive directives unit tests"
    [

      testList
        "tryParseLoad - Basic functionality"
        [ testCase "happy cases"
          <| fun () ->
               let cases =
                 [ "#load \"script\"", 0, "script"
                   "#load \"script\"", 2, "script"
                   "#load  \"script\" ", 0, "script"
                   "#load @\"script\"", 0, "script"
                   "#load @\"path/to\\script\"", 0, "path/to\\script"
                   "#load \"\"\"script\"\"\"", 0, "script"
                   "#load \"script.fsx\"", 0, "script.fsx" ]

               Expect.all
                 cases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "all are valid cases"

          testCase "invalid directives"
          <| fun () ->
               let cases =
                 [ "load \"script\"", 0
                   "##load \"script\"", 0
                   "#loda \"script\"", 0
                   "#LOAD \"script\"", 0
                   "  #load \"script\"", 1
                   "#load \"script", 0 ]

               Expect.all
                 cases
                 (fun (line, index) ->
                   let actual = tryParseLoad line index
                   actual = None)
                 "all are invalid cases" ]
      
      testList
        "tryParseLoad - String parsing edge cases"
        [ testCase "escaped characters in standard strings"
          <| fun () ->
               let cases =
                 [ "#load \"file\\nname.fsx\"", 0, "file\nname.fsx"
                   "#load \"file\\tname.fsx\"", 0, "file\tname.fsx"
                   "#load \"file\\rname.fsx\"", 0, "file\rname.fsx"
                   "#load \"file\\bname.fsx\"", 0, "file\bname.fsx"
                   "#load \"file\\\\name.fsx\"", 0, "file\\name.fsx"
                   "#load \"file\\\"name.fsx\"", 0, "file\"name.fsx"
                   "#load \"file\\'name.fsx\"", 0, "file'name.fsx" ]

               Expect.all
                 cases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "escaped characters should be handled correctly"
          
          testCase "unicode escape sequences"
          <| fun () ->
               let cases =
                 [ "#load \"\\u0041.fsx\"", 0, "A.fsx"  // \u0041 = 'A'
                   "#load \"\\u0048\\u0065\\u006C\\u006C\\u006F.fsx\"", 0, "Hello.fsx"
                   "#load \"\\U00000041.fsx\"", 0, "A.fsx" ] // \U00000041 = 'A'

               Expect.all
                 cases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "unicode sequences should be handled correctly"
          
          testCase "invalid unicode sequences"
          <| fun () ->
               let cases =
                 [ "#load \"\\uZZZZ.fsx\"", 0, "\\uZZZZ.fsx"  // Invalid hex
                   "#load \"\\u12.fsx\"", 0, "\\u12.fsx"    // Too short
                   "#load \"\\U1234.fsx\"", 0, "\\U1234.fsx" ] // Too short for U

               Expect.all
                 cases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "invalid unicode sequences should be passed through"
          
          testCase "verbatim string edge cases"
          <| fun () ->
               let cases =
                 [ "#load @\"path\\to\\file.fsx\"", 0, "path\\to\\file.fsx"
                   "#load @\"file\"\"with\"\"quotes.fsx\"", 0, "file\"with\"quotes.fsx"
                   "#load @\"C:\\Program Files\\script.fsx\"", 0, "C:\\Program Files\\script.fsx" ]

               Expect.all
                 cases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "verbatim strings should handle backslashes and quotes correctly"
          
          testCase "triple quoted string edge cases"
          <| fun () ->
               let cases =
                 [ "#load \"\"\"multi\nline\nfile.fsx\"\"\"", 0, "multi\nline\nfile.fsx"
                   "#load \"\"\"file with \"quotes\" inside.fsx\"\"\"", 0, "file with \"quotes\" inside.fsx"
                   "#load \"\"\"file with 'single quotes' inside.fsx\"\"\"", 0, "file with 'single quotes' inside.fsx" ]

               Expect.all
                 cases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "triple quoted strings should handle multiline and quotes correctly"
        ]
      
      testList
        "tryParseLoad - Column positioning"
        [ testCase "multiple #load directives in same line"
          <| fun () ->
               let line = "#load \"first.fsx\"; #load \"second.fsx\""
               
               let result1 = tryParseLoad line 0
               Expect.equal result1 (Some "first.fsx") "Should parse first directive at start"
               
               let result2 = tryParseLoad line 20
               Expect.equal result2 (Some "second.fsx") "Should parse second directive"
               
               let result3 = tryParseLoad line 10
               Expect.equal result3 (Some "first.fsx") "Should still parse first directive when cursor is between"
          
          testCase "cursor position affects parsing"
          <| fun () ->
               let line = "    #load \"indented.fsx\""
               
               let result1 = tryParseLoad line 3  // Before #load
               Expect.equal result1 None "Should not parse before #load directive"
               
               let result2 = tryParseLoad line 4  // At #load
               Expect.equal result2 (Some "indented.fsx") "Should parse at #load directive"
               
               let result3 = tryParseLoad line 15  // In the string
               Expect.equal result3 (Some "indented.fsx") "Should parse when cursor is in string"
        ]
      
      testList
        "tryParseLoad - Whitespace handling"
        [ testCase "various whitespace patterns"
          <| fun () ->
               let validCases =
                 [ "#load\t\"tab.fsx\"", 0, "tab.fsx"
                   "#load   \"spaces.fsx\"", 0, "spaces.fsx"
                   "#load \t  \"mixed.fsx\"", 0, "mixed.fsx" ]

               let invalidCase = "#load\n\"newline.fsx\"", 0  // This should fail as newline breaks directive
               
               Expect.all
                 validCases
                 (fun (line, index, expected) ->
                   let actual = tryParseLoad line index
                   actual = Some expected)
                 "valid whitespace patterns should work"
               
               let (invalidLine, invalidIndex) = invalidCase
               let invalidResult = tryParseLoad invalidLine invalidIndex
               Expect.equal invalidResult None "newline should break directive"
        ]
    ]
