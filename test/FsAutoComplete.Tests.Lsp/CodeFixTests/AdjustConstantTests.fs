module private FsAutoComplete.Tests.CodeFixTests.AdjustConstantTests

open System
open Expecto
open Helpers
open Utils.ServerTests
open Utils.Server
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.AdjustConstant
open Utils.Tests
open Utils.TextEdit
open Utils.CursorbasedTests.CodeFix
open Ionide.LanguageServerProtocol.Types

module private ConvertIntToOtherBase =
  let baseOf (str: String) =
    if str.Contains "0b" then Base.Binary
    elif str.Contains "0x" then Base.Hexadecimal
    elif str.Contains "0o" then Base.Octal
    else Base.Decimal

  let selectIntCodeFix (base': Base) =
    match base' with
    | Base.Decimal -> Title.Int.Convert.toDecimal
    | Base.Hexadecimal -> Title.Int.Convert.toHexadecimal
    | Base.Octal -> Title.Int.Convert.toOctal
    | Base.Binary -> Title.Int.Convert.toBinary
    |> CodeFix.withTitle
  /// empty `expected`: no corresponding fix
  let private checkBase
    doc
    (source: String, cursor: Range)
    base'
    expected
    =
    let name = 
      if String.IsNullOrWhiteSpace expected then
        $"cannot convert to {base'}"
      else
        $"can convert to {base'}"
    testCaseAsync name (async {
      let! (doc, diags) = doc
      let expected =
        if String.IsNullOrWhiteSpace expected then
          ExpectedResult.NotApplicable
        else
          ExpectedResult.After expected
      do! checkFixAt 
            (doc, diags) 
            (source, cursor) 
            Diagnostics.acceptAll
            (selectIntCodeFix base')
            expected
    })
  /// empty `expectedXXX`: there should be no corresponding Fix
  let check
    server
    name
    (beforeWithCursor: String)
    (expectedDecimal: String)
    (expectedHexadecimal: String)
    (expectedOctal: String)
    (expectedBinary: String)
    =
    let (cursor, source) = Cursor.assertExtractRange beforeWithCursor
    documentTestList name server (Server.createUntitledDocument source) (fun doc -> [
      checkBase doc (source, cursor) Base.Decimal expectedDecimal
      checkBase doc (source, cursor) Base.Hexadecimal expectedHexadecimal
      checkBase doc (source, cursor) Base.Octal expectedOctal
      checkBase doc (source, cursor) Base.Binary expectedBinary
    ])
  /// Checks all combinations of base': Can convert from any base to all others but not to self
  /// 
  /// `template`: without cursor, but with `{number}` marker: number gets inserted here and cursor placed at end
  /// 
  /// empty `valueXXX`: there should be no corresponding Fix
  let private checkAll
    server
    name
    (template: String)
    (decimalNumber: String)
    (hexadecimalNumber: String)
    (octalNumber: String)
    (binaryNumber: String)
    =
    let applyTemplate cursor number =
      let number =
        if cursor then
          number + "$0"
        else
          number
      template.Replace("{number}", number)

    testList name [
      let data = [(Base.Decimal, decimalNumber); (Base.Hexadecimal, hexadecimalNumber); (Base.Octal, octalNumber); (Base.Binary, binaryNumber)]
      let valueOf (base') = 
        data
        |> List.find (fun (b,_) -> b = base')
        |> snd
      for (base', value) in data do
        if String.IsNullOrEmpty value then
          ()
        else
          let mkExpected (b) =
            if base' = b || String.IsNullOrEmpty (valueOf b) then 
              ""
            else
              applyTemplate false (valueOf b)
          check server $"can convert from {base'}"
            (applyTemplate true value)
            (mkExpected Base.Decimal)
            (mkExpected Base.Hexadecimal)
            (mkExpected Base.Octal)
            (mkExpected Base.Binary)
    ]

  type private Journey =
    | JustDestination of string
    | JustSource of string
    | InOut of string
    | Neither
  module private Journey =
    let source =
      function
      | JustSource value | InOut value -> Some value
      | JustDestination _ | Neither -> None
    let destination =
      function
      | JustDestination value | InOut value -> Some value
      | JustSource _ | Neither -> None
  let private checkAllJourneys
    server
    name
    (template: String)
    (decimalNumber: Journey)
    (hexadecimalNumber: Journey)
    (octalNumber: Journey)
    (binaryNumber: Journey)
    =
    let applyTemplate cursor number =
      let number = if cursor then number + "$0" else number
      template.Replace("{number}", number)

    testList name [
      let data = [(Base.Decimal, decimalNumber); (Base.Hexadecimal, hexadecimalNumber); (Base.Octal, octalNumber); (Base.Binary, binaryNumber)]

      for (base', j) in data do
        match j |> Journey.source with
        | None -> ()
        | Some value ->

          let mkExpected b =
            if base' = b then
              ""
            else
              data 
              |> List.find (fst >> (=) b) 
              |> snd 
              |> Journey.destination
              |> Option.map (applyTemplate false)
              |> Option.defaultValue ""

          check server $"can convert from {base'}"
            (applyTemplate true value)
            (mkExpected Base.Decimal)
            (mkExpected Base.Hexadecimal)
            (mkExpected Base.Octal)
            (mkExpected Base.Binary)
    ]

  let tests state =
    serverTestList "Convert int-number to other bases" state defaultConfigDto None (fun server -> [
      checkAll server "can convert simple number"
        "let n = {number}"
        "123"
        "0x7B"
        "0o173"
        "0b1111011"
      checkAll server "can convert simple negative number"
        "let n = {number}"
        "-123"
        "-0x7B"
        "-0o173"
        "-0b1111011"
      checkAll server "can convert 0"
        "let n = {number}"
        "0"
        "0x0"
        "0o0"
        "0b0"
      checkAll server "can convert 1"
        "let n = {number}"
        "1"
        "0x1"
        "0o1"
        "0b1"
      checkAll server "can convert -1"
        "let n = {number}"
        "-1"
        "-0x1"
        "-0o1"
        "-0b1"

      testList "extrema" [
        // Note regarding negative `MinValue`:
        //   Only decimal has `-` sign -- all other should not.
        //   While `-0b1000_0000y` is valid -- it has basically two minus signs: one `-` and one minus bit.
        //   The Quick Fix removes that `-` sign when converting from decimal to other base.
        //   However: it does NOT remove the `-` sign when it already exists for a non-decimal base:
        //   `-0b1000_0000y` becomes `-0x80y`, while `0b1000_0000y` becomes `0x80y`
        testList "sbyte" [
          checkAll server "can convert MaxValue"
            "let n = {number} = System.SByte.MaxValue"
            "127y"
            "0x7Fy"
            "0o177y"
            "0b1111111y"
          checkAll server "can convert MinValue (no `-`)"
            "let n = {number} = System.SByte.MinValue"
            "-128y"
            "0x80y"
            "0o200y"
            "0b10000000y"
          checkAllJourneys server "can convert MinValue (keep `-`)"
            "let n = {number} = System.SByte.MinValue"
            (JustDestination "-128y")
            (InOut "-0x80y")
            (InOut "-0o200y")
            (InOut "-0b10000000y")
        ]
        testList "byte" [
          checkAll server "can convert MaxValue"
            "let n = {number} = System.Byte.MaxValue"
            "255uy"
            "0xFFuy"
            "0o377uy"
            "0b11111111uy"
          checkAll server "can convert MinValue"
            "let n = {number} = System.Byte.MinValue"
            "0uy"
            "0x0uy"
            "0o0uy"
            "0b0uy"
        ]

        testList "uint64" [
          checkAll server "can convert MaxValue"
            "let n = {number} = System.UInt64.MaxValue"
            "18446744073709551615UL"
            "0xFFFFFFFFFFFFFFFFUL"
            "0o1777777777777777777777UL"
            "0b1111111111111111111111111111111111111111111111111111111111111111UL"
          checkAll server "can convert MinValue"
            "let n = {number} = System.UInt64.MinValue"
            "0UL"
            "0x0UL"
            "0o0UL"
            "0b0UL"
        ]
        testList "int64" [
          // let value = Int64.MinValue in sprintf "\"%i\"\n\"0x%X\"\n\"0o%o\"\n\"0b%B\"" value value value value;;
          checkAll server "can convert MaxValue"
            "let n = {number} = System.Int64.MaxValue"
            "9223372036854775807UL"
            "0x7FFFFFFFFFFFFFFFUL"
            "0o777777777777777777777UL"
            "0b111111111111111111111111111111111111111111111111111111111111111UL"
          checkAll server "can convert MinValue (no `-`)"
            "let n = {number} = System.Int64.MinValue"
            "-9223372036854775808L"
            "0x8000000000000000L"
            "0o1000000000000000000000L"
            "0b1000000000000000000000000000000000000000000000000000000000000000L"
          checkAllJourneys server "can convert MinValue (keep `-`)"
            "let n = {number} = System.Int64.MinValue"
            (JustDestination "-9223372036854775808L")
            (InOut "-0x8000000000000000L")
            (InOut "-0o1000000000000000000000L")
            (InOut "-0b1000000000000000000000000000000000000000000000000000000000000000L")
        ]

        testList "int (without suffix)" [
          checkAll server "can convert Int64.MaxValue"
            "let n = {number} = Int32.MaxValue"
            "2147483647"
            "0x7FFFFFFF"
            "0o17777777777"
            "0b1111111111111111111111111111111"
          checkAll server "can convert System.Int32.MinValue"
            "let n = {number} = System.Int32.MinValue"
            "-2147483648"
            "0x80000000"
            "0o20000000000"
            "0b10000000000000000000000000000000"
          checkAllJourneys server "can convert MinValue (keep `-`)"
            "let n = {number} = System.Int32.MinValue"
            (JustDestination "-2147483648")
            (InOut "-0x80000000")
            (InOut "-0o20000000000")
            (InOut "-0b10000000000000000000000000000000")
        ]
      ]

      testList "types" [
        let suffixes = [
          ("sbyte", ["y"])
          ("byte", ["uy"])
          ("int16", ["s"])
          ("uint16", ["us"])
          ("int32", [""; "l"])
          ("uint32", ["u"; "ul"])
          ("nativeint", ["n"])
          ("unativeint", ["un"])
          ("int64", ["L"])
          ("uint64", ["UL"])
        ]

        for (name, suffixes) in suffixes do
          testList $"can convert {name}" [
            for suffix in suffixes do
              testList $"with suffix {suffix}" [
                checkAll server $"with value 123"
                  $"let n = {{number}}{suffix}"
                  "123"
                  "0x7B"
                  "0o173"
                  "0b1111011"
                  
                if not (name.StartsWith "u") && name <> "byte" then
                  checkAll server $"with value -123"
                    $"let n = {{number}}{suffix}"
                    "123"
                    "0x7B"
                    "0o173"
                    "0b1111011"
              ]
          ]

        testCaseAsync "does not trigger for bigint" <|
          CodeFix.checkNotApplicable server
            "let n = 9999999999999999999999999999$0I"
            Diagnostics.acceptAll
            (selectIntCodeFix Base.Hexadecimal)
      ]

      testList "sign shenanigans" [
        testList "keep unnecessary sign" [
          checkAll server "keep + in +123"
            "let n = {number}"
            "+123"
            "+0x7B"
            "+0o173"
            "+0b1111011"
          checkAll server "keep + in +0"
            "let n = {number}"
            "+0"
            "+0x0"
            "+0o0"
            "+0b0"
          checkAll server "keep - in -0"
            "let n = {number}"
            "-0"
            "-0x0"
            "-0o0"
            "-0b0"
          checkAllJourneys server "keep + in +(-123)"
            "let n = {number}"
            (JustDestination "-123")
            (InOut "+0xFFFFFF85")
            (InOut "+0o37777777605")
            (InOut "+0b11111111111111111111111110000101")
        ]

        testList "explicit sign and actual sign do not match" [
          testList "keep explicit `-` in positive constant" [
            // Hex/Oct/Bin have sign bit, but can additional have explicit `-` sign
            checkAllJourneys server "keep - in -(-123)"
              "let n = {number}"
              (JustDestination "123")
              (InOut "-0xFFFFFF85")
              (InOut "-0o37777777605")
              (InOut "-0b11111111111111111111111110000101")
          ] 
          testList "keep explicit `+` in negative constant" [
            checkAllJourneys server "keep + in +(-123)"
              "let n = {number}"
              (JustDestination "-123")
              (InOut "+0xFFFFFF85")
              (InOut "+0o37777777605")
              (InOut "+0b11111111111111111111111110000101")
          ]
        ]
      ]

      testList "locations" [
        check server "can convert in math expression"
          "let n = max (123 + 456$0 / 13 * 17 - 9) (456 - 123)"
          ""
          "let n = max (123 + 0x1C8 / 13 * 17 - 9) (456 - 123)"
          "let n = max (123 + 0o710 / 13 * 17 - 9) (456 - 123)"
          "let n = max (123 + 0b111001000 / 13 * 17 - 9) (456 - 123)"
        check server "can convert inside member"
          """
          type T() =
            member _.DoStuff(arg: int) =
              arg + 3 * 456$0 / 3
          """
          ""
          """
          type T() =
            member _.DoStuff(arg: int) =
              arg + 3 * 0x1C8 / 3
          """
          """
          type T() =
            member _.DoStuff(arg: int) =
              arg + 3 * 0o710 / 3
          """
          """
          type T() =
            member _.DoStuff(arg: int) =
              arg + 3 * 0b111001000 / 3
          """
        testList "can convert in enum" [
          check server "just value"
            """
            type MyEnum =
              | Alpha = 123
              | Beta = 456$0
              | Gamma = 789
            """
            ""
            """
            type MyEnum =
              | Alpha = 123
              | Beta = 0x1C8
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = 0o710
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = 0b111001000
              | Gamma = 789
            """
          check server "in parens"
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (456$0)
              | Gamma = 789
            """
            ""
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (0x1C8)
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (0o710)
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (0b111001000)
              | Gamma = 789
            """
          check server "in app (lhs)"
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (456$0 >>> 2)
              | Gamma = 789
            """
            ""
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (0x1C8 >>> 2)
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (0o710 >>> 2)
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (0b111001000 >>> 2)
              | Gamma = 789
            """
          check server "in app (rhs)"
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (1 <<< 456$0)
              | Gamma = 789
            """
            ""
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (1 <<< 0x1C8)
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (1 <<< 0o710)
              | Gamma = 789
            """
            """
            type MyEnum =
              | Alpha = 123
              | Beta = (1 <<< 0b111001000)
              | Gamma = 789
            """
        ]
        check server "can convert in pattern"
          """
          let f arg =
            match arg with
            | 123 -> 1
            | 456$0 -> 2
            | 789 -> 3
            | _ -> -1
          """
          ""
          """
          let f arg =
            match arg with
            | 123 -> 1
            | 0x1C8 -> 2
            | 789 -> 3
            | _ -> -1
          """
          """
          let f arg =
            match arg with
            | 123 -> 1
            | 0o710 -> 2
            | 789 -> 3
            | _ -> -1
          """
          """
          let f arg =
            match arg with
            | 123 -> 1
            | 0b111001000 -> 2
            | 789 -> 3
            | _ -> -1
          """
        check server "can convert with measure"
          """
          [<Measure>] type km 
          let n = 456$0<km>
          """
          ""
          """
          [<Measure>] type km 
          let n = 0x1C8<km>
          """
          """
          [<Measure>] type km 
          let n = 0o710<km>
          """
          """
          [<Measure>] type km 
          let n = 0b111001000<km>
          """
      ]
          
      checkAllJourneys server "does not trigger for invalid int"
        // Value for invalid `SynConst` is always `0` -> cannot convert
        "let n = {number}"
        (JustSource "1099511627775")
        (JustSource "0xFFFFFFFFFF")
        (JustSource "0o17777777777777")
        (JustSource "0b1111111111111111111111111111111111111111")

      testCaseAsync "does not trigger on comment after constant" <|
        CodeFix.checkNotApplicable server
          "let n = 123 // some$0 comment"
          Diagnostics.acceptAll
          (selectIntCodeFix Base.Hexadecimal)

      testList "different upper-lower-cases in bases" [
        testList "hexadecimal" [
          testCaseAsync "0x" <|
            CodeFix.checkApplicable server
              "let n = 0x123$0"
              Diagnostics.acceptAll
              (selectIntCodeFix Base.Decimal)
          testCaseAsync "0X" <|
            CodeFix.checkApplicable server
              "let n = 0X123$0"
              Diagnostics.acceptAll
              (selectIntCodeFix Base.Decimal)
        ]
        testList "octal" [
          testCaseAsync "0o" <|
            CodeFix.checkApplicable server
              "let n = 0o443$0"
              Diagnostics.acceptAll
              (selectIntCodeFix Base.Decimal)
          testCaseAsync "0O" <|
            CodeFix.checkApplicable server
              "let n = 0O443$0"
              Diagnostics.acceptAll
              (selectIntCodeFix Base.Decimal)
        ]
        testList "binary" [
          testCaseAsync "0b" <|
            CodeFix.checkApplicable server
              "let n = 0b100100011$0"
              Diagnostics.acceptAll
              (selectIntCodeFix Base.Decimal)
          testCaseAsync "0B" <|
            CodeFix.checkApplicable server
              "let n = 0B100100011$0"
              Diagnostics.acceptAll
              (selectIntCodeFix Base.Decimal)
        ]
      ]
    ])

  module Float =
    let tests state =
      serverTestList "Convert float-number in Hex/Oct/Bin to other bases" state defaultConfigDto None (fun server -> [
        // Note: No Decimal: cannot be represented as Hex/Oct/Bin

        let checkAll 
          server
          name template
          (hexadecimalNumber: String)
          (octalNumber: String)
          (binaryNumber: String)
          =
          checkAllJourneys server name template
            (Neither)
            (InOut hexadecimalNumber)
            (InOut octalNumber)
            (InOut binaryNumber)

        testList "can convert pi" [
          // let value = Math.PI in let bits = BitConverter.DoubleToUInt64Bits(value) in [ $"0x%X{bits}LF"; $"0o%o{bits}LF"; $"0b%B{bits}LF" ];;
          checkAll server "float"
            "let n = {number}"
            "0x400921FB54442D18LF"
            "0o400111037552421026430LF"
            "0b100000000001001001000011111101101010100010001000010110100011000LF"
          // let value = MathF.PI in let bits = BitConverter.SingleToUInt32Bits(value) in [ $"0x%X{bits}lf"; $"0o%o{bits}lf"; $"0b%B{bits}lf" ];;
          checkAll server "float32"
            "let n = {number}"
            "0x40490FDBlf"
            "0o10022207733lf"
            "0b1000000010010010000111111011011lf"
        ]
        testList "can convert 0" [
          checkAll server "float"
            "let n = {number}"
            "0x0LF"
            "0o0LF"
            "0b0LF"
          checkAll server "float32"
            "let n = {number}"
            "0x0lf"
            "0o0lf"
            "0b0lf"
        ]
        testList "can convert -pi" [
          checkAll server "float"
            "let n = {number}"
            "0xC00921FB54442D18LF"
            "0o1400111037552421026430LF"
            "0b1100000000001001001000011111101101010100010001000010110100011000LF"
          checkAll server "float32"
            "let n = {number}"
            "0xC0490FDBlf"
            "0o30022207733lf"
            "0b11000000010010010000111111011011lf"

          testList "keep existing `-`" [
            checkAll server "float"
              "let n = {number}"
              "-0x400921FB54442D18LF"
              "-0o400111037552421026430LF"
              "-0b100000000001001001000011111101101010100010001000010110100011000LF"
            checkAll server "float32"
              "let n = {number}"
              "-0x40490FDBlf"
              "-0o10022207733lf"
              "-0b1000000010010010000111111011011lf"
          ]
        ]

        testList "can convert MaxValue" [
          checkAll server "float"
            "let n = {number}"
            "0x7FEFFFFFFFFFFFFFLF"
            "0o777577777777777777777LF"
            "0b111111111101111111111111111111111111111111111111111111111111111LF"
          checkAll server "float32"
            "let n = {number}"
            "0x7F7FFFFFlf"
            "0o17737777777lf"
            "0b1111111011111111111111111111111lf"
        ]
        testList "can convert MinValue" [
          checkAll server "float"
            "let n = {number}"
            "0xFFEFFFFFFFFFFFFFLF"
            "0o1777577777777777777777LF"
            "0b1111111111101111111111111111111111111111111111111111111111111111LF"
          checkAll server "float32"
            "let n = {number}"
            "0xFF7FFFFFlf"
            "0o37737777777lf"
            "0b11111111011111111111111111111111lf"
          
          testList "keep existing `-`" [
            // Note: unlike int numbers: float is symmetric: `MinValue = - MaxValue` -> just negative bit changed
            checkAll server "float"
              "let n = {number}"
              "-0x7FEFFFFFFFFFFFFFLF"
              "-0o777577777777777777777LF"
              "-0b111111111101111111111111111111111111111111111111111111111111111LF"
            checkAll server "float32"
              "let n = {number}"
              "-0x7F7FFFFFlf"
              "-0o17737777777lf"
              "-0b1111111011111111111111111111111lf"
          ]
        ]

        testList "can convert nan" [
          // `nan`, `nanf`
          checkAll server "float - nan"
            "let n = {number}"
            "0xFFF8000000000000LF"
            "0o1777700000000000000000LF"
            "0b1111111111111000000000000000000000000000000000000000000000000000LF"
          checkAll server "float32 - nanf"
            "let n = {number}"
            "0xFFC00000lf"
            "0o37760000000lf"
            "0b11111111110000000000000000000000lf"

          // `nan` that are different from default F# `nan` (-> tests above)
          checkAll server "float - different nan"
            "let n = {number}"
            "0xFFF800C257000000LF"
            "0o1777700014112700000000LF"
            "0b1111111111111000000000001100001001010111000000000000000000000000LF"
          checkAll server "float32 -- different nan"
            "let n = {number}"
            "0xFFC00000lf"
            "0o37760000000lf"
            "0b11111111110000000000000000000000lf"

        ]
        testList "can convert infinity" [
          testList "+" [
            checkAll server "float"
              "let n = {number}"
              "0x7FF0000000000000LF"
              "0o777600000000000000000LF"
              "0b111111111110000000000000000000000000000000000000000000000000000LF"
            checkAll server "float32"
              "let n = {number}"
              "0x7F800000lf"
              "0o17740000000lf"
              "0b1111111100000000000000000000000lf"
          ]
          testList "-" [
            checkAll server "float"
              "let n = {number}"
              "0xFFF0000000000000LF"
              "0o1777600000000000000000LF"
              "0b1111111111110000000000000000000000000000000000000000000000000000LF"
            checkAll server "float32"
              "let n = {number}"
              "0xFF800000lf"
              "0o37740000000lf"
              "0b11111111100000000000000000000000lf"
          ]
        ]
      ])

module private ConvertCharToOtherForm =
  let private tryExtractChar (title: String) =
    let (start, fin) = "Convert to `", "`"
    if title.StartsWith start && title.EndsWith fin then
      let c = title.Substring(start.Length, title.Length - start.Length - fin.Length).ToString()
      let c =
        if c.Length > 3 && c.StartsWith "'" && c.EndsWith "'B" then
          // byte char (only when converting from int to char representation. Otherwise no `B` suffix in title)
          c.Substring(1, c.Length - 2)
        else
          c
      c
      |> Some
    else
      None
  let private extractFormat (char: String) =
    if char.StartsWith "\\u" then
      CharFormat.Utf16Hexadecimal
    elif char.StartsWith "\\U" then
      CharFormat.Utf32Hexadecimal
    elif char.StartsWith "\\x" then
      CharFormat.Hexadecimal
    elif char.Length >= 2 && char[0] = '\\' && Char.IsDigit char[1] then
      CharFormat.Decimal
    else
      CharFormat.Char
  let private tryExtractCharAndFormat (title: String) =
    tryExtractChar title
    |> Option.map (fun c -> c, extractFormat c)
    
  let selectCharCodeFix (format: CharFormat) =
    let f (a: CodeAction) =
      a.Title
      |> tryExtractCharAndFormat 
      |> Option.map (snd >> (=) format)
      |> Option.defaultValue false
    CodeFix.matching f

  let private checkFormat
    doc
    (source: String, cursor: Range)
    (format: CharFormat)
    expected
    =
    let name = 
      if String.IsNullOrWhiteSpace expected then
        $"cannot convert to {format}"
      else
        $"can convert to {format}"
    testCaseAsync name (async {
      let! (doc, diags) = doc
      let expected =
        if String.IsNullOrWhiteSpace expected then
          ExpectedResult.NotApplicable
        else
          ExpectedResult.After expected
      do! checkFixAt
            (doc, diags) 
            (source, cursor) 
            Diagnostics.acceptAll
            (selectCharCodeFix (format))
            expected
    })

  let check
    server
    name
    (beforeWithCursor: String)
    (expectedChar: String)
    (expectedDecimal: String)
    (expectedHexadecimal: String)
    (expectedUtf16Hexadecimal: String)
    (expectedUtf32Hexadecimal: String)
    =
    let (cursor, source) = Cursor.assertExtractRange beforeWithCursor
    documentTestList name server (Server.createUntitledDocument source) (fun doc -> [
      checkFormat doc (source, cursor) (CharFormat.Char) expectedChar
      checkFormat doc (source, cursor) (CharFormat.Decimal) expectedDecimal
      checkFormat doc (source, cursor) (CharFormat.Hexadecimal) expectedHexadecimal
      checkFormat doc (source, cursor) (CharFormat.Utf16Hexadecimal) expectedUtf16Hexadecimal
      checkFormat doc (source, cursor) (CharFormat.Utf32Hexadecimal) expectedUtf32Hexadecimal
    ])
  /// in `template`: use `{char}` as placeholder
  let private checkAll
    server
    name
    (template: String)
    (charValue: String)
    (decimalValue: String)
    (hexadecimalValue: String)
    (utf16HexadecimalValue: String)
    (utf32HexadecimalValue: String)
    =
    let applyTemplate cursor number =
      let number =
        if cursor then
          number + "$0"
        else
          number
      template.Replace("{char}", number)

    testList name [
      let data = [
        CharFormat.Char, charValue
        CharFormat.Decimal, decimalValue
        CharFormat.Hexadecimal, hexadecimalValue
        CharFormat.Utf16Hexadecimal, utf16HexadecimalValue
        CharFormat.Utf32Hexadecimal, utf32HexadecimalValue
      ]
      let valueOf (format) = 
        data
        |> List.find (fun (b,_) -> b = format)
        |> snd
      for (format, value) in data do
        if String.IsNullOrEmpty value then
          ()
        else
          let mkExpected (f) =
            if format = f || String.IsNullOrEmpty (valueOf f) then 
              ""
            else
              applyTemplate false (valueOf f)
          check server $"can convert from {format}"
            (applyTemplate true value)
            (mkExpected CharFormat.Char)
            (mkExpected CharFormat.Decimal)
            (mkExpected CharFormat.Hexadecimal)
            (mkExpected CharFormat.Utf16Hexadecimal)
            (mkExpected CharFormat.Utf32Hexadecimal)
    ]
      
  let tests state =
    serverTestList "Convert char" state defaultConfigDto None (fun server -> [
      checkAll server "can convert ç"
        "let c = '{char}'"
        "ç"
        "\\231"
        "\\xE7"
        "\\u00E7"
        "\\U000000E7"
      checkAll server "can convert \\n"
        "let c = '{char}'"
        "\\n"
        "\\010"
        "\\x0A"
        "\\u000A"
        "\\U0000000A"
      checkAll server "can convert \\000 except to char"
        "let c = '{char}'"
        ""
        "\\000"
        "\\x00"
        "\\u0000"
        "\\U00000000"

      checkAll server "can convert \\u2248 only to formats that are big enough"
        "let c = '{char}'"
        "≈"
        ""
        ""
        "\\u2248"
        "\\U00002248"

      checkAll server "can convert single quotation mark"
        "let c = '{char}'"
        "\\\'"
        "\\039"
        "\\x27"
        "\\u0027"
        "\\U00000027"
      
      checkAll server "can convert unescaped double quotation mark"
        "let c = '{char}'"
        "\""
        "\\034"
        "\\x22"
        "\\u0022"
        "\\U00000022"
      // Note: Just check from `'"` to number forms. 
      //       Other directions produce unescaped quotation mark
      //       -> Handled in test above
      check server "can convert escaped double quotation mark"
        "let c = '\"$0'"
        "" 
        "let c = '\\034'"
        "let c = '\\x22'"
        "let c = '\\u0022'"
        "let c = '\\U00000022'"

      testList "byte" [
        let checkAll
          server
          name
          (template: String)
          (charValue: String)
          (decimalValue: String)
          (hexadecimalValue: String)
          (utf16HexadecimalValue: String)
          (utf32HexadecimalValue: String)
          =
          // Note: `\x` & `\U` are currently not supported for byte char
          //TODO: change once supported was added
          checkAll server name template
            charValue
            decimalValue
            ""
            utf16HexadecimalValue
            ""

        checkAll server "can convert f"
          "let c = '{char}'B"
          "f"
          "\\102"
          "\\x66"
          "\\u0066"
          "\\U00000066"
        checkAll server "can convert \\n"
          "let c = '{char}'B"
          "\\n"
          "\\010"
          "\\x0A"
          "\\u000A"
          "\\U0000000A"
        checkAll server "can convert \\000 except to char"
          "let c = '{char}'B"
          ""
          "\\000"
          "\\x00"
          "\\u0000"
          "\\U00000000"
        check server "does not trigger for char outside of byte range"
          "let c = 'ç$0'B"
          "" "" "" "" ""
      ]
    ])

module private ConvertByteBetweenIntAndChar =
  let tests state =
    serverTestList "Convert Byte between Int And Char" state defaultConfigDto None (fun server -> [
      let template = sprintf "let c = %s"
      let charTemplate (c: string) = template $"'%s{c}'B"
      ConvertCharToOtherForm.check server "can convert from int to char"
        (template "102$0uy")
        (charTemplate "f")
        (charTemplate "\\102")
        ""// (charTemplate "\\x66")
        (charTemplate "\\u0066")
        ""// (charTemplate "\\U00000066")

      let template = sprintf "let c = %s"
      let intTemplate (c: string) = template $"%s{c}uy"
      ConvertIntToOtherBase.check server "can convert from char to int"
        (template "'f$0'B")
        (intTemplate "102")
        (intTemplate "0x66")
        (intTemplate "0o146")
        (intTemplate "0b1100110")

      testCaseAsync "cannot convert from int > 127 to char" <|
        CodeFix.checkNotApplicable server
          "let c = 250$0uy"
          Diagnostics.acceptAll
          (ConvertCharToOtherForm.selectCharCodeFix CharFormat.Char)
      testCaseAsync "cannot convert from char > 127 to int" <|
        CodeFix.checkNotApplicable server
          "let c = 'ú$0'B;"
          Diagnostics.acceptAll
          (ConvertIntToOtherBase.selectIntCodeFix Base.Decimal)
    ])

module private AddDigitGroupSeparator =
  let private intTests state =
    serverTestList "To int numbers" state defaultConfigDto None (fun server -> [
      testCaseAsync "can add separator to long decimal int" <|
        CodeFix.check server
          "let value = 1234567890$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
          "let value = 1_234_567_890"
      testCaseAsync "cannot add separator short decimal int" <|
        CodeFix.checkNotApplicable server
          "let value = 123$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
      testCaseAsync "cannot add separator to decimal int with existing separator" <|
        CodeFix.checkNotApplicable server
          "let value = 123456789_0$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
      testCaseAsync "can add separator to long negative decimal int" <|
        CodeFix.check server
          "let value = -1234567890$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
          "let value = -1_234_567_890"
      testCaseAsync "can add separator to decimal int with leading zeros" <|
        CodeFix.check server
          "let value = 0000000090$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
          "let value = 0_000_000_090"
      testCaseAsync "can add separator to too-long decimal int" <|
        CodeFix.check server
          "let value = 12345678901234567890$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
          "let value = 12_345_678_901_234_567_890"
      testCaseAsync "can add separator to long decimal int64" <|
        CodeFix.check server
          "let value = 12345678901234567L$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
          "let value = 12_345_678_901_234_567L"

      testList "can add separator to hexadecimal int" [
        testCaseAsync "words" <|
          CodeFix.check server
            "let value = 0x1234578$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Separate.hexadecimal4)
            "let value = 0x123_4578"
        testCaseAsync "bytes" <|
          CodeFix.check server
            "let value = 0x1234578$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Separate.hexadecimal2)
            "let value = 0x1_23_45_78"
      ]
      testCaseAsync "can add separator to octal int" <|
        CodeFix.check server
          "let value = 0o1234567$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.octal3)
          "let value = 0o1_234_567"
      testList "can add separator to binary int" [
        testCaseAsync "nibbles" <|
          CodeFix.check server
            "let value = 0b1010101010101010101$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Separate.binary4)
            "let value = 0b101_0101_0101_0101_0101"
        testCaseAsync "bytes" <|
          CodeFix.check server
            "let value = 0b1010101010101010101$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Separate.binary8)
            "let value = 0b101_01010101_01010101"
      ]
      testCaseAsync "can add separator to bigint" <|
        CodeFix.check server
          "let value = 9999999999999999999999999999$0I"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
          "let value = 9_999_999_999_999_999_999_999_999_999I"

      testCaseAsync "does not trigger for short number" <|
        CodeFix.checkNotApplicable server
          "let value = 123$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Int.Separate.decimal3)
    ])

  let private floatTests state =
    serverTestList "To float numbers" state defaultConfigDto None (fun server -> [
      testCaseAsync "can add separator to X.X float" <|
        CodeFix.check server
          "let value = 1234567.01234567$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567.012_345_67"
      testCaseAsync "can add separator to X.XeX float" <|
        CodeFix.check server
          "let value = 1234567.01234567e12345678$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567.012_345_67e12_345_678"
      testCaseAsync "can add separator to X. float" <|
        CodeFix.check server
          "let value = 1234567.$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567."
      testCaseAsync "can add separator to XeX float" <|
        CodeFix.check server
          "let value = 1234567e12345678$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567e12_345_678"

      testCaseAsync "can add separator to float32" <|
        CodeFix.check server
          "let value = 1234567.01234567f$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567.012_345_67f"
      testCaseAsync "can add separator to decimal" <|
        CodeFix.check server
          "let value = 1234567.01234567m$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567.012_345_67m"

      testCaseAsync "keep sign" <|
        CodeFix.check server
          "let value = -1234567.01234567e12345678$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = -1_234_567.012_345_67e12_345_678"
      testCaseAsync "keep sign for exponent" <|
        CodeFix.check server
          "let value = 1234567.01234567e+12345678$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567.012_345_67e+12_345_678"

      testCaseAsync "cannot add separator when existing separator" <|
        CodeFix.checkNotApplicable server
          "let value = 1234567.0123_4567$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)

      testCaseAsync "does not trigger for short number" <|
        CodeFix.checkNotApplicable server
          "let value = 123.012e123$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)

      testCaseAsync "can add separator to just decimal part when other parts are too short" <|
        CodeFix.check server
          "let value = 123.01234567e+123$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 123.012_345_67e+123"
      testCaseAsync "can add separator to just int part when other parts are too short" <|
        CodeFix.check server
          "let value = 1234567.012e+123$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 1_234_567.012e+123"
      testCaseAsync "can add separator to just exponent part when other parts are too short" <|
        CodeFix.check server
          "let value = 123.012e+1234567$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 123.012e+1_234_567"
      testCaseAsync "can add separator to decimal & exponent parts when int part is too short" <|
        CodeFix.check server
          "let value = 123.012345678e+1234567$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle Title.Float.Separate.all3)
          "let value = 123.012_345_678e+1_234_567"
    ])

  let tests state =
    testList "Add Digit Group Separator" [
      intTests state
      floatTests state
    ]

module private ReplaceWithName =
  /// Note: `System` is `open`
  let checkReplaceWith server tyName value fieldName =
    let replacement = $"{tyName}.{fieldName}"
    CodeFix.check server
      $"open System\nlet value = {value}$0"
      Diagnostics.acceptAll
      (CodeFix.withTitle (Title.replaceWith replacement))
      $"open System\nlet value = {replacement}"
  let checkCannotReplaceWith server tyName value fieldName =
    let replacement = $"{tyName}.{fieldName}"
    CodeFix.checkNotApplicable server
      $"open System\nlet value = {value}$0"
      Diagnostics.acceptAll
      (CodeFix.withTitle (Title.replaceWith replacement))

  let private intTests state =
    serverTestList "Replace Int" state defaultConfigDto None (fun server -> [
      let checkReplaceWith = checkReplaceWith server
      let checkCannotReplaceWith = checkCannotReplaceWith server

      /// Formats with suffix
      let inline format value = sprintf "%A" value

      testList "can replace SByte" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.SByte) (format SByte.MaxValue) (nameof System.SByte.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.SByte) (format SByte.MinValue) (nameof(System.SByte.MinValue))
      ]
      testList "can replace Byte" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.Byte) (format Byte.MaxValue) (nameof System.Byte.MaxValue)
        testCaseAsync "not with MinValue" <|
          checkCannotReplaceWith (nameof System.Byte) (format Byte.MinValue) (nameof(System.Byte.MinValue))
      ]
      testList "can replace Int16" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.Int16) (format Int16.MaxValue) (nameof System.Int16.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.Int16) (format Int16.MinValue) (nameof(System.Int16.MinValue))
      ]
      testList "can replace UInt16" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.UInt16) (format UInt16.MaxValue) (nameof System.UInt16.MaxValue)
        testCaseAsync "not with MinValue" <|
          checkCannotReplaceWith (nameof System.UInt16) (format UInt16.MinValue) (nameof(System.UInt16.MinValue))
      ]
      testList "can replace Int32" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.Int32) (format Int32.MaxValue) (nameof System.Int32.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.Int32) (format Int32.MinValue) (nameof(System.Int32.MinValue))
      ]
      testList "can replace UInt32" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.UInt32) (format UInt32.MaxValue) (nameof System.UInt32.MaxValue)
        testCaseAsync "not with MinValue" <|
          checkCannotReplaceWith (nameof System.UInt32) (format UInt32.MinValue) (nameof(System.UInt32.MinValue))
      ]
      testList "can replace NativeInt" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.IntPtr) (format IntPtr.MaxValue) (nameof System.IntPtr.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.IntPtr) (format IntPtr.MinValue) (nameof(System.IntPtr.MinValue))
      ]
      testList "can replace UNativeInt" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.UIntPtr) (format UIntPtr.MaxValue) (nameof System.UIntPtr.MaxValue)
        testCaseAsync "not with MinValue" <|
          checkCannotReplaceWith (nameof System.UIntPtr) (format UIntPtr.MinValue) (nameof(System.UIntPtr.MinValue))
      ]
      testList "can replace Int64" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.Int64) (format Int64.MaxValue) (nameof System.Int64.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.Int64) (format Int64.MinValue) (nameof(System.Int64.MinValue))
      ]
      testList "can replace UInt64" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.UInt64) (format UInt64.MaxValue) (nameof System.UInt64.MaxValue)
        testCaseAsync "not with MinValue" <|
          checkCannotReplaceWith (nameof System.UInt64) (format UInt64.MinValue) (nameof(System.UInt64.MinValue))
      ]

      testCaseAsync "Emit leading System if System not open" <|
        CodeFix.check server
          $"let value = {format Int32.MaxValue}$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle (Title.replaceWith "Int32.MaxValue"))
          $"let value = System.Int32.MaxValue"
    ])

  let private floatTests state =
    serverTestList "Replace Float" state defaultConfigDto None (fun server -> [
      // Beware of rounding in number printing!
      // For example:
      // ```fsharp
      // > Double.MaxValue;;
      // val it: float = 1.797693135e+308
      // > 1.797693135e+308;;
      // val it: float = infinity

      // > Double.MaxValue.ToString();;
      // val it: string = "1.7976931348623157E+308"
      // ```

      let checkReplaceWith = checkReplaceWith server
      let checkCannotReplaceWith = checkCannotReplaceWith server
      let checkReplaceWith' value name =
        CodeFix.check server
          $"let value = {value}$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle (Title.replaceWith name))
          $"let value = {name}"

      testList "can replace float" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.Double) "1.7976931348623157E+308" (nameof System.Double.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.Double) "-1.7976931348623157E+308" (nameof System.Double.MinValue)
        testCaseAsync "with Epsilon" <|
          checkReplaceWith (nameof System.Double) "5E-324" (nameof System.Double.Epsilon)
        testCaseAsync "with infinity" <|
          checkReplaceWith' "123456789e123456789" "infinity"
        testCaseAsync "with infinity (int)" <|
          checkReplaceWith' "0x7FF0000000000000LF" "infinity"
        testCaseAsync "with -infinity" <|
          checkReplaceWith' "-123456789e123456789" "-infinity"
        testCaseAsync "with -infinity (int)" <|
          checkReplaceWith' "0o1777600000000000000000LF" "-infinity"
        testCaseAsync "with nan (int)" <|
          checkReplaceWith' "0b1111111111111000000100010001010010010010001000100010001000100100LF" "nan"
      ]
      testList "can replace float32" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.Single) "3.4028235E+38f" (nameof System.Single.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.Single) "-3.4028235E+38f" (nameof System.Single.MinValue)
        testCaseAsync "with Epsilon" <|
          checkReplaceWith (nameof System.Single) "1.401298464e-45f" (nameof System.Single.Epsilon)
        testCaseAsync "with infinity" <|
          checkReplaceWith' "123456789e123456789f" "infinityf"
        testCaseAsync "with infinity (int)" <|
          checkReplaceWith' "0x7F800000lf" "infinityf"
        testCaseAsync "with -infinity" <|
          checkReplaceWith' "-123456789e123456789f" "-infinityf"
        testCaseAsync "with -infinity (int)" <|
          checkReplaceWith' "0o37740000000lf" "-infinityf"
        testCaseAsync "with nan (int)" <|
          checkReplaceWith' "0b1111111101001000100100100100100lf" "nanf"
      ]

      testCaseAsync "Emit leading System if System not open" <|
        CodeFix.check server
          $"let value = 1.7976931348623157E+308$0"
          Diagnostics.acceptAll
          (CodeFix.withTitle (Title.replaceWith "Double.MaxValue"))
          $"let value = System.Double.MaxValue"

      testList "can replace decimal" [
        testCaseAsync "with MaxValue" <|
          checkReplaceWith (nameof System.Decimal) "79228162514264337593543950335m" (nameof System.Decimal.MaxValue)
        testCaseAsync "with MinValue" <|
          checkReplaceWith (nameof System.Decimal) "-79228162514264337593543950335m" (nameof System.Decimal.MinValue)
      ]

    ])
  let tests state = 
    testList "Replace With Name" [
      intTests state
      floatTests state
    ]

module SignHelpers =
  let tests state =
    serverTestList "Sign Helpers" state defaultConfigDto None (fun server -> [
      testList "extract `-`" [
        testCaseAsync "from bin int" <|
          CodeFix.check server
            "let value = 0b10000101y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.extractMinusFromNegativeConstant)
            "let value = -0b1111011y"
        testCaseAsync "from hex int" <|
          CodeFix.check server
            "let value = 0x85y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.extractMinusFromNegativeConstant)
            "let value = -0x7By"
        testCaseAsync "from oct int" <|
          CodeFix.check server
            "let value = 0o205y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.extractMinusFromNegativeConstant)
            "let value = -0o173y"
        testCaseAsync "does not trigger for decimal int" <|
          CodeFix.checkNotApplicable server
            "let value = -123y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.extractMinusFromNegativeConstant)
      ]
      testList "integrate `-`" [
        testCaseAsync "into bin int" <|
          CodeFix.check server
            "let value = -0b1111011y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.integrateExplicitMinus)
            "let value = 0b10000101y"
        testCaseAsync "into hex int" <|
          CodeFix.check server
            "let value = -0x7By$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.integrateExplicitMinus)
            "let value = 0x85y"
        testCaseAsync "into oct int" <|
          CodeFix.check server
            "let value = -0o173y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.integrateExplicitMinus)
            "let value = 0o205y"
        testCaseAsync "does not trigger for decimal int" <|
          CodeFix.checkNotApplicable server
            "let value = -123y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.integrateExplicitMinus)
      ]

      testList "MinValue" [
        testCaseAsync "can remove explicit `-`" <|
          CodeFix.check server
            "let value = -0b10000000y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.removeExplicitMinusWithMinValue)
            "let value = 0b10000000y"
        testCaseAsync "does not trigger for decimal int" <|
          CodeFix.checkNotApplicable server
            "let value = -127y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.removeExplicitMinusWithMinValue)
      ]

      testList "use implicit `+`" [
        testCaseAsync "can change to positive" <|
          CodeFix.check server
            "let value = -0b1111_1101y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.useImplicitPlusInPositiveConstantWithMinusSign)
            "let value = 0b11y"
      ]

      testList "ensure valid sign" [
        // QuickFixes might add sign which might lead to invalid code:
        // ```fsharp
        // //             -91y
        // let value = 5y+0b1010_0101y

        // // => Convert to decimal

        // let value = 5y+-91y
        // //            ^^
        // //            The type 'sbyte' does not support the operator '+-'
        // ```
        //
        // -> insert space before sign if necessary

        testCaseAsync "add space when new `-` sign immediately after `+`" <|
          CodeFix.check server
            "let value = 5y+0b1010_0101y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.toDecimal)
            "let value = 5y+ -91y"
        testCaseAsync "don't add space when `-` with space before" <|
          CodeFix.check server
            "let value = 5y+ 0b1010_0101y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.toDecimal)
            "let value = 5y+ -91y"
        testCaseAsync "don't add space when new `-` sign immediately after `(`" <|
          CodeFix.check server
            "let value = 5y+(0b1010_0101y$0)"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.toDecimal)
            "let value = 5y+(-91y)"
        testCaseAsync "add space when new `-` sign immediately after `<|`" <|
          CodeFix.check server
            "let value = max 5y <|0b1010_0101y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.toDecimal)
            "let value = max 5y <| -91y"
        testCaseAsync "don't add space when no new `-` sign" <|
          CodeFix.check server
            "let value = 5y+0b1011011y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.toDecimal)
            "let value = 5y+91y"

        testCaseAsync "add space when convert to other base" <|
          CodeFix.check server
            "let value = 5y+0b1010_0101y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.toDecimal)
            "let value = 5y+ -91y"
        testCaseAsync "add space when extract `-`" <|
          CodeFix.check server
            "let value = 5y+0b10000101y$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle Title.Int.Convert.SpecialCase.extractMinusFromNegativeConstant)
            "let value = 5y+ -0b1111011y"

        testCaseAsync "add space when convert to `-infinity`" <|
          CodeFix.check server
            "let value = 5.0+0o1777600000000000000000LF$0"
            Diagnostics.acceptAll
            (CodeFix.withTitle (Title.replaceWith "-infinity"))
            "let value = 5.0+ -infinity"
      ]
    ])

let tests state =
  testList (nameof AdjustConstant) [
    ConvertIntToOtherBase.tests state
    ConvertIntToOtherBase.Float.tests state
    ConvertCharToOtherForm.tests state
    ConvertByteBetweenIntAndChar.tests state
    
    ReplaceWithName.tests state
    SignHelpers.tests state

    AddDigitGroupSeparator.tests state
  ]
