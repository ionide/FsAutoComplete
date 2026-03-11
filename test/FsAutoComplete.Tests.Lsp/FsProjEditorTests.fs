module FsAutoComplete.Tests.FsProjEditorTests

open Expecto
open FsAutoComplete
open System
open System.IO
open System.Xml

/// Create a temporary .fsproj file whose ItemGroup contains the given files
/// (one <Compile Include="..." /> per line, LF line endings).
/// Returns the path; caller must delete.
let private createTempFsproj (files: string list) =
  let path = Path.ChangeExtension(Path.GetTempFileName(), ".fsproj")

  let filesXml =
    files
    |> List.map (fun f -> $"    <Compile Include=\"{f}\" />")
    |> String.concat "\n"

  let content =
    $"<Project Sdk=\"Microsoft.NET.Sdk\">\n  <PropertyGroup>\n    <TargetFramework>net8.0</TargetFramework>\n  </PropertyGroup>\n  <ItemGroup>\n{filesXml}\n  </ItemGroup>\n</Project>"

  File.WriteAllText(path, content)
  path

/// Create a temporary .fsproj file with CRLF line endings.
let private createTempFsprojCrlf (files: string list) =
  let path = Path.ChangeExtension(Path.GetTempFileName(), ".fsproj")

  let filesXml =
    files
    |> List.map (fun f -> $"    <Compile Include=\"{f}\" />")
    |> String.concat "\r\n"

  let content =
    $"<Project Sdk=\"Microsoft.NET.Sdk\">\r\n  <PropertyGroup>\r\n    <TargetFramework>net8.0</TargetFramework>\r\n  </PropertyGroup>\r\n  <ItemGroup>\r\n{filesXml}\r\n  </ItemGroup>\r\n</Project>"

  File.WriteAllText(path, content)
  path

/// Return the ordered list of <Compile Include="..." /> values from a .fsproj.
let private getCompileOrder (fsprojPath: string) =
  let xDoc = XmlDocument()
  xDoc.Load fsprojPath

  xDoc.SelectNodes("//Compile[@Include]")
  |> Seq.cast<XmlNode>
  |> Seq.map (fun n -> n.Attributes.["Include"].InnerText)
  |> Seq.toList

/// Return true when every <Compile .../> element appears on its own line
/// (i.e. no two Compile tags share the same line in the saved file).
let private eachCompileOnOwnLine (fsprojPath: string) =
  let lines = File.ReadAllLines(fsprojPath)

  lines
  |> Array.filter (fun l -> l.Contains("<Compile"))
  |> Array.forall (fun l ->
    // A line that contains exactly one <Compile open tag is fine.
    // Count occurrences of "<Compile" – if > 1 they are horizontal.
    let occurrences =
      let mutable count = 0
      let mutable idx = 0

      while idx < l.Length do
        let pos = l.IndexOf("<Compile", idx, StringComparison.Ordinal)

        if pos < 0 then
          idx <- l.Length
        else
          count <- count + 1
          idx <- pos + 1

      count

    occurrences <= 1)

let allTests =
  testList
    "FsProjEditor"
    [ testList
        "moveFileUp"
        [ testCase "moves second file above first"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "B.fs"
              Expect.equal (getCompileOrder path) [ "B.fs"; "A.fs"; "C.fs" ] "B.fs should precede A.fs"
            finally
              File.Delete path

          testCase "moves last file up"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "C.fs"
              Expect.equal (getCompileOrder path) [ "A.fs"; "C.fs"; "B.fs" ] "C.fs should precede B.fs"
            finally
              File.Delete path

          testCase "first file is unchanged"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "A.fs"
              Expect.equal (getCompileOrder path) [ "A.fs"; "B.fs"; "C.fs" ] "Order should be unchanged"
            finally
              File.Delete path

          testCase "moving up twice reaches top"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "C.fs"
              FsProjEditor.moveFileUp path "C.fs"
              Expect.equal (getCompileOrder path) [ "C.fs"; "A.fs"; "B.fs" ] "C.fs should be first"
            finally
              File.Delete path

          testCase "single press actually changes compile order (regression: required two presses)"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "C.fs"
              let order = getCompileOrder path
              Expect.notEqual order [ "A.fs"; "B.fs"; "C.fs" ] "Compile order must change on first call"
            finally
              File.Delete path

          testCase "preserves vertical formatting with LF line endings"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "B.fs"
              Expect.isTrue (eachCompileOnOwnLine path) "Each Compile element must remain on its own line"
            finally
              File.Delete path

          testCase "preserves vertical formatting with CRLF line endings"
          <| fun _ ->
            let path = createTempFsprojCrlf [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "B.fs"
              Expect.isTrue (eachCompileOnOwnLine path) "Each Compile element must remain on its own line (CRLF)"
            finally
              File.Delete path ]

      testList
        "moveFileDown"
        [ testCase "moves first file below second"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "A.fs"
              Expect.equal (getCompileOrder path) [ "B.fs"; "A.fs"; "C.fs" ] "A.fs should follow B.fs"
            finally
              File.Delete path

          testCase "moves middle file down"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "B.fs"
              Expect.equal (getCompileOrder path) [ "A.fs"; "C.fs"; "B.fs" ] "B.fs should follow C.fs"
            finally
              File.Delete path

          testCase "last file is unchanged"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "C.fs"
              Expect.equal (getCompileOrder path) [ "A.fs"; "B.fs"; "C.fs" ] "Order should be unchanged"
            finally
              File.Delete path

          testCase "moving down twice reaches bottom"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "A.fs"
              FsProjEditor.moveFileDown path "A.fs"
              Expect.equal (getCompileOrder path) [ "B.fs"; "C.fs"; "A.fs" ] "A.fs should be last"
            finally
              File.Delete path

          testCase "single press actually changes compile order (regression: required two presses)"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "A.fs"
              let order = getCompileOrder path
              Expect.notEqual order [ "A.fs"; "B.fs"; "C.fs" ] "Compile order must change on first call"
            finally
              File.Delete path

          testCase "preserves vertical formatting with LF line endings"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "B.fs"
              Expect.isTrue (eachCompileOnOwnLine path) "Each Compile element must remain on its own line"
            finally
              File.Delete path

          testCase "preserves vertical formatting with CRLF line endings"
          <| fun _ ->
            let path = createTempFsprojCrlf [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "B.fs"
              Expect.isTrue (eachCompileOnOwnLine path) "Each Compile element must remain on its own line (CRLF)"
            finally
              File.Delete path ]

      testList
        "moveFileUp and moveFileDown roundtrip"
        [ testCase "move up then down restores original order"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileUp path "B.fs"
              FsProjEditor.moveFileDown path "B.fs"
              Expect.equal (getCompileOrder path) [ "A.fs"; "B.fs"; "C.fs" ] "Round-trip should restore original order"
            finally
              File.Delete path

          testCase "move down then up restores original order"
          <| fun _ ->
            let path = createTempFsproj [ "A.fs"; "B.fs"; "C.fs" ]

            try
              FsProjEditor.moveFileDown path "B.fs"
              FsProjEditor.moveFileUp path "B.fs"
              Expect.equal (getCompileOrder path) [ "A.fs"; "B.fs"; "C.fs" ] "Round-trip should restore original order"
            finally
              File.Delete path ] ]
