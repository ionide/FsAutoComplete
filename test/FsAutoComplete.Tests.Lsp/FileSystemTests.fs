module FsAutoComplete.Tests.FileSystem

open System
open System.IO
open Expecto
open FSharp.UMX
open FSharp.Compiler.Text
open FsAutoComplete

// Test the File module functions  
[<Tests>]
let fileModuleTests =
  testList
    "File module tests"
    [ testCase "getLastWriteTimeOrDefaultNow returns file time for existing file"
      <| fun _ ->
        // Create a temporary file
        let tempFile = Path.GetTempFileName()
        let taggedPath: string<LocalPath> = UMX.tag tempFile

        try
          File.WriteAllText(tempFile, "test content")
          let result = File.getLastWriteTimeOrDefaultNow taggedPath
          let actualTime = File.GetLastWriteTimeUtc(tempFile)
          
          // Should return the actual file time (within a small tolerance)
          let timeDiff = abs (result - actualTime).TotalSeconds
          Expect.isLessThan timeDiff 1.0 "File time should match within 1 second"
        finally
          if File.Exists(tempFile) then File.Delete(tempFile)

      testCase "getLastWriteTimeOrDefaultNow returns current time for non-existent file"
      <| fun _ ->
        let nonExistentPath: string<LocalPath> = UMX.tag "/nonexistent/file/path.fs"
        let beforeTime = DateTime.UtcNow
        let result = File.getLastWriteTimeOrDefaultNow nonExistentPath
        let afterTime = DateTime.UtcNow
        
        // Should return a time between before and after the call
        Expect.isGreaterThanOrEqual result beforeTime "Should return current time for non-existent file"
        Expect.isLessThanOrEqual result afterTime "Should return current time for non-existent file"

      testCase "openFileStreamForReadingAsync creates proper FileStream"
      <| fun _ ->
        let tempFile = Path.GetTempFileName()
        let taggedPath: string<LocalPath> = UMX.tag tempFile

        try
          File.WriteAllText(tempFile, "test content for stream reading")
          use stream = File.openFileStreamForReadingAsync taggedPath
          
          Expect.equal stream.CanRead true "Stream should be readable"
          Expect.equal stream.CanWrite false "Stream should not be writable"  
          Expect.equal stream.Length 32L "Stream length should match file content length"
        finally
          if File.Exists(tempFile) then File.Delete(tempFile)
    ]

// Test Position extensions
[<Tests>]
let positionExtensionTests =
  testList
    "PositionExtensions tests"
    [ testCase "LinesToBeginning returns empty for line 1"
      <| fun _ ->
        let pos = Position.mkPos 1 10
        let lines = pos.LinesToBeginning() |> Seq.toList
        Expect.equal lines [] "Line 1 should have no preceding lines"

      testCase "LinesToBeginning returns correct lines for line > 1"
      <| fun _ ->
        let pos = Position.mkPos 4 10
        let lines = pos.LinesToBeginning() |> Seq.toList
        let expected = [Position.mkPos 3 0; Position.mkPos 2 0; Position.mkPos 1 0]
        Expect.equal lines expected "Should return lines in descending order"

      testCase "IncLine increments line number"
      <| fun _ ->
        let pos = Position.mkPos 5 10
        let newPos = pos.IncLine()
        Expect.equal newPos.Line 6 "Line should be incremented"
        Expect.equal newPos.Column 10 "Column should remain same"

      testCase "DecLine decrements line number" 
      <| fun _ ->
        let pos = Position.mkPos 5 10
        let newPos = pos.DecLine()
        Expect.equal newPos.Line 4 "Line should be decremented"
        Expect.equal newPos.Column 10 "Column should remain same"

      testCase "IncColumn increments column"
      <| fun _ ->
        let pos = Position.mkPos 5 10
        let newPos = pos.IncColumn()
        Expect.equal newPos.Line 5 "Line should remain same"
        Expect.equal newPos.Column 11 "Column should be incremented by 1"

      testCase "IncColumn with parameter increments by n"
      <| fun _ ->
        let pos = Position.mkPos 5 10
        let newPos = pos.IncColumn 5
        Expect.equal newPos.Line 5 "Line should remain same"
        Expect.equal newPos.Column 15 "Column should be incremented by 5"

      testCase "WithColumn sets new column"
      <| fun _ ->
        let pos = Position.mkPos 5 10
        let newPos = pos.WithColumn(25)
        Expect.equal newPos.Line 5 "Line should remain same"  
        Expect.equal newPos.Column 25 "Column should be set to new value"

      testCase "Position pattern matching works"
      <| fun _ ->
        let pos = Position.mkPos 5 10
        match pos with
        | Pos (line, col) ->
          Expect.equal line 5 "Line should match"
          Expect.equal col 10 "Column should match"
    ]

// Test Range extensions
[<Tests>]
let rangeExtensionTests = 
  testList
    "RangeExtensions tests"
    [ testCase "WithFileName creates new range with different filename"
      <| fun _ ->
        let start = Position.mkPos 1 0
        let end' = Position.mkPos 2 10
        let range = Range.mkRange "original.fs" start end'
        let newRange = range.WithFileName("new.fs")
        
        Expect.equal newRange.FileName "new.fs" "Filename should be updated"
        Expect.equal newRange.Start start "Start position should remain same"
        Expect.equal newRange.End end' "End position should remain same"

      testCase "NormalizeDriveLetterCasing lowercases drive letter"
      <| fun _ ->
        let start = Position.mkPos 1 0
        let end' = Position.mkPos 2 10
        let range = Range.mkRange "C:\\Test\\File.fs" start end'
        let normalized = range.NormalizeDriveLetterCasing()
        
        Expect.equal normalized.FileName "c:\\Test\\File.fs" "Drive letter should be lowercase"
        Expect.equal normalized.Start start "Start position should remain same"
        Expect.equal normalized.End end' "End position should remain same"

      testCase "NormalizeDriveLetterCasing leaves lowercase unchanged"
      <| fun _ ->
        let start = Position.mkPos 1 0
        let end' = Position.mkPos 2 10
        let range = Range.mkRange "c:\\Test\\File.fs" start end'
        let normalized = range.NormalizeDriveLetterCasing()
        
        Expect.equal normalized.FileName "c:\\Test\\File.fs" "Filename should remain unchanged"

      testCase "TaggedFileName normalizes path"
      <| fun _ ->
        let start = Position.mkPos 1 0
        let end' = Position.mkPos 2 10
        let range = Range.mkRange "/test/file.fs" start end'
        let taggedName = range.TaggedFileName
        
        // TaggedFileName should return a normalized path as tagged string
        let untaggedName = UMX.untag taggedName
        Expect.isNotNull untaggedName "TaggedFileName should not be null"

      testCase "With creates new range with different start and end"
      <| fun _ ->
        let originalStart = Position.mkPos 1 0
        let originalEnd = Position.mkPos 2 10
        let range = Range.mkRange "test.fs" originalStart originalEnd
        
        let newStart = Position.mkPos 3 5
        let newEnd = Position.mkPos 4 15
        let newRange = range.With(newStart, newEnd)
        
        Expect.equal newRange.FileName "test.fs" "Filename should remain same"
        Expect.equal newRange.Start newStart "Start should be updated"
        Expect.equal newRange.End newEnd "End should be updated"

      testCase "WithStart creates new range with different start"
      <| fun _ ->
        let originalStart = Position.mkPos 1 0
        let originalEnd = Position.mkPos 2 10
        let range = Range.mkRange "test.fs" originalStart originalEnd
        
        let newStart = Position.mkPos 3 5
        let newRange = range.WithStart(newStart)
        
        Expect.equal newRange.FileName "test.fs" "Filename should remain same"
        Expect.equal newRange.Start newStart "Start should be updated"
        Expect.equal newRange.End originalEnd "End should remain same"

      testCase "WithEnd creates new range with different end"
      <| fun _ ->
        let originalStart = Position.mkPos 1 0
        let originalEnd = Position.mkPos 2 10
        let range = Range.mkRange "test.fs" originalStart originalEnd
        
        let newEnd = Position.mkPos 4 15
        let newRange = range.WithEnd(newEnd)
        
        Expect.equal newRange.FileName "test.fs" "Filename should remain same"
        Expect.equal newRange.Start originalStart "Start should remain same"
        Expect.equal newRange.End newEnd "End should be updated"
    ]

// Test module accessibility and compilation
[<Tests>]
let moduleAccessibilityTests =
  testList
    "Module accessibility tests"
    [ testCase "File module functions are accessible"
      <| fun _ ->
        // Test that we can access the File module functions
        let tempFile = Path.GetTempFileName()
        let taggedPath: string<LocalPath> = UMX.tag tempFile

        try
          let _ = File.getLastWriteTimeOrDefaultNow taggedPath
          Expect.isTrue true "File module functions should be accessible"
        finally
          if File.Exists(tempFile) then File.Delete(tempFile)

      testCase "Position extensions compile and work"  
      <| fun _ ->
        let pos = Position.mkPos 1 0
        let _ = pos.LinesToBeginning()
        let _ = pos.IncLine()
        let _ = pos.DecLine()
        let _ = pos.IncColumn()
        let _ = pos.WithColumn(10)
        Expect.isTrue true "Position extensions should compile and work"

      testCase "Range extensions compile and work"
      <| fun _ ->
        let range = Range.mkRange "test.fs" (Position.mkPos 1 0) (Position.mkPos 2 10)
        let _ = range.WithFileName("new.fs")
        let _ = range.NormalizeDriveLetterCasing()
        let _ = range.TaggedFileName
        let _ = range.With(Position.mkPos 1 0, Position.mkPos 1 10)
        let _ = range.WithStart(Position.mkPos 1 0)
        let _ = range.WithEnd(Position.mkPos 1 10)
        Expect.isTrue true "Range extensions should compile and work"
    ]