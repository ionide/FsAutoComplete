module FsAutoComplete.Tests.FsprojEditTests

open Expecto
open System
open System.IO
open System.Xml
open FsAutoComplete.FsProjEditor
open Helpers.Expecto.ShadowedTimeouts

let private createTempFsprojFile content =
  let tempPath = Path.GetTempFileName()
  File.WriteAllText(tempPath, content)
  tempPath

let private readFsprojFile path =
  let doc = XmlDocument()
  doc.Load(path: string)
  doc.OuterXml

let private sampleFsprojContent = """<?xml version="1.0" encoding="utf-8"?>
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="File1.fs" />
    <Compile Include="File2.fs" />
    <Compile Include="File3.fs" />
  </ItemGroup>
</Project>"""

let private sampleFsprojWithWindowsPaths = """<?xml version="1.0" encoding="utf-8"?>
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Folder\SubFile1.fs" />
    <Compile Include="Folder\SubFile2.fs" />
  </ItemGroup>
</Project>"""

let private emptyFsprojContent = """<?xml version="1.0" encoding="utf-8"?>
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>
</Project>"""

let fsprojEditTests =
  testSequenced <| testList "FsprojEdit Tests" [
    
    testList "addFile Tests" [
      testCase "addFile - adds new file to existing ItemGroup"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            let result = addFile tempPath "NewFile.fs"
            Expect.isOk result "Should successfully add new file"
            
            let content = readFsprojFile tempPath
            Expect.stringContains content "NewFile.fs" "Should contain the new file"
            // Should be added at the beginning of existing ItemGroup
            Expect.stringContains content """<Compile Include="NewFile.fs" />""" "Should have proper Compile element"
          finally
            File.Delete(tempPath)
      
      testCase "addFile - creates ItemGroup when none exists"
      <| fun () ->
          let tempPath = createTempFsprojFile emptyFsprojContent
          try
            let result = addFile tempPath "FirstFile.fs"
            Expect.isOk result "Should successfully add file to empty project"
            
            let content = readFsprojFile tempPath
            Expect.stringContains content "FirstFile.fs" "Should contain the new file"
            Expect.stringContains content "<ItemGroup>" "Should create ItemGroup"
          finally
            File.Delete(tempPath)
      
      testCase "addFile - returns error when file already exists"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            let result = addFile tempPath "File1.fs"
            Expect.isError result "Should return error for duplicate file"
            
            match result with
            | Error msg -> Expect.equal msg "File already included in the project" "Should have correct error message"
            | Ok _ -> failtest "Should have returned error"
          finally
            File.Delete(tempPath)
    ]
    
    testList "addFileAbove Tests" [
      testCase "addFileAbove - adds file above specified file"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            let result = addFileAbove tempPath "File2.fs" "NewFile.fs"
            Expect.isOk result "Should successfully add file above File2.fs"
            
            let content = readFsprojFile tempPath
            Expect.stringContains content "NewFile.fs" "Should contain the new file"
            
            // Check ordering by position in string
            let file1Pos = content.IndexOf("File1.fs")
            let newFilePos = content.IndexOf("NewFile.fs")
            let file2Pos = content.IndexOf("File2.fs")
            
            Expect.isLessThan newFilePos file2Pos "NewFile should come before File2"
            Expect.isLessThan file1Pos newFilePos "File1 should come before NewFile"
          finally
            File.Delete(tempPath)
      
      testCase "addFileAbove - returns error for duplicate file"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            let result = addFileAbove tempPath "File2.fs" "File3.fs"
            Expect.isError result "Should return error for duplicate file"
          finally
            File.Delete(tempPath)
    ]
    
    testList "addFileBelow Tests" [
      testCase "addFileBelow - adds file below specified file"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            let result = addFileBelow tempPath "File2.fs" "NewFile.fs"
            Expect.isOk result "Should successfully add file below File2.fs"
            
            let content = readFsprojFile tempPath
            Expect.stringContains content "NewFile.fs" "Should contain the new file"
            
            // Check ordering
            let file2Pos = content.IndexOf("File2.fs")
            let newFilePos = content.IndexOf("NewFile.fs")
            let file3Pos = content.IndexOf("File3.fs")
            
            Expect.isLessThan file2Pos newFilePos "File2 should come before NewFile"
            Expect.isLessThan newFilePos file3Pos "NewFile should come before File3"
          finally
            File.Delete(tempPath)
    ]
    
    testList "removeFile Tests" [
      testCase "removeFile - removes existing file"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            removeFile tempPath "File2.fs"
            
            let content = readFsprojFile tempPath
            Expect.isFalse (content.Contains("File2.fs")) "Should not contain removed file"
            Expect.stringContains content "File1.fs" "Should still contain File1.fs"
            Expect.stringContains content "File3.fs" "Should still contain File3.fs"
          finally
            File.Delete(tempPath)
      
      testCase "removeFile - handles non-existent file gracefully"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            removeFile tempPath "NonExistentFile.fs" // Should not throw
            let content = readFsprojFile tempPath
            // Original content should be unchanged
            Expect.stringContains content "File1.fs" "Should still contain all original files"
            Expect.stringContains content "File2.fs" "Should still contain all original files"
            Expect.stringContains content "File3.fs" "Should still contain all original files"
          finally
            File.Delete(tempPath)
      
      testCase "removeFile - handles different path separators"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojWithWindowsPaths
          try
            // Remove with Unix-style path separator
            removeFile tempPath "Folder/SubFile1.fs"
            
            let content = readFsprojFile tempPath
            Expect.isFalse (content.Contains("SubFile1.fs")) "Should remove file with Windows path"
            Expect.stringContains content "SubFile2.fs" "Should still contain SubFile2.fs"
          finally
            File.Delete(tempPath)
    ]
    
    testList "renameFile Tests" [
      testCase "renameFile - renames existing file"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            renameFile tempPath "File2.fs" "RenamedFile.fs"
            
            let content = readFsprojFile tempPath
            Expect.isFalse (content.Contains("File2.fs")) "Should not contain old filename"
            Expect.stringContains content "RenamedFile.fs" "Should contain new filename"
            Expect.stringContains content "File1.fs" "Should still contain other files"
            Expect.stringContains content "File3.fs" "Should still contain other files"
          finally
            File.Delete(tempPath)
    ]
    
    testList "moveFileUp Tests" [
      testCase "moveFileUp - moves file up in order"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            moveFileUp tempPath "File2.fs"
            
            let content = readFsprojFile tempPath
            let file1Pos = content.IndexOf("File1.fs")
            let file2Pos = content.IndexOf("File2.fs")
            
            Expect.isLessThan file2Pos file1Pos "File2 should now come before File1"
          finally
            File.Delete(tempPath)
      
      testCase "moveFileUp - does nothing when file is already at top"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            moveFileUp tempPath "File1.fs" // Already first
            
            let content = readFsprojFile tempPath
            let file1Pos = content.IndexOf("File1.fs")
            let file2Pos = content.IndexOf("File2.fs")
            
            Expect.isLessThan file1Pos file2Pos "File1 should remain first"
          finally
            File.Delete(tempPath)
    ]
    
    testList "moveFileDown Tests" [
      testCase "moveFileDown - moves file down in order"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            moveFileDown tempPath "File2.fs"
            
            let content = readFsprojFile tempPath
            let file2Pos = content.IndexOf("File2.fs")
            let file3Pos = content.IndexOf("File3.fs")
            
            Expect.isLessThan file3Pos file2Pos "File2 should now come after File3"
          finally
            File.Delete(tempPath)
      
      testCase "moveFileDown - does nothing when file is already at bottom"
      <| fun () ->
          let tempPath = createTempFsprojFile sampleFsprojContent
          try
            moveFileDown tempPath "File3.fs" // Already last
            
            let content = readFsprojFile tempPath
            let file2Pos = content.IndexOf("File2.fs")
            let file3Pos = content.IndexOf("File3.fs")
            
            Expect.isLessThan file2Pos file3Pos "File3 should remain last"
          finally
            File.Delete(tempPath)
    ]
    
    testList "addExistingFile Tests" [
      testCase "addExistingFile - adds file with relative path"
      <| fun () ->
          let tempDir = Path.GetTempPath()
          let fsprojPath = Path.Combine(tempDir, "TestProject.fsproj")
          let existingFilePath = Path.Combine(tempDir, "ExistingFile.fs")
          
          try
            File.WriteAllText(fsprojPath, sampleFsprojContent)
            File.WriteAllText(existingFilePath, "// Test file content")
            
            let result = addExistingFile fsprojPath existingFilePath
            Expect.isOk result "Should successfully add existing file"
            
            let content = readFsprojFile fsprojPath
            Expect.stringContains content "ExistingFile.fs" "Should contain the existing file"
          finally
            if File.Exists(fsprojPath) then File.Delete(fsprojPath)
            if File.Exists(existingFilePath) then File.Delete(existingFilePath)
    ]
  ]