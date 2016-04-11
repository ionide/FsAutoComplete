namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCoreCracker = 
  let GetProjectOptionsFromProjectFile (file : string)  = 
    let projDir = Path.GetDirectoryName file
    let rsp =  
      Directory.GetFiles(projDir, "dotnet-compile-fsc.rsp", SearchOption.AllDirectories) 
      |> Seq.head
      |> File.ReadAllLines
      
    { 
      ProjectFileName = file
      ProjectFileNames = [||]
      OtherOptions = rsp
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None;
    }