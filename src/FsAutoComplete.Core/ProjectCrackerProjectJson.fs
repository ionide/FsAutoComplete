namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCrackerProjectJson =

  let getProjectOptionsFromResponseFile (file : string)  =
    let projDir = Path.GetDirectoryName file
    let rsp =
      Directory.GetFiles(projDir, "dotnet-compile-fsc.rsp", SearchOption.AllDirectories)
      |> Seq.head
      |> File.ReadAllLines
      |> Array.map Utils.normalizePath
      |> Array.filter((<>) "--nocopyfsharpcore")

    {
      ProjectId = Some file
      ProjectFileName = file
      SourceFiles = [||]
      OtherOptions = rsp
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None
      Stamp = None
      OriginalLoadReferences = []
      ExtraProjectInfo = Some (box {
        ExtraProjectInfoData.ProjectSdkType = ProjectSdkType.ProjectJson
        ExtraProjectInfoData.ProjectOutputType = rsp |> List.ofArray |> FscArguments.outType
      })
    }

  let load file =
      try
        let po = getProjectOptionsFromResponseFile file
        let compileFiles = FscArguments.compileFiles (po.OtherOptions |> List.ofArray)
        Ok (po, Seq.toList compileFiles, Map<string,string>([||]))
      with e ->
        Error (GenericError(file, e.Message))
