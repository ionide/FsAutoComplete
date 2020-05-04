namespace ProjectSystem

open System
open System.IO

type GetProjectOptionsErrors = Dotnet.ProjInfo.Workspace.GetProjectOptionsErrors

type [<RequireQualifiedAccess>] WorkspaceProjectState =
    | Loading of string
    | Loaded of FSharp.Compiler.SourceCodeServices.FSharpProjectOptions * Dotnet.ProjInfo.Workspace.ExtraProjectInfoData * Dotnet.ProjInfo.Workspace.ProjectViewerItem list * Map<string,string>
    | Failed of string * GetProjectOptionsErrors

module internal ProjectRecognizer =

    let (|NetCoreProjectJson|FSharpNetSdk|NetCoreSdk|Net45|Unsupported|) (file: string) =
        //.NET Core Sdk preview3+ replace project.json with fsproj
        //Easy way to detect new fsproj is to check the msbuild version of .fsproj
        //Post preview5 has (`Sdk="FSharp.NET.Sdk;Microsoft.NET.Sdk"`), use that
        //  for checking .NET Core fsproj. NB: casing of FSharp may be inconsistent.
        //The `dotnet-compile-fsc.rsp` are created also in `preview3+`, so we can
        //  reuse the same behaviour of `preview2`
        let rec getProjectType lines =
            // post preview5 dropped this, check Sdk field
            let isNetCore (line:string) = line.ToLower().Contains("sdk=")
            let isFSharpNetSdk (line:string) = line.ToLower().Contains("fsharp.net.sdk")
            match lines with
            | [] ->
                Unsupported // unsupported project type
            | (line: string) :: xs ->
                if not <| line.Contains("ToolsVersion") && not <| line.Contains("Sdk=") then
                    getProjectType xs
                else // both net45 and preview3-5 have 'ToolsVersion', > 5 has 'Sdk'
                    if isNetCore line then
                        if isFSharpNetSdk line then FSharpNetSdk else NetCoreSdk
                    else
                        Net45
        if Path.GetExtension(file) = ".json" then
            NetCoreProjectJson // dotnet core preview 2 or earlier
        else
            File.ReadLines(file)
            |> Seq.truncate 3
            |> List.ofSeq
            |> getProjectType

    ///We only support Old Verbose projects and modern SDK projects. We don't support .Net Core preview `project.json` nor .Net Core 1.X `FSharp.NET.Sdk`
    let isSupported (file: string) =
      match file with
      | Net45
      | NetCoreSdk -> true
      | _ -> false
