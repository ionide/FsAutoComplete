namespace FsAutoComplete

open System
open System.IO

[<RequireQualifiedAccess>]
type ProjectSdkType =
    | Verbose
    | ProjectJson
    | DotnetSdk of ProjectSdkTypeDotnetSdk
and ProjectSdkTypeDotnetSdk =
    {
      IsTestProject: bool
      Configuration: string // Debug
      IsPackable: bool // true
      TargetFramework: string // netcoreapp1.0
      TargetFrameworkIdentifier: string // .NETCoreApp
      TargetFrameworkVersion: string // v1.0

      MSBuildAllProjects: FilePath list //;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\FSharp.NET.Sdk\Sdk\Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\Sdk\Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.DefaultItems.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.SupportedTargetFrameworks.props;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.nuget.g.props;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\FSharp.NET.Sdk\Sdk\Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\Sdk\Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.BeforeCommon.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DefaultAssemblyInfo.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DefaultOutputPaths.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.TargetFrameworkInference.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.RuntimeIdentifierInference.targets;C:\Users\e.sada\.nuget\packages\fsharp.net.sdk\1.0.5\build\FSharp.NET.Core.Sdk.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\c1.fsproj;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Microsoft.Common.CurrentVersion.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\NuGet.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\15.0\Microsoft.Common.targets\ImportAfter\Microsoft.TestPlatform.ImportAfter.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Microsoft.TestPlatform.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.nuget.g.targets;e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\c1.fsproj.proj-info.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.Common.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.PackageDependencyResolution.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Sdk.DefaultItems.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.DisableStandardFrameworkResolution.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.GenerateAssemblyInfo.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.Publish.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\Microsoft.NET.Sdk\build\Microsoft.NET.PreserveCompilationContext.targets;C:\dotnetcli\dotnet-dev-win-x64.1.0.4\sdk\1.0.4\Sdks\NuGet.Build.Tasks.Pack\build\NuGet.Build.Tasks.Pack.targets
      MSBuildToolsVersion: string // 15.0

      ProjectAssetsFile: FilePath // e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\obj\project.assets.json
      RestoreSuccess: bool // True

      Configurations: string list // Debug;Release
      TargetFrameworks: string list // netcoreapp1.0;netstandard1.6

      //may not exists
      RunArguments: string option // exec "e:\github\DotnetNewFsprojTestingSamples\sdk1.0\sample1\c1\bin\Debug\netcoreapp1.0\c1.dll"
      RunCommand: string option // dotnet

      //from 2.0
      IsPublishable: bool option // true

    }

type ExtraProjectInfoData =
    {
        ProjectOutputType: ProjectOutputType
        ProjectSdkType: ProjectSdkType
    }
and ProjectOutputType =
    | Library
    | Exe
    | Custom of string


type GetProjectOptionsErrors =
     | ProjectNotRestored of string
     | GenericError of string


module ProjectRecognizer =

    let (|NetCoreProjectJson|NetCoreSdk|Net45|Unsupported|) file =
        //.NET Core Sdk preview3+ replace project.json with fsproj
        //Easy way to detect new fsproj is to check the msbuild version of .fsproj
        //Post preview5 has (`Sdk="FSharp.NET.Sdk;Microsoft.NET.Sdk"`), use that
        //  for checking .NET Core fsproj. NB: casing of FSharp may be inconsistent.
        //The `dotnet-compile-fsc.rsp` are created also in `preview3+`, so we can
        //  reuse the same behaviour of `preview2`
        let rec getProjectType (sr:StreamReader) limit =
            // post preview5 dropped this, check Sdk field
            let isNetCore (line:string) = line.ToLower().Contains("sdk=")
            if limit = 0 then
                Unsupported // unsupported project type
            else
                let line = sr.ReadLine()
                if not <| line.Contains("ToolsVersion") && not <| line.Contains("Sdk=") then
                    getProjectType sr (limit-1)
                else // both net45 and preview3-5 have 'ToolsVersion', > 5 has 'Sdk'
                    if isNetCore line then NetCoreSdk else Net45
        if Path.GetExtension file = ".json" then
            NetCoreProjectJson // dotnet core preview 2 or earlier
        else
            use sr = File.OpenText(file)
            getProjectType sr 3
