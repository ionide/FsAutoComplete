namespace FsAutoComplete

module NETFrameworkInfoProvider =

  open System
  open System.IO
  open Dotnet.ProjInfo.Workspace

  let netFWInfo =
    let config = NetFWInfoConfig.Default Environment.msbuildLocator
    let netFwInfo = NetFWInfo.Create(config)
    netFwInfo

  let installedNETVersions () =
    netFWInfo.InstalledNetFws()

  let latestInstalledNETVersion () =
    netFWInfo.LatestVersion()

