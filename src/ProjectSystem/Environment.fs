namespace ProjectSystem

open System
open System.IO
open Dotnet.ProjInfo.Workspace
open Newtonsoft.Json.Linq
open System.Collections.Generic

#if NETSTANDARD2_0
open System.Runtime.InteropServices
#endif

[<RequireQualifiedAccess>]
module Environment =

  /// Determines if the current system is an Unix system.
  /// See http://www.mono-project.com/docs/faq/technical/#how-to-detect-the-execution-platform
  let isUnix =
  #if NETSTANDARD2_0
      RuntimeInformation.IsOSPlatform(OSPlatform.Linux) ||
      RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
  #else
      int System.Environment.OSVersion.Platform |> fun p -> (p = 4) || (p = 6) || (p = 128)
  #endif

  /// Determines if the current system is a MacOs system
  let isMacOS =
  #if NETSTANDARD2_0
      RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
  #else
      (System.Environment.OSVersion.Platform = PlatformID.MacOSX) ||
          // osascript is the AppleScript interpreter on OS X
          File.Exists "/usr/bin/osascript"
  #endif

  /// Determines if the current system is a Linux system
  let isLinux =
  #if NETSTANDARD2_0
      RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
  #else
      isUnix && not isMacOS
  #endif

  /// Determines if the current system is a Windows system
  let isWindows =
  #if NETSTANDARD2_0
      RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
  #else
      match System.Environment.OSVersion.Platform with
      | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE -> true
      | _ -> false
  #endif


  let runningOnMono =
    try not << isNull <| Type.GetType "Mono.Runtime"
    with _ -> false

  let msbuildLocator = MSBuildLocator()

  let msbuild =
    let msbuildPath = msbuildLocator.LatestInstalledMSBuildNET()

    match msbuildPath with
    | Dotnet.ProjInfo.Inspect.MSBuildExePath.Path path ->
      Some path
    | Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild p ->
      // failwithf "expected msbuild, not 'dotnet %s'" p
      None

  let private environVar v = Environment.GetEnvironmentVariable v

  let private programFilesX86 =
      let wow64 = environVar "PROCESSOR_ARCHITEW6432"
      let globalArch = environVar "PROCESSOR_ARCHITECTURE"
      match wow64, globalArch with
      | "AMD64", "AMD64"
      | null, "AMD64"
      | "x86", "AMD64" -> environVar "ProgramFiles(x86)"
      | _ -> environVar "ProgramFiles"
      |> fun detected -> if detected = null then @"C:\Program Files (x86)\" else detected

  // Below code slightly modified from FAKE MSBuildHelper.fs

  let private vsSkus = ["Community"; "Professional"; "Enterprise"; "BuildTools"]
  let private vsVersions = ["2019"; "2017";]
  let private cartesian a b =
    [ for a' in a do
        for b' in b do
          yield a', b' ]

  let private vsRoots =
    cartesian vsVersions vsSkus
    |> List.map (fun (version, sku) -> programFilesX86 </> "Microsoft Visual Studio" </> version </> sku)

  /// these are the single-instance installation paths on windows from FSharp versions < 4.5
  let private legacyFSharpInstallationPaths =
    ["10.1"; "4.1"; "4.0"; "3.1"; "3.0"]
    |> List.map (fun v -> programFilesX86 </> @"\Microsoft SDKs\F#\" </> v </> @"\Framework\v4.0")

  /// starting with F# 4.5 the binaries are installed in a side-by-side manner to a per-VS-edition folder
  let private sideBySideFSharpInstallationPaths =
    let pattern root = root </> "Common7" </> "IDE" </> "CommonExtensions" </> "Microsoft" </> "FSharp"
    vsRoots |> List.map pattern

  let private fsharpInstallationPath =
    sideBySideFSharpInstallationPaths @ legacyFSharpInstallationPaths
    |> List.tryFind Directory.Exists

  let fsi =
    // on netcore on non-windows we just deflect to fsharpi as usual
    if runningOnMono || not isWindows then Some "fsharpi"
    else
      // if running on windows, non-mono we can't yet send paths to the netcore version of fsi.exe so use the one from full-framework
      fsharpInstallationPath |> Option.map (fun root -> root </> "fsi.exe")

  let fsc =
    if runningOnMono || not isWindows then Some "fsharpc"
    else
      // if running on windows, non-mono we can't yet send paths to the netcore version of fsc.exe so use the one from full-framework
      fsharpInstallationPath |> Option.map (fun root -> root </> "fsc.exe")

  let fsharpCore =
    let dir = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    dir </> "FSharp.Core.dll"

  let workspaceLoadDelay () =
    match System.Environment.GetEnvironmentVariable("FSAC_WORKSPACELOAD_DELAY") with
    | delayMs when not (String.IsNullOrWhiteSpace(delayMs)) ->
        match System.Int32.TryParse(delayMs) with
        | true, x -> TimeSpan.FromMilliseconds(float x)
        | false, _ -> TimeSpan.Zero
    | _ -> TimeSpan.Zero

  /// The sdk root that we assume for FSI-ref-location purposes.
  /// TODO: make this settable via ENV variable or explicit LSP config
  let dotnetSDKRoot =
    lazy (
      let fromEnv =
        Environment.GetEnvironmentVariable "DOTNET_ROOT"
        |> Option.ofObj
      defaultArg fromEnv FSIRefs.defaultDotNetSDKRoot
    )

  let private maxVersionWithThreshold (minVersion: NugetVersion.NugetVersion) (versions: NugetVersion.NugetVersion []) =
    versions
    |> Array.filter (fun v -> NugetVersion.compareNugetVersion v minVersion >= 0) // get all versions that compare as greater than the minVersion
    |> Array.sortWith NugetVersion.compareNugetVersion
    |> Array.tryLast

  /// because 3.x is the minimum SDK that we support for FSI, we want to float to the latest
  /// 3.x sdk that the user has installed, to prevent hard-coding.
  let latest3xSdkVersion dotnetRoot =
    let minSDKVersion = NugetVersion.NugetVersion(3,0,100,"")
    lazy (
      match FSIRefs.sdkVersions dotnetRoot with
      | None -> None
      | Some sortedSdkVersions ->
        maxVersionWithThreshold minSDKVersion sortedSdkVersions
    )

  /// because 3.x is the minimum runtime that we support for FSI, we want to float to the latest
  /// 3.x runtime that the user has installed, to prevent hard-coding.
  let latest3xRuntimeVersion dotnetRoot =
    let minRuntimeVersion = NugetVersion.NugetVersion(3,0,0,"")
    lazy (
      match FSIRefs.runtimeVersions dotnetRoot with
      | None -> None
      | Some sortedRuntimeVersions ->
        maxVersionWithThreshold minRuntimeVersion sortedRuntimeVersions
    )

  /// attempts to read the specified SDK version from the global.json file in the workspace root.
  let versionFromGlobalJson (globalJsonPath: string): NugetVersion.NugetVersion option =
    if not (File.Exists globalJsonPath)
    then None
    else
      use file = File.OpenText globalJsonPath
      use reader = new Newtonsoft.Json.JsonTextReader(file)
      let content = Newtonsoft.Json.Linq.JObject.Load reader
      let sdkVersion: string = content.SelectToken("sdk.version") |> JToken.op_Explicit
      NugetVersion.tryDeconstructVersion sdkVersion

  type private MajorMinor = MajorMinor of major: int * minor: int
  with
    static member ofNugetVersion (NugetVersion.NugetVersion(maj, min, _, _)) = MajorMinor(maj, min)

  //oddjob httpclient, singleton so that we don't spam the OS TCP connections
  let private httpclient = new System.Net.Http.HttpClient()

  [<CLIMutable>]
  type Release = { runtime : {| version: string |}; sdk: {| version: string |} }
  [<CLIMutable>]
  type ChannelReleases = { ``releases``: Release [] }
  [<CLIMutable>]
  type ChannelReleaseMeta = { ``channel-version``: string; ``releases.json``: string }
  [<CLIMutable>]
  type Channels = { ``releases-index``: ChannelReleaseMeta []}

  /// reads `releases.json` from the microsoft downloads to find the matching runtime version number for the given SDK version.
  let runtimeVersionForSdk =
    let allChannels =
        httpclient.GetStringAsync("https://raw.githubusercontent.com/dotnet/core/master/release-notes/releases-index.json")
        |> Async.AwaitTask
        |> Async.map Newtonsoft.Json.JsonConvert.DeserializeObject<Channels>

    let discoverReleases (MajorMinor(maj, min))= async {
      let! allReleases = allChannels |> Async.map (fun channels -> channels.``releases-index``)
      let release = allReleases |> Array.tryFind (fun c -> c.``channel-version`` = sprintf "%d.%d" maj min)
      let releaseManifestUrl: string = release.Value.``releases.json``
      let! releaseManifest =
        httpclient.GetStringAsync(releaseManifestUrl)
        |> Async.AwaitTask
        |> Async.map Newtonsoft.Json.JsonConvert.DeserializeObject<ChannelReleases>
      let releaseList = releaseManifest.releases
      let versionMaps =
        releaseList
        |> Seq.choose (fun release ->
          let sdkVersion =
            let v = release.sdk.version
            NugetVersion.tryDeconstructVersion v
          let runtimeVersion =
            let v = release.runtime.version
            NugetVersion.tryDeconstructVersion v
          match sdkVersion, runtimeVersion with
          | Some s, Some r -> Some (s, Some r)
          | _ -> None
        )

      let d = Dictionary<_,_>()
      for (k,v) in versionMaps do
        d.Add(k, v)
      return d
    }

    let findOrSetNone (releases: Dictionary<NugetVersion.NugetVersion, NugetVersion.NugetVersion option>) sdkVersion =
      match releases.TryGetValue sdkVersion with
      | true, discoveredVersion ->
        async.Return discoveredVersion
      | false, _ ->
        releases.[sdkVersion] <- None
        async.Return None

    let channels = Dictionary<MajorMinor, Dictionary<NugetVersion.NugetVersion, NugetVersion.NugetVersion option>>()

    fun (sdkVersion: NugetVersion.NugetVersion) ->
      let channel = MajorMinor.ofNugetVersion sdkVersion
      match channels.TryGetValue channel with
      | true, releases ->
        findOrSetNone releases sdkVersion
      | false, _ ->
        async {
          // populate channels listing, then discover runtime version
          let! releasesForChannel = discoverReleases channel
          channels.[channel] <- releasesForChannel
          return! findOrSetNone releasesForChannel sdkVersion
        }
