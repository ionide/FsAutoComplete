/// Functions to retrieve .net framework and .net core dependencies
/// for resolution of FSI

[<RequireQualifiedAccess>]
module ProjectSystem.FSIRefs

open System
open System.IO
open System.Runtime.InteropServices

let defaultDotNetSDKRoot =
  if RuntimeInformation.IsOSPlatform OSPlatform.OSX
  then "/usr/local/share/dotnet"
  else if RuntimeInformation.IsOSPlatform OSPlatform.Linux
  then "/usr/share/dotnet"
  else @"C:\Program Files\dotnet"

type TFM =
| NetFx
| NetCore

let versionDirectoriesIn (baseDir: string) =
  baseDir
  |> Directory.EnumerateDirectories
  |> Array.ofSeq // have to convert to array to get a sortWith that takes our custom comparison function
  |> Array.map (Path.GetFileName >> NugetVersion.deconstructVersion)
  |> Array.sortWith NugetVersion.compareNugetVersion

/// path to the directory where .Net SDK versions are stored
let sdkDir dotnetRoot = Path.Combine (dotnetRoot, "sdk")

/// returns a sorted list of the SDK versions available at the given dotnet root
let sdkVersions dotnetRoot =
  let sdkDir = sdkDir dotnetRoot
  if Directory.Exists sdkDir
  then Some (versionDirectoriesIn sdkDir)
  else None

/// path to the .netcoreapp reference assembly storage location
let netcoreAppPacksDir dotnetRoot = Path.Combine(dotnetRoot, "packs/Microsoft.NETCore.App.Ref")
/// path to the .netcoreapp implementation assembly storage location
let netcoreAppDir dotnetRoot = Path.Combine(dotnetRoot, "shared/Microsoft.NETCore.App")

/// Returns a sorted list of the .Net Core runtime versions available at the given dotnet root.
///
/// If the reference-dll packs directory (`<dotnet root>/packs/Microsoft.NETCore.App.Ref`) is present that is used, otherwise
/// defaults to the actual runtime implementation dlls (`<dotnet root>/shared/Microsoft.NETCore.app`).
let runtimeVersions dotnetRoot =
  let runtimesDir = netcoreAppDir dotnetRoot
  let packsDir = netcoreAppPacksDir dotnetRoot
  if Directory.Exists packsDir
  then
    Some (versionDirectoriesIn packsDir)
    else
      if Directory.Exists runtimesDir
      then Some (versionDirectoriesIn runtimesDir)
      else None

let appPackDir dotnetRoot runtimeVersion tfm =
  let packDir = Path.Combine(netcoreAppPacksDir dotnetRoot, runtimeVersion, "ref", tfm)
  if Directory.Exists packDir then Some packDir else None

let compilerDir dotnetRoot sdkVersion =
  let compilerDir = Path.Combine(sdkDir dotnetRoot, sdkVersion, "FSharp")
  if Directory.Exists compilerDir then Some compilerDir else None

let runtimeDir dotnetRoot runtimeVersion =
  let runtimeDir = Path.Combine(netcoreAppDir dotnetRoot, runtimeVersion)
  if Directory.Exists runtimeDir then Some runtimeDir else None

/// given the pack directory and the runtime directory, we prefer the pack directory (because these are ref dlls)
/// but will accept the runtime dir if no pack exists.
let findRuntimeRefs packDir runtimeDir =
  match packDir, runtimeDir with
  | Some refDir, _
  | _, Some refDir ->
    Directory.EnumerateFiles(refDir, "*.dll")
    // SUPER IMPORTANT: netstandard/netcore assembly resolution _must not_ contain mscorlib or else
    // its presence triggers old netfx fallbacks, which end up bringing assemblies that aren't part
    // of netcore.
    |> Seq.filter (fun r -> Path.GetFileNameWithoutExtension(r) <> "mscorlib")
    |> Seq.toArray
  | None, None -> [||]

/// given the compiler root dir and if to include FSI refs, returns the set of compiler assemblies to references if that dir exists.
let compilerAndInteractiveRefs compilerDir useFsiAuxLib =
  match compilerDir with
  | Some dir ->
    [ yield Path.Combine(dir, "FSharp.Core.dll")
      if useFsiAuxLib then yield Path.Combine(dir, "FSharp.Compiler.Interactive.Settings.dll") ]
  | None -> []

/// refs for .net core include:
/// * runtime-required refs (either ref façades if available or full assemblies if no ref façades are present), and
/// * compiler/fsi-required refs

let netCoreRefs dotnetRoot sdkVersion runtimeVersion tfm useFsiAuxLib =

  [ yield! findRuntimeRefs (appPackDir dotnetRoot runtimeVersion tfm) (runtimeDir dotnetRoot runtimeVersion)
    yield! compilerAndInteractiveRefs (compilerDir dotnetRoot sdkVersion) useFsiAuxLib ]

/// picks a TFM for F# scripts based on the provided SDK version.
let tfmForRuntime =
  let netcore3 = NugetVersion.NugetVersion(3, 0, 100, "")
  let netcore31 = NugetVersion.NugetVersion(3, 1, 100, "")
  fun (sdkVersion: NugetVersion.NugetVersion) ->
    match NugetVersion.compareNugetVersion sdkVersion netcore3 with
    | 1 | 0 when NugetVersion.compareNugetVersion sdkVersion netcore31 = -1 -> "netcoreapp3.0"
    | 1 | 0 -> "netcoreapp3.1"
    | _ -> "netcoreapp2.2"
