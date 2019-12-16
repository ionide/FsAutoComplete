/// Functions to retrieve .net framework and .net core dependencies
/// for resolution of FSI

[<RequireQualifiedAccess>]
module ProjectSystem.FSIRefs

open System
open System.IO

let defaultDotNetSDKRoot =
  match Environment.OSVersion.Platform with
  | PlatformID.MacOSX | PlatformID.Unix -> "/usr/local/share/dotnet"
  | _ -> @"C:\Program Files\dotnet"

type TFM =
| NetFx
| NetCore

[<NoComparison>]
type NugetVersion = NugetVersion of major: int * minor: int * build: int * suffix: string
with
  override x.ToString() =
    match x with
    | NugetVersion(major, minor, build, suffix) when String.IsNullOrWhiteSpace suffix -> sprintf "%d.%d.%d" major minor build
    | NugetVersion(major, minor, build, suffix) -> sprintf "%d.%d.%d-%s" major minor build suffix

/// custom comparison for these versions means that you compare major/minor/build,
/// but an empty preview string marks a stable version, which is greater
let compareNugetVersion (NugetVersion(lM, lm, lb, ls)) (NugetVersion(rM, rm, rb, rs)) =
  match compare lM rM with
  | 0 ->
    match compare lm rm with
    | 0 ->
      match compare lb rb with
      | 0 ->
        match ls, rs with
        | "", s -> 1 // no preview string means left is a stable release and so is greater than right
        | s, "" -> -1 // no preview string means right is a stable release and so is greater than left
        | ls, rs -> compare ls rs // no shortcut means compare lexigrapically
      | n -> n
    | n -> n
  | n -> n

/// Parse nuget version strings into numeric and suffix parts
///
/// Format: `$(Major).$(Minor).$(Build) [-SomeSuffix]`
let deconstructVersion (version: string): NugetVersion =
  let version, suffix =
    let pos = version.IndexOf("-")
    if pos >= 0
    then
      version.Substring(0, pos), version.Substring(pos + 1)
    else
      version, ""

  let elements = version.Split('.')

  if elements.Length < 3 then
      NugetVersion(0, 0, 0, suffix)
  else
      NugetVersion(Int32.Parse(elements.[0]), Int32.Parse(elements.[1]), Int32.Parse(elements.[2]), suffix)

let versionDirectoriesIn (baseDir: string) =
  baseDir
  |> Directory.EnumerateDirectories
  |> Array.ofSeq // have to convert to array to get a sortWith that takes our custom comparison function
  |> Array.map (Path.GetFileName >> deconstructVersion)
  |> Array.sortWith compareNugetVersion

/// returns a sorted list of the SDK versions available at the given dotnet root
let sdkVersions dotnetRoot =
  let sdkDir = Path.Combine (dotnetRoot, "sdk")
  if Directory.Exists sdkDir
  then Some (versionDirectoriesIn sdkDir)
  else None

/// returns a sorted list of the .Net Core runtime versions available at the given dotnet root
let runtimeVersions dotnetRoot =
  let runtimesDir = Path.Combine(dotnetRoot, "shared/Microsoft.NETCore.App")
  if Directory.Exists runtimesDir
  then Some (versionDirectoriesIn runtimesDir)
  else None

let appPackDir dotnetRoot runtimeVersion tfm =
  let packDir = Path.Combine(dotnetRoot, "packs/Microsoft.NETCore.App.Ref", runtimeVersion, "ref", tfm)
  if Directory.Exists packDir then Some packDir else None

let compilerDir dotnetRoot sdkVersion =
  let compilerDir = Path.Combine(dotnetRoot, "sdk", sdkVersion, "FSharp")
  if Directory.Exists compilerDir then Some compilerDir else None

let runtimeDir dotnetRoot runtimeVersion =
  let runtimeDir = Path.Combine(dotnetRoot, "shared/Microsoft.NETCore.App", runtimeVersion)
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
    |> Seq.filter (fun r -> not (Path.GetFileNameWithoutExtension(r) = "mscorlib"))
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
  let netcore3 = NugetVersion(3, 0, 100, "")
  let netcore31 = NugetVersion(3, 1, 100, "")
  fun (sdkVersion: NugetVersion) ->
    match compareNugetVersion sdkVersion netcore3 with
    | 1 | 0 when compareNugetVersion sdkVersion netcore31 = -1 -> "netcoreapp3.0"
    | 1 | 0 -> "netcoreapp3.1"
    | _ -> "netcoreapp2.2"