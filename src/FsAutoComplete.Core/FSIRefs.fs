/// Functions to retrieve .net framework and .net core dependencies
/// for resolution of FSI

#if !INTERACTIVE
module FSharp.Compiler.FSIRefs
#endif

open System
open System.IO

let defaultDotNetSDKRoot =
  match Environment.OSVersion.Platform with
  | PlatformID.MacOSX | PlatformID.Unix -> "/usr/local/share/dotnet"
  | _ -> failwith "Add windows default"

/// Parse nuget version strings into numeric and suffix parts
///
/// Format: `$(Major).$(Minor).$(Build) [-SomeSuffix]`
let deconstructVersion (version: string) =
  let version, suffix =
    let pos = version.IndexOf("-")
    if pos >= 0
    then
      version.Substring(0, pos), version.Substring(pos + 1)
    else
      version, ""

  let elements = version.Split('.')
  if elements.Length < 3 then
      struct (0, 0, 0, suffix)
  else
      struct (Int32.Parse(elements.[0]), Int32.Parse(elements.[1]), Int32.Parse(elements.[2]), suffix)

/// compare two nuget versions:
/// * Major, Minor, Build collates normally
/// * Strings without -SomeSuffix collate higher than SomeSuffix,
/// * SomeSuffix collates using normal alphanumeric rules
let versionCompare c1 c2 =
  if c1 = c2 then 0
  else
    try
      let struct (major1, minor1, build1, suffix1 ) = deconstructVersion c1
      let struct (major2, minor2, build2, suffix2 ) = deconstructVersion c2
      let v = major1 - major2
      if v <> 0 then v
      else
        let v = minor1 - minor2
        if v <> 0 then v
        else
          let v = build1 - build2
          if v <> 0 then v
          else
            match String.IsNullOrEmpty(suffix1), String.IsNullOrEmpty(suffix2) with
            | true, true -> 0
            | true, false -> 1
            | false, true -> -1
            | false, false -> String.Compare(suffix1, suffix2, StringComparison.InvariantCultureIgnoreCase)
    with _ -> 0

let sdkVersions dotnetRoot =
  let sdkDir = Path.Combine (dotnetRoot, "sdk")
  if Directory.Exists sdkDir
  then sdkDir |> Directory.EnumerateDirectories |> Seq.map (Path.GetDirectoryName >> deconstructVersion) |> List.ofSeq |> Some
  else None

let runtimeVersions dotnetRoot =
  let runtimesDir = Path.Combine(dotnetRoot, "shared/Microsoft.NETCore.App")
  if Directory.Exists runtimesDir
  then runtimesDir |> Directory.EnumerateDirectories |> Seq.map (Path.GetDirectoryName >> deconstructVersion) |> List.ofSeq |> Some
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
  | _, Some refDir -> Directory.EnumerateFiles(refDir, "*.dll") |> Seq.toList
  | None, None -> []

let compilerAndInteractiveRefs compilerDir useFsiAuxLib =
  match compilerDir with
  | Some dir ->
    [ yield Path.Combine(dir, "FSharp.Core.dll")
      if useFsiAuxLib then yield Path.Combine(dir, "FSharp.Compiler.Interactive.Settings.dll") ]
  | None -> []

let netCoreRefs dotnetRoot sdkVersion runtimeVersion tfm useFsiAuxLib =
  // refs for .net core include:
  // * runtime-required refs
  // * compiler/fsi-required refs

  [ yield! findRuntimeRefs (appPackDir dotnetRoot runtimeVersion tfm) (runtimeDir dotnetRoot runtimeVersion)
    yield! compilerAndInteractiveRefs (compilerDir dotnetRoot sdkVersion) useFsiAuxLib ]

