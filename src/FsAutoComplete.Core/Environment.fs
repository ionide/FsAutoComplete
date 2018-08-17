namespace FsAutoComplete

open System
open System.IO
open Utils
#if NETSTANDARD2_0
open System.Runtime.InteropServices
#endif

module Environment =
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

  let private tryFindFile dirs file =
      let files =
          dirs
          |> Seq.map (fun (path : string) ->
              try
                 let path =
                    if path.StartsWith("\"") && path.EndsWith("\"")
                    then path.Substring(1, path.Length - 2)
                    else path
                 let dir = new DirectoryInfo(path)
                 if not dir.Exists then ""
                 else
                     let fi = new FileInfo(dir.FullName </> file)
                     if fi.Exists then fi.FullName
                     else ""
              with
              | _ -> "")
          |> Seq.filter ((<>) "")
          |> Seq.cache
      if not (Seq.isEmpty files) then Some(Seq.head files)
      else None

  let private tryFindPath backupPaths tool =
      let paths = Environment.GetEnvironmentVariable "PATH" + string Path.PathSeparator + backupPaths
      let paths = paths.Split(Path.PathSeparator)
      tryFindFile paths tool

  let private findPath backupPaths tool =
      match tryFindPath backupPaths tool with
      | Some file -> file
      | None -> tool

  let private vsSkus = ["Community"; "Professional"; "Enterprise"; "BuildTools"]
  let private vsVersions = ["2017"]
  let cartesian a b =
    [ for a' in a do
        for b' in b do
          yield a', b' ]

  let private vsRoots =
    cartesian vsVersions vsSkus 
    |> List.map (fun (version, sku) -> programFilesX86 </> "Microsoft Visual Studio" </> version </> sku) 

  let msbuild =
      if Utils.runningOnMono then "msbuild" // we're way past 5.0 now, time to get updated
      else
        let legacyPaths =
            [ programFilesX86 </> @"\MSBuild\14.0\Bin"
              programFilesX86 </> @"\MSBuild\12.0\Bin"
              programFilesX86 </> @"\MSBuild\12.0\Bin\amd64"
              @"c:\Windows\Microsoft.NET\Framework\v4.0.30319"
              @"c:\Windows\Microsoft.NET\Framework\v4.0.30128"
              @"c:\Windows\Microsoft.NET\Framework\v3.5" ]
        
        let sideBySidePaths =
          vsRoots
          |> List.map (fun root -> root </> "MSBuild" </> "15.0" </> "bin" )
        let MSBuildPath =
          sideBySidePaths @ legacyPaths
          |> String.concat ";"
        let ev = Environment.GetEnvironmentVariable "MSBuild"
        if not (String.IsNullOrEmpty ev) then ev
        else findPath MSBuildPath "MSBuild.exe"

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
    if Utils.runningOnMono || not Utils.isWindows then "fsharpi"
    else
      // if running on windows, non-mono we can't yet send paths to the netcore version of fsi.exe so use the one from full-framework
      Option.getOrElse "" fsharpInstallationPath </> "fsi.exe"

  let fsc =
    if Utils.runningOnMono || not Utils.isWindows then "fsharpc"
    else
      // if running on windows, non-mono we can't yet send paths to the netcore version of fsc.exe so use the one from full-framework
      Option.getOrElse "" fsharpInstallationPath </> "fsc.exe"

  let fsharpCoreOpt =
#if SCRIPT_REFS_FROM_MSBUILD
    if not(System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
      None
#else
    if Utils.runningOnMono then
      let mscorlibDir = Path.GetDirectoryName typeof<obj>.Assembly.Location
      if List.forall File.Exists (List.map (combinePaths mscorlibDir) ["FSharp.Core.dll"; "FSharp.Core.optdata"; "FSharp.Core.sigdata"]) then
        Some (mscorlibDir </> "FSharp.Core.dll")
      else
        None
#endif
    else
      let referenceAssembliesPath =
        programFilesX86 </> @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\"
      let fsharpCoreVersions = ["4.4.1.0"; "4.4.0.0"; "4.3.1.0"; "4.3.0.0"]
      tryFindFile (List.map (combinePaths referenceAssembliesPath) fsharpCoreVersions) "FSharp.Core.dll"

#if SCRIPT_REFS_FROM_MSBUILD
#else
  let referenceAssembliesPath () =
    Some (programFilesX86 </> @"Reference Assemblies\Microsoft\Framework\.NETFramework")

  let dotNetVersions () =
    match referenceAssembliesPath () |> Option.filter Directory.Exists with
    | Some path ->
      Directory.EnumerateDirectories path
      |> Seq.filter (fun s -> not(s.EndsWith(".X"))) //may contain only xml files, not assemblies
      |> Seq.sort
      |> Seq.toArray
      |> Array.rev
    | None ->
      Array.empty
#endif

  let netReferecesAssembliesTFM () =
#if SCRIPT_REFS_FROM_MSBUILD
    NETFrameworkInfoProvider.installedNETVersions ()
    |> Array.ofList
#else
    dotNetVersions ()
    |> Array.map Path.GetFileName
#endif

  let netReferecesAssembliesTFMLatest () =
    netReferecesAssembliesTFM ()
    |> Array.sortWith (fun x y -> StringComparer.OrdinalIgnoreCase.Compare(x, y))
    |> Array.rev
    |> Array.tryHead

  let workspaceLoadDelay () =
    match System.Environment.GetEnvironmentVariable("FSAC_WORKSPACELOAD_DELAY") with
    | delayMs when not (String.IsNullOrWhiteSpace(delayMs)) ->
        match System.Int32.TryParse(delayMs) with
        | true, x -> TimeSpan.FromMilliseconds(float x)
        | false, _ -> TimeSpan.Zero
    | _ -> TimeSpan.Zero
