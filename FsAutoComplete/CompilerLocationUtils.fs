namespace FsAutoComplete

open System
open System.IO

module DotNetEnvironment =
  let environVar v = Environment.GetEnvironmentVariable v

  let programFilesX86 =
      let wow64 = environVar "PROCESSOR_ARCHITEW6432"
      let globalArch = environVar "PROCESSOR_ARCHITECTURE"
      match wow64, globalArch with
      | "AMD64", "AMD64"
      | null, "AMD64"
      | "x86", "AMD64" -> environVar "ProgramFiles(x86)"
      | _ -> environVar "ProgramFiles"
      |> fun detected -> if detected = null then @"C:\Program Files (x86)\" else detected

  // Below code slightly modified from FAKE MSBuildHelper.fs

  let inline combinePaths path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

  let inline (@@) path1 path2 = combinePaths path1 path2

  let tryFindFile dirs file =
      let files =
          dirs
          |> Seq.map (fun (path : string) ->
                 let dir = new DirectoryInfo(path)
                 if not dir.Exists then ""
                 else
                     let fi = new FileInfo(dir.FullName @@ file)
                     if fi.Exists then fi.FullName
                     else "")
          |> Seq.filter ((<>) "")
          |> Seq.cache
      if not (Seq.isEmpty files) then Some(Seq.head files)
      else None

  let tryFindPath backupPaths tool =
      let paths = Environment.GetEnvironmentVariable "PATH" + string Path.PathSeparator + backupPaths
      let paths = paths.Split(Path.PathSeparator)
      tryFindFile paths tool

  let findPath backupPaths tool =
      match tryFindPath backupPaths tool with
      | Some file -> file
      | None -> tool

  let msBuildExe =
      if Utils.runningOnMono then "xbuild"
      else
        let MSBuildPath =
            (programFilesX86 @@ @"\MSBuild\14.0\Bin") + ";" +
            (programFilesX86 @@ @"\MSBuild\12.0\Bin") + ";" +
            (programFilesX86 @@ @"\MSBuild\12.0\Bin\amd64") + ";" +
            @"c:\Windows\Microsoft.NET\Framework\v4.0.30319\;" +
            @"c:\Windows\Microsoft.NET\Framework\v4.0.30128\;" +
            @"c:\Windows\Microsoft.NET\Framework\v3.5\"
        let ev = Environment.GetEnvironmentVariable "MSBuild"
        if not (String.IsNullOrEmpty ev) then ev
        else findPath MSBuildPath "MSBuild.exe"

  let fsharpInstallationPath =
    List.tryFind (fun v -> Directory.Exists (programFilesX86 @@ @"\Microsoft SDKs\F#\" @@ v @@ "\Framework\v4.0"))
                 ["4.0"; "3.1"; "3.0"]

  let fsi =
    if Utils.runningOnMono then "fsharpi"
    else
      Option.getOrElse "" fsharpInstallationPath @@ "fsi.exe"

  let fsc =
    if Utils.runningOnMono then "fsharpc"
    else
      Option.getOrElse "" fsharpInstallationPath @@ "fsc.exe"

  let fsharpCoreOpt =
    if Utils.runningOnMono then
      let mscorlibDir = Path.GetDirectoryName typeof<obj>.Assembly.Location
      if List.forall File.Exists (List.map (combinePaths mscorlibDir) ["FSharp.Core.dll"; "FSharp.Core.optdata"; "FSharp.Core.sigdata"]) then
        Some (mscorlibDir @@ "FSharp.Core.dll")
      else
        None
    else
      let referenceAssembliesPath =
        programFilesX86 @@ @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\"
      let fsharpCoreVersions = ["4.4.0.0"; "4.3.1.0"; "4.3.0.0"]
      tryFindFile (List.map (combinePaths referenceAssembliesPath) fsharpCoreVersions) "FSharp.Core.dll"
      
