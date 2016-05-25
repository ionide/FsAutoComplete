namespace FsAutoComplete

open System
open System.IO
open Utils 

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
                     let fi = new FileInfo(dir.FullName @@ file)
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

  let msbuild =
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

  let private fsharpInstallationPath =
    ["4.0"; "3.1"; "3.0"]
    |> List.map (fun v -> programFilesX86 @@ @"\Microsoft SDKs\F#\" @@ v @@ @"\Framework\v4.0")
    |> List.tryFind Directory.Exists

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

  let referenceAssembliesPath = 
     programFilesX86 @@ @"Reference Assemblies\Microsoft\Framework\.NETFramework"
  
  let dotNetVersions () = 
    Directory.EnumerateDirectories referenceAssembliesPath
    |> Seq.sort
    |> Seq.toArray
    |> Array.rev
      
