namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCoreCracker =
  let GetProjectOptionsFromResponseFile (file : string)  =
    let projDir = Path.GetDirectoryName file
    let rsp =
      Directory.GetFiles(projDir, "dotnet-compile-fsc.rsp", SearchOption.AllDirectories)
      |> Seq.head
      |> File.ReadAllLines
      |> Array.map (fun s -> if s.EndsWith ".fs" then
                                let p = Path.GetFullPath s
                                (p.Chars 0).ToString().ToLower() + p.Substring(1)
                             else s )
      |> Array.filter((<>) "--nocopyfsharpcore")

    {
      ProjectFileName = file
      ProjectFileNames = [||]
      OtherOptions = rsp
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None;
      OriginalLoadReferences = []
      ExtraProjectInfo = None
    }

  let runProcess (workingDir: string) (exePath: string) (args: string) =
      let psi = System.Diagnostics.ProcessStartInfo()
      psi.FileName <- exePath
      psi.WorkingDirectory <- workingDir 
      psi.RedirectStandardOutput <- false
      psi.RedirectStandardError <- false
      psi.Arguments <- args
      psi.CreateNoWindow <- true
      psi.UseShellExecute <- false

      use p = new System.Diagnostics.Process()
      p.StartInfo <- psi
      p.Start() |> ignore
      p.WaitForExit()
      
      let exitCode = p.ExitCode
      exitCode, ()

  let GetProjectOptionsFromProjectFile (file : string) =
    let rec projInfo file =
        let projDir = Path.GetDirectoryName file

        let runCmd exePath args = runProcess projDir exePath (args |> String.concat " ")

        let msbuildExec = Dotnet.ProjInfo.Inspect.dotnetMsbuild runCmd
        let getFscArgs = Dotnet.ProjInfo.Inspect.getFscArgs
        let getP2PRefs = Dotnet.ProjInfo.Inspect.getP2PRefs
        let gp () = Dotnet.ProjInfo.Inspect.getProperties ["TargetPath"]
        let log = ignore

        let results =
          file
          |> Dotnet.ProjInfo.Inspect.getProjectInfos log msbuildExec [getFscArgs; getP2PRefs; gp] []
    
        // $(TargetPath)
        let mutable rsp : string list = []
        let mutable p2p : string list = []
        let mutable props : (string * string) list = []

        let doResult result =
          match result with
          | Choice1Of2 (Dotnet.ProjInfo.Inspect.GetResult.FscArgs x) -> rsp <- x
          | Choice1Of2 (Dotnet.ProjInfo.Inspect.GetResult.P2PRefs x) -> p2p <- x
          | Choice1Of2 (Dotnet.ProjInfo.Inspect.GetResult.Properties p) -> props <- p
          | Choice2Of2 _ -> failwith "errors"

        match results with
        | Choice1Of2 r -> r |> List.iter doResult
        | Choice2Of2 r -> failwith "errors"

        //TODO cache projects info of p2p ref
        let p2pProjects = p2p |> List.map projInfo

        let tar =
            match props |> Map.ofList |> Map.tryFind "TargetPath" with
            | Some t -> t
            | None -> failwith "error, 'TargetPath' property not found"

        let po =
            {
                ProjectFileName = file
                ProjectFileNames = [||]
                OtherOptions = rsp |> Array.ofList
                ReferencedProjects = p2pProjects |> Array.ofList
                IsIncompleteTypeCheckEnvironment = false
                UseScriptResolutionRules = false
                LoadTime = DateTime.Now
                UnresolvedReferences = None;
                OriginalLoadReferences = []
                ExtraProjectInfo = None
            }

        tar, po

    let _, po = projInfo file
    po
