namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCrackerVerbose =
  let load ensureCorrectFSharpCore file verbose =
    if not (File.Exists file) then
      Err (GenericError(sprintf "File '%s' does not exist" file))
    else
      try
        let po, logMap =
          let p, logMap = ProjectCracker.GetProjectOptionsFromProjectFileLogged(file, enableLogging=verbose)
          let opts =
            if not (Seq.exists (fun (s: string) -> s.Contains "FSharp.Core.dll") p.OtherOptions) then
              ensureCorrectFSharpCore p.OtherOptions
            else
               p.OtherOptions
          { p with OtherOptions = opts }, logMap

        let po =
            match po.SourceFiles with
            | [||] ->
                let compileFiles, otherOptions =
                    po.OtherOptions |> Array.partition (fun (s:string) -> s.EndsWith(".fs") || s.EndsWith (".fsi"))
                { po with SourceFiles = compileFiles; OtherOptions = otherOptions }
            | _ ->
                let fsiFiles, otherOptions =
                    po.OtherOptions |> Array.partition (fun (s:string) -> s.EndsWith (".fsi"))
                let fileNames =
                    po.SourceFiles
                    |> Array.fold (fun acc e ->
                        match fsiFiles |> Array.tryFind ((=) (e + "i")) with
                        | Some fsi ->
                            [| yield! acc; yield fsi; yield e  |]
                        | None -> [| yield! acc; yield e |] ) [||]

                { po with SourceFiles = fileNames ; OtherOptions = otherOptions }


        let po = { po with SourceFiles = po.SourceFiles |> Array.map normalizeDirSeparators }
        let outputFile = Seq.tryPick (chooseByPrefix "--out:") po.OtherOptions
        let references = Seq.choose (chooseByPrefix "-r:") po.OtherOptions
        let outType =
            match Seq.tryPick (chooseByPrefix "--target:") po.OtherOptions with
            | Some "library" -> ProjectOutputType.Library
            | Some "exe" -> ProjectOutputType.Exe
            | Some v -> ProjectOutputType.Custom v
            | None -> ProjectOutputType.Exe // default if arg is not passed to fsc
        let rec setExtraInfo po =
            { po with
                 ExtraProjectInfo = Some (box {
                    ExtraProjectInfoData.ProjectSdkType = ProjectSdkType.Verbose
                    ProjectOutputType = outType
                 })
                 ReferencedProjects = po.ReferencedProjects |> Array.map (fun (path,p2p) -> path, (setExtraInfo p2p)) }

        Ok (setExtraInfo po, Array.toList po.SourceFiles, outputFile, Seq.toList references, logMap)
      with e ->
        Err (GenericError(e.Message))

