namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCrackerVerbose =

  // interesting things:
  // 1 - if fsharp.core.dll is not referenced, add it from ensureCorrectFSharpCore
  // 2 - fsi are compile files
  // varie:
  // - search delle cose e' fatto con EndsWith, doesnt check if is an argument like -r
  // - riarrangia i files fsi per po.SourceFiles in modo che siano prima del proprio file .fs **e lo toglie da project options**
  // - po.SourceFiles e' ritonato come snd, a questi files viene appicciata la po (FSharpProjectOptions), quindi importante 
  //   di filtrarli corretamente altrimenti non sarebbero parsabili
  // - let po = { po with SourceFiles = po.SourceFiles |> Array.map normalizeDirSeparators }

  let load ensureCorrectFSharpCore file verbose =
    if not (File.Exists file) then
      Err (GenericError(sprintf "File '%s' does not exist" file))
    else
      try
        let po, logMap =
          let p, logMap = ProjectCracker.GetProjectOptionsFromProjectFileLogged(file, enableLogging=verbose)

          match p.SourceFiles, p.OtherOptions, logMap |> Map.isEmpty with
          | [| |], [| |], false ->
            //HACK project cracker has failed
            //  ref https://github.com/fsharp/FSharp.Compiler.Service/issues/804
            //  the ProjectCracker.GetProjectOptionsFromProjectFileLogged doesnt throw, just return an
            //  uninitalized FSharpProjectOptions and some log, who contains the exception
            let logs =
                logMap
                |> Map.toArray
                |> Array.map (fun (k,v) -> sprintf "%s: %s" k v)
                |> fun a -> String.Join(Environment.NewLine, a)
            failwithf "Failed parsing project file: %s" logs
          | _ -> ()

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
                //HACK ref https://github.com/fsharp/FSharp.Compiler.Service/issues/803
                //  the ProjectCracker.GetProjectOptionsFromProjectFileLogged doesnt return the .fsi files inside
                //  the SourceFiles property, but are instead inside OtherOptions
                //  this workaround moving these **before** the corrisponding .fs file (.fsi file must precede the .fs)
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
        let outType = FscArguments.outType (po.OtherOptions |> List.ofArray)

        let rec setExtraInfo po =
            { po with
                 ExtraProjectInfo = Some (box {
                    ExtraProjectInfoData.ProjectSdkType = ProjectSdkType.Verbose
                    ProjectOutputType = outType
                 })
                 ReferencedProjects = po.ReferencedProjects |> Array.map (fun (path,p2p) -> path, (setExtraInfo p2p)) }

        Ok (setExtraInfo po, Array.toList po.SourceFiles, logMap)
      with e ->
        Err (GenericError(e.Message))

