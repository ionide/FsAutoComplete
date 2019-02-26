namespace FsAutoComplete

open System
open System.IO

open FSharp.Compiler.SourceCodeServices

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

  let load notifyState ensureCorrectFSharpCore file verbose =
      try
        notifyState (WorkspaceProjectState.Loading file)

        let po, logMap =
          let p, logMap =
            try
                ProjectCracker.GetProjectOptionsFromProjectFileLogged(file, enableLogging=verbose)
            with
            | e ->
                //HACK - Project cracker is failing to correctly deserialize cracking results
                // if there was some additional content in MsBuild's stdout. As a result it returns
                //whole stdout in error message. This checks if the error message contains correct JSON object
                //reporesenting FSharpProjectOptions
                if e.Message.Contains """{"Error@":null""" then
                    let startIndex = e.Message.IndexOf("""{"Error@":null""")
                    let endIndex = e.Message.IndexOf("stderr was:")
                    let msg = e.Message.Substring(startIndex, endIndex-startIndex)
                    let ser = new System.Runtime.Serialization.Json.DataContractJsonSerializer(typeof<ProjectCrackerTool.ProjectOptions>)
                    let stringBytes = System.Text.Encoding.Unicode.GetBytes msg
                    use ms = new MemoryStream(stringBytes)
                    let opts = ser.ReadObject(ms) :?> ProjectCrackerTool.ProjectOptions
                    Utils.Convert DateTime.Now opts
                else
                    raise e
          //printfn "from cracker: %A" p
          match p.SourceFiles, p.OtherOptions, logMap |> Map.isEmpty with
          | [| |], [| |], false ->
            //HACK project cracker has failed
            //  ref https://github.com/fsharp/FSharp.Compiler.Service/issues/804
            //  the ProjectCracker.GetProjectOptionsFromProjectFileLogged doesnt throw, just return an
            //  uninitalized FSharpProjectOptions and some log, who contains the exception
            printfn "MAP: %A" logMap
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
                    po.OtherOptions |> Array.partition (FscArguments.isCompileFile)
                { po with SourceFiles = compileFiles; OtherOptions = otherOptions }
            | _ ->
                //HACK ref https://github.com/fsharp/FSharp.Compiler.Service/issues/803
                //  the ProjectCracker.GetProjectOptionsFromProjectFileLogged doesnt return the .fsi files inside
                //  the SourceFiles property, but are instead inside OtherOptions
                //  this workaround moving these **before** the corrisponding .fs file (.fsi file must precede the .fs)
                let fsiFiles, otherOptions =
                    po.OtherOptions |> Array.partition (fun (s:string) -> FscArguments.isCompileFile s && s.EndsWith(".fsi"))
                let fileNames =
                    po.SourceFiles
                    |> Array.fold (fun acc e ->
                        match fsiFiles |> Array.tryFind ((=) (e + "i")) with
                        | Some fsi ->
                            [| yield! acc; yield fsi; yield e  |]
                        | None -> [| yield! acc; yield e |] ) [||]

                { po with SourceFiles = fileNames ; OtherOptions = otherOptions }

        let rec setExtraInfo po =
            let outPath =
                match FscArguments.outputFile (Path.GetDirectoryName(po.ProjectFileName)) (po.OtherOptions |> List.ofArray) with
                | Some path -> path
                | None -> failwithf "Cannot find output argument (-o, --out) in project '%s' with args %A" po.ProjectFileName po
            { po with
                 SourceFiles = po.SourceFiles |> Array.map normalizeDirSeparators
                 ExtraProjectInfo = Some (box {
                    ExtraProjectInfoData.ProjectSdkType = ProjectSdkType.Verbose { TargetPath = outPath }
                    ProjectOutputType = po.OtherOptions |> List.ofArray |> FscArguments.outType
                 })
                 ReferencedProjects = po.ReferencedProjects |> Array.map (fun (path,p2p) -> path, (setExtraInfo p2p)) }

        Ok (setExtraInfo po, Array.toList po.SourceFiles, logMap)
      with e ->
        Error (GenericError(file, e.Message))
