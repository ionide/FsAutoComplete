namespace FsAutoComplete

open System

module DotnetCli =

  let private convertObjToString (o: obj) : string =
    let result =
      match o with
      | :? string as s -> sprintf "%s" s
      | :? bool as s -> if s then "true" else "false"
      | :? (string list) as str -> String.concat ", " (str |> List.map string)
      | _ -> failwithf "The value %A is not supported as parameter" o

    result

  let runDotnet args =
    let si = Diagnostics.ProcessStartInfo()
    si.FileName <- "dotnet"
    si.Arguments <- args
    si.UseShellExecute <- false
    si.RedirectStandardOutput <- true
    si.WorkingDirectory <- Environment.CurrentDirectory
    let proc = Diagnostics.Process.Start(si)
    Utils.ProcessHelper.WaitForExitAsync proc


  let dotnetNew
    (templateShortName: string)
    (name: string option)
    (output: string option)
    (parameterList: (string * obj) list)
    =
    let str = "new " + templateShortName + " -lang F#"

    let str =
      match name with
      | None -> str
      | Some s -> str + " -n " + s

    let str =
      match output with
      | None -> str
      | Some s -> str + " -o " + s

    let plist =
      parameterList
      |> List.map (fun (k, v) ->
        let asString = convertObjToString v
        k, asString)

    let str2 =
      plist
      |> List.map (fun (k, v) ->
        let theString = k + " " + v
        theString)
      |> String.concat " "

    let args = str + " " + str2
    runDotnet args

  let dotnetAddProject (toProject: string) (reference: string) =
    let args = sprintf "add %s reference %s" toProject reference
    runDotnet args

  let dotnetRemoveProject (fromProjet: string) (reference: string) =
    let args = sprintf "remove %s reference %s" fromProjet reference
    runDotnet args

  let dotnetSlnAdd (toSln: string) (project: string) =
    let args = sprintf "sln %s add %s" toSln project
    runDotnet args
