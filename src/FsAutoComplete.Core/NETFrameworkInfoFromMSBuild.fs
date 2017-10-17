namespace FsAutoComplete

open System
open System.IO
open System.Reflection

open Microsoft.FSharp.Compiler.SourceCodeServices

module NETFrameworkInfoFromMSBuild =

  let private getEnvironmentInfoProjResource () =
    let assembly = typeof<Utils.Pos>.GetTypeInfo().Assembly
    let resourceName = "EnvironmentInfo.proj"

    use stream = assembly.GetManifestResourceStream(resourceName)
    match stream with
    | null -> failwithf "Resource '%s' not found in assembly '%s'" resourceName (assembly.FullName)
    | stream ->
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd()

  type private FrameworkInfoFromMsbuild = {
    TargetFrameworkRootPath: string option
    FrameworkPathOverride: string option
    ReferencePath: string list
  }

  let private getFrameworkInfo targetFramework =
    let createTempDir () =
      let tempPath = System.IO.Path.GetTempFileName()
      File.Delete(tempPath)
      Directory.CreateDirectory(tempPath).FullName

    let tempDir = createTempDir ()
    let proj = Path.Combine(tempDir, "EnvironmentInfo.proj")
    let projContent = getEnvironmentInfoProjResource ()
    File.WriteAllText(proj, projContent)

    let outFile = Path.Combine(tempDir, "fsxScriptReferences.txt")
    let msbuildArgs =
      [
        yield "EnvironmentInfo.proj"
        yield "/t:_GetFsxScriptReferences"
        yield sprintf "\"/p:_GetFsxScriptReferences_OutFile=%s\"" outFile
        match targetFramework with
        | Some tfm -> yield sprintf "/p:TargetFrameworkVersion=%s" tfm
        | None -> () ]
      |> String.concat " "
    //TODO log output for diagnostics
    let x, _ = Utils.runProcess ignore tempDir "msbuild" msbuildArgs
    let lines = System.IO.File.ReadAllLines(outFile) |> List.ofArray

    { TargetFrameworkRootPath = lines |> List.tryPick (chooseByPrefix "TargetFrameworkRootPath=")
      FrameworkPathOverride = lines |> List.tryPick (chooseByPrefix "FrameworkPathOverride=")
      ReferencePath = lines |> List.choose (chooseByPrefix "ReferencePath=") }

  let getAdditionalArguments targetFramework =
    [ yield "--simpleresolution"
      yield "--noframework"
      yield! getFrameworkInfo targetFramework
             |> fun x -> x.ReferencePath
             |> List.map (sprintf "-r:%s") ]

  let private lazyGetReferenceAssembliesPath =
    lazy(
      getFrameworkInfo None
      |> fun x -> x.FrameworkPathOverride
      |> Option.map Path.GetDirectoryName )

  let getReferenceAssembliesPath () =
    lazyGetReferenceAssembliesPath.Force()
