namespace FsAutoComplete

open System
open System.IO
open System.Reflection

open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCrackerScript =

  let private getEnvironmentInfoProjResource () =
    let assembly = typeof<Utils.Pos>.GetTypeInfo().Assembly
    let resourceName = "EnvironmentInfo.proj"

    use stream = assembly.GetManifestResourceStream(resourceName)
    match stream with
    | null -> failwithf "Resource '%s' not found in assembly '%s'" resourceName (assembly.FullName)
    | stream ->
        use reader = new System.IO.StreamReader(stream)

        reader.ReadToEnd()

  let getAdditionalArguments () =

    let getCorlibAssemblies () =
      let createTempDir () =
        let tempPath = System.IO.Path.GetTempFileName()
        File.Delete(tempPath)
        Directory.CreateDirectory(tempPath).FullName

      let tempDir = createTempDir ()
      let proj = Path.Combine(tempDir, "EnvironmentInfo.proj")
      let projContent = getEnvironmentInfoProjResource ()
      File.WriteAllText(proj, projContent)

      let outFile = Path.Combine(tempDir, "fsxScriptReferences.txt")
      let targetFramework = "v4.5"
      let x, _ = ProjectCrackerDotnetSdk.runProcess (printfn "%s") tempDir "msbuild" (sprintf "EnvironmentInfo.proj /t:_GetFsxScriptReferences \"/p:_GetFsxScriptReferences_OutFile=%s\" /p:TargetFrameworkVersion=%s" outFile targetFramework)
      System.IO.File.ReadAllLines(outFile)

    [ yield "--simpleresolution"
      yield! getCorlibAssemblies ()
             |> Array.map (sprintf "-r:%s") ]
