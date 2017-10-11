namespace FsAutoComplete

open System
open System.IO

open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCrackerScript =

  let getAdditionalArguments () =

    let getCorlibAssemblies () =
      let tempFile = System.IO.Path.GetTempFileName()
      let x, _ = ProjectCrackerDotnetSdk.runProcess (printfn "%s") @"E:\temp\getass\" "msbuild" (sprintf "prova2.1.csproj /t:_GetFsxScriptReferences \"/p:_GetFsxScriptReferences_OutFile=%s\" /p:TargetFrameworkVersion=v4.5" tempFile)
      System.IO.File.ReadAllLines(tempFile)

    [ yield "--simpleresolution"
      yield! getCorlibAssemblies ()
             |> Array.map (sprintf "-r:%s") ]
