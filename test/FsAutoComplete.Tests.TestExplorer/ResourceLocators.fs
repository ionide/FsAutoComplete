module internal ResourceLocators 

    open FsAutoComplete.TestServer
    open System.IO
    let tryFindVsTest () : string =
        let dotnetBinary =  
            Ionide.ProjInfo.Paths.dotnetRoot.Value
            |> Option.defaultWith (fun () -> failwith "Couldn't find dotnet root. The dotnet sdk must be installed to run these tests")

        let cwd = System.Environment.CurrentDirectory |> Some
        
        VSTestWrapper.tryFindVsTestFromDotnetRoot dotnetBinary.FullName cwd
        |> Result.defaultWith failwith
        |> _.FullName

    let sampleProjectsRootDir = Path.Combine(__SOURCE_DIRECTORY__, "../SampleTestProjects")
