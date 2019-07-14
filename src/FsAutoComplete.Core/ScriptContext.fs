/// Wraps up logic around converting project files into lists of arguments for FSI
module FsAutoComplete.ScriptContext

let inline isRefAssembly (reference: string) =
    reference.Contains("/ref/")

let isValidFSIReference (reference: string) = not <| isRefAssembly reference


module List =
    let satisfyAll tests list =
        let combinedTest = tests |> List.reduce (fun f j -> fun x -> f x && j x)
        list |> List.filter combinedTest

let isValidFSIOption =
    let badOptions =
        Set.ofList
            [ "--nocopyfsharpcore"
              "--noframework"
              "--highentropyva-" ]
    badOptions.Contains >> not

let isNotReference (option: string) = not <| option.StartsWith "-r:"

let isNotOutput (option: string) = not <| option.StartsWith "-o:"

let isNotTarget (option: string) = not <| option.StartsWith "--target:"

let makeForProject (projectInfo: ProjectCrackerCache) =
    // TODO: TFM differences?
    let dllReferences =
        projectInfo.References
        |> List.filter isValidFSIReference
        |> List.map (sprintf "--reference:%s")

    let otherOptions =
        projectInfo.Options.OtherOptions
        |> List.ofArray
        |> List.satisfyAll [ isValidFSIOption; isNotReference; isNotOutput; isNotTarget]

    // TODO: check referenced projects and figure out
    // a) what their output dll path is
    // b) if it's built or not.
    // if not built, throw an error?
    let referencedProjects = []

    let allFSIOptions =
        dllReferences @ otherOptions @ referencedProjects

    [], allFSIOptions
