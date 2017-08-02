module FsAutoComplete.WorkspacePeek

open System
open System.IO

type SolutionData = {
    Items: SolutionItem list
    Configurations: SolutionConfiguration list
    }
and SolutionConfiguration = {
    Id: string
    ConfigurationName: string
    PlatformName: string
    IncludeInBuild: bool
    }
and SolutionItem = {
    Guid: Guid
    Name: string
    Kind: SolutionItemKind
    }
and SolutionItemKind =
    | MsbuildFormat of SolutionItemMsbuildConfiguration list
    | Folder of (SolutionItem list) * (string list)
    | Unsupported
    | Unknown
and SolutionItemMsbuildConfiguration = {
    Id: string
    ConfigurationName: string
    PlatformName: string
    }

[<RequireQualifiedAccess>]
type Interesting =
| Solution of string * SolutionData
| Directory of string
| Fsx of string

let tryParseSln slnFilePath = 
    let slnFile =
        try
            Microsoft.Build.Construction.SolutionFile.Parse(slnFilePath)
            |> Some
        with _ ->
            None
    match slnFile with
    | None -> None
    | Some sln ->
        let rec parseItem (item: Microsoft.Build.Construction.ProjectInSolution) : SolutionItem =
            let parseKind (item: Microsoft.Build.Construction.ProjectInSolution) : SolutionItemKind =
                match item.ProjectType with
                | Microsoft.Build.Construction.SolutionProjectType.KnownToBeMSBuildFormat ->
                    SolutionItemKind.MsbuildFormat []
                | Microsoft.Build.Construction.SolutionProjectType.SolutionFolder ->
                    let children =
                        sln.ProjectsInOrder
                        |> Seq.filter (fun x -> x.ParentProjectGuid = item.ProjectGuid)
                        |> Seq.map parseItem
                        |> List.ofSeq
                    SolutionItemKind.Folder (children, (item.FolderFiles |> List.ofSeq))
                | Microsoft.Build.Construction.SolutionProjectType.EtpSubProject
                | Microsoft.Build.Construction.SolutionProjectType.WebDeploymentProject
                | Microsoft.Build.Construction.SolutionProjectType.WebProject ->
                    SolutionItemKind.Unsupported
                | Microsoft.Build.Construction.SolutionProjectType.Unknown
                | _ ->
                    SolutionItemKind.Unknown

            { Guid = item.ProjectGuid |> Guid.Parse
              Name = item.ProjectName
              Kind = parseKind item }

        let items =
            sln.ProjectsInOrder
            |> Seq.filter (fun x -> isNull x.ParentProjectGuid)
            |> Seq.map parseItem
        let data = {
            Items = items |> List.ofSeq
            Configurations = []
        }
        Some (Interesting.Solution (slnFilePath, data))

open System.IO

let peek13 (rootDir: string) deep =
    let dirInfo = DirectoryInfo(rootDir)

    let ignored (s: string) = s.StartsWith(".")

    let scanDir (dirInfo: DirectoryInfo) =
        let slnsFiles, fsxsFiles =
            dirInfo.EnumerateFiles("*.*", SearchOption.TopDirectoryOnly)
            |> Seq.filter (fun s -> s.FullName.EndsWith(".sln") || s.FullName.EndsWith(".fsx"))
            |> Seq.toArray
            |> Array.partition (fun s -> s.FullName.EndsWith(".sln"))

        let slns =
            slnsFiles
            |> Array.choose (fun f -> tryParseSln f.FullName)
        let fsxs =
            fsxsFiles
            |> Array.map (fun f -> Interesting.Fsx (f.FullName))
        slns, fsxs

    let dirs =
        let rec scanDirs (dirInfo: DirectoryInfo) lvl =
            seq {
                if lvl <= deep then
                    yield dirInfo
                    for s in dirInfo.GetDirectories() do
                        if not(ignored s.Name) then
                            yield! scanDirs s (lvl + 1)
            }

        scanDirs dirInfo 0
        |> Array.ofSeq

    let found = dirs |> Array.Parallel.map scanDir

    let slns = found |> Array.map fst |> Array.collect id |> Array.sortBy (function Interesting.Solution (p,_) -> p | _ -> "")
    let fsxs = found |> Array.map snd |> Array.collect id |> Array.sortBy (function Interesting.Fsx p -> p | _ -> "")

    let dir = Interesting.Directory rootDir
    [ yield! slns; yield! fsxs; yield dir ]
