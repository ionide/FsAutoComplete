module FsAutoComplete.SymbolLocation

open FSharp.Compiler.EditorServices
open FSharp.Compiler.CodeAnalysis
open System.IO
open FSharp.UMX

[<RequireQualifiedAccess; NoComparison>]
type SymbolDeclarationLocation =
  | CurrentDocument
  | Projects of FSharpProjectOptions list * isLocalForProject: bool

let getDeclarationLocation
  (
    symbolUse: FSharpSymbolUse,
    currentDocument: NamedText,
    state: State
  ) : SymbolDeclarationLocation option =
  if symbolUse.IsPrivateToFile then
    Some SymbolDeclarationLocation.CurrentDocument
  else
    let isSymbolLocalForProject = symbolUse.Symbol.IsInternalToProject

    let declarationLocation =
      match symbolUse.Symbol.ImplementationLocation with
      | Some x -> Some x
      | None -> symbolUse.Symbol.DeclarationLocation

    match declarationLocation with
    | Some loc ->
      let filePath =
        Path.FilePathToUri(Path.GetFullPathSafe loc.FileName)
        |> Path.FileUriToLocalPath

      let isScript = isAScript filePath
      let taggedFilePath = UMX.tag filePath
      let normalizedFilePath = Path.GetFullPathSafe filePath

      if isScript
         && taggedFilePath = currentDocument.FileName then
        Some SymbolDeclarationLocation.CurrentDocument
      elif isScript then
        // The standalone script might include other files via '#load'
        // These files appear in project options and the standalone file
        // should be treated as an individual project
        state.GetProjectOptions(taggedFilePath)
        |> Option.map (fun p -> SymbolDeclarationLocation.Projects([ p ], isSymbolLocalForProject))
      else
        let projectsThatContainFile =
          state.ProjectController.ProjectOptions
          |> Seq.choose (fun (_, p) ->
            if p.SourceFiles |> Array.contains normalizedFilePath //
            then
              Some p
            else
              None)
          |> Seq.distinct
          |> Seq.toList

        let projectsThatDependOnContainingProjects =
          state.ProjectController.GetDependentProjectsOfProjects projectsThatContainFile

        match projectsThatDependOnContainingProjects with
        | [] -> Some(SymbolDeclarationLocation.Projects(projectsThatContainFile, isSymbolLocalForProject))
        | projects ->
          Some(SymbolDeclarationLocation.Projects(projectsThatContainFile @ projects, isSymbolLocalForProject))
    | None -> None
