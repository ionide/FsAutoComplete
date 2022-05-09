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
      let filePath = Path.GetFullPathSafe loc.FileName
      let isScript = isAScript filePath
      let filePath = UMX.tag filePath

      if isScript && filePath = currentDocument.FileName then
        Some SymbolDeclarationLocation.CurrentDocument
      elif isScript then
        // The standalone script might include other files via '#load'
        // These files appear in project options and the standalone file
        // should be treated as an individual project
        state.GetProjectOptions(filePath)
        |> Option.map (fun p -> SymbolDeclarationLocation.Projects([ p ], isSymbolLocalForProject))
      else
        let projectsThatContainFile =
          state.ProjectController.ProjectOptions
          |> Seq.choose (fun (_, p) ->
            if
              p.SourceFiles
              |> Array.contains (UMX.untag filePath)
            then
              Some p
            else
              None)
          |> Seq.distinct
          |> Seq.toList

        let projectsThatDependOnContainingProjects =
          state.ProjectController.GetDependentProjectsOfProjects projectsThatContainFile


        match projectsThatDependOnContainingProjects with
        | [] -> None
        | projects ->
          Some(SymbolDeclarationLocation.Projects(projectsThatContainFile @ projects, isSymbolLocalForProject))
    | None -> None
