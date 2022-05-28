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
      let isScript = isAScript loc.FileName
      // sometimes the source file locations start with a capital, despite all of our efforts.
      let normalizedPath =
        if System.Char.IsUpper(loc.FileName[0]) then
          string (System.Char.ToLowerInvariant loc.FileName[0])
          + (loc.FileName.Substring(1))
        else
          loc.FileName

      let taggedFilePath = UMX.tag normalizedPath

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
        let projectsThatContainFile = state.ProjectController.ProjectsThatContainFile(taggedFilePath)
        let projectsThatDependOnContainingProjects =
          state.ProjectController.GetDependentProjectsOfProjects projectsThatContainFile

        match projectsThatDependOnContainingProjects with
        | [] -> Some(SymbolDeclarationLocation.Projects(projectsThatContainFile, isSymbolLocalForProject))
        | projects ->
          Some(SymbolDeclarationLocation.Projects(projectsThatContainFile @ projects, isSymbolLocalForProject))
    | None -> None
