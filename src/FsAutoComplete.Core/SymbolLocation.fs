module FsAutoComplete.SymbolLocation

open FSharp.Compiler.EditorServices
open FSharp.Compiler.CodeAnalysis
open System.IO
open FSharp.UMX
open FsToolkit.ErrorHandling

[<RequireQualifiedAccess; NoComparison>]
type SymbolDeclarationLocation =
  | CurrentDocument
  | Projects of FSharpProjectSnapshot list * isLocalForProject: bool

let getDeclarationLocation
  (
    symbolUse: FSharpSymbolUse,
    currentDocument: IFSACSourceText,
    getProjectOptions,
    projectsThatContainFile: string<LocalPath> -> Async<FSharpProjectSnapshot list>,
    getDependentProjectsOfProjects: FSharpProjectSnapshot list -> FSharpProjectSnapshot list
  ) : Async<Option<SymbolDeclarationLocation>> =
  asyncOption {

    // `symbolUse.IsPrivateToFile` throws exception when no `DeclarationLocation`
    if
      symbolUse.Symbol.DeclarationLocation |> Option.isSome
      && symbolUse.IsPrivateToFile
    then
      return SymbolDeclarationLocation.CurrentDocument
    else
      let isSymbolLocalForProject = symbolUse.Symbol.IsInternalToProject

      let declarationLocation =
        match symbolUse.Symbol.ImplementationLocation with
        | Some x -> Some x
        | None -> symbolUse.Symbol.DeclarationLocation

      let! loc = declarationLocation
      let isScript = isAScript loc.FileName
      // sometimes the source file locations start with a capital, despite all of our efforts.
      let normalizedPath =
        if System.Char.IsUpper(loc.FileName[0]) then
          string (System.Char.ToLowerInvariant loc.FileName[0])
          + (loc.FileName.Substring(1))
        else
          loc.FileName

      let taggedFilePath = UMX.tag normalizedPath

      if isScript && taggedFilePath = currentDocument.FileName then
        return SymbolDeclarationLocation.CurrentDocument
      elif isScript then
        // The standalone script might include other files via '#load'
        // These files appear in project options and the standalone file
        // should be treated as an individual project
        return!
          getProjectOptions (taggedFilePath)
          |> AsyncOption.map (fun p -> SymbolDeclarationLocation.Projects([ p ], isSymbolLocalForProject))
      else
        match! projectsThatContainFile (taggedFilePath) with
        | [] -> return! None
        | projectsThatContainFile ->
          let projectsThatDependOnContainingProjects = getDependentProjectsOfProjects projectsThatContainFile

          match projectsThatDependOnContainingProjects with
          | [] -> return (SymbolDeclarationLocation.Projects(projectsThatContainFile, isSymbolLocalForProject))
          | projects ->
            return (SymbolDeclarationLocation.Projects(projectsThatContainFile @ projects, isSymbolLocalForProject))

  }
