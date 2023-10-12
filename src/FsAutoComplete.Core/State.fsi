namespace FsAutoComplete

open System
open System.Collections.Concurrent
open System.Threading
open FSharp.Compiler.Text
open Ionide.ProjInfo.ProjectSystem
open FSharp.UMX
open System.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FsToolkit.ErrorHandling
open FsAutoComplete.FCSPatches

[<AutoOpen>]
module ProjInfoExtensions =
  type FSharpReferencedProject with

    member ProjectFilePath: string option

  type FSharpProjectOptions with

    member OutputDll: string
    member SourceFilesThatThisFileDependsOn: file: string<LocalPath> -> string array
    member SourceFilesThatDependOnFile: file: string<LocalPath> -> string array

  type ProjectController with

    /// returns all projects that depend on this one, transitively
    member GetDependentProjectsOfProjects: ps: FSharpProjectOptions list -> FSharpProjectOptions list
    /// crawls the project set and returns projects that contain a given file
    member ProjectsThatContainFile: file: string<LocalPath> -> FSharpProjectOptions list

type DeclName = string

type CompletionNamespaceInsert =
  { Namespace: string
    Position: Position
    Scope: ScopeKind }
