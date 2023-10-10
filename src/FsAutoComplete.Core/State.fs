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
open FCSPatches

[<AutoOpen>]
module ProjInfoExtensions =

  let private internalGetCSharpReferenceInfo =
    fun (r: FSharpReferencedProject) ->
      let rCase, fields =
        FSharp.Reflection.FSharpValue.GetUnionFields(
          r,
          typeof<FSharpReferencedProject>,
          System.Reflection.BindingFlags.Public
          ||| System.Reflection.BindingFlags.NonPublic
          ||| System.Reflection.BindingFlags.Instance
        )

      if rCase.Name = "PEReference" then
        let getStamp: unit -> DateTime = fields[0] :?> _
        let reader = fields[1]
        Some(getStamp, reader)
      else
        None

  let private internalGetProjectOptions =
    fun (r: FSharpReferencedProject) ->
      let rCase, fields =
        FSharp.Reflection.FSharpValue.GetUnionFields(
          r,
          typeof<FSharpReferencedProject>,
          System.Reflection.BindingFlags.Public
          ||| System.Reflection.BindingFlags.NonPublic
          ||| System.Reflection.BindingFlags.Instance
        )

      if rCase.Name = "FSharpReference" then
        let projOptions: FSharpProjectOptions = rCase.GetFields().[1].GetValue(box r) :?> _
        Some projOptions
      else
        None

  type FSharpReferencedProject with

    member x.ProjectFilePath =
      let rCase, fields =
        FSharp.Reflection.FSharpValue.GetUnionFields(
          x,
          typeof<FSharpReferencedProject>,
          System.Reflection.BindingFlags.Public
          ||| System.Reflection.BindingFlags.NonPublic
          ||| System.Reflection.BindingFlags.Instance
        )

      if rCase.Name = "FSharpReference" then
        (fields[1] :?> FSharpProjectOptions).ProjectFileName |> Some
      else
        None

  type FSharpProjectOptions with

    member x.OutputDll =
      x.OtherOptions |> Array.find (fun o -> o.StartsWith("-o:")) |> (fun s -> s[3..])

    member x.SourceFilesThatThisFileDependsOn(file: string<LocalPath>) =
      let untagged = UMX.untag file

      match Array.tryFindIndex ((=) untagged) x.SourceFiles with
      | None -> [||]
      | Some 0 -> [||] // at the start, so no possible dependents
      | Some index -> x.SourceFiles[0..index]


    member x.SourceFilesThatDependOnFile(file: string<LocalPath>) =
      let untagged = UMX.untag file

      match Array.tryFindIndex ((=) untagged) x.SourceFiles with
      | None -> [||]
      | Some index when index < x.SourceFiles.Length -> x.SourceFiles[index + 1 ..]
      | Some index -> [||] // at the end, so return empty list

  type ProjectController with

    /// returns all projects that depend on this one, transitively
    member x.GetDependentProjectsOfProjects(ps: FSharpProjectOptions list) : list<FSharpProjectOptions> =
      let projectSnapshot = x.ProjectOptions |> Seq.map snd
      let allDependents = System.Collections.Generic.HashSet<FSharpProjectOptions>()

      let currentPass = ResizeArray()
      currentPass.AddRange(ps |> List.map (fun p -> p.ProjectFileName))

      let mutable continueAlong = true

      while continueAlong do
        let dependents =
          projectSnapshot
          |> Seq.filter (fun p ->
            p.ReferencedProjects
            |> Seq.exists (fun r ->
              match r.ProjectFilePath with
              | None -> false
              | Some p -> currentPass.Contains(p)))

        if Seq.isEmpty dependents then
          continueAlong <- false
          currentPass.Clear()
        else
          for d in dependents do
            allDependents.Add d |> ignore<bool>

          currentPass.Clear()
          currentPass.AddRange(dependents |> Seq.map (fun p -> p.ProjectFileName))

      Seq.toList allDependents

    /// crawls the project set and returns projects that contain a given file
    member x.ProjectsThatContainFile(file: string<LocalPath>) =
      let untagged = UMX.untag file

      x.ProjectOptions
      |> Seq.choose (fun (_, p) ->
        if p.SourceFiles |> Array.contains untagged then
          Some p
        else
          None)
      |> Seq.distinct
      |> Seq.toList

type DeclName = string

type CompletionNamespaceInsert =
  { Namespace: string
    Position: Position
    Scope: ScopeKind }
