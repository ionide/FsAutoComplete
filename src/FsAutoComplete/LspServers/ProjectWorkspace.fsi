namespace FsAutoComplete.ProjectWorkspace

open System

module Snapshots =
  open System
  open FsAutoComplete
  open System.Threading
  open FSharp.UMX
  open System.Threading.Tasks
  open Ionide.ProjInfo.Types
  open FSharp.Compiler.CodeAnalysis.ProjectSnapshot
  open System.IO
  open FSharp.Compiler.CodeAnalysis
  open FSharp.Data.Adaptive
  open FSharp.Compiler.Text
  open FsAutoComplete.Adaptive
  open Ionide.ProjInfo.Logging
  open System.Collections.Generic


  /// <summary>This will create FSharpProjectSnapshots for each ProjectOptions.</summary>
  /// <param name="inMemorySourceFiles">List of files opened in memory or by the editor</param>
  /// <param name="sourceTextFactory">Factory for retrieving ISourceText</param>
  /// <param name="loadedProjectsA">Projects that have been loaded by msbuild</param>
  /// <remarks>
  /// This allows us to create the DAG of snapshots. Various changes to parent snapshots (Source file changes, referenced assembly changes)
  /// will propagate creating new snapshots to its children.
  /// </remarks>
  /// <returns>An AMap of Project Options with an Adaptive FSharpProjectSnapshot</returns>
  val createSnapshots:
    inMemorySourceFiles: amap<string<LocalPath>, aval<VolatileFile>> ->
    sourceTextFactory: aval<ISourceTextFactory> ->
    loadedProjectsA: amap<string<LocalPath>, ProjectOptions> ->
      amap<string<LocalPath>, (ProjectOptions * aval<FSharpProjectSnapshot>)>
