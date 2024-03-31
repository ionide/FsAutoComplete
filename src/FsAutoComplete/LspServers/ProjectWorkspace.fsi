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

    val createSnapshots:
        inMemorySourceFiles: amap<string<LocalPath>, aval<VolatileFile>> ->
        sourceTextFactory: aval<ISourceTextFactory> ->
        loadedProjectsA: amap<string<LocalPath>, ProjectOptions> ->
            amap<string<LocalPath>, (ProjectOptions * aval<FSharpProjectSnapshot>)>
