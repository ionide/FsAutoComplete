namespace FsAutoComplete.Lsp

open System.IO
open Ionide.LanguageServerProtocol.Server
open FsAutoComplete
open FSharp.Compiler.CodeAnalysis

type FSharpLspServer =
    new: state: State * lspClient: FSharpLspClient -> FSharpLspServer
    interface IFSharpLspServer

    member ScriptFileProjectOptions: IEvent<FSharpProjectOptions>

module FSharpLspServer =
    open System.Threading.Tasks
    open StreamJsonRpc

    val startCore:
        toolsPath: Ionide.ProjInfo.Types.ToolsPath ->
        stateStorageDir: DirectoryInfo ->
        workspaceLoaderFactory: (Ionide.ProjInfo.Types.ToolsPath -> Ionide.ProjInfo.IWorkspaceLoader) ->
            LspCloseReason

    val start: startCore: (unit -> LspCloseReason) -> int
