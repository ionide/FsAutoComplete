namespace FsAutoComplete.Lsp

open Ionide.LanguageServerProtocol.Server
open Ionide.ProjInfo
open FsAutoComplete
open FSharp.Compiler.CodeAnalysis

type AdaptiveFSharpLspServer =
  new:
    workspaceLoader: IWorkspaceLoader * lspClient: FSharpLspClient * sourceTextFactory: ISourceTextFactory ->
      AdaptiveFSharpLspServer

  interface IFSharpLspServer

  member ScriptFileProjectOptions: IEvent<FSharpProjectSnapshot>

module AdaptiveFSharpLspServer =
  open System.Threading.Tasks
  open StreamJsonRpc

  val createRpc: handler: IJsonRpcMessageHandler -> JsonRpc

  val startCore:
    toolsPath: 'a ->
    workspaceLoaderFactory: ('a -> #IWorkspaceLoader) ->
    sourceTextFactory: ISourceTextFactory ->
      LspCloseReason

  val start: startCore: (unit -> LspCloseReason) -> int
