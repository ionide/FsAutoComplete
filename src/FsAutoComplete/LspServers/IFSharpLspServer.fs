namespace FsAutoComplete.Lsp

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Ionide.LanguageServerProtocol.JsonRpc
open FsAutoComplete.LspHelpers

type OptionallyVersionedTextDocumentPositionParams =
  {
    /// The text document.
    TextDocument: VersionedTextDocumentIdentifier
    /// The position inside the text document.
    Position: Ionide.LanguageServerProtocol.Types.Position
  }

  interface ITextDocumentPositionParams with
    member this.TextDocument = { Uri = this.TextDocument.Uri }
    member this.Position = this.Position

[<Interface>]
type IFSharpLspServer =
  inherit ILspServer
  abstract FSharpSignature: TextDocumentPositionParams -> Async<LspResult<PlainNotification option>>
  abstract FSharpSignatureData: TextDocumentPositionParams -> Async<LspResult<PlainNotification option>>
  abstract FSharpDocumentationGenerator: OptionallyVersionedTextDocumentPositionParams -> AsyncLspResult<unit>
  abstract FSharpLineLens: ProjectParms -> Async<LspResult<PlainNotification option>>
  abstract FSharpWorkspaceLoad: WorkspaceLoadParms -> Async<LspResult<PlainNotification>>
  abstract FSharpWorkspacePeek: WorkspacePeekRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpProject: ProjectParms -> Async<LspResult<PlainNotification>>
  abstract FSharpFsdn: FsdnRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpDotnetNewList: DotnetNewListRequest -> Async<LspResult<PlainNotification option>>
  abstract FSharpDotnetNewRun: DotnetNewRunRequest -> Async<LspResult<PlainNotification option>>
  abstract FSharpDotnetAddProject: DotnetProjectRequest -> Async<LspResult<PlainNotification option>>
  abstract FSharpDotnetRemoveProject: DotnetProjectRequest -> Async<LspResult<PlainNotification option>>
  abstract FSharpDotnetSlnAdd: DotnetProjectRequest -> Async<LspResult<PlainNotification option>>
  abstract FSharpHelp: TextDocumentPositionParams -> Async<LspResult<PlainNotification option>>
  abstract FSharpDocumentation: TextDocumentPositionParams -> Async<LspResult<PlainNotification option>>
  abstract FSharpDocumentationSymbol: DocumentationForSymbolRequest -> Async<LspResult<PlainNotification option>>
  abstract FSharpLiterateRequest: FSharpLiterateRequest -> Async<LspResult<PlainNotification>>
  abstract LoadAnalyzers: obj -> Async<LspResult<unit>>
  abstract FSharpPipelineHints: FSharpPipelineHintRequest -> Async<LspResult<PlainNotification option>>
  abstract FsProjMoveFileUp: DotnetFileRequest -> Async<LspResult<PlainNotification option>>
  abstract FsProjMoveFileDown: DotnetFileRequest -> Async<LspResult<PlainNotification option>>
  abstract FsProjAddFileAbove: DotnetFile2Request -> Async<LspResult<PlainNotification option>>
  abstract FsProjAddFileBelow: DotnetFile2Request -> Async<LspResult<PlainNotification option>>
  abstract FsProjRenameFile: DotnetRenameFileRequest -> Async<LspResult<PlainNotification option>>
  abstract FsProjAddFile: DotnetFileRequest -> Async<LspResult<PlainNotification option>>
  abstract FsProjRemoveFile: DotnetFileRequest -> Async<LspResult<PlainNotification option>>
  abstract FsProjAddExistingFile: DotnetFileRequest -> Async<LspResult<PlainNotification option>>
  abstract TestDiscoverTests: unit -> Async<LspResult<PlainNotification option>>
  abstract TestRunTests: unit -> Async<LspResult<PlainNotification option>>
