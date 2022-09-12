namespace FsAutoComplete.Lsp
open Ionide.LanguageServerProtocol.Types
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
  // inherit ILspServer
  abstract FSharpSignature : TextDocumentPositionParams -> Async<LspResult<PlainNotification>>
  abstract FSharpSignatureData : TextDocumentPositionParams -> Async<LspResult<PlainNotification>>
  abstract FSharpDocumentationGenerator : OptionallyVersionedTextDocumentPositionParams -> AsyncLspResult<unit>
  abstract FSharpLineLense : ProjectParms -> Async<LspResult<PlainNotification>>
  abstract FSharpCompilerLocation : obj ->  Async<LspResult<PlainNotification>>
  abstract FSharpWorkspaceLoad : WorkspaceLoadParms -> Async<LspResult<PlainNotification>>
  abstract FSharpWorkspacePeek : WorkspacePeekRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpProject : ProjectParms ->  Async<LspResult<PlainNotification>>
  abstract FSharpFsdn : FsdnRequest ->  Async<LspResult<PlainNotification>>
  abstract FSharpDotnetNewList :DotnetNewListRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpDotnetNewRun :DotnetNewRunRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpDotnetAddProject :DotnetProjectRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpDotnetRemoveProject :DotnetProjectRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpDotnetSlnAdd :DotnetProjectRequest -> Async<LspResult<PlainNotification>>
  abstract FSharpHelp :TextDocumentPositionParams -> Async<LspResult<PlainNotification>>
  abstract FSharpDocumentation :TextDocumentPositionParams -> Async<LspResult<PlainNotification>>
  abstract FSharpDocumentationSymbol :DocumentationForSymbolReuqest -> Async<LspResult<PlainNotification>>
  abstract FSharpLiterateRequest : FSharpLiterateRequest -> Async<LspResult<PlainNotification>>
  abstract LoadAnalyzers : obj -> Async<LspResult<PlainNotification>>
  abstract FSharpPipelineHints :FSharpPipelineHintRequest -> Async<LspResult<PlainNotification>>
  abstract FsProjMoveFileUp :DotnetFileRequest -> Async<LspResult<PlainNotification>>
  abstract FsProjMoveFileDown :DotnetFileRequest -> Async<LspResult<PlainNotification>>
  abstract FsProjAddFileAbove :DotnetFile2Request -> Async<LspResult<PlainNotification>>
  abstract FsProjAddFileBelow :DotnetFile2Request -> Async<LspResult<PlainNotification>>
  abstract FsProjAddFile :DotnetFileRequest -> Async<LspResult<PlainNotification>>
  abstract FsProjRemoveFile :DotnetFileRequest -> Async<LspResult<PlainNotification>>
  abstract FsProjAddExistingFile :DotnetFileRequest -> Async<LspResult<PlainNotification>>
