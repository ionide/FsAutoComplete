/// LSP Types for [LSP 3.17.0](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
/// -- which isn't released yet!  
/// -> proposed state
module Ionide.LanguageServerProtocol.Types

open Ionide.LanguageServerProtocol.Types

/// Inlay hint client capabilities.
type InlayHintClientCapabilitiesResolveSupport = {
  /// The properties that a client can resolve lazily.
  Properties: string[]
}
type InlayHintClientCapabilities = {
  /// Whether inlay hints support dynamic registration.
  DynamicRegistration: bool option
  /// Indicates which properties a client can resolve lazily on a inlay
  /// hint.
  ResolveSupport: InlayHintClientCapabilitiesResolveSupport option
}


/// Inlay hint options used during static registration.
type InlayHintOptions = (*WorkDoneProgressOptions &*) {
  /// The server provides support to resolve additional
  /// information for an inlay hint item.
  ResolveProvider: bool option
}
/// Inlay hint options used during static or dynamic registration.
type InlayHintRegistrationOptions = InlayHintOptions (*& TextDocumentRegistrationOptions & StaticRegistrationOptions*)


/// A parameter literal used in inlay hint requests.
type InlayHintParams = (*WorkDoneProgressParams &*) {
  /// The text document.
  TextDocument: TextDocumentIdentifier
  /// The visible document range for which inlay hints should be computed.
  Range: Range
}

/// Inlay hint kinds.
[<RequireQualifiedAccess>]
type InlayHintKind = 
    /// An inlay hint that for a type annotation.
  | Type = 1
    /// An inlay hint that is for a parameter.
  | Parameter = 2
[<ErasedUnion>]
[<RequireQualifiedAccess>]
type InlayHintTooltip =
  | String of string
  | Markup of MarkupContent
/// An inlay hint label part allows for interactive and composite labels
/// of inlay hints.
type InlayHintLabelPart = {
  /// The value of this label part.
  Value: string
  /// The tooltip text when you hover over this label part. Depending on
  /// the client capability `inlayHint.resolveSupport` clients might resolve
  /// this property late using the resolve request.
  Tooltip: InlayHintTooltip option
  /// An optional source code location that represents this
  /// label part.
  ///
  /// The editor will use this location for the hover and for code navigation
  /// features: This part will become a clickable link that resolves to the
  /// definition of the symbol at the given location (not necessarily the
  /// location itself), it shows the hover that shows at the given location,
  /// and it shows a context menu with further code navigation commands.
  ///
  /// Depending on the client capability `inlayHint.resolveSupport` clients
  /// might resolve this property late using the resolve request.
  Location: Location option
  /// An optional command for this label part.
  ///
  /// Depending on the client capability `inlayHint.resolveSupport` clients
  /// might resolve this property late using the resolve request.
  Command: Command option
}
[<ErasedUnion>]
[<RequireQualifiedAccess>]
type InlayHintLabel = 
  | String of string
  | Parts of InlayHintLabelPart[]
/// Inlay hint information.
type InlayHint<'Data> = {
  /// The position of this hint.
  Position: Position
  /// The label of this hint. A human readable string or an array of
  /// InlayHintLabelPart label parts.
  ///
  /// *Note* that neither the string nor the label part can be empty.
  Label: InlayHintLabel
  /// he kind of this hint. Can be omitted in which case the client
  /// should fall back to a reasonable default.
  Kind: InlayHintKind option
  /// Optional text edits that are performed when accepting this inlay hint.
  ///
  /// *Note* that edits are expected to change the document so that the inlay
  /// hint (or its nearest variant) is now part of the document and the inlay
  /// hint itself is now obsolete.
  ///
  /// Depending on the client capability `inlayHint.resolveSupport` clients
  /// might resolve this property late using the resolve request.
  TextEdits: TextEdit[] option
  /// The tooltip text when you hover over this item.
  ///
  /// Depending on the client capability `inlayHint.resolveSupport` clients
  /// might resolve this property late using the resolve request.
  Tooltip: InlayHintTooltip option
  /// Render padding before the hint.
  ///
  /// Note: Padding should use the editor's background color, not the
  /// background color of the hint itself. That means padding can be used
  /// to visually align/separate an inlay hint.
  PaddingLeft: bool option
  /// Render padding after the hint.
  ///
  /// Note: Padding should use the editor's background color, not the
  /// background color of the hint itself. That means padding can be used
  /// to visually align/separate an inlay hint.
  PaddingRight: bool option

  /// A data entry field that is preserved on a inlay hint between
  /// a `textDocument/inlayHint` and a `inlayHint/resolve` request.
  /// 
  /// Note: In LSP specs: of type `LSPAny`:
  /// ```typescript
  /// export type LSPAny = LSPObject | LSPArray | string | integer | uinteger | decimal | boolean | null;
  /// export type LSPObject = { [key: string]: LSPAny };
  /// export type LSPArray = LSPAny[];
  /// ```
  /// -> `'Data` must adhere to specs
  Data: 'Data option
}

/// Client workspace capabilities specific to inlay hints.
type InlayHintWorkspaceClientCapabilities = {
  /// Whether the client implementation supports a refresh request sent from
  /// the server to the client.
  ///
  /// Note that this event is global and will force the client to refresh all
  /// inlay hints currently shown. It should be used with absolute care and
  /// is useful for situation where a server for example detects a project wide
  /// change that requires such a calculation.
  RefreshSupport: bool option
}
