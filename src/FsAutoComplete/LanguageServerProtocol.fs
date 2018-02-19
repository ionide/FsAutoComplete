module LanguageServerProtocol


let private dbgf format = Printf.ksprintf (fun s -> System.Diagnostics.Trace.WriteLine(s)) format

[<AutoOpen>]
module LspJsonConverters =
    open Microsoft.FSharp.Reflection
    open Newtonsoft.Json
    open System
    open System.Collections.Generic

    let inline memorise f =
        let d = Dictionary<_, _>()
        fun key ->
            match d.TryGetValue(key) with
            | (true, v) -> v
            | (false, _) ->
                let result = f key
                d.[key] <- result
                result

    type ErasedUnionAttribute() =
        inherit Attribute()

    type ErasedUnionConverter() =
        inherit JsonConverter()

        let canConvert =
            memorise (fun t ->
                if not (FSharpType.IsUnion t) then
                    false
                else
                    t.BaseType.GetCustomAttributes(typedefof<ErasedUnionAttribute>, false).Length > 0)

        override __.CanConvert(t) = canConvert t

        override __.WriteJson(writer, value, serializer) =
            let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
            let unionField = fields.[0]
            serializer.Serialize(writer, unionField)

        override __.ReadJson(_reader, _t, _existingValue, _serializer) =
            failwith "Not implemented"

module Protocol =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Newtonsoft.Json.Converters
    open System

    type TextDocumentSyncKind =
        | None = 0
        | Full = 1
        | Incremental = 2

    [<AutoOpen>]
    module General =
        type TraceSetting =
            | Off = 0
            | Messages = 1
            | Verbose = 2

        /// Capabilities for methods that support dynamic registration.
        type DynamicCapabilities = {
            /// Method supports dynamic registration.
            DynamicRegistration: bool option
        }

        /// Capabilities specific to `WorkspaceEdit`s
        type WorkspaceEditCapabilities = {
            /// The client supports versioned document changes in `WorkspaceEdit`s
            DocumentChanges: bool option
        }

        type SymbolKind =
        | File = 1
        | Module = 2
        | Namespace = 3
        | Package = 4
        | Class = 5
        | Method = 6
        | Property = 7
        | Field = 8
        | Constructor = 9
        | Enum = 10
        | Interface = 11
        | Function = 12
        | Variable = 13
        | Constant = 14
        | String = 15
        | Number = 16
        | Boolean = 17
        | Array = 18
        | Object = 19
        | Key = 20
        | Null = 21
        | EnumMember = 22
        | Struct = 23
        | Event = 24
        | Operator = 25
        | TypeParameter = 26

        /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
        type SymbolKindCapabilities = {
            /// The symbol kind values the client supports. When this
            /// property exists the client also guarantees that it will
            /// handle values outside its set gracefully and falls back
            /// to a default value when unknown.
            ///
            /// If this property is not present the client only supports
            /// the symbol kinds from `File` to `Array` as defined in
            /// the initial version of the protocol.
            ValueSet: SymbolKind[] option
        }
        with
            static member DefaultValueSet =
                [
                    SymbolKind.File
                    SymbolKind.Module
                    SymbolKind.Namespace
                    SymbolKind.Package
                    SymbolKind.Class
                    SymbolKind.Method
                    SymbolKind.Property
                    SymbolKind.Field
                    SymbolKind.Constructor
                    SymbolKind.Enum
                    SymbolKind.Interface
                    SymbolKind.Function
                    SymbolKind.Variable
                    SymbolKind.Constant
                    SymbolKind.String
                    SymbolKind.Number
                    SymbolKind.Boolean
                    SymbolKind.Array
                ]

        /// Capabilities specific to the `workspace/symbol` request.
        type SymbolCapabilities = {
            /// Symbol request supports dynamic registration.
            DynamicRegistration: bool option

            /// Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
            SymbolKind: SymbolKindCapabilities option
        }

        /// Workspace specific client capabilities.
        type WorkspaceClientCapabilities = {
            /// The client supports applying batch edits to the workspace by supporting
            /// the request 'workspace/applyEdit'
            ApplyEdit: bool option

            /// Capabilities specific to `WorkspaceEdit`s
            WorkspaceEdit: WorkspaceEditCapabilities option

            /// Capabilities specific to the `workspace/didChangeConfiguration` notification.
            DidChangeConfiguration: DynamicCapabilities option

            /// Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
            DidChangeWatchedFiles: DynamicCapabilities option

            /// Capabilities specific to the `workspace/symbol` request.
            Symbol: SymbolCapabilities option
        }

        type SynchronizationCapabilities = {
            /// Whether text document synchronization supports dynamic registration.
            DynamicRegistration: bool option

            /// The client supports sending will save notifications.
            WillSave: bool option

            /// The client supports sending a will save request and
            /// waits for a response providing text edits which will
            /// be applied to the document before it is saved.
            WillSaveWaitUntil: bool option

            /// The client supports did save notifications.
            DidSave: bool option
        }

        module MarkupKind =
            let PlainText = "plaintext"
            let Markdown = "markdown"

        type HoverCapabilities = {
            /// Whether hover synchronization supports dynamic registration.
            DynamicRegistration: bool option

            /// Client supports the follow content formats for the content
            /// property. The order describes the preferred format of the client.
            /// See `MarkupKind` for common values
            ContentFormat: string[] option
        }

        type CompletionItemCapabilities = {
            /// Client supports snippets as insert text.
            ///
            /// A snippet can define tab stops and placeholders with `$1`, `$2`
            /// and `${3:foo}`. `$0` defines the final tab stop, it defaults to
            /// the end of the snippet. Placeholders with equal identifiers are linked,
            /// that is typing in one will update others too.
            SnippetSupport: bool option

            /// Client supports commit characters on a completion item.
            CommitCharactersSupport: bool option

            /// Client supports the follow content formats for the documentation
            /// property. The order describes the preferred format of the client.
            /// See `MarkupKind` for common values
            DocumentationFormat: string[] option
        }

        type CompletionItemKind =
        | Text = 1
        | Method = 2
        | Function = 3
        | Constructor = 4
        | Field = 5
        | Variable = 6
        | Class = 7
        | Interface = 8
        | Module = 9
        | Property = 10
        | Unit = 11
        | Value = 12
        | Enum = 13
        | Keyword = 14
        | Snippet = 15
        | Color = 16
        | File = 17
        | Reference = 18
        | Folder = 19
        | EnumMember = 20
        | Constant = 21
        | Struct = 22
        | Event = 23
        | Operator = 24
        | TypeParameter = 25

        type CompletionItemKindCapabilities = {
            /// The completion item kind values the client supports. When this
            /// property exists the client also guarantees that it will
            /// handle values outside its set gracefully and falls back
            /// to a default value when unknown.
            ///
            /// If this property is not present the client only supports
            /// the completion items kinds from `Text` to `Reference` as defined in
            /// the initial version of the protocol.
            ValueSet: CompletionItemKind[] option
        }
        with
            static member DefaultValueSet =
                [
                    CompletionItemKind.Text
                    CompletionItemKind.Method
                    CompletionItemKind.Function
                    CompletionItemKind.Constructor
                    CompletionItemKind.Field
                    CompletionItemKind.Variable
                    CompletionItemKind.Class
                    CompletionItemKind.Interface
                    CompletionItemKind.Module
                    CompletionItemKind.Property
                    CompletionItemKind.Unit
                    CompletionItemKind.Value
                    CompletionItemKind.Enum
                    CompletionItemKind.Keyword
                    CompletionItemKind.Snippet
                    CompletionItemKind.Color
                    CompletionItemKind.File
                    CompletionItemKind.Reference
                ]

        /// Capabilities specific to the `textDocument/completion`
        type CompletionCapabilities = {
            /// Whether completion supports dynamic registration.
            DynamicRegistration: bool option

            /// The client supports the following `CompletionItem` specific
            /// capabilities.
            CompletionItem: CompletionItemCapabilities option

            CompletionItemKind: CompletionItemKindCapabilities option

            /// The client supports to send additional context information for a
            /// `textDocument/completion` request.
            ContextSupport: bool option
        }

        type SignatureInformationCapabilities = {
            /// Client supports the follow content formats for the documentation
            /// property. The order describes the preferred format of the client.
            /// See `MarkupKind` for common values
            DocumentationFormat: string[] option
        }

        type SignatureHelpCapabilities = {
            /// Whether signature help supports dynamic registration.
            DynamicRegistration: bool option

            /// The client supports the following `SignatureInformation`
            /// specific properties.
            SignatureInformation: SignatureInformationCapabilities option
        }

        /// apabilities specific to the `textDocument/documentSymbol`
        type DocumentSymbolCapabilities = {
            /// Whether document symbol supports dynamic registration.
            DynamicRegistration: bool option

            /// Specific capabilities for the `SymbolKind`.
            SymbolKind: SymbolKindCapabilities option
        }

        /// Text document specific client capabilities.
        type TextDocumentClientCapabilities = {
            Synchronization: SynchronizationCapabilities option

            /// Capabilities specific to the `textDocument/completion`
            Completion: CompletionCapabilities option

            /// Capabilities specific to the `textDocument/hover`
            Hover: HoverCapabilities option

            /// Capabilities specific to the `textDocument/signatureHelp`
            SignatureHelp: SignatureHelpCapabilities option

            /// Capabilities specific to the `textDocument/references`
            References: DynamicCapabilities option

            /// Whether document highlight supports dynamic registration.
            DocumentHighlight: DynamicCapabilities option

            /// apabilities specific to the `textDocument/documentSymbol`
            DocumentSymbol: DocumentSymbolCapabilities option

            /// Capabilities specific to the `textDocument/formatting`
            Formatting: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/rangeFormatting`
            RangeFormatting: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/onTypeFormatting`
            OnTypeFormatting: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/definition`
            Definition: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/codeAction`
            CodeAction: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/codeLens`
            CodeLens: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/documentLink`
            DocumentLink: DynamicCapabilities option

            /// Capabilities specific to the `textDocument/rename`
            Rename: DynamicCapabilities option
        }

        type ClientCapabilities = {
            /// Workspace specific client capabilities.
            Workspace: WorkspaceClientCapabilities option

            /// Text document specific client capabilities.
            TextDocument: TextDocumentClientCapabilities option

            /// Experimental client capabilities.
            Experimental: JToken option
        }

        type InitializeParams = {
            ProcessId: int option
            RootPath: string option
            RootUri: string option
            InitializationOptions: JToken option
            Capabilities: ClientCapabilities option
            [<JsonConverter(typeof<StringEnumConverter>, true)>]
            trace: string option
        }

        /// Completion options.
        type CompletionOptions = {
            /// The server provides support to resolve additional information for a completion item.
            ResolveProvider: bool option

            /// The characters that trigger completion automatically.
            TriggerCharacters: string[] option
        }

        /// Signature help options.
        type SignatureHelpOptions = {
            /// The characters that trigger signature help automatically.
            TriggerCharacters: string[] option
        }

        /// Code Lens options.
        type CodeLensOptions = {
            /// Code lens has a resolve provider as well.
            ResolveProvider: bool option;
        }

        /// Format document on type options
        type DocumentOnTypeFormattingOptions = {
            /// A character on which formatting should be triggered, like `}`.
            FirstTriggerCharacter: string

            /// More trigger characters.
            MoreTriggerCharacter: string[] option
        }

        /// Document link options
        type DocumentLinkOptions = {
            /// Document links have a resolve provider as well.
            ResolveProvider: bool option
        }

        /// Execute command options.
        type ExecuteCommandOptions = {
            /// The commands to be executed on the server
            commands: string[] option
        }

        /// Save options.
        type SaveOptions = {
            /// The client is supposed to include the content on save.
            IncludeText: bool option
        }

        type TextDocumentSyncOptions = {
            /// Open and close notifications are sent to the server.
            OpenClose: bool option

            /// Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
            /// and TextDocumentSyncKindIncremental.
            Change: TextDocumentSyncKind option

            /// Will save notifications are sent to the server.
            WillSave: bool option

            /// Will save wait until requests are sent to the server.
            WillSaveWaitUntil: bool option

            /// Save notifications are sent to the server.
            Save: SaveOptions option
        }
        with
            static member Default =
                {
                    OpenClose = None
                    Change = None
                    WillSave = None
                    WillSaveWaitUntil = None
                    Save = None
                }

        type ServerCapabilities = {
            /// Defines how text documents are synced. Is either a detailed structure defining each notification or
            /// for backwards compatibility the TextDocumentSyncKind number.
            TextDocumentSync: TextDocumentSyncOptions option

            /// The server provides hover support.
            HoverProvider: bool option

            /// The server provides completion support.
            CompletionProvider: CompletionOptions option

            /// The server provides signature help support.
            SignatureHelpProvider: SignatureHelpOptions option

            /// The server provides goto definition support.
            DefinitionProvider: bool option

            /// The server provides find references support.
            ReferencesProvider: bool option

            /// The server provides document highlight support.
            DocumentHighlightProvider: bool option

            /// The server provides document symbol support.
            DocumentSymbolProvider: bool option

            /// The server provides workspace symbol support.
            WorkspaceSymbolProvider: bool option

            /// The server provides code actions.
            CodeActionProvider: bool option

            /// The server provides code lens.
            CodeLensProvider: CodeLensOptions option

            /// The server provides document formatting.
            DocumentFormattingProvider: bool option

            /// The server provides document range formatting.
            DocumentRangeFormattingProvider: bool option

            /// The server provides document formatting on typing.
            DocumentOnTypeFormattingProvider: DocumentOnTypeFormattingOptions option

            /// The server provides rename support.
            RenameProvider: bool option

            /// The server provides document link support.
            DocumentLinkProvider: DocumentLinkOptions option

            /// The server provides execute command support.
            ExecuteCommandProvider: ExecuteCommandOptions option

            /// Experimental server capabilities.
            Experimental: JToken option
        }
        with
            static member Default =
                {
                    HoverProvider = None
                    TextDocumentSync = None
                    CompletionProvider = None
                    SignatureHelpProvider = None
                    DefinitionProvider = None
                    ReferencesProvider = None
                    DocumentHighlightProvider = None
                    DocumentSymbolProvider = None
                    WorkspaceSymbolProvider = None
                    CodeActionProvider = None
                    CodeLensProvider = None
                    DocumentFormattingProvider = None
                    DocumentRangeFormattingProvider = None
                    DocumentOnTypeFormattingProvider = None
                    RenameProvider = None
                    DocumentLinkProvider = None
                    ExecuteCommandProvider = None
                    Experimental = None
                }

        type InitializeResult = {
            Capabilities: ServerCapabilities
        }
        with
            static member Default =
                {
                    Capabilities = ServerCapabilities.Default
                }

    [<AutoOpen>]
    module Window =
        type MessageType =
            | Error = 1
            | Warning = 2
            | Info = 3
            | Log = 4

        type LogMessageParams = {
            Type: MessageType
            Message: string
        }

        type ShowMessageParams = {
            Type: MessageType
            Message: string
        }

    /// Position in a text document expressed as zero-based line and zero-based character offset.
    /// A position is between two characters like an ‘insert’ cursor in a editor.
    type Position = {
        /// Line position in a document (zero-based).
        Line: int

        /// Character offset on a line in a document (zero-based). Assuming that the line is
        /// represented as a string, the `character` value represents the gap between the
        /// `character` and `character + 1`.
        ///
        /// If the character value is greater than the line length it defaults back to the
        /// line length.
        Character: int
    }

    /// A range in a text document expressed as (zero-based) start and end positions.
    /// A range is comparable to a selection in an editor. Therefore the end position is exclusive.
    /// If you want to specify a range that contains a line including the line ending character(s)
    /// then use an end position denoting the start of the next line. For example:
    type Range = {
        /// The range's start position.
        Start: Position

        /// The range's end position.
        End: Position
    }

    type DocumentUri = string

    type ITextDocumentIdentifier =
        abstract member Uri : DocumentUri with get

    type TextDocumentIdentifier =
        {
            /// The text document's URI.
            Uri: DocumentUri
        }
        interface ITextDocumentIdentifier with
            member this.Uri with get() = this.Uri

    type VersionedTextDocumentIdentifier =
        {
            /// The text document's URI.
            Uri: DocumentUri

            /// The version number of this document. If a versioned text document identifier
            /// is sent from the server to the client and the file is not open in the editor
            /// (the server has not received an open notification before) the server can send
            /// `null` to indicate that the version is known and the content on disk is the
            /// truth (as speced with document content ownership)
            Version: int option;
        }
        interface ITextDocumentIdentifier with
            member this.Uri with get() = this.Uri

    type ITextDocumentPositionParams =
        /// The text document.
        abstract member TextDocument : TextDocumentIdentifier with get
        /// The position inside the text document.
        abstract member Position : Position with get

    type TextDocumentPositionParams =
        {
            /// The text document.
            TextDocument: TextDocumentIdentifier
            /// The position inside the text document.
            Position: Position
        }
        interface ITextDocumentPositionParams with
            member this.TextDocument with get() = this.TextDocument
            member this.Position with get() = this.Position

    /// A `MarkupContent` literal represents a string value which content is interpreted base on its
    /// kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
    ///
    /// If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
    /// See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
    ///
    /// Here is an example how such a string can be constructed using JavaScript / TypeScript:
    /// ```ts
    /// let markdown: MarkdownContent = {
    ///     kind: MarkupKind.Markdown,
    ///     value: [
    ///         '# Header',
    ///         'Some text',
    ///         '```typescript',
    ///         'someCode();',
    ///         '```'
    ///     ].join('\n')
    /// };
    /// ```
    ///
    /// *Please Note* that clients might sanitize the return markdown. A client could decide to
    /// remove HTML from the markdown to avoid script execution.
    type MarkupContent = {
        /// The type of the Markup
        Kind: string

        // The content itself
        Value: string
    }

    type MarkedStringData = {
        Language: string
        Value: string
    }

    [<ErasedUnion>]
    type MarkedString =
    | JustString of string
    | StringAndLanguage of MarkedStringData

    let plaintext s = { Kind = MarkupKind.PlainText; Value = s }
    let markdown s = { Kind = MarkupKind.Markdown; Value = s }

    [<ErasedUnion>]
    type HoverContent =
        | MarkedString of MarkedString
        | MarkedStrings of MarkedString []
        | MarkupContent of MarkupContent

    /// The result of a hover request.
    type Hover = {
        /// The hover's content
        Contents: HoverContent

        /// An optional range is a range inside a text document
        /// that is used to visualize a hover, e.g. by changing the background color.
        Range: Range option
    }

    /// An item to transfer a text document from the client to the server.
    type TextDocumentItem = {
        /// The text document's URI.
        Uri: DocumentUri

        /// The text document's language identifier.
        LanguageId: string

        /// The version number of this document (it will increase after each
        /// change, including undo/redo).
        Version: int

        /// The content of the opened text document.
        Text: string
    }

    type DidOpenTextDocumentParams = {
        /// The document that was opened.
        TextDocument: TextDocumentItem
    }

    /// An event describing a change to a text document. If range and rangeLength are omitted
    /// the new text is considered to be the full content of the document.
    type TextDocumentContentChangeEvent = {
        /// The range of the document that changed.
        Range: Range option

        /// The length of the range that got replaced.
        RangeLength: int option

        /// The new text of the range/document.
        Text: string
    }

    type DidChangeTextDocumentParams = {
        /// The document that did change. The version number points
        /// to the version after all provided content changes have
        /// been applied.
        TextDocument: VersionedTextDocumentIdentifier;

        /// The actual content changes. The content changes describe single state changes
        /// to the document. So if there are two content changes c1 and c2 for a document
        /// in state S10 then c1 move the document to S11 and c2 to S12.
        ContentChanges: TextDocumentContentChangeEvent[];
    }

    type FileChangeType =
        | Created = 1
        | Changed = 2
        | Deleted = 3

    /// An event describing a file change.
    type FileEvent ={
        /// The file's URI.
        Uri: DocumentUri

        /// The change type.
        Type: FileChangeType
    }

    type DidChangeWatchedFilesParams = {
        /// The actual file events.
        Changes: FileEvent[]
    }

    [<Flags>]
    type WatchKind =
        | Create = 1
        | Change = 2
        | Delete = 4

    type FileSystemWatcher = {
        /// The  glob pattern to watch
        GlobPattern: string

        /// The kind of events of interest. If omitted it defaults
        /// to WatchKind.Create | WatchKind.Change | WatchKind.Delete
        /// which is 7.
        Kind: WatchKind option
    }

    /// Describe options to be used when registered for text document change events.
    type DidChangeWatchedFilesRegistrationOptions = {
        /// The watchers to register.
        Watchers: FileSystemWatcher[]
    }

    /// How a completion was triggered
    type CompletionTriggerKind =
        /// Completion was triggered by typing an identifier (24x7 code
        /// complete), manual invocation (e.g Ctrl+Space) or via API.
        | Invoked = 1
        /// Completion was triggered by a trigger character specified by
        /// the `triggerCharacters` properties of the `CompletionRegistrationOptions`.
        | TriggerCharacter = 2

    type CompletionContext = {
        ///  How the completion was triggered.
        triggerKind: CompletionTriggerKind

        /// The trigger character (a single character) that has trigger code complete.
        /// Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`
        triggerCharacter: string option
    }

    type CompletionParams =
        {
            /// The text document.
            TextDocument: TextDocumentIdentifier

            /// The position inside the text document.
            Position: Position

            /// The completion context. This is only available it the client specifies
            /// to send this using `ClientCapabilities.textDocument.completion.contextSupport === true`
            Context: CompletionContext option
        }
        interface ITextDocumentPositionParams with
            member this.TextDocument with get() = this.TextDocument
            member this.Position with get() = this.Position

    /// A textual edit applicable to a text document.
    type TextEdit = {
        /// The range of the text document to be manipulated. To insert
        /// text into a document create a range where start === end.
        Range: Range

        /// The string to be inserted. For delete operations use an
        /// empty string.
        NewText: string
    }

    /// Represents a reference to a command. Provides a title which will be used to represent a command in the UI.
    /// Commands are identified by a string identifier. The protocol currently doesn’t specify a set of well-known
    /// commands. So executing a command requires some tool extension code.
    type Command = {
        /// Title of the command, like `save`.
        Title: string

        /// The identifier of the actual command handler.
        Command: string

        /// Arguments that the command handler should be
        /// invoked with.
        Arguments: JToken[] option
    }

    /// Defines whether the insert text in a completion item should be interpreted as
    /// plain text or a snippet.
    type InsertTextFormat =
        /// The primary text to be inserted is treated as a plain string.
        | PlainText = 1
        /// The primary text to be inserted is treated as a snippet.
        ///
        /// A snippet can define tab stops and placeholders with `$1`, `$2`
        /// and `${3:foo}`. `$0` defines the final tab stop, it defaults to
        /// the end of the snippet. Placeholders with equal identifiers are linked,
        /// that is typing in one will update others too.
        | Snippet = 2

    [<ErasedUnion>]
    type CompletionItemDocumentation =
    | StringDocumentation of string
    | MarkupDocumentation of MarkupContent

    type CompletionItem = {
        /// The label of this completion item. By default
        /// also the text that is inserted when selecting
        /// this completion.
        Label: string

        /// The kind of this completion item. Based of the kind
        /// an icon is chosen by the editor.
        Kind: int option

        /// A human-readable string with additional information
        /// about this item, like type or symbol information.
        Detail: string option

        /// A human-readable string that represents a doc-comment.
        Documentation: CompletionItemDocumentation option

        /// A string that should be used when comparing this item
        /// with other items. When `falsy` the label is used.
        SortText: string option

        /// A string that should be used when filtering a set of
        /// completion items. When `falsy` the label is used.
        FilterText: string option

        /// A string that should be inserted into a document when selecting
        /// this completion. When `falsy` the label is used.
        ///
        /// The `insertText` is subject to interpretation by the client side.
        /// Some tools might not take the string literally. For example
        /// VS Code when code complete is requested in this example `con<cursor position>`
        /// and a completion item with an `insertText` of `console` is provided it
        /// will only insert `sole`. Therefore it is recommended to use `textEdit` instead
        /// since it avoids additional client side interpretation.
        ///
        /// @deprecated Use textEdit instead.
        InsertText: string option

        /// The format of the insert text. The format applies to both the `insertText` property
        /// and the `newText` property of a provided `textEdit`.
        InsertTextFormat: InsertTextFormat option

        /// An edit which is applied to a document when selecting this completion. When an edit is provided the value of
        /// `insertText` is ignored.
        ///
        /// *Note:* The range of the edit must be a single line range and it must contain the position at which completion
        /// has been requested.
        TextEdit: TextEdit option

        /// An optional array of additional text edits that are applied when
        /// selecting this completion. Edits must not overlap with the main edit
        /// nor with themselves.
        AdditionalTextEdits: TextEdit[] option

        /// An optional set of characters that when pressed while this completion is active will accept it first and
        /// then type that character. *Note* that all commit characters should have `length=1` and that superfluous
        /// characters will be ignored.
        CommitCharacters: string[] option

        /// An optional command that is executed *after* inserting this completion. *Note* that
        /// additional modifications to the current document should be described with the
        /// additionalTextEdits-property.
        Command: Command option

        /// An data entry field that is preserved on a completion item between
        /// a completion and a completion resolve request.
        Data: JToken option
    }

    type CompletionList = {
        /// This list it not complete. Further typing should result in recomputing
        /// this list.
        IsIncomplete: bool

        /// The completion items.
        Items: CompletionItem[]
    }

    [<AutoOpen>]
    module Messages =
        type ClientRequest =
            | Initialize of InitializeParams
            | DidOpenTextDocument of DidOpenTextDocumentParams
            | DidChangeTextDocument of DidChangeTextDocumentParams
            | DidChangeWatchedFiles of DidChangeWatchedFilesParams
            | Completion of CompletionParams
            | Hover of TextDocumentPositionParams
            | Initialized
            | Shutdown
            | Exit
        with
            member this.IsNotification
                with get() =
                    match this with
                    | Initialize _
                    | Hover _
                    | Completion _ -> false
                    | DidChangeTextDocument _
                    | DidOpenTextDocument _
                    | DidChangeWatchedFiles _
                    | Initialized
                    | Shutdown
                    | Exit -> true

        type ServerRequest =
            | LogMessage of LogMessageParams
            | ShowMessage of ShowMessageParams
        with
            member this.AsJsonSerializable
                with get() =
                    match this with
                    | LogMessage x -> box x
                    | ShowMessage x -> box x
            member this.Method
                with get() =
                    match this with
                    | LogMessage _ -> "window/showMessage"
                    | ShowMessage _ -> "window/showMessage"

        type ServerResponse =
            | NoResponse
            | InvalidRequest of string
            | UnhandledRequest
            | InitializeResponse of InitializeResult
            | CompletionResponse of CompletionList option
            | HoverResponse of Hover option
        with
            member this.AsJsonSerializable
                with get() =
                    match this with
                    | InitializeResponse x -> box x
                    | HoverResponse x -> box x
                    | CompletionResponse x -> box x
                    | NoResponse
                    | InvalidRequest _
                    | UnhandledRequest -> failwith "Technical responses can't be sent as JSON"


module LowLevel =
    open System
    open System.IO
    open System.Text
    open System.Threading.Tasks
    open FSharp.Control.Tasks.ContextInsensitive

    let headerBufferSize = 300
    let minimumHeaderLength = 21
    let cr = byte '\r'
    let lf = byte '\f'
    let headerEncoding = Encoding.ASCII

    let private readLine (stream: Stream) =
        let buffer = Array.zeroCreate<byte> headerBufferSize
        let readCount = stream.Read(buffer, 0, 2)
        let mutable count = readCount
        if count < 2 then
            None
        else
            // TODO: Check that we don't over-fill headerBufferSize
            while count < headerBufferSize && (buffer.[count-2] <> cr && buffer.[count-1] <> lf) do
                 let additionalBytesRead  = stream.Read(buffer, count, 1)
                 // TODO: exit when additionalBytesRead = 0, end of stream
                 count <- count + additionalBytesRead

            if count >= headerBufferSize then
                None
            else
                Some (headerEncoding.GetString(buffer, 0, count - 2))

    let rec private readHeaders (stream: Stream) =
        let line = readLine stream
        match line with
        | Some "" -> []
        | Some line ->
            let separatorPos = line.IndexOf(": ")
            if separatorPos = -1 then
                raise (Exception(sprintf "Separator not found in header '%s'" line))
            else
                let name = line.Substring(0, separatorPos)
                let value = line.Substring(separatorPos + 2)
                let otherHeaders = readHeaders stream
                (name,value) :: otherHeaders
        | None ->
            raise (EndOfStreamException())

    let read (stream: Stream) =
        let headers = readHeaders stream

        let contentLength =
            headers
            |> List.tryFind(fun (name, _) -> name = "Content-Length")
            |> Option.map snd
            |> Option.bind (fun s -> match Int32.TryParse(s) with | true, x -> Some x | _ -> None)

        if contentLength = None then
            failwithf "Content-Length header not found"
        else
            let result = Array.zeroCreate<byte> contentLength.Value
            let readCount = stream.Read(result, 0, contentLength.Value)
            let str = Encoding.UTF8.GetString(result, 0, readCount)
            headers, str

    let write (stream: Stream) (data: string) =
        let asciiWriter = new StreamWriter(stream, Encoding.ASCII, 1024 * 1024, true)
        let bytes = Encoding.UTF8.GetBytes(data)
        asciiWriter.Write("Content-Length: ")
        asciiWriter.WriteLine(string bytes.Length)
        asciiWriter.WriteLine("Content-Type: utf-8")
        asciiWriter.WriteLine()
        asciiWriter.Flush()
        asciiWriter.Dispose()

        stream.Write(bytes, 0, bytes.Length)

module JsonRpc =
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq

    type Request = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Method: string
        Params: JToken option
    }
    with
        static member Create(id: int, method': string, rpcParams: JToken) =
            { Version = "2.0"; Id = Some id; Method = method'; Params = Some rpcParams }

        static member Create(method': string, rpcParams: JToken) =
            { Version = "2.0"; Id = None; Method = method'; Params = Some rpcParams }

    type Error = {
        Code: int
        Message: string
        Data: JToken option
    }
    with
        static member Create(code: int, message: string) =
            { Code = code; Message = message; Data = None }

    type Response = {
        [<JsonProperty("jsonrpc")>] Version: string
        Id: int option
        Error: Error option
        Result: JToken option
    }
    with
        static member Success(id: int, result: JToken) =
            { Version = "2.0"; Id = Some id; Result = Some result; Error = None }
        static member Failure(id: int option, error: Error) =
            { Version = "2.0"; Id = id; Result = None; Error = Some error }

module Server =
    open System.IO
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Newtonsoft.Json.Serialization

    open Protocol

    let jsonSettings =
        let result = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
        result.Converters.Add(FsAutoComplete.OptionConverter())
        result.Converters.Add(ErasedUnionConverter())
        result.ContractResolver <- CamelCasePropertyNamesContractResolver()
        result

    let jsonSerializer = JsonSerializer.Create(jsonSettings)

    let private parseRequest<'a> (f: 'a -> ClientRequest) (paramsToken: JToken option) =
        paramsToken
        |> Option.map (fun paramsToken ->
            paramsToken.ToObject<'a>(jsonSerializer) |> f)

    let private parseEmpty (req: ClientRequest) (_params: JToken option) =
        Some req

    let requestParser = function
    | "initialize" -> parseRequest<InitializeParams> Initialize
    | "initialized" -> parseEmpty Initialized
    | "textDocument/hover" -> parseRequest<TextDocumentPositionParams> Hover
    | "textDocument/didOpen" -> parseRequest<DidOpenTextDocumentParams> DidOpenTextDocument
    | "textDocument/didChange" -> parseRequest<DidChangeTextDocumentParams> DidChangeTextDocument
    | "textDocument/completion" -> parseRequest<CompletionParams> Completion
    | "workspace/didChangeWatchedFiles" -> parseRequest<DidChangeWatchedFilesParams> DidChangeWatchedFiles
    | "shutdown" -> parseEmpty Shutdown
    | "exit" -> parseEmpty Exit
    | _ -> fun _ -> None

    type RequestSender = ServerRequest -> unit

    let start (input: Stream) (output: Stream) (handleRequest: RequestSender -> ClientRequest -> Async<ServerResponse>) =
        dbgf "Starting up !"

        let sender = MailboxProcessor<string>.Start(fun inbox ->
            let rec loop () = async {
                let! str = inbox.Receive()
                dbgf "Writing to client: %s" str
                LowLevel.write output str
                return! loop ()
            }
            loop ())

        /// When the server wants to send a request/notification to the client
        let sendServerRequest (serverRequest: ServerRequest) =
            let serializedResponse = JToken.FromObject(serverRequest.AsJsonSerializable, jsonSerializer)
            let req = JsonRpc.Request.Create(serverRequest.Method, serializedResponse)
            let reqString = JsonConvert.SerializeObject(req, jsonSettings)
            sender.Post(reqString)

        let handleRequest = handleRequest sendServerRequest

        let handleClientRequest (requestId: int option) (clientRequest: ClientRequest) = async {
            let! response = handleRequest clientRequest
            dbgf "Will answer: %A" response
            let rpcResponse =
                match response with
                | NoResponse -> None
                | InvalidRequest message ->
                    JsonRpc.Response.Failure(requestId, JsonRpc.Error.Create(-32602, message)) |> Some
                | UnhandledRequest ->
                    JsonRpc.Response.Failure(requestId, JsonRpc.Error.Create(-32601, "Method not found")) |> Some
                | response ->
                    let boxed = response.AsJsonSerializable
                    let serializedResponse = if isNull boxed then null else JToken.FromObject(boxed, jsonSerializer)
                    JsonRpc.Response.Success(requestId.Value, serializedResponse) |> Some
            match rpcResponse with
            | Some rpcResponse ->
                let rpcResponseString = JsonConvert.SerializeObject(rpcResponse, jsonSettings)
                sender.Post(rpcResponseString)
            | _ -> ()
        }

        while true do
            try
                let _, rpcRequestString = LowLevel.read input
                dbgf "Received: %s" rpcRequestString

                let rpcRequest = JsonConvert.DeserializeObject<JsonRpc.Request>(rpcRequestString, jsonSettings)
                let request = requestParser rpcRequest.Method rpcRequest.Params
                dbgf "Parsed as: %A" request

                match request with
                | Some request ->
                    handleClientRequest rpcRequest.Id request |> Async.StartAsTask |> ignore
                | _ ->
                    dbgf "Unable to parse request: %s" rpcRequestString
            with
            | :? EndOfStreamException ->
                dbgf "Client closed the input stream"
                System.Environment.Exit(0)
            | ex -> dbgf "%O" ex
