module FsAutoComplete.TipFormatter

open System
open System.IO
open System.Xml
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FsAutoComplete.Logging
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types

val inline nl<'T> : string
val logger: ILog

module private Section =
    val inline addSection: name: string -> content: string -> string
    val fromKeyValueList: name: string -> content: list<KeyValuePair<string, string>> -> string
    val fromOption: name: string -> content: string option -> string
    val fromList: name: string -> content: string seq -> string

module private Format =
    val tagPattern: tagName: string -> string

    type TagInfo =
        | VoidElement of attributes: Map<string, string>
        | NonVoidElement of innerText: string * attributes: Map<string, string>

    type FormatterInfo =
        { TagName: string
          Formatter: TagInfo -> string option }

    val private extractTextFromQuote: quotedText: string -> string
    val extractMemberText: text: string -> string
    val private getAttributes: attributes: Group -> Map<string, string>
    type AttrLookup = Map<string, string> -> Option<string>
    val private cref: AttrLookup
    val private langword: AttrLookup
    val private href: AttrLookup
    val private lang: AttrLookup
    val private name: AttrLookup
    val private applyFormatter: info: FormatterInfo -> text: string -> string
    val private codeBlock: (string -> string)
    val private example: (string -> string)
    val private codeInline: (string -> string)
    val private link: text: string -> uri: string -> string
    val private code: text: string -> string
    val private anchor: (string -> string)
    val private paragraph: (string -> string)
    val private block: (string -> string)
    val private see: (string -> string)
    val private xref: (string -> string)
    val private paramRef: (string -> string)
    val private typeParamRef: (string -> string)
    val private fixPortableClassLibrary: text: string -> string
    /// <summary>Handle Microsoft 'or' formatting blocks</summary>
    /// <remarks>
    /// <para>We don't use the formatter API here because we are not handling a "real XML element"</para>
    /// <para>We don't use regex neither because I am not able to create one covering all the possible case</para>
    /// <para>
    /// There are 2 types of 'or' blocks:
    ///
    /// - Inlined: [...]  -or-  [...]  -or-  [...]
    /// - Blocked:
    /// [...]
    /// -or-
    /// [...]
    /// -or-
    /// [...]
    /// </para>
    /// <para>
    /// This function can convert both styles. If an 'or' block is encounter the whole section will always result in a multiline output
    /// </para>
    /// <para>
    /// If we pass any of the 2 previous example, it will generate the same Markdown string as a result (because they have the same number of 'or' section). The result will be:
    /// </para>
    /// <para>
    /// >    [...]
    ///
    /// *or*
    ///
    /// >    [...]
    ///
    /// *or*
    ///
    /// >    [...]
    /// </para>
    /// </remarks>
    val private handleMicrosoftOrList: text: string -> string
    /// <summary>Remove all invalid 'or' block found</summary>
    /// <remarks>
    /// If an 'or' block is found between 2 elements then we remove it as we can't generate a valid markdown for it
    ///
    /// For example, <td> Some text -or- another text </td> cannot be converted into a multiline string
    /// and so we prefer to remove the 'or' block instead of having some weird markdown artefacts
    ///
    /// For now, we only consider text between <td></td> to be invalid
    /// We can add more in the future if needed, but I want to keep this as minimal as possible to avoid capturing false positive
    /// </remarks>
    val private removeInvalidOrBlock: text: string -> string
    val private convertTable: (string -> string)
    type private Term = string
    type private Definition = string

    type private ListStyle =
        | Bulleted
        | Numbered
        | Tablered

    /// ItemList allow a permissive representation of an Item.
    /// In theory, TermOnly should not exist but we added it so part of the documentation doesn't disappear
    /// TODO: Allow direct text support without <description> and <term> tags
    type private ItemList =
        /// A list where the items are just contains in a <description> element
        | DescriptionOnly of string
        /// A list where the items are just contains in a <term> element
        | TermOnly of string
        /// A list where the items are a term followed by a definition (ie in markdown: * <TERM> - <DEFINITION>)
        | Definitions of Term * Definition

    val private itemListToStringAsMarkdownList: prefix: string -> item: ItemList -> string
    val private list: (string -> string)
    /// <summary>
    /// Unescape XML special characters
    ///
    /// For example, this allows to print '>' in the tooltip instead of '&gt;'
    /// </summary>
    val private unescapeSpecialCharacters: text: string -> string
    val applyAll: text: string -> string

[<RequireQualifiedAccess>]
type FormatCommentStyle =
    | Legacy
    | FullEnhanced
    | SummaryOnly
    | Documentation

type private XmlDocMember =
    new: doc: XmlDocument * indentationSize: int * columnOffset: int -> XmlDocMember
    override ToString: unit -> string
    member ToSummaryOnlyString: unit -> string
    member HasTruncatedExamples: bool
    member ToFullEnhancedString: unit -> string
    member ToDocumentationString: unit -> string
    member FormatComment: formatStyle: FormatCommentStyle -> string

val private readXmlDoc:
    reader: XmlReader -> indentationSize: int -> acc: Map<string, XmlDocMember> -> Map<string, XmlDocMember>

val private xmlDocCache: Collections.Concurrent.ConcurrentDictionary<string, Map<string, XmlDocMember>>
val private getXmlDoc: dllFile: string -> Map<string, XmlDocMember> option

[<RequireQualifiedAccess>]
type private TryGetXmlDocMemberResult =
    | Some of XmlDocMember
    | None
    | Error

[<RequireQualifiedAccess>]
type TipFormatterResult<'T> =
    | Success of 'T
    | Error of string
    | None

val private tryGetXmlDocMember: xmlDoc: FSharpXmlDoc -> TryGetXmlDocMemberResult

[<Literal>]
val private ERROR_WHILE_PARSING_DOC_COMMENT: string = "An error occurred when parsing the doc comment, please check that your doc comment is valid.\n\nMore info can be found in the LSP output"

val private formatTaggedText: t: TaggedText -> string
val private formatUntaggedText: t: TaggedText -> string
val private formatUntaggedTexts: (TaggedText array -> string)
val private formatTaggedTexts: (TaggedText array -> string)
val private formatGenericParameters: typeMappings: TaggedText[] list -> string
/// CompletionItems are formatted with an unmodified signature since the signature portion of the
/// item isn't markdown-compatible. The documentation shown however is markdown.
val formatCompletionItemTip: ToolTipText -> string * string
/// Formats a tooltip signature for output as a signatureHelp,
/// which means no markdown formatting.
val formatPlainTip: ToolTipText -> string * string
val prepareSignature: signatureText: string -> string
val prepareFooterLines: footerText: string -> string array

val private tryComputeTooltipInfo:
    ToolTipText ->
    formatCommentStyle: FormatCommentStyle ->
        Result<{| DocComment: string
                  HasTruncatedExamples: bool |}, string> option

/// <summary>
/// Try format the given tooltip with the requested style.
/// </summary>
/// <param name="toolTipText">Tooltip documentation to render in the middle</param>
/// <param name="formatCommentStyle">Style of tooltip</param>
/// <returns>
/// - <c>TipFormatterResult.Success {| DocComment; HasTruncatedExamples |}</c> if the doc comment has been formatted
///
///   Where DocComment is the format tooltip and HasTruncatedExamples is true if examples have been truncated
///
/// - <c>TipFormatterResult.None</c> if the doc comment has not been found
/// - <c>TipFormatterResult.Error string</c> if an error occurred while parsing the doc comment
/// </returns>
val tryFormatTipEnhanced:
    toolTipText: ToolTipText ->
    formatCommentStyle: FormatCommentStyle ->
        TipFormatterResult<{| DocComment: string
                              HasTruncatedExamples: bool |}>

/// <summary>
/// Generate the 'Show documentation' link for the tooltip.
///
/// The link is rendered differently depending on if examples
/// have been truncated or not.
/// </summary>
/// <param name="hasTruncatedExamples"><c>true</c> if the examples have been truncated</param>
/// <param name="xmlDocSig">XmlDocSignature in the format of <c>T:System.String.concat</c></param>
/// <param name="assemblyName">Assembly name, example <c>FSharp.Core</c></param>
/// <returns>Returns a string which represent the show documentation link</returns>
val renderShowDocumentationLink: hasTruncatedExamples: bool -> xmlDocSig: string -> assemblyName: string -> string
/// <summary>
/// Try format the given tooltip as documentation.
/// </summary>
/// <param name="toolTipText">Tooltip to format</param>
/// <returns>
/// - <c>TipFormatterResult.Success string</c> if the doc comment has been formatted
/// - <c>TipFormatterResult.None</c> if the doc comment has not been found
/// - <c>TipFormatterResult.Error string</c> if an error occurred while parsing the doc comment
/// </returns>
val tryFormatDocumentationFromTooltip: toolTipText: ToolTipText -> TipFormatterResult<string>
/// <summary>
/// Try format the doc comment based on the XmlSignature and the assembly name.
/// </summary>
/// <param name="xmlSig">
/// XmlSignature used to identify the doc comment to format
///
/// Example: <c>T:System.String.concat</c>
/// </param>
/// <param name="assembly">
/// Assembly name used to identify the doc comment to format
///
/// Example: <c>FSharp.Core</c>
/// </param>
/// <returns>
/// - <c>TipFormatterResult.Success string</c> if the doc comment has been formatted
/// - <c>TipFormatterResult.None</c> if the doc comment has not been found
/// - <c>TipFormatterResult.Error string</c> if an error occurred while parsing the doc comment
/// </returns>
val tryFormatDocumentationFromXmlSig: xmlSig: string -> assembly: string -> TipFormatterResult<string>
val formatDocumentationFromXmlDoc: xmlDoc: FSharpXmlDoc -> TipFormatterResult<string>
val extractSignature: ToolTipText -> string
/// extracts any generic parameters present in this tooltip, rendering them as plain text
val extractGenericParameters: ToolTipText -> string list
