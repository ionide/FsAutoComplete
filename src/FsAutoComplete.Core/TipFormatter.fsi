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

  val extractMemberText: text: string -> string
  type AttrLookup = Map<string, string> -> Option<string>

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

/// CompletionItems are formatted with an unmodified signature since the signature portion of the
/// item isn't markdown-compatible. The documentation shown however is markdown.
val formatCompletionItemTip: ToolTipText -> string * string
/// Formats a tooltip signature for output as a signatureHelp,
/// which means no markdown formatting.
val formatPlainTip: ToolTipText -> string * string
val prepareSignature: signatureText: string -> string
val prepareFooterLines: footerText: string -> string array

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
