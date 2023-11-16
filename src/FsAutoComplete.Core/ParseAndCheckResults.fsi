namespace FsAutoComplete

open FsAutoComplete.Logging
open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis
open FSharp.UMX
open System
open System.IO
open Utils
open FSharp.Compiler.Tokenization
open FSharp.Compiler.Syntax

[<RequireQualifiedAccess>]
type FindDeclarationResult =
  | ExternalDeclaration of Decompiler.ExternalContentPosition
  | Range of FSharp.Compiler.Text.Range
  /// The declaration refers to a file.
  | File of string

[<RequireQualifiedAccess>]
module TryGetToolTipEnhancedResult =
  type SymbolInfo =
    | Keyword of string
    | Symbol of
      {| XmlDocSig: string
         Assembly: string |}

type TryGetToolTipEnhancedResult =
  { ToolTipText: ToolTipText
    Signature: string
    Footer: string
    SymbolInfo: TryGetToolTipEnhancedResult.SymbolInfo }

type ParseAndCheckResults =
  new:
    parseResults: FSharpParseFileResults * checkResults: FSharpCheckFileResults * entityCache: EntityCache ->
      ParseAndCheckResults

  member TryFindDeclaration: pos: Position -> lineStr: LineStr -> Async<Result<FindDeclarationResult, string>>
  member TryFindLoadDirectiveSource: pos: Position -> lineStr: LineStr -> Async<Result<FindDeclarationResult, string>>

  member TryFindIdentifierDeclaration: pos: Position -> lineStr: LineStr -> Async<Result<FindDeclarationResult, string>>

  member TryFindTypeDeclaration: pos: Position -> lineStr: LineStr -> Async<Result<FindDeclarationResult, string>>
  member TryGetToolTip: pos: Position -> lineStr: LineStr -> ToolTipText option

  member TryGetToolTipEnhanced: pos: Position -> lineStr: LineStr -> TryGetToolTipEnhancedResult option

  member TryGetFormattedDocumentation:
    pos: Position ->
    lineStr: LineStr ->
      Result<(ToolTipText option *
      (string * string) option *
      (string * DocumentationFormatter.EntityInfo) *
      string *
      string), string>

  member TryGetFormattedDocumentationForSymbol:
    xmlSig: string ->
    assembly: string ->
      Result<(string * string * FSharpXmlDoc * (string * DocumentationFormatter.EntityInfo) * string * string), string>

  member TryGetSymbolUse: pos: Position -> lineStr: LineStr -> FSharpSymbolUse option

  member TryGetSymbolUses: pos: Position -> lineStr: LineStr -> FSharpSymbolUse list

  member TryGetSymbolUseAndUsages:
    pos: Position -> lineStr: LineStr -> Result<(FSharpSymbolUse * FSharpSymbolUse array), string>

  member TryGetSignatureData:
    pos: Position -> lineStr: LineStr -> Result<(string * (string * string) list list * string list), string>

  member TryGetF1Help: pos: Position -> lineStr: LineStr -> Result<string, string>

  member TryGetCompletions:
    pos: Position ->
    lineStr: LineStr ->
    filter: string option ->
    getAllSymbols: (unit -> AssemblySymbol list) ->
      Async<(DeclarationListItem array * string * bool) option>

  member GetAllEntities: publicOnly: bool -> AssemblySymbol list
  member GetAllSymbolUsesInFile: unit -> seq<FSharpSymbolUse>
  member GetSemanticClassification: SemanticClassificationItem array
  member GetAST: ParsedInput
  member GetCheckResults: FSharpCheckFileResults
  member GetParseResults: FSharpParseFileResults
  member FileName: string<LocalPath>
