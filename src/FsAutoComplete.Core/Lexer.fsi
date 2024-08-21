namespace FsAutoComplete

open FSharp.Compiler.Tokenization
open FsAutoComplete.Logging
open FsAutoComplete.Logging.Types

type SymbolKind =
  | Ident
  | Operator
  | GenericTypeParameter
  | StaticallyResolvedTypeParameter
  | ActivePattern
  | Keyword
  | Dot
  | Other

type LexerSymbol =
  { Kind: SymbolKind
    Line: uint32
    LeftColumn: uint32
    RightColumn: uint32
    Text: string }

[<RequireQualifiedAccess>]
type SymbolLookupKind =
  | Fuzzy
  | ByLongIdent
  | Simple
  | ForCompletion

type private DraftToken =
  { Kind: SymbolKind
    Token: FSharpTokenInfo
    RightColumn: uint32 }

  static member inline Create: kind: SymbolKind -> token: FSharpTokenInfo -> DraftToken

module Lexer =
  val logger: ILog
  /// Return all tokens of current line
  val tokenizeLine: args: string[] -> lineStr: string -> FSharpTokenInfo list

  val getSymbol:
    line: uint32 ->
    col: uint32 ->
    lineStr: string ->
    lookupKind: SymbolLookupKind ->
    args: string[] ->
      LexerSymbol option

  val findIdents: col: uint32 -> lineStr: string -> lookupType: SymbolLookupKind -> (uint32 * string array) option
  val findLongIdents: col: uint32 * lineStr: string -> (uint32 * string array) option
  val findLongIdentsAndResidue: col: uint32 * lineStr: string -> string list * string
  val findClosestIdent: col: uint32 -> lineStr: string -> (uint32 * string array) option
