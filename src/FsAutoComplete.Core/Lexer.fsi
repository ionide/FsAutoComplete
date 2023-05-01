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
    Line: int
    LeftColumn: int
    RightColumn: int
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
    RightColumn: int }

  static member inline Create: kind: SymbolKind -> token: FSharpTokenInfo -> DraftToken

module Lexer =
  val logger: ILog
  /// Return all tokens of current line
  val tokenizeLine: args: string[] -> lineStr: string -> FSharpTokenInfo list

  val getSymbol:
    line: int -> col: int -> lineStr: string -> lookupKind: SymbolLookupKind -> args: string[] -> LexerSymbol option

  val findIdents: col: int -> lineStr: string -> lookupType: SymbolLookupKind -> (int * string array) option
  val findLongIdents: col: int * lineStr: string -> (int * string array) option
  val findLongIdentsAndResidue: col: int * lineStr: string -> string list * string
  val findClosestIdent: col: int -> lineStr: string -> (int * string array) option
