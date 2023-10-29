namespace FsAutoComplete

[<AutoOpen>]
module PrintParameter =
  val print: sb: System.Text.StringBuilder -> (string -> unit)

module SignatureFormatter =
  open FSharp.Compiler.CodeAnalysis
  open FSharp.Compiler.Symbols
  open FSharp.Compiler.Syntax
  open FSharp.Compiler.Tokenization
  open System
  open System.Text

  val nl: string
  val maxPadding: int
  /// Concat two strings with a space between if both a and b are not IsNullOrWhiteSpace
  val internal (++): a: string -> b: string -> string
  val entityIsArray: entity: FSharpEntity -> bool
  val formatFSharpType: context: FSharpDisplayContext -> typ: FSharpType -> string

  val formatGenericParameter:
    includeMemberConstraintTypes: bool ->
    displayContext: FSharpDisplayContext ->
    param: FSharpGenericParameter ->
      string

  val getUnionCaseSignature: displayContext: FSharpDisplayContext -> unionCase: FSharpUnionCase -> string

  val getFuncSignatureWithIdent:
    displayContext: FSharpDisplayContext -> func: FSharpMemberOrFunctionOrValue -> ident: int -> string

  val getFuncSignatureForTypeSignature:
    displayContext: FSharpDisplayContext ->
    func: FSharpMemberOrFunctionOrValue ->
    overloads: int ->
    getter: bool ->
    setter: bool ->
      string

  val getFuncSignature: f: FSharpDisplayContext -> c: FSharpMemberOrFunctionOrValue -> string
  val getValSignature: displayContext: FSharpDisplayContext -> v: FSharpMemberOrFunctionOrValue -> string
  val getFieldSignature: displayContext: FSharpDisplayContext -> field: FSharpField -> string
  val getAPCaseSignature: displayContext: FSharpDisplayContext -> apc: FSharpActivePatternCase -> string
  val getEntitySignature: displayContext: FSharpDisplayContext -> fse: FSharpEntity -> string
  val footerForType: entity: FSharpSymbolUse -> string
  ///Returns formatted symbol signature and footer that can be used to enhance standard FCS' text tooltips
  val getTooltipDetailsFromSymbolUse: symbol: FSharpSymbolUse -> (string * string) option
