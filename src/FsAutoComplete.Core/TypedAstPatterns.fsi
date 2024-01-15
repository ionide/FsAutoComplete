[<AutoOpen>]
module FsAutoComplete.Patterns

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

/// Active patterns over `FSharpSymbolUse`.
module SymbolUse =
  val (|ActivePatternCase|_|): symbol: FSharpSymbolUse -> FSharpActivePatternCase option
  val (|Entity|_|): symbol: FSharpSymbolUse -> (FSharpEntity * string list) option
  val (|Field|_|): symbol: FSharpSymbolUse -> FSharpField option
  val (|GenericParameter|_|): symbol: FSharpSymbolUse -> FSharpGenericParameter option
  val (|MemberFunctionOrValue|_|): symbol: FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option
  val (|ActivePattern|_|): (FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option)
  val (|Parameter|_|): symbol: FSharpSymbolUse -> FSharpParameter option
  val (|StaticParameter|_|): FSharpSymbolUse -> ('a -> 'a option)
  val (|UnionCase|_|): symbol: FSharpSymbolUse -> FSharpUnionCase option
  val (|Constructor|_|): (FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option)
  val (|TypeAbbreviation|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Class|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Delegate|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Event|_|): (FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option)
  val (|Property|_|): (FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option)
  val (|Method|_|): symbolUse: FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option
  val (|Function|_|): symbolUse: FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option
  val (|Operator|_|): symbolUse: FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option
  val (|Pattern|_|): symbolUse: FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option
  val (|ClosureOrNestedFunction|_|): (FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option)
  val (|Val|_|): (FSharpSymbolUse -> FSharpMemberOrFunctionOrValue option)
  val (|Enum|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Interface|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Module|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Namespace|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Record|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|Union|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|ValueType|_|): (FSharpSymbolUse -> FSharpEntity option)
  val (|ComputationExpression|_|): symbol: FSharpSymbolUse -> FSharpSymbolUse option
  val (|Attribute|_|): (FSharpSymbolUse -> FSharpEntity option)

/// Active patterns over `FSharpSymbol`.
[<AutoOpen>]
module SymbolPatterns =
  val (|Entity|_|): symbol: FSharpSymbolUse -> (FSharpEntity * string list) option
  val (|EntityFromSymbol|_|): symbol: FSharpSymbol -> (FSharpEntity * string list) option
  val (|AbbreviatedType|_|): entity: FSharpEntity -> FSharpType option
  val (|TypeWithDefinition|_|): ty: FSharpType -> FSharpEntity option
  val getEntityAbbreviatedType: entity: FSharpEntity -> FSharpEntity * FSharpType option
  val getAbbreviatedType: fsharpType: FSharpType -> FSharpType
  val (|Attribute|_|): entity: FSharpEntity -> unit option
  val (|ValueType|_|): e: FSharpEntity -> unit option
  val (|Class|_|): original: FSharpEntity * abbreviated: FSharpEntity * 'a -> unit option
  val (|Record|_|): e: FSharpEntity -> unit option
  val (|UnionType|_|): e: FSharpEntity -> unit option
  val (|Delegate|_|): e: FSharpEntity -> unit option
  val (|FSharpException|_|): e: FSharpEntity -> unit option
  val (|Interface|_|): e: FSharpEntity -> unit option
  val (|AbstractClass|_|): e: FSharpEntity -> unit option
  val (|FSharpType|_|): e: FSharpEntity -> unit option
  val (|ProvidedType|_|): FSharpEntity -> 'a option
  val (|ByRef|_|): e: FSharpEntity -> unit option
  val (|Array|_|): e: FSharpEntity -> unit option
  val (|FSharpModule|_|): entity: FSharpEntity -> unit option
  val (|Namespace|_|): entity: FSharpEntity -> unit option
  val (|ProvidedAndErasedType|_|): FSharpEntity -> 'a option
  val (|Enum|_|): entity: FSharpEntity -> unit option
  val (|Tuple|_|): ty: FSharpType option -> unit option
  val (|RefCell|_|): ty: FSharpType -> unit option
  val (|FunctionType|_|): ty: FSharpType -> unit option
  val (|Pattern|_|): symbol: FSharpSymbol -> unit option
  /// Field (field, fieldAbbreviatedType)
  val (|Field|_|): symbol: FSharpSymbol -> (FSharpField * FSharpType) option
  val (|MutableVar|_|): symbol: FSharpSymbol -> unit option
  /// Entity (originalEntity, abbreviatedEntity, abbreviatedType)
  val (|FSharpEntity|_|): symbol: FSharpSymbol -> (FSharpEntity * FSharpEntity * FSharpType option) option
  val (|Parameter|_|): symbol: FSharpSymbol -> unit option
  val (|UnionCase|_|): e: FSharpSymbol -> FSharpUnionCase option
  val (|RecordField|_|): e: FSharpSymbol -> FSharpField option
  val (|ActivePatternCase|_|): symbol: FSharpSymbol -> FSharpActivePatternCase option
  /// Func (memberFunctionOrValue, fullType)
  val (|MemberFunctionOrValue|_|): symbol: FSharpSymbol -> FSharpMemberOrFunctionOrValue option
  /// Constructor (enclosingEntity)
  val (|Constructor|_|): (FSharpSymbol -> FSharpMemberOrFunctionOrValue option)
  val (|Function|_|): symbol: FSharpSymbol -> FSharpMemberOrFunctionOrValue option
  val (|ExtensionMember|_|): func: FSharpMemberOrFunctionOrValue -> unit option
  val (|Event|_|): func: FSharpMemberOrFunctionOrValue -> unit option
  val (|Operator|_|): symbolUse: FSharpSymbol -> FSharpMemberOrFunctionOrValue option
  val (|Property|_|): (FSharpSymbol -> FSharpMemberOrFunctionOrValue option)
  val (|ClosureOrNestedFunction|_|): (FSharpSymbol -> FSharpMemberOrFunctionOrValue option)
  val (|Val|_|): (FSharpSymbol -> FSharpMemberOrFunctionOrValue option)
  val (|GenericParameter|_|): symbol: FSharpSymbol -> FSharpGenericParameter option
