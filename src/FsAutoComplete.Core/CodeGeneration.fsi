namespace FsAutoComplete

open System
open System.IO
open System.CodeDom.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Tokenization
open FSharp.Compiler.CodeAnalysis
open FsToolkit.ErrorHandling
open FSharp.UMX

[<Measure>]
type Line0

[<Measure>]
type Line1

type ICodeGenerationService =
    abstract TokenizeLine: string<LocalPath> * int -> Async<option<list<FSharpTokenInfo>>>
    abstract GetSymbolAtPosition: string<LocalPath> * Position -> Async<option<LexerSymbol>>

    abstract GetSymbolAndUseAtPositionOfKind:
        string<LocalPath> * Position * SymbolKind -> Async<option<LexerSymbol * option<FSharpSymbolUse>>>

    abstract ParseFileInProject: string<LocalPath> -> Async<option<FSharpParseFileResults>>

type CodeGenerationService =
    new: checker: FSharpCompilerServiceChecker * state: State -> CodeGenerationService
    interface ICodeGenerationService

module CodeGenerationUtils =
    open FSharp.Compiler.Syntax.PrettyNaming

    type ColumnIndentedTextWriter =
        new: unit -> ColumnIndentedTextWriter
        member Write: s: string -> unit
        member Write: s: string * [<ParamArray>] objs: obj[] -> unit
        member WriteLine: s: string -> unit
        member WriteLine: s: string * [<ParamArray>] objs: obj[] -> unit
        member WriteBlankLines: count: int -> unit
        member Indent: i: int -> unit
        member Unindent: i: int -> unit
        member Dump: unit -> string
        interface IDisposable

    val hasAttribute<'T> : attrs: seq<FSharpAttribute> -> bool
    val internal getNonAbbreviatedType: typ: FSharpType -> FSharpType

    val tryFindTokenLPosInRange:
        codeGenService: ICodeGenerationService ->
        range: Range ->
        document: Document ->
        predicate: (FSharpTokenInfo -> bool) ->
            Async<(FSharpTokenInfo * pos) option>

    /// Represent environment where a captured identifier should be renamed
    type NamesWithIndices = Map<string, Set<int>>
    val keywordSet: Set<string>
    val getTypeParameterName: typar: FSharpGenericParameter -> string

    [<NoComparison>]
    type Context =
        {
            Writer: ColumnIndentedTextWriter
            /// Map generic types to specific instances for specialized interface implementation
            TypeInstantations: Map<string, string>
            /// Data for interface instantiation
            ArgInstantiations: (FSharpGenericParameter * FSharpType) seq
            /// Indentation inside method bodies
            Indentation: int
            /// Object identifier of the interface e.g. 'x', 'this', '__', etc.
            ObjectIdent: string
            /// A list of lines represents skeleton of each member
            MethodBody: string[]
            /// Context in order to display types in the short form
            DisplayContext: FSharpDisplayContext
        }

    val (|AllAndLast|_|): xs: 'T list -> ('T list * 'T) option
    val bracket: str: string -> string
    val formatType: ctx: Context -> typ: FSharpType -> string
    val normalizeArgName: namesWithIndices: NamesWithIndices -> nm: string -> string * NamesWithIndices

    val formatArgUsage:
        ctx: Context ->
        hasTypeAnnotation: bool ->
        namesWithIndices: Map<string, Set<int>> ->
        arg: FSharpParameter ->
            string * NamesWithIndices

    val formatArgsUsage:
        ctx: Context ->
        hasTypeAnnotation: bool ->
        v: FSharpMemberOrFunctionOrValue ->
        args: #FSharpParameter list list ->
            string * Map<string, Set<int>>

    [<RequireQualifiedAccess; NoComparison>]
    type MemberInfo =
        | PropertyGetSet of FSharpMemberOrFunctionOrValue * FSharpMemberOrFunctionOrValue
        | Member of FSharpMemberOrFunctionOrValue

    val getArgTypes: ctx: Context -> v: FSharpMemberOrFunctionOrValue -> FSharpParameter list list * string
    /// Convert a getter/setter to its canonical form
    val normalizePropertyName: v: FSharpMemberOrFunctionOrValue -> string
    val isEventMember: m: FSharpMemberOrFunctionOrValue -> bool
    /// Rename a given argument if the identifier has been used
    val formatMember: ctx: Context -> m: MemberInfo -> verboseMode: bool -> unit
    val (|MemberFunctionType|_|): typ: FSharpType -> FSharpType option
    val (|TypeOfMember|_|): m: FSharpMemberOrFunctionOrValue -> FSharpType option
    val removeWhitespace: str: string -> string
    /// Filter out duplicated interfaces in inheritance chain
    val internal getInterfaces: e: FSharpEntity -> seq<FSharpEntity * seq<FSharpGenericParameter * FSharpType>>
    /// Use this hack when FCS doesn't return enough information on .NET properties and events.
    /// we use this to filter out the 'meta' members in favor of providing the underlying members for template generation
    /// eg: a property _also_ has the relevant get/set members, so we don't need them.
    val isSyntheticMember: m: FSharpMemberOrFunctionOrValue -> bool
    val isAbstractNonVirtualMember: m: FSharpMemberOrFunctionOrValue -> bool
    val isAbstractClass: e: FSharpEntity -> bool
    val getAbstractNonVirtualMembers: e: FSharpEntity -> seq<FSharpMemberOrFunctionOrValue * seq<'a>>
    val (|LongIdentPattern|_|): (SynPat -> (string * range) option)
    /// Get name and associated range of a member
    /// On merged properties (consisting both getters and setters), they have the same range values,
    /// so we use 'get_' and 'set_' prefix to ensure corresponding symbols are retrieved correctly.
    /// We also get the range of the leading keyword to establish indent position
    val (|MemberNamePlusRangeAndKeywordRange|_|): (SynBinding -> (string * range * range) option)
    val normalizeEventName: m: FSharpMemberOrFunctionOrValue -> string

    /// Ideally this info should be returned in error symbols from FCS.
    /// Because it isn't, we implement a crude way of getting member signatures:
    ///  (1) Crack ASTs to get member names and their associated ranges
    ///  (2) Check symbols of those members based on ranges
    ///  (3) If any symbol found, capture its member signature
    val getImplementedMemberSignatures:
        getMemberByLocation: (string * range * 'a -> FSharpSymbolUse option) ->
        displayContext: FSharpDisplayContext ->
        memberNamesAndRanges: (string * range * 'a) list ->
            Set<string>

    /// Check whether an entity is an interface or type abbreviation of an interface
    val isInterface: e: FSharpEntity -> bool
    val findLastIdentifier: tokens: FSharpTokenInfo list -> lastValidToken: FSharpTokenInfo -> FSharpTokenInfo
    /// The code below is responsible for handling the code generation and determining the insert position
    val getLineIdent: lineStr: string -> int

    val formatMembersAt:
        startColumn: int ->
        indentation: int ->
        typeInstances: string[] ->
        objectIdent: string ->
        methodBody: string ->
        displayContext: FSharpDisplayContext ->
        excludedMemberSignatures: Set<string> ->
        e: FSharpEntity ->
        getMembersToImplement:
            (FSharpEntity -> seq<FSharpMemberOrFunctionOrValue * seq<FSharpGenericParameter * FSharpType>>) ->
        verboseMode: bool ->
            string

    val (|RationalConst|): (SynRationalConst -> string)
    val (|TypeIdent|_|): (SynType -> string option)
    val expandTypeParameters: typ: SynType -> string array
