namespace FsAutoComplete

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Symbols
open UntypedAstUtils

[<AutoOpen>]
module TypedAstUtils =
  val isSymbolLocalForProject: symbol: FSharpSymbol -> bool
  val getTypeIfConstructor: symbol: FSharpSymbol -> FSharpEntity option
  val isAttribute<'T> : attribute: FSharpAttribute -> bool
  val hasAttribute<'T> : attributes: seq<FSharpAttribute> -> bool
  val tryGetAttribute<'T> : attributes: seq<FSharpAttribute> -> FSharpAttribute option
  val hasModuleSuffixAttribute: entity: FSharpEntity -> bool
  val isOperator: name: string -> bool
  val isUnnamedUnionCaseField: field: FSharpField -> bool

[<AutoOpen>]
module TypedAstExtensionHelpers =
  type FSharpEntity with

    member TryGetFullName: unit -> string option
    member TryGetFullDisplayName: unit -> string option
    member TryGetFullCompiledName: unit -> string option
    member PublicNestedEntities: seq<FSharpEntity>
    member TryGetMembersFunctionsAndValues: Collections.Generic.IList<FSharpMemberOrFunctionOrValue>
    member TryGetFullNameWithUnderScoreTypes: unit -> string option
    member UnAnnotate: unit -> FSharpEntity
    member InheritanceDepth: unit -> int
    member AllBaseTypes: FSharpType list

  type FSharpMemberOrFunctionOrValue with

    member FullTypeSafe: FSharpType option
    member TryGetFullDisplayName: unit -> string option
    member TryGetFullCompiledOperatorNameIdents: unit -> Idents option
    member IsConstructor: bool
    member IsOperatorOrActivePattern: bool
    member EnclosingEntitySafe: FSharpEntity option

  type FSharpAssemblySignature with

    member TryGetEntities: unit -> seq<FSharpEntity>

  type FSharpSymbol with

    /// <summary>
    /// If this member is a type abbreviation (<c>type Foo = Bar&lt;string&gt;</c> for example),
    /// resolves the underlying type. Otherwise returns this type.
    /// </summary>
    member GetAbbreviatedParent: unit -> FSharpSymbol
    member IsPrivateToFile: bool
    member IsInternalToProject: bool
    member XmlDocSig: string
    member XmlDoc: FSharpXmlDoc

  type FSharpGenericParameterMemberConstraint with

    member IsProperty: bool
