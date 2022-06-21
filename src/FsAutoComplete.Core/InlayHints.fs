module FsAutoComplete.Core.InlayHints

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FsToolkit.ErrorHandling
open FsAutoComplete
open FSharp.Compiler.Symbols
open FSharp.UMX
open System.Linq
open System.Collections.Immutable
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text.Range
open FsAutoComplete.Core.Workaround.ServiceParseTreeWalk

/// `traversePat`from `SyntaxTraversal.Traverse`
///
/// Reason for extra function:
/// * can be used to traverse when traversal isn't available via `defaultTraverse` (for example: in `VisitExpr`, and want traverse a `SynPat`)
/// * visits `SynPat.Record(fieldPats)`
///
/// Note: doesn't visit `SynPat.Typed(targetType)`: requires traversal into `SynType` (`SynPat.Typed(pat)` gets visited!)
let rec private traversePat (visitor: SyntaxVisitorBase<_>) origPath pat =
  let defaultTraverse = defaultTraversePat visitor origPath
  visitor.VisitPat(origPath, defaultTraverse, pat)
and private defaultTraversePat visitor origPath pat =
  let path = SyntaxNode.SynPat pat :: origPath

  match pat with
  | SynPat.Paren (p, _) -> traversePat visitor path p
  | SynPat.As (p1, p2, _)
  | SynPat.Or (p1, p2, _, _) ->
    [ p1; p2 ]
    |> List.tryPick (traversePat visitor path)
  | SynPat.Ands (ps, _)
  | SynPat.Tuple (_, ps, _)
  | SynPat.ArrayOrList (_, ps, _) -> ps |> List.tryPick (traversePat visitor path)
  | SynPat.Attrib (p, _, _) -> traversePat visitor path p
  | SynPat.LongIdent (argPats = args) ->
    match args with
    | SynArgPats.Pats ps -> ps |> List.tryPick (traversePat visitor path)
    | SynArgPats.NamePatPairs (ps, _) ->
      ps
      |> List.map (fun (_, _, pat) -> pat)
      |> List.tryPick (traversePat visitor path)
  | SynPat.Typed (p, _ty, _) -> 
      traversePat visitor path p
      // no access to `traverseSynType` -> no traversing into `ty`
  | SynPat.Record (fieldPats = fieldPats) ->
    fieldPats
    |> List.map (fun (_, _, pat) -> pat)
    |> List.tryPick (traversePat visitor path)
  | _ -> None

type HintKind =
  | Parameter
  | Type

type HintInsertion = { Pos: Position; Text: string }

type Hint =
  { IdentRange: Range
    Kind: HintKind
    Pos: Position
    Text: string
    Insertions: HintInsertion[] option
    //ENHANCEMENT: allow xml doc
    Tooltip: string option }

let private getArgumentsFor (state: FsAutoComplete.State, p: ParseAndCheckResults, identText: Range) =
  option {

    let! contents =
      state.TryGetFileSource p.FileName
      |> Option.ofResult

    let! line = contents.GetLine identText.End
    let! symbolUse = p.TryGetSymbolUse identText.End line

    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv when
      mfv.IsFunction
      || mfv.IsConstructor
      || mfv.CurriedParameterGroups.Count <> 0
      ->
      let parameters = mfv.CurriedParameterGroups

      let formatted =
        parameters
        |> Seq.collect (fun pGroup -> pGroup |> Seq.map (fun p -> p.DisplayName + ":"))

      return formatted |> Array.ofSeq
    | _ -> return! None
  }

let private isSignatureFile (f: string<LocalPath>) =
  System.IO.Path.GetExtension(UMX.untag f) = ".fsi"

type private FSharp.Compiler.CodeAnalysis.FSharpParseFileResults with
  // duplicates + extends the logic in FCS to match bindings of the form `let x: int = 12`
  // so that they are considered logically the same as a 'typed' SynPat
  member x.IsTypeAnnotationGivenAtPositionPatched pos =
    let visitor: SyntaxVisitorBase<Range> =
      { new SyntaxVisitorBase<_>() with
          override _.VisitExpr(_path, _traverseSynExpr, defaultTraverse, expr) =
            match expr with
            | SynExpr.Typed (_expr, _typeExpr, range) when Position.posEq range.Start pos -> Some range
            | _ -> defaultTraverse expr

          override _.VisitSimplePats(_path, pats) =
            match pats with
            | [] -> None
            | _ ->
              let exprFunc pat =
                match pat with
                | SynSimplePat.Typed (_pat, _targetExpr, range) when Position.posEq range.Start pos -> Some range
                | _ -> None

              pats |> List.tryPick exprFunc

          override visitor.VisitPat(path, defaultTraverse, pat) =
            match pat with
            | SynPat.Typed (_pat, _targetType, range) when Position.posEq range.Start pos -> Some range
            | _ -> defaultTraversePat visitor path pat

          override _.VisitBinding(_path, defaultTraverse, binding) =
            match binding with
            | SynBinding (headPat = SynPat.Named (range = patRange)
                          returnInfo = Some (SynBindingReturnInfo(typeName = SynType.LongIdent (idents)))) ->
              Some patRange
            | _ -> defaultTraverse binding }

    let result = SyntaxTraversal.Traverse(pos, x.ParseTree, visitor)
    result.IsSome

let private getFirstPositionAfterParen (str: string) startPos =
  match str with
  | null -> -1
  | str when startPos > str.Length -> -1
  | str -> str.IndexOf('(') + 1

let private maxHintLength = 30

let inline private shouldTruncate (s: string) = s.Length > maxHintLength

let inline private tryTruncate (s: string) =
  if shouldTruncate s then
    s.Substring(0, maxHintLength) + "..." |> Some
  else
    None

let truncated (s: string) = tryTruncate s |> Option.defaultValue s

let private createParamHint (identRange: Range) (paramName: string) =
  let (truncated, tooltip) =
    match tryTruncate paramName with
    | None -> (paramName, None)
    | Some truncated -> (truncated, Some paramName)

  { IdentRange = identRange
    Pos = identRange.Start
    Kind = Parameter
    Text = truncated + " ="
    Insertions = None
    Tooltip = tooltip }

module private ShouldCreate =
  let private isNotWellKnownName =
    let names = Set.ofList [ "value"; "x" ]

    fun (p: FSharpParameter) ->
      match p.Name with
      | None -> true
      | Some n -> not (Set.contains n names)


  [<return: Struct>]
  let private (|StartsWith|_|) (v: string) (fullName: string) =
    if fullName.StartsWith v then
      ValueSome()
    else
      ValueNone
  // doesn't differentiate between modules, types, namespaces
  // -> is just for documentation in code
  [<return: Struct>]
  let private (|Module|_|) = (|StartsWith|_|)

  [<return: Struct>]
  let private (|Type|_|) = (|StartsWith|_|)

  [<return: Struct>]
  let private (|Namespace|_|) = (|StartsWith|_|)

  let private commonCollectionParams =
    Set.ofList
      [ "mapping"
        "projection"
        "chooser"
        "value"
        "predicate"
        "folder"
        "state"
        "initializer"
        "action"

        "list"
        "array"
        "source"
        "lists"
        "arrays"
        "sources" ]

  let private isWellKnownParameterOrFunction (func: FSharpMemberOrFunctionOrValue) (param: FSharpParameter) =
    match func.FullName with
    | Module "Microsoft.FSharp.Core.Option" ->
      // don't show param named `option`, but other params for Option
      match param.Name with
      | Some "option" -> true
      | _ -> false
    | Module "Microsoft.FSharp.Core.ValueOption" ->
      match param.Name with
      | Some "voption" -> true
      | _ -> false
    | Module "Microsoft.FSharp.Core.ExtraTopLevelOperators" // only printf-members have `format`
    | Module "Microsoft.FSharp.Core.Printf" ->
      // don't show param named `format`
      match param.Name with
      | Some "format" -> true
      | _ -> false
    | Namespace "Microsoft.FSharp.Collections" ->
      match param.Name with
      | Some name -> commonCollectionParams |> Set.contains name
      | _ -> false
    | _ -> false

  let inline private hasName (p: FSharpParameter) =
    not (String.IsNullOrEmpty p.DisplayName)
    && p.DisplayName <> "````"

  let inline private isMeaningfulName (p: FSharpParameter) = p.DisplayName.Length > 2

  let inline private isOperator (func: FSharpMemberOrFunctionOrValue) = func.CompiledName.StartsWith "op_"

  /// Doesn't consider lower/upper cases:
  /// * `areSame "foo" "FOO" = true`
  /// * `areSame "Foo" "Foo" = true`
  let inline private areSame (a: ReadOnlySpan<char>) (b: ReadOnlySpan<char>) =
    a.Equals(b, StringComparison.OrdinalIgnoreCase)

  /// Boundary checks:
  /// * word boundary (-> upper case letter)
  ///   `"foo" |> isPrefixOf "fooBar"`
  /// Doesn't consider capitalization, except for word boundary after prefix:
  /// * `foo` prefix of `fooBar`
  /// * `foo` not prefix of `foobar`
  let inline private isPrefixOf (root: ReadOnlySpan<char>) (check: ReadOnlySpan<char>) =
    root.StartsWith(check, StringComparison.OrdinalIgnoreCase)
    && (
    // same
    root.Length <= check.Length
    ||
    // rest must start with upper case -> new word
    Char.IsUpper root[check.Length])

  /// Boundary checks:
  /// * word boundary (-> upper case letter)
  ///   `"bar" |> isPostifxOf "fooBar"`
  /// * `.` boundary (-> property access)
  ///   `"bar" |> isPostifxOf "data.bar"`
  ///
  /// Doesn't consider capitalization, except for word boundary at start of postfix:
  /// * `bar` postfix of `fooBar`
  /// * `bar` not postfix of `foobar`
  let inline private isPostfixOf (root: ReadOnlySpan<char>) (check: ReadOnlySpan<char>) =
    root.EndsWith(check, StringComparison.OrdinalIgnoreCase)
    && (root.Length <= check.Length
        ||
        // postfix must start with upper case -> word boundary
        Char.IsUpper root[root.Length - check.Length])

  let inline private removeLeadingUnderscore (name: ReadOnlySpan<char>) = name.TrimStart '_'
  let inline private removeTrailingTick (name: ReadOnlySpan<char>) = name.TrimEnd '\''

  let inline private extractLastIdentifier (name: ReadOnlySpan<char>) =
    // exclude backticks for now: might contain `.` -> difficult to split
    if name.StartsWith "``" || name.EndsWith "``" then
      name
    else
      match name.LastIndexOf '.' with
      | -1 -> name
      | i -> name.Slice(i + 1)

  /// Note: when in parens: might not be an identifier, but expression!
  ///
  /// Note: might result in invalid expression (because no matching parens `string (2)` -> `string (2`)
  let inline private trimParensAndSpace (name: ReadOnlySpan<char>) = name.TrimStart("( ").TrimEnd(" )")

  /// Note: including `.`
  let inline private isLongIdentifier (name: ReadOnlySpan<char>) =
    // name |> Seq.forall PrettyNaming.IsLongIdentifierPartCharacter
    let mutable valid = true
    let mutable i = 0

    while valid && i < name.Length do
      if PrettyNaming.IsLongIdentifierPartCharacter name[i] then
        i <- i + 1
      else
        valid <- false

    valid

  let private areSimilar (paramName: string) (argumentText: string) =
    // no pipe with span ...
    let paramName = removeTrailingTick (removeLeadingUnderscore (paramName.AsSpan()))

    let argumentName =
      let argumentText = argumentText.AsSpan()
      let argTextNoParens = trimParensAndSpace argumentText

      if isLongIdentifier argTextNoParens then
        removeTrailingTick (extractLastIdentifier argTextNoParens)
      else
        argumentText

    // // covered by each isPre/PostfixOf
    // areSame paramName argumentName
    // ||
    isPrefixOf argumentName paramName
    || isPostfixOf argumentName paramName
    || isPrefixOf paramName argumentName
    || isPostfixOf paramName argumentName

  let inline private doesNotMatchArgumentText (parameterName: string) (userArgumentText: string) =
    parameterName <> userArgumentText
    && not (userArgumentText.StartsWith parameterName)

  let private isParamNamePostfixOfFuncName (func: FSharpMemberOrFunctionOrValue) (paramName: string) =
    let funcName = func.DisplayName.AsSpan()
    let paramName = removeLeadingUnderscore (paramName.AsSpan())

    isPostfixOf funcName paramName

  /// </summary>
  /// We filter out parameters that generate lots of noise in hints.
  /// * parameter has no name
  /// * parameter has length > 2
  /// * parameter is one of a set of 'known' names that clutter (like printfn formats)
  /// * param & function is "well known"/commonly used
  /// * parameter does match or is a pre/postfix of user-entered text
  /// * user-entered text does match or is a pre/postfix of parameter
  /// * parameter is postfix of function name
  /// </summary>
  let paramHint (func: FSharpMemberOrFunctionOrValue) (p: FSharpParameter) (argumentText: string) =
    hasName p
    && isMeaningfulName p
    && isNotWellKnownName p
    && (not (isWellKnownParameterOrFunction func p))
    && (not (isOperator func))
    && (not (areSimilar p.DisplayName argumentText))
    && (not (isParamNamePostfixOfFuncName func p.DisplayName))


type TypeName = string
type TypeNameForAnnotation = TypeName

type SpecialRule =
  /// For Optional: `?v` -> `?v: int`, NOT `v: int option`
  /// And parens must include optional, not just `v`
  | RemoveOptionFromType

type SpecialRules = SpecialRule list

[<RequireQualifiedAccess>]
type Parens =
  | Forbidden
  /// Technically `Optional` too: Usually additional parens are ok
  ///
  /// Note: `additionalParens` are inside of existing parens:
  /// `(|ident|)`
  /// * `()`: existing parens
  /// * `||`: additional parens location
  | Exist of additionalParens: Range
  | Optional of Range
  | Required of Range

type MissingExplicitType =
  { Ident: Range
    InsertAt: Position
    Parens: Parens
    SpecialRules: SpecialRules }

type MissingExplicitType with
  /// <returns>
  /// * type name
  /// * type name formatted with `SpecialRules`
  ///   -> to use as type annotation
  /// </returns>
  member x.FormatType(ty: FSharpType, displayContext: FSharpDisplayContext) : TypeName * TypeNameForAnnotation =
    //TODO: Format vs FormatWithConstraints?
    let typeName = ty.Format displayContext

    let anno =
      if x.SpecialRules
         |> List.contains RemoveOptionFromType then
        // Optional parameter:
        // `static member F(?a) =` -> `: int`, NOT `: int option`
        if typeName.EndsWith " option" then
          typeName.Substring(0, typeName.Length - " option".Length)
        else
          // TODO: always just trailing `option`? or can be `Option<int>`? -- maybe even with Namespace?
          typeName
      else
        typeName

    (typeName, anno)

  member x.CreateEdits(typeForAnnotation) =
    [| match x.Parens with
       | Parens.Required range -> { Pos = range.Start; Text = "(" }
       | _ -> ()

       { Pos = x.InsertAt; Text = ": " }
       { Pos = x.InsertAt
         Text = typeForAnnotation }

       match x.Parens with
       | Parens.Required range -> { Pos = range.End; Text = ")" }
       | _ -> () |]

  member x.TypeAndEdits(ty: FSharpType, displayContext: FSharpDisplayContext) =
    let (ty, tyForAnntotation) = x.FormatType(ty, displayContext)
    let edits = x.CreateEdits(tyForAnntotation)
    (ty, edits)

  /// Note: No validation of `mfv`!
  member x.TypeAndEdits(mfv: FSharpMemberOrFunctionOrValue, displayContext: FSharpDisplayContext) =
    x.TypeAndEdits(mfv.FullType, displayContext)


/// Note: Missing considers only directly typed, not parently (or ancestorly) typed:
/// ```fsharp
/// let (value: int, _) = (1,2)
/// //   ^^^^^ directly typed -> Exists
/// let (value,_): int*int = (1,2)
/// //             ^^^ parently typed -> Missing
/// ```
[<RequireQualifiedAccess>]
type ExplicitType =
  /// in for loop (only indent allowed -- nothing else (neither type nor parens))
  | Invalid
  | Exists
  | Missing of MissingExplicitType
  //TODO: remove
  | Debug of string

type ExplicitType with
  member x.TryGetTypeAndEdits(ty: FSharpType, displayContext: FSharpDisplayContext) =
    match x with
    | ExplicitType.Missing data -> data.TypeAndEdits(ty, displayContext) |> Some
    | _ -> None

/// Type Annotation must be directly for identifier, not somewhere up the line:
/// `v: int` -> directly typed
/// `(v,_): int*int` -> parently typed
///
/// Still considered directly typed:
/// * Parentheses: `(v): int`
/// * Attributes: `([<Attr>]v): int`
let rec private isDirectlyTyped (identStart: Position) (path: SyntaxVisitorPath) =
  //TODO: handle SynExpr.Typed? -> not at binding, but usage
  match path with
  | [] -> false
  | SyntaxNode.SynPat (SynPat.Typed (pat = pat)) :: _ when rangeContainsPos pat.Range identStart -> true
  | SyntaxNode.SynPat (SynPat.Paren _) :: path -> isDirectlyTyped identStart path
  | SyntaxNode.SynPat (SynPat.Attrib (pat = pat)) :: path when rangeContainsPos pat.Range identStart ->
    isDirectlyTyped identStart path
  | SyntaxNode.SynBinding (SynBinding (headPat = headPat; returnInfo = Some (SynBindingReturnInfo _))) :: _ when
    rangeContainsPos headPat.Range identStart
    ->
    true
  | SyntaxNode.SynExpr (SynExpr.Paren _) :: path -> isDirectlyTyped identStart path
  | SyntaxNode.SynExpr (SynExpr.Typed (expr = expr)) :: _ when rangeContainsPos expr.Range identStart -> true
  | _ -> false

/// Note: FULL range of pattern -> everything in parens
///   For `SynPat.Named`: Neither `range` nor `ident.idRange` span complete range: Neither includes Accessibility:
///   `let private (a: int)` is not valid, must include private: `let (private a: int)`
let rec private getParensForPatternWithIdent (patternRange: Range) (identStart: Position) (path: SyntaxVisitorPath) =
  match path with
  | SyntaxNode.SynPat (SynPat.Paren _) :: _ ->
    // (x)
    Parens.Exist patternRange
  | SyntaxNode.SynBinding (SynBinding (headPat = headPat)) :: _ when rangeContainsPos headPat.Range identStart ->
    // let x =
    Parens.Optional patternRange
  | SyntaxNode.SynPat (SynPat.Tuple(isStruct = true)) :: _ ->
    // struct (x,y)
    Parens.Optional patternRange
  | SyntaxNode.SynPat (SynPat.Tuple _) :: SyntaxNode.SynPat (SynPat.Paren _) :: _ ->
    // (x,y)
    Parens.Optional patternRange
  | SyntaxNode.SynPat (SynPat.Tuple _) :: _ ->
    // x,y
    Parens.Required patternRange
  | SyntaxNode.SynPat (SynPat.ArrayOrList _) :: _ ->
    // [x;y;z]
    Parens.Optional patternRange
  | SyntaxNode.SynPat (SynPat.As (rhsPat = pat)) :: _ when rangeContainsPos pat.Range identStart ->
    // _ as (value: int)
    Parens.Required patternRange
  | SyntaxNode.SynPat (SynPat.As (lhsPat = pat)) :: _ when rangeContainsPos pat.Range identStart ->
    // value: int as _
    // ^^^^^^^^^^ unlike rhs this here doesn't require parens...
    Parens.Optional patternRange
  | SyntaxNode.SynPat (SynPat.Record _) :: _ ->
    // { Value=value }
    Parens.Optional patternRange
  | SyntaxNode.SynPat (SynPat.LongIdent(argPats = SynArgPats.NamePatPairs (range = range))) :: _ when
    rangeContainsPos range identStart
    ->
    // U (Value=value)
    //   ^           ^
    //   must exist to be valid
    Parens.Optional patternRange
  | SyntaxNode.SynExpr (SynExpr.LetOrUseBang(isUse = true)) :: _ ->
    // use! x =
    // Note: Type is forbidden too...
    Parens.Forbidden
  | SyntaxNode.SynExpr (SynExpr.LetOrUseBang(isUse = false)) :: _ ->
    // let! x =
    Parens.Required patternRange
  | SyntaxNode.SynExpr (SynExpr.ForEach _) :: _ ->
    // for i in [1..4] do
    Parens.Optional patternRange
  | []
  | _ -> Parens.Required patternRange

/// Gets range of `SynPat.Named`
///
/// Issue with range of `SynPat.Named`:
/// `pat.range` only covers ident (-> `= ident.idRange`),
/// not `accessibility`.
///
/// Note: doesn't handle when accessibility is on prev line
let private rangeOfNamedPat (text: NamedText) (pat: SynPat) =
  match pat with
  | SynPat.Named(accessibility = None) -> pat.Range
  | SynPat.Named (ident = ident; accessibility = Some (access)) ->
    maybe {
      let start = ident.idRange.Start
      let! line = text.GetLine start

      let access = access.ToString().ToLowerInvariant().AsSpan()
      // word before ident must be access
      let pre = line.AsSpan(0, start.Column)

      match pre.LastIndexOf(access) with
      | -1 -> return! None
      | c ->
        // must be directly before ident
        let word = pre.Slice(c).TrimEnd()

        if word.Length = access.Length then
          let start = Position.mkPos start.Line c

          let range =
            let range = ident.idRange
            Range.mkRange range.FileName start range.End

          return range
        else
          return! None
    }
    |> Option.defaultValue pat.Range
  | _ -> failwith "Pattern must be Named!"

/// Note: (deliberately) fails when `pat` is neither `Named` nor `OptionalVal`
let rec private getParensForIdentPat (text: NamedText) (pat: SynPat) (path: SyntaxVisitorPath) =
  match pat with
  | SynPat.Named (ident = ident) ->
    // neither `range`, not `pat.Range` includes `accessibility`...
    // `let private (a: int)` is not valid, must include private: `let (private a: int)`
    let patternRange = rangeOfNamedPat text pat
    let identStart = ident.idRange.Start
    getParensForPatternWithIdent patternRange identStart path
  | SynPat.OptionalVal (ident = ident) ->
    let patternRange = pat.Range
    let identStart = ident.idRange.Start
    getParensForPatternWithIdent patternRange identStart path
  | _ -> failwith "Pattern must be Named or OptionalVal!"

let tryGetExplicitTypeInfo (text: NamedText, ast: ParsedInput) (pos: Position) : ExplicitType option =
  SyntaxTraversal.Traverse(
    pos,
    ast,
    { new SyntaxVisitorBase<_>() with
        member x.VisitExpr(path, traverseSynExpr, defaultTraverse, expr) =
          match expr with
          // special case:
          // for loop:
          // for i = 1 to 3 do
          //     ^ -> just Ident (neither SynPat nor SynSimplePat)
          //     -> no type allowed (not even parens)...
          | SynExpr.For (ident = ident) when rangeContainsPos ident.idRange pos -> ExplicitType.Invalid |> Some
          | SynExpr.Lambda(parsedData = Some (args, body)) ->
            // original visitor walks down `SynExpr.Lambda(args; body)`
            // Issue:
            //  `args` are `SynSimplePats` -> no complex pattern
            //  When pattern: is in body. In `args` then generated Identifier:
            //  * `let f1 = fun v -> v + 1`
            //    -> `v` is in `args` (-> SynSimplePat)
            //  * `let f2 = fun (Value v) -> v + 1`
            //    -> compiler generated `_arg1` in `args`,
            //    and `v` is inside match expression in `body` & `parsedData` (-> `SynPat` )
            // -> unify by looking into `parsedData` (-> args & body):
            //    -> `parsedData |> fst` contains `args` as `SynPat`
            //TODO: always correct?
            let arg =
              args
              |> List.tryFind (fun pat -> rangeContainsPos pat.Range pos)

            if arg |> Option.isSome then
              let pat = arg.Value
              traversePat x (SyntaxNode.SynExpr(expr) :: path) pat
            elif rangeContainsPos body.Range pos then
              traverseSynExpr body
            else
              None
          | _ -> defaultTraverse expr

        member visitor.VisitPat(path, defaultTraverse, pat) =
          let invalidPositionForTypeAnnotation (pos: Position) (path: SyntaxNode list) =
            match path with
            | SyntaxNode.SynExpr (SynExpr.LetOrUseBang(isUse = true)) :: _ ->
              // use! value =
              true
            | _ -> false

          //TODO: differentiate between directly typed and parently typed?
          //        (maybe even further ancestorly typed?)
          // ```fsharp
          // let (a: int,b) = (1,2)
          // //      ^^^ directly typed
          // let (a,b): int*int = (1,2)
          // //         ^^^ parently typed
          // ```
          // currently: only directly typed is typed
          match pat with
          // no simple way out: Range for `SynPat.LongIdent` doesn't cover full pats (just ident)...
          // | _ when not (rangeContainsPos pat.Range pos) -> None
          | SynPat.Named (ident = ident) when
            rangeContainsPos ident.idRange pos
            && invalidPositionForTypeAnnotation pos path
            ->
            ExplicitType.Invalid |> Some
          | SynPat.Named (ident = ident; isThisVal = false) when rangeContainsPos ident.idRange pos ->
            let typed = isDirectlyTyped ident.idRange.Start path

            if typed then
              ExplicitType.Exists |> Some
            else
              let parens = getParensForIdentPat text pat path

              ExplicitType.Missing
                { Ident = ident.idRange
                  InsertAt = ident.idRange.End
                  Parens = parens
                  SpecialRules = [] }
              |> Some
          | SynPat.OptionalVal (ident = ident) when rangeContainsPos ident.idRange pos ->
            let typed = isDirectlyTyped ident.idRange.Start path

            if typed then
              ExplicitType.Exists |> Some
            else
              let parens = getParensForIdentPat text pat path

              ExplicitType.Missing
                { Ident = ident.idRange
                  InsertAt = ident.idRange.End
                  Parens = parens
                  SpecialRules = [ RemoveOptionFromType ]
                //              ^^^^^^^^^^^^^^^^^^^^
                //              `?v: int`, NOT `?v: int option`
                }
              |> Some
          | _ -> defaultTraversePat visitor path pat

        member _.VisitSimplePats(path, pats) =
          // SynSimplePats at:
          // * Primary ctor:
          //    * SynMemberDefn.ImplicitCtor.ctorArgs
          //    * SynTypeDefnSimpleRepr.General.implicitCtorSynPats
          //      //TODO: when? example?
          // * Lambda: SynExpr.Lambda.args
          //   * issue: might or might not be actual identifier
          //      * `let f1 = fun v -> v + 1`
          //         -> `v` is in `args` (-> SynSimplePat)
          //      * `let f2 = fun (Value v) -> v + 1`
          //        -> compiler generated `_arg1` in `args`,
          //           and `v` is inside match expression in `body` & `parsedData` (-> `SynPat` )
          maybe {
            let! pat =
              pats
              |> List.tryFind (fun p -> rangeContainsPos p.Range pos)

            let rec tryGetIdent pat =
              match pat with
              | SynSimplePat.Id (ident = ident) when rangeContainsPos ident.idRange pos -> Some pat
              | SynSimplePat.Attrib (pat = pat) when rangeContainsPos pat.Range pos -> tryGetIdent pat
              | SynSimplePat.Typed (pat = pat) when rangeContainsPos pat.Range pos -> tryGetIdent pat
              | _ -> None

            let! ident = tryGetIdent pat

            match ident with
            | SynSimplePat.Id(isCompilerGenerated = false) ->
              let rec isTyped =
                function
                | SynSimplePat.Typed _ -> true
                | SynSimplePat.Id _ -> false
                | SynSimplePat.Attrib (pat = pat) -> isTyped pat

              let typed = isTyped pat

              if typed then
                return ExplicitType.Exists
              else
                let isCtor =
                  path
                  |> List.tryHead
                  |> Option.map (function
                    // normal ctor in type: `type A(v) = ...`
                    | SyntaxNode.SynMemberDefn (SynMemberDefn.ImplicitCtor _) -> true
                    //TODO: when? example?
                    | SyntaxNode.SynTypeDefn (SynTypeDefn(typeRepr = SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.General(implicitCtorSynPats = Some (ctorPats))))) when
                      rangeContainsPos ctorPats.Range pos
                      ->
                      true
                    | _ -> false)
                  |> Option.defaultValue false

                if isCtor then
                  return
                    ExplicitType.Missing
                      { Ident = ident.Range
                        InsertAt = ident.Range.End
                        Parens = Parens.Forbidden
                        SpecialRules = [] }
                else
                  // lambda
                  return! None
            | _ -> return! None
          } }
  )

/// Note: No exhausting check. Doesn't check for:
/// * is already typed (-> done by getting `ExplicitType`)
/// * Filters like excluding functions (vs. lambda functions)
/// * `mfv.IsFromDefinition`
///
/// `allowFunctionValues`: `let f = fun a b -> a + b`
/// -> enabled: `f` is target
/// Note: NOT actual functions with direct parameters:
/// `let f a b = a + b` -> `f` isn't target
/// Note: can be parameters too:
/// `let map f v = f v` -> `f` is target
let isPotentialTargetForTypeAnnotation
  (allowFunctionValues: bool)
  (symbolUse: FSharpSymbolUse, mfv: FSharpMemberOrFunctionOrValue)
  =
  //ENHANCEMENT: extract settings
  (mfv.IsValue
   || (allowFunctionValues && mfv.IsFunction))
  && not (
    mfv.IsMember
    || mfv.IsMemberThisValue
    || mfv.IsConstructorThisValue
    || PrettyNaming.IsOperatorDisplayName mfv.DisplayName
  )

let tryGetDetailedExplicitTypeInfo
  (isValidTarget: FSharpSymbolUse * FSharpMemberOrFunctionOrValue -> bool)
  (text: NamedText, parseAndCheck: ParseAndCheckResults)
  (pos: Position)
  =
  maybe {
    let! line = text.GetLine pos
    let! symbolUse = parseAndCheck.TryGetSymbolUse pos line

    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv when isValidTarget (symbolUse, mfv) ->
      let! explTy = tryGetExplicitTypeInfo (text, parseAndCheck.GetAST) pos
      return (symbolUse, mfv, explTy)
    | _ -> return! None
  }

let private tryCreateTypeHint (explicitType: ExplicitType) (ty: FSharpType) (displayContext: FSharpDisplayContext) =
  match explicitType with
  | ExplicitType.Missing data ->
    let (ty, tyForAnno) = data.FormatType(ty, displayContext)

    let (truncated, tooltip) =
      match tryTruncate ty with
      | None -> (ty, None)
      | Some truncated -> (truncated, Some ty)

    { IdentRange = data.Ident
      Pos = data.InsertAt
      Kind = Type
      // TODO: or use tyForAnno?
      Text = ": " + truncated
      Insertions = Some <| data.CreateEdits tyForAnno
      Tooltip = tooltip }
    |> Some
  | _ -> None

let provideHints (text: NamedText, parseAndCheck: ParseAndCheckResults, range: Range) : Async<Hint[]> =
  asyncResult {
    let! cancellationToken = Async.CancellationToken

    let symbolUses =
      parseAndCheck.GetCheckResults.GetAllUsesOfAllSymbolsInFile(cancellationToken)
      |> Seq.filter (fun su -> rangeContainsRange range su.Range)

    let typeHints = ImmutableArray.CreateBuilder()
    let parameterHints = ImmutableArray.CreateBuilder()

    for symbolUse in symbolUses do
      match symbolUse.Symbol with
      | :? FSharpMemberOrFunctionOrValue as mfv when
        symbolUse.IsFromDefinition
        && isPotentialTargetForTypeAnnotation false (symbolUse, mfv)
        ->
        tryGetExplicitTypeInfo (text, parseAndCheck.GetAST) symbolUse.Range.Start
        |> Option.bind (fun explTy -> tryCreateTypeHint explTy mfv.FullType symbolUse.DisplayContext)
        |> Option.iter typeHints.Add

      | :? FSharpMemberOrFunctionOrValue as func when func.IsFunction && not symbolUse.IsFromDefinition ->
        let appliedArgRangesOpt =
          parseAndCheck.GetParseResults.GetAllArgumentsForFunctionApplicationAtPostion symbolUse.Range.Start

        match appliedArgRangesOpt with
        | None -> ()
        | Some [] -> ()
        | Some appliedArgRanges ->
          let parameters = func.CurriedParameterGroups |> Seq.concat
          let appliedArgRanges = appliedArgRanges |> Array.ofList
          let definitionArgs = parameters |> Array.ofSeq
          // invariant - definitionArgs should be at least as long as applied args.
          // if this is not the case (printfs?) we truncate to the lesser of the two
          let minLength = min definitionArgs.Length appliedArgRanges.Length

          if minLength = 0 then
            ()
          else
            for idx = 0 to minLength - 1 do
              let appliedArgRange = appliedArgRanges.[idx]
              let! appliedArgText = text[appliedArgRange]
              let definitionArg = definitionArgs.[idx]
              let definitionArgName = definitionArg.DisplayName

              if ShouldCreate.paramHint func definitionArg appliedArgText then
                let hint = createParamHint appliedArgRange definitionArgName
                parameterHints.Add(hint)
      | :? FSharpMemberOrFunctionOrValue as methodOrConstructor when methodOrConstructor.IsConstructor -> // TODO: support methods when this API comes into FCS
        let endPosForMethod = symbolUse.Range.End
        let line, _ = Position.toZ endPosForMethod

        let afterParenPosInLine =
          getFirstPositionAfterParen (text.Lines.[line].ToString()) (endPosForMethod.Column)

        let tupledParamInfos =
          parseAndCheck.GetParseResults.FindParameterLocations(Position.fromZ line afterParenPosInLine)

        let appliedArgRanges =
          parseAndCheck.GetParseResults.GetAllArgumentsForFunctionApplicationAtPostion symbolUse.Range.Start

        match tupledParamInfos, appliedArgRanges with
        | None, None -> ()

        // Prefer looking at the "tupled" view if it exists, even if the other ranges exist.
        // M(1, 2) can give results for both, but in that case we want the "tupled" view.
        | Some tupledParamInfos, _ ->
          let parameters =
            methodOrConstructor.CurriedParameterGroups
            |> Seq.concat
            |> Array.ofSeq // TODO: need ArgumentLocations to be surfaced

          for idx = 0 to parameters.Length - 1 do
            // let paramLocationInfo = tupledParamInfos.ArgumentLocations.[idx]
            let param = parameters.[idx]
            let paramName = param.DisplayName

            // if shouldCreateHint param && paramLocationInfo.IsNamedArgument then
            //     let hint = { Text = paramName + " ="; Pos = paramLocationInfo.ArgumentRange.Start; Kind = Parameter }
            //     parameterHints.Add(hint)
            ()

        // This will only happen for curried methods defined in F#.
        | _, Some appliedArgRanges ->
          let parameters =
            methodOrConstructor.CurriedParameterGroups
            |> Seq.concat

          let appliedArgRanges = appliedArgRanges |> Array.ofList
          let definitionArgs = parameters |> Array.ofSeq

          for idx = 0 to appliedArgRanges.Length - 1 do
            let appliedArgRange = appliedArgRanges.[idx]
            let! appliedArgText = text[appliedArgRange]
            let definitionArg = definitionArgs.[idx]

            if ShouldCreate.paramHint methodOrConstructor definitionArg appliedArgText then
              let hint = createParamHint appliedArgRange definitionArg.DisplayName
              parameterHints.Add(hint)

      | _ -> ()

    let typeHints = typeHints.ToImmutableArray()
    let parameterHints = parameterHints.ToImmutableArray()

    return typeHints.AddRange(parameterHints).ToArray()
  }
  |> AsyncResult.foldResult id (fun _ -> [||])
