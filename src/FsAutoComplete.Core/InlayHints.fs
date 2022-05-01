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

type HintKind =
  | Parameter
  | Type

type Hint =
  { Text: string
    InsertText: string option
    Pos: Position
    Kind: HintKind }

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
                    | SynExpr.Typed (_expr, _typeExpr, range) when Position.posEq range.Start pos ->
                        Some range
                    | _ -> defaultTraverse expr

                override _.VisitSimplePats(_path, pats) =
                    match pats with
                    | [] -> None
                    | _ ->
                        let exprFunc pat =
                            match pat with
                            | SynSimplePat.Typed (_pat, _targetExpr, range) when Position.posEq range.Start pos ->
                                Some range
                            | _ ->
                                None

                        pats |> List.tryPick exprFunc

                override _.VisitPat(_path, defaultTraverse, pat) =
                    match pat with
                    | SynPat.Typed (_pat, _targetType, range) when Position.posEq range.Start pos ->
                        Some range
                    | _ -> defaultTraverse pat

                override _.VisitBinding(_path, defaultTraverse, binding) =
                  match binding with
                  | SynBinding(headPat = SynPat.Named (range = patRange); returnInfo = Some (SynBindingReturnInfo(typeName = SynType.LongIdent (idents)))) -> Some patRange
                  | _ -> defaultTraverse binding

              }
        let result = SyntaxTraversal.Traverse(pos, x.ParseTree, visitor)
        result.IsSome

let private getFirstPositionAfterParen (str: string) startPos =
  match str with
  | null -> -1
  | str when startPos > str.Length -> -1
  | str -> str.IndexOf('(') + 1

let private maxHintLength = 30

let truncated (s: string) =
  if s.Length > maxHintLength then
    s.Substring(0, maxHintLength) + "..."
  else
    s

let private createParamHint
  (range: Range)
  (paramName: string)
  =
  let format p = p + " ="
  {
    Text = format (truncated paramName)
    InsertText = None
    Pos = range.Start
    Kind = Parameter
  }
let private createTypeHint
  (range: Range)
  (ty: FSharpType)
  (displayContext: FSharpDisplayContext)
  =
  let ty = ty.Format displayContext
  let format ty = ": " + ty
  {
    Text = format (truncated ty)
    InsertText = Some (format ty)
    Pos = range.End
    Kind = Type
  }

module private ShouldCreate =
  let private isNotWellKnownName =
    let names = Set.ofList [
      "value"
      "x"
    ]

    fun (p: FSharpParameter) ->
    match p.Name with
    | None -> true
    | Some n -> not (Set.contains n names)


  [<return: Struct>]
  let private (|StartsWith|_|) (v: string) (fullName: string) =
    if fullName.StartsWith v then
      ValueSome ()
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

  let private isWellKnownParameterOrFunction 
    (func: FSharpMemberOrFunctionOrValue)
    (param: FSharpParameter)
    =
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
        | Some "mapping" -> true
        | _ -> false
    | _ -> false

  let inline private hasName (p: FSharpParameter) =
    not (String.IsNullOrEmpty p.DisplayName)
    && p.DisplayName <> "````"

  let inline private isMeaningfulName (p: FSharpParameter) =
    p.DisplayName.Length > 2

  let inline private isOperator (func: FSharpMemberOrFunctionOrValue) =
    func.CompiledName.StartsWith "op_"

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
    &&
    (
      // same
      root.Length <= check.Length
      ||
      // rest must start with upper case -> new word
      Char.IsUpper root[check.Length]
    )
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
    &&
    (
      root.Length <= check.Length
      ||
        // postfix must start with upper case -> word boundary
        Char.IsUpper root[root.Length - check.Length]
    )

  let inline private removeLeadingUnderscore (name: ReadOnlySpan<char>) = 
    name.TrimStart '_'
  let inline private removeTrailingTick (name: ReadOnlySpan<char>) =
    name.TrimEnd '\''
  let inline private extractLastIdentifier (name: ReadOnlySpan<char>) =
    // exclude backticks for now: might contain `.` -> difficult to split
    if name.StartsWith "``" || name.EndsWith "``" then
      name
    else
      match name.LastIndexOf '.' with
      | -1 -> name
      | i -> name.Slice(i+1)
  /// Note: when in parens: might not be an identifier, but expression!
  /// 
  /// Note: might result in invalid expression (because no matching parens `string (2)` -> `string (2`)
  let inline private trimParensAndSpace (name: ReadOnlySpan<char>) =
    name.TrimStart("( ").TrimEnd(" )")

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
        //todo: expression -> early out? or further processing? special processing?
        argumentText

    //todo: all useful?

    // // covered by each isPre/PostfixOf
    // areSame paramName argumentName
    // ||
    isPrefixOf argumentName paramName
    ||
    isPostfixOf argumentName paramName
    ||
    isPrefixOf paramName argumentName
    ||
    isPostfixOf paramName argumentName

  let inline private doesNotMatchArgumentText (parameterName: string) (userArgumentText: string) =
    parameterName <> userArgumentText
    && not (userArgumentText.StartsWith parameterName)

  let private isParamNamePostfixOfFuncName
    (func: FSharpMemberOrFunctionOrValue)
    (paramName: string)
    =
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
  let paramHint
    (func: FSharpMemberOrFunctionOrValue)
    (p: FSharpParameter)
    (argumentText: string)
    =
    hasName p
    && isMeaningfulName p
    && isNotWellKnownName p
    && (not (isWellKnownParameterOrFunction func p))
    && (not (isOperator func))
    && (not (areSimilar p.DisplayName argumentText))
    && (not (isParamNamePostfixOfFuncName func p.DisplayName))


let provideHints (text: NamedText, p: ParseAndCheckResults, range: Range) : Async<Hint []> =
  asyncResult {
    let parseFileResults, checkFileResults = p.GetParseResults, p.GetCheckResults
    let! cancellationToken = Async.CancellationToken

    let symbolUses =
      checkFileResults.GetAllUsesOfAllSymbolsInFile(cancellationToken)
      |> Seq.filter (fun su -> Range.rangeContainsRange range su.Range)
      |> Seq.toList

    let typeHints = ImmutableArray.CreateBuilder()
    let parameterHints = ImmutableArray.CreateBuilder()

    let isValidForTypeHint (funcOrValue: FSharpMemberOrFunctionOrValue) (symbolUse: FSharpSymbolUse) =
      let isLambdaIfFunction =
        funcOrValue.IsFunction
        && parseFileResults.IsBindingALambdaAtPosition symbolUse.Range.Start

      let isTypedPat (r: Range)=
        parseFileResults.IsTypeAnnotationGivenAtPositionPatched r.Start

      (funcOrValue.IsValue || isLambdaIfFunction)
      && not (isTypedPat symbolUse.Range)
      && symbolUse.IsFromDefinition
      && not funcOrValue.IsMember
      && not funcOrValue.IsMemberThisValue
      && not funcOrValue.IsConstructorThisValue
      && not (PrettyNaming.IsOperatorDisplayName funcOrValue.DisplayName)

    for symbolUse in symbolUses do
      match symbolUse.Symbol with
      | :? FSharpMemberOrFunctionOrValue as funcOrValue when
        isValidForTypeHint funcOrValue symbolUse
        ->
        let hint = createTypeHint symbolUse.Range funcOrValue.ReturnParameter.Type symbolUse.DisplayContext
        typeHints.Add(hint)

      | :? FSharpMemberOrFunctionOrValue as func when func.IsFunction && not symbolUse.IsFromDefinition ->
        let appliedArgRangesOpt =
          parseFileResults.GetAllArgumentsForFunctionApplicationAtPostion symbolUse.Range.Start

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

              if
                ShouldCreate.paramHint func definitionArg appliedArgText
              then
                let hint = createParamHint appliedArgRange definitionArgName
                parameterHints.Add(hint)

      | :? FSharpMemberOrFunctionOrValue as methodOrConstructor when methodOrConstructor.IsConstructor -> // TODO: support methods when this API comes into FCS
        let endPosForMethod = symbolUse.Range.End
        let line, _ = Position.toZ endPosForMethod

        let afterParenPosInLine =
          getFirstPositionAfterParen (text.Lines.[line].ToString()) (endPosForMethod.Column)

        let tupledParamInfos =
          parseFileResults.FindParameterLocations(Position.fromZ line afterParenPosInLine)

        let appliedArgRanges =
          parseFileResults.GetAllArgumentsForFunctionApplicationAtPostion symbolUse.Range.Start

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
