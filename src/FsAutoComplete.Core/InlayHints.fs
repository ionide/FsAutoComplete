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
open System.Text

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

type FSharp.Compiler.CodeAnalysis.FSharpParseFileResults with
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

let inline private truncated (s: string) =
  if s.Length > maxHintLength then
    s.Substring(0, maxHintLength) + "..."
  else
    s

let private isNotWellKnownName =
  let names = Set.ofList [
    "mapping"
    "format"
    "value"
    "x"
  ]

  fun (p: FSharpParameter) ->
  match p.Name with
  | None -> true
  | Some n -> not (Set.contains n names)

let inline hasName (p: FSharpParameter) =
  not (String.IsNullOrEmpty p.DisplayName)
  && p.DisplayName <> "````"

let inline isMeaningfulName (p: FSharpParameter) =
  p.DisplayName.Length > 2

let inline doesNotMatchArgumentText (parameterName: string) (userArgumentText: string) =
  parameterName <> userArgumentText
  && not (userArgumentText.StartsWith parameterName)

/// </summary>
/// We filter out parameters that generate lots of noise in hints.
/// * parameter has a name
/// * parameter is one of a set of 'known' names that clutter (like printfn formats)
/// * parameter has length > 2
/// * parameter does not match (or is an extension of) the user-entered text
/// </summary>
let shouldCreateHint (p: FSharpParameter) (matchingArgumentText: string) =
  hasName p
  && isNotWellKnownName p
  && isMeaningfulName p
  && doesNotMatchArgumentText p.DisplayName matchingArgumentText

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

        let layout = $": {truncated(funcOrValue.ReturnParameter.Type.Format symbolUse.DisplayContext)}"
        let insertText = $": {funcOrValue.ReturnParameter.Type.Format symbolUse.DisplayContext}"

        let hint =
          { Text = layout
            InsertText = Some insertText
            Pos = symbolUse.Range.End
            Kind = Type }

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
                shouldCreateHint definitionArg appliedArgText
              then
                let hint =
                  { Text = $"{truncated definitionArgName} ="
                    InsertText = None
                    Pos = appliedArgRange.Start
                    Kind = Parameter }

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

            if shouldCreateHint definitionArg appliedArgText then
              let hint =
                { Text = $"{truncated definitionArg.DisplayName} ="
                  InsertText = None
                  Pos = appliedArgRange.Start
                  Kind = Parameter }

              parameterHints.Add(hint)
      | _ -> ()

    let typeHints = typeHints.ToImmutableArray()
    let parameterHints = parameterHints.ToImmutableArray()

    return typeHints.AddRange(parameterHints).ToArray()
  }
  |> AsyncResult.foldResult id (fun _ -> [||])
