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

type HintKind = Parameter | Type
type Hint = { Text: string; Pos: Position; Kind: HintKind }

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

let getFirstPositionAfterParen (str: string) startPos =
  match str with
  | null -> -1
  | str when startPos > str.Length -> -1
  | str -> str.IndexOf('(') + 1

let provideHints (text: NamedText, p: ParseAndCheckResults, range: Range) : Async<Hint []> = async {
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

    (funcOrValue.IsValue || isLambdaIfFunction)
    && not (parseFileResults.IsTypeAnnotationGivenAtPosition symbolUse.Range.Start)
    && symbolUse.IsFromDefinition
    && not funcOrValue.IsMember
    && not funcOrValue.IsMemberThisValue
    && not funcOrValue.IsConstructorThisValue
    && not (PrettyNaming.IsOperatorDisplayName funcOrValue.DisplayName)

  for symbolUse in symbolUses do
    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as funcOrValue when isValidForTypeHint funcOrValue symbolUse ->
      let layout =
        ": "
        + funcOrValue.ReturnParameter.Type.Format symbolUse.DisplayContext

      let hint =
        { Text = layout
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
        if minLength = 0 then ()
        else
          for idx = 0 to minLength - 1 do
            let appliedArgRange = appliedArgRanges.[idx]
            let definitionArgName = definitionArgs.[idx].DisplayName

            if not (String.IsNullOrWhiteSpace(definitionArgName)) && definitionArgName <> "````" then
              let hint =
                { Text = definitionArgName + " ="
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
          // let paramLocationInfo = tupledParamInfos. .ArgumentLocations.[idx]
          // let paramName = parameters.[idx].DisplayName
          // if not paramLocationInfo.IsNamedArgument && not (String.IsNullOrWhiteSpace(paramName)) then
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
          let definitionArgName = definitionArgs.[idx].DisplayName

          if not (String.IsNullOrWhiteSpace(definitionArgName)) then
            let hint =
              { Text = definitionArgName + " ="
                Pos = appliedArgRange.Start
                Kind = Parameter }

            parameterHints.Add(hint)
    | _ -> ()

  let typeHints = typeHints.ToImmutableArray()
  let parameterHints = parameterHints.ToImmutableArray()

  return typeHints.AddRange(parameterHints).ToArray()
}
