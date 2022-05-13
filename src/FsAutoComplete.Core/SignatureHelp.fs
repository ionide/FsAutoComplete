module FsAutoComplete.SignatureHelp

open System
open FSharp.Compiler.Text
open FsToolkit.ErrorHandling
open FsAutoComplete
open FSharp.UMX
open FsAutoComplete.FCSPatches
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Tokenization

type SignatureHelpKind =
  | MethodCall
  | FunctionApplication

type SignatureHelpInfo =
  {
    /// all potential overloads of the member at the position where signaturehelp was invoked
    Methods: MethodGroupItem[]
    /// if present, the index of the method we think is the current one (will never be outside the bounds of the Methods array)
    ActiveOverload: int option
    /// if present, the index of the parameter on the active method (will never be outside the bounds of the Parameters array on the selected method)
    ActiveParameter: int option
    SigHelpKind: SignatureHelpKind
  }

let private getSignatureHelpForFunctionApplication
  (
    tyRes: ParseAndCheckResults,
    caretPos: Position,
    endOfPreviousIdentPos: Position,
    lines: NamedText
  ) : Async<SignatureHelpInfo option> =
  asyncMaybe {
    let! lineStr = lines.GetLine endOfPreviousIdentPos

    let! possibleApplicationSymbolEnd =
      maybe {
        if tyRes.GetParseResults.IsPosContainedInApplication endOfPreviousIdentPos then
          let! funcRange = tyRes.GetParseResults.TryRangeOfFunctionOrMethodBeingApplied endOfPreviousIdentPos
          return funcRange.End
        else
          return endOfPreviousIdentPos
      }

    let! possibleApplicationSymbolLineStr = lines.GetLine possibleApplicationSymbolEnd
    let! (endCol, names) = Lexer.findLongIdents (possibleApplicationSymbolEnd.Column, possibleApplicationSymbolLineStr)
    let idents = List.ofArray names

    let! symbolUse =
      tyRes.GetCheckResults.GetSymbolUseAtLocation(possibleApplicationSymbolEnd.Line, endCol, lineStr, idents)

    let isValid (mfv: FSharpMemberOrFunctionOrValue) =
      not (PrettyNaming.IsOperatorDisplayName mfv.DisplayName)
      && not mfv.IsProperty
      && mfv.CurriedParameterGroups.Count > 0

    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv when isValid mfv ->
      let tooltip =
        tyRes.GetCheckResults.GetToolTip(
          possibleApplicationSymbolEnd.Line,
          endCol,
          possibleApplicationSymbolLineStr,
          idents,
          FSharpTokenTag.IDENT
        )

      match tooltip with
      | ToolTipText []
      | ToolTipText [ ToolTipElement.None ] -> return! None
      | _ ->
        let symbolStart = symbolUse.Range.Start

        let possiblePipelineIdent =
          tyRes.GetParseResults.TryIdentOfPipelineContainingPosAndNumArgsApplied symbolStart

        let numArgsAlreadyApplied =
          possiblePipelineIdent |> Option.map snd |> Option.defaultValue 0

        let definedArgs = mfv.CurriedParameterGroups |> Array.ofSeq
        let numDefinedArgs = definedArgs.Length

        let curriedArgsInSource =
          tyRes.GetParseResults.GetAllArgumentsForFunctionApplicationAtPostion symbolStart
          |> Option.defaultValue []
          |> Array.ofList

        do! Option.guard (numDefinedArgs >= curriedArgsInSource.Length)

        let! argumentIndex =
          let possibleExactIndex =
            curriedArgsInSource
            |> Array.tryFindIndex (fun argRange -> Range.rangeContainsPos argRange caretPos)

          match possibleExactIndex with
          | Some index -> Some index
          | None ->
            let possibleNextIndex =
              curriedArgsInSource
              |> Array.tryFindIndex (fun argRange -> Position.posGeq argRange.Start caretPos)

            match possibleNextIndex with
            | Some index -> Some index
            | None ->
              if numDefinedArgs - numArgsAlreadyApplied > curriedArgsInSource.Length then
                Some(numDefinedArgs - (numDefinedArgs - curriedArgsInSource.Length))
              else
                None

        let! symbolStartLineText = lines.GetLine symbolStart

        let methods =
          tyRes.GetCheckResults.GetMethods(symbolStart.Line, symbolUse.Range.EndColumn, symbolStartLineText, None)

        return
          { ActiveParameter = Some argumentIndex
            Methods = methods.Methods
            ActiveOverload = None
            SigHelpKind = FunctionApplication }
    | _ -> return! None
  }

let private getSignatureHelpForMethod (tyRes: ParseAndCheckResults, caretPos: Position, lines: NamedText, triggerChar) =
  asyncMaybe {
    let! paramLocations = tyRes.GetParseResults.FindParameterLocations caretPos
    let names = paramLocations.LongId
    let lidEnd = paramLocations.LongIdEndLocation
    let! lineText = lines.GetLine lidEnd

    let methodGroup =
      tyRes.GetCheckResults.GetMethods(lidEnd.Line, lidEnd.Column, lineText, Some names)

    let methods = methodGroup.Methods

    do! Option.guard (methods.Length > 0 && not (methodGroup.MethodName.EndsWith("> )")))

    let isStaticArgTip = lines.TryGetChar paramLocations.OpenParenLocation = Some '<'

    let filteredMethods =
      [| for m in methods do
           // need to distinguish TP<...>(...)  angle brackets tip from parens tip
           if
             (isStaticArgTip && m.StaticParameters.Length > 0)
             || (not isStaticArgTip && m.HasParameters)
           then
             m |]

    do! Option.guard (filteredMethods.Length > 0)

    let endPos =
      let last = paramLocations.TupleEndLocations |> Array.last

      if paramLocations.IsThereACloseParen then
        lines.PrevPos last |> Option.defaultValue last
      else
        last

    let startOfArgs = lines.NextPos paramLocations.OpenParenLocation

    let tupleEnds =
      [| yield! Option.toList startOfArgs
         for i in 0 .. paramLocations.TupleEndLocations.Length - 2 do
           yield paramLocations.TupleEndLocations.[i]
         yield endPos |]
    // If we are pressing "(" or "<" or ",", then only pop up the info if this is one of the actual, real detected positions in the detected promptable call
    //
    // For example the last "(" in
    //    List.map (fun a -> (
    // should not result in a prompt.
    //
    // Likewise the last "," in
    //    Console.WriteLine( [(1,
    // should not result in a prompt, whereas this one will:
    //    Console.WriteLine( [(1,2)],
    match triggerChar with
    | Some ('<'
    | '('
    | ',') when not (tupleEnds |> Array.exists (fun lp -> lp.Column = caretPos.Column)) -> return! None // comma or paren at wrong location = remove help display
    | _ ->
      // Compute the argument index by working out where the caret is between the various commas.
      let argumentIndex =
        let computedTextSpans =
          tupleEnds
          |> Array.pairwise
          |> Array.map (fun (lp1, lp2) -> Range.mkRange "" lp1 lp2)

        computedTextSpans
        |> Array.tryFindIndex (fun t -> Range.rangeContainsPos t caretPos)
        |> Option.defaultValue 0

      // todo: this picks the 'first' overload with the correct arity, but really we should be smarter
      let methodCandidate =
        filteredMethods
        |> Array.tryFindIndex (fun m -> m.Parameters.Length >= argumentIndex + 1)


      return
        { ActiveParameter = Some argumentIndex
          Methods = filteredMethods
          ActiveOverload = methodCandidate
          SigHelpKind = MethodCall }
  }

let getSignatureHelpFor
  (
    tyRes: ParseAndCheckResults,
    pos: Position,
    lines: NamedText,
    triggerChar,
    possibleSessionKind
  ) =
  asyncResult {
    let previousNonWhitespaceChar =
      let rec loop ch pos =
        if Char.IsWhiteSpace ch then
          match lines.TryGetPrevChar pos with
          | Some (prevPos, prevChar) -> loop prevChar prevPos
          | None -> None
        else
          Some(pos, ch)

      match lines.TryGetPrevChar pos with
      | Some (prevPos, prevChar) -> loop prevChar prevPos
      | None -> None

    let! (previousNonWhitespaceCharPos, previousNonWhitespaceChar) =
      previousNonWhitespaceChar
      |> Result.ofOption (fun _ -> "Couldn't find previous non-whitespace char")

    let! charAtPos =
      triggerChar
      |> Option.orElseWith (fun _ -> lines.TryGetChar pos)
      |> Result.ofOption (fun _ -> "Couldn't find a trigger char")

    match charAtPos, possibleSessionKind with
    // Generally ' ' indicates a function application, but it's also used commonly after a comma in a method call.
    // This means that the adjusted position relative to the caret could be a ',' or a '(' or '<',
    // which would mean we're already inside of a method call - not a function argument. So we bail if that's the case.
    | (' ', _)
    | (_, Some FunctionApplication) when
      previousNonWhitespaceChar <> ','
      && previousNonWhitespaceChar <> '('
      && previousNonWhitespaceChar <> '<'
      ->
      return! getSignatureHelpForFunctionApplication (tyRes, pos, previousNonWhitespaceCharPos, lines)
    | _ -> return! getSignatureHelpForMethod (tyRes, pos, lines, triggerChar)
  }
