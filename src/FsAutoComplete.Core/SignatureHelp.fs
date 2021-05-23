module FsAutoComplete.SignatureHelp

open System
open FSharp.Compiler.Text
open FSharp.Compiler.SourceCodeServices
open FsToolkit.ErrorHandling
open FsAutoComplete
open FSharp.UMX
open FsAutoComplete.FCSPatches

type SignatureHelpKind = MethodCall | FunctionApplication

type SignatureHelpInfo = {
  /// all potential overloads of the member at the position where signaturehelp was invoked
  Methods: FSharpMethodGroupItem []
  /// if present, the index of the method we think is the current one (will never be outside the bounds of the Methods array)
  ActiveOverload: int option
  /// if present, the index of the parameter on the active method (will never be outside the bounds of the Parameters array on the selected method)
  ActiveParameter: int option
  SigHelpKind: SignatureHelpKind
}

let private lineText (lines: ISourceText) (pos: Pos) = lines.GetLineString(pos.Line - 1)

let private charAt (lines: ISourceText) (pos: Pos) =
      (lineText lines pos).[pos.Column - 1]

let dec (lines: ISourceText) (pos: Pos): Pos =
  if pos.Column = 0 then
    let prevLine = lines.GetLineString (pos.Line - 2)
    // retreat to the end of the previous line
    Pos.mkPos (pos.Line - 1) (prevLine.Length - 1)
  else
    Pos.mkPos pos.Line (pos.Column - 1)

let inc (lines: ISourceText) (pos: Pos): Pos =
  let currentLine = lineText lines pos
  if pos.Column - 1 = currentLine.Length then
    // advance to the beginning of the next line
    Pos.mkPos (pos.Line + 1) 0
  else
    Pos.mkPos pos.Line (pos.Column + 1)

let getText (lines: ISourceText) (range: Range) =
  if range.Start.Line = range.End.Line then
    let line = lineText lines range.Start
    line.Substring(range.StartColumn - 1, (range.End.Column - range.Start.Column))
  else
    String.concat Environment.NewLine (seq {
      let startLine = lineText lines range.Start
      yield startLine.Substring(range.StartColumn - 1, (startLine.Length - 1 - range.Start.Column))
      for lineNo in (range.Start.Line+1)..(range.End.Line-1) do
        yield lines.GetLineString(lineNo - 1)
      let endLine = lineText lines range.End
      yield endLine.Substring(0, range.End.Column - 1)
    })

let private getSignatureHelpForFunctionApplication (tyRes: ParseAndCheckResults, caretPos: Pos, endOfPreviousIdentPos: Pos, lines: ISourceText) : Async<SignatureHelpInfo option> =
  asyncMaybe {
    let lineStr = lineText lines endOfPreviousIdentPos
    let! possibleApplicationSymbolEnd = maybe {
      if tyRes.GetParseResults.IsPosContainedInApplicationPatched endOfPreviousIdentPos then
        let! funcRange = tyRes.GetParseResults.TryRangeOfFunctionOrMethodBeingAppliedPatched endOfPreviousIdentPos
        return funcRange.End
      else return endOfPreviousIdentPos
    }

    let possibleApplicationSymbolLineStr = lineText lines possibleApplicationSymbolEnd
    let! (endCol, names) = Lexer.findLongIdents(possibleApplicationSymbolEnd.Column, possibleApplicationSymbolLineStr)
    let idents = List.ofArray names
    let! symbolUse = tyRes.GetCheckResults.GetSymbolUseAtLocation(possibleApplicationSymbolEnd.Line, endCol, lineStr, idents)

    let isValid (mfv: FSharpMemberOrFunctionOrValue) =
      not (PrettyNaming.IsOperatorName mfv.DisplayName) &&
      not mfv.IsProperty &&
      mfv.CurriedParameterGroups.Count > 0

    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv when isValid mfv ->
      let tooltip = tyRes.GetCheckResults.GetToolTipText(possibleApplicationSymbolEnd.Line, endCol, possibleApplicationSymbolLineStr, idents, FSharpTokenTag.IDENT)
      match tooltip with
      | FSharpToolTipText []
      | FSharpToolTipText [FSharpToolTipElement.None] -> return! None
      | _ ->
        let symbolStart = symbolUse.RangeAlternate.Start
        let possiblePipelineIdent = tyRes.GetParseResults.TryIdentOfPipelineContainingPosAndNumArgsApplied symbolStart
        let numArgsAlreadyApplied = possiblePipelineIdent |> Option.map snd |> Option.defaultValue 0
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
              |> Array.tryFindIndex(fun argRange -> Range.rangeContainsPos argRange caretPos)

          match possibleExactIndex with
          | Some index -> Some index
          | None ->
              let possibleNextIndex =
                  curriedArgsInSource
                  |> Array.tryFindIndex(fun argRange -> Pos.posGeq argRange.Start caretPos)

              match possibleNextIndex with
              | Some index -> Some index
              | None ->
                  if numDefinedArgs - numArgsAlreadyApplied > curriedArgsInSource.Length then
                      Some (numDefinedArgs - (numDefinedArgs - curriedArgsInSource.Length))
                  else
                      None
        let methods = tyRes.GetCheckResults.GetMethods(symbolStart.Line, symbolUse.RangeAlternate.End.Column, lineText lines symbolStart, None)

        return {
          ActiveParameter = Some argumentIndex
          Methods = methods.Methods
          ActiveOverload = None
          SigHelpKind = FunctionApplication
        }
    | _ ->
      return! None
  }

let private getSignatureHelpForMethod (tyRes: ParseAndCheckResults, caretPos: Pos, lines: ISourceText, triggerChar) =
  asyncMaybe {
    let! paramLocations = tyRes.GetParseResults.FindNoteworthyParamInfoLocations caretPos
    let names = paramLocations.LongId
    let lidEnd = paramLocations.LongIdEndLocation
    let lineText = lineText lines lidEnd
    let methodGroup = tyRes.GetCheckResults.GetMethods(lidEnd.Line, lidEnd.Column, lineText, Some names)
    let methods = methodGroup.Methods
    do! Option.guard (methods.Length > 0 && not(methodGroup.MethodName.EndsWith("> )")))

    let isStaticArgTip = charAt lines paramLocations.OpenParenLocation = '<'
    let filteredMethods =
      [|
        for m in methods do
          // need to distinguish TP<...>(...)  angle brackets tip from parens tip
          if (isStaticArgTip && m.StaticParameters.Length > 0) || (not isStaticArgTip && m.HasParameters) then m
      |]
    do! Option.guard (filteredMethods.Length > 0)

    let endPos =
      let last = paramLocations.TupleEndLocations |> Array.last
      if paramLocations.IsThereACloseParen then dec lines last else last

    let startOfArgs = inc lines paramLocations.OpenParenLocation
    let tupleEnds =
      [|
          startOfArgs
          for i in 0..paramLocations.TupleEndLocations.Length-2 do
              paramLocations.TupleEndLocations.[i]
          endPos
      |]
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
    | Some ('<' | '(' | ',') when not (tupleEnds |> Array.exists (fun lp -> lp.Column = caretPos.Column)) ->
      return! None // comma or paren at wrong location = remove help display
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


      return {
        ActiveParameter = Some argumentIndex
        Methods = filteredMethods
        ActiveOverload = methodCandidate
        SigHelpKind = MethodCall
      }
  }

let getSignatureHelpFor (tyRes : ParseAndCheckResults, pos: Pos, lines: ISourceText, triggerChar, possibleSessionKind) =
  asyncResult {
    let previousNonWhitespaceCharPos =
      let rec loop ch pos =
        if Char.IsWhiteSpace ch then
          let prevPos = dec lines pos
          loop (charAt lines prevPos) prevPos
        else
          pos
      let initialPos = dec lines pos
      loop (charAt lines initialPos) initialPos

    let charAtPos = match triggerChar with Some char -> char | None -> charAt lines pos

    let previousNonWhitespaceChar = charAt lines previousNonWhitespaceCharPos
    match charAtPos, possibleSessionKind with
    // Generally ' ' indicates a function application, but it's also used commonly after a comma in a method call.
    // This means that the adjusted position relative to the caret could be a ',' or a '(' or '<',
    // which would mean we're already inside of a method call - not a function argument. So we bail if that's the case.
    | (' ', _) | (_, Some FunctionApplication) when previousNonWhitespaceChar <> ',' && previousNonWhitespaceChar <> '(' && previousNonWhitespaceChar <> '<' ->
      return! getSignatureHelpForFunctionApplication (tyRes, pos, previousNonWhitespaceCharPos, lines)
    | _ ->
      return! getSignatureHelpForMethod (tyRes, pos, lines, triggerChar)
  }

