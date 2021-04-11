module FsAutoComplete.SignatureHelp

open FSharp.Compiler.Text
open FSharp.Compiler.SourceCodeServices
open FsToolkit.ErrorHandling
open FsAutoComplete
open System
open FSharp.UMX

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

let private lineText (lines: LineStr []) (pos: Pos) = lines.[pos.Line - 1]

let private charAt (lines: LineStr []) (pos: Pos) =
      (lineText lines pos).[pos.Column - 1]

let private getSignatureHelpForFunctionApplication (tyRes: ParseAndCheckResults, caretPos: Pos, endOfPreviousIdentPos: Pos, lines: LineStr[]) : Async<SignatureHelpInfo option> =
  asyncMaybe {
    let lineStr = lineText lines endOfPreviousIdentPos
    let! possibleApplicationSymbolEnd = maybe {
      if tyRes.GetParseResults.IsPosContainedInApplication endOfPreviousIdentPos then
        let! funcRange = tyRes.GetParseResults.TryRangeOfFunctionOrMethodBeingApplied endOfPreviousIdentPos
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

let getSignatureHelpFor (tyRes : ParseAndCheckResults, pos: Pos, lines: LineStr[], triggerChar, possibleSessionKind) =
  asyncResult {
    let charAt (pos: Pos) =
      lines.[pos.Line - 1].[pos.Column - 1]

    let previousNonWhitespaceCharPos =
      let dec (pos: Pos): Pos =
        if pos.Column = 0 then
          let prevLine = lines.[pos.Line - 2]
          // retreat to the end of the previous line
          Pos.mkPos (pos.Line - 1) (prevLine.Length - 1)
        else
          Pos.mkPos (pos.Line) (pos.Column - 1)

      let rec loop ch pos =
        if Char.IsWhiteSpace ch then
          let prevPos = dec pos
          loop (charAt prevPos) prevPos
        else
          pos
      let initialPos = dec pos
      loop (charAt initialPos) initialPos

    let previousNonWhitespaceChar = charAt previousNonWhitespaceCharPos
    match triggerChar, possibleSessionKind with
    // Generally ' ' indicates a function application, but it's also used commonly after a comma in a method call.
    // This means that the adjusted position relative to the caret could be a ',' or a '(' or '<',
    // which would mean we're already inside of a method call - not a function argument. So we bail if that's the case.
    | (Some ' ', _) | (_, Some FunctionApplication) when previousNonWhitespaceChar <> ',' && previousNonWhitespaceChar <> '(' && previousNonWhitespaceChar <> '<' ->
      return! getSignatureHelpForFunctionApplication (tyRes, pos, previousNonWhitespaceCharPos, lines)
    | _ ->
      let! methods, commas = tyRes.TryGetMethodOverrides lines pos
      if methods.Methods.Length = 0 then
        return None
      else
        let overloadsByParameterCount = methods.Methods |> Array.sortBy (fun m -> m.Parameters.Length)
        // naievely assume that the first overload with the same or greater number of parameters is our match
        let activeSig = overloadsByParameterCount |> Array.tryFindIndex (fun m -> m.Parameters.Length >= commas)
        return Some {
          Methods = overloadsByParameterCount
          ActiveOverload = activeSig
          ActiveParameter = Some commas
          SigHelpKind = MethodCall
        }
  }

