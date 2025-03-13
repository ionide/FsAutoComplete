module FsAutoComplete.NestedLanguages

open FsAutoComplete.Logging
open FsToolkit.ErrorHandling
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

#nowarn "57" // from-end slicing

let logger = LogProvider.getLoggerByName "NestedLanguages"

type private StringParameter =
  { methodIdent: LongIdent
    parameterRange: Range
    rangesToRemove: Range array
    parameterPosition: int }

let private pattern =
  System.Text.RegularExpressions.Regex(
    @"(?<percent>%)(?<flags>[0\+\-]?)(?<width>\d*)(\.(?<precision>\d+))?(?<type>[bscdiuxXoBeEfFgGMOAat%])",
    System.Text.RegularExpressions.RegexOptions.Compiled
  )

let private isFormatSpecifier (text: string) = pattern.Match(text)

/// for virtual documents based on interpolated strings we need to remove two kinds of trivia from the overall string portions.
/// * for interpolation expressions we need to remove the entire range of the expression - this will be invisible to the virtual document since it is F# code.
/// * for string literals, we need to remove the prefix/suffix tokens (quotes, interpolation brackets, format specifiers, etc) so that the only content visible
/// to the virtual document is the actual string content.
///
/// FEATURE GAP: we don't know in the AST the locations of the string trivia, so we can't support format specifiers or variable-length
///              interpolation start/end tokens.
let private discoverRangesToRemoveForInterpolatedString
  (stringKind: SynStringKind)
  (parts: SynInterpolatedStringPart[])
  =
  parts
  |> Array.indexed
  |> Array.collect (fun (index, part) ->
    match part with
    | SynInterpolatedStringPart.FillExpr(fillExpr = e) -> [| e.Range |]
    // for the first part we have whatever 'leading' element on the left and a trailing interpolation piece (which can include a format specifier) on the right
    | SynInterpolatedStringPart.String(value = value; range = range) when index = 0 ->
      [|
         // leading tokens adjustment
         // GAP: we don't know how many interpolation $ or " there are, so we are guessing
         let startRange =
           match stringKind with
           | SynStringKind.Regular ->
             // 'regular' means $" leading identifier
             range.WithEnd(range.Start.WithColumn(range.StartColumn + 2))
           | SynStringKind.TripleQuote ->
             // 'triple quote' means $""" leading identifier
             range.WithEnd(range.Start.WithColumn(range.StartColumn + 4))
           // there's no such thing as a verbatim interpolated string
           | SynStringKind.Verbatim -> range


         // GAP: we don't know if there's a format specifier at the front: %[flags][width][.precision][type]
         // flags are 0,+,-, width is an integer, precision is `.` followed by an integer, type is one of the following: b, s, c, d, i, u, x, X, o, B, e, E, f, F, g, G, M, O, A, a, t, %
         let adjustedRangeForFormatSpecifier =
           let formatmatch = isFormatSpecifier value

           if formatmatch.Success then
             startRange.WithEnd(startRange.End.WithColumn(startRange.StartColumn + formatmatch.Index - 1))
           else
             startRange

         adjustedRangeForFormatSpecifier

         // trailing token adjustment- only an opening bracket {
         // GAP: this is the feature gap - we don't know about format specifiers
         range.WithStart(range.End.WithColumn(range.EndColumn - 1))

         |]
    // for the last part we have a single-character interpolation bracket on the left and the 'trailing' string elements on the right
    | SynInterpolatedStringPart.String(range = range) when index = parts.Length - 1 ->
      [|
         // leading token adjustment - only a closing bracket }
         range.WithEnd(range.Start.WithColumn(range.StartColumn + 1))

         // trailing tokens adjustment
         // GAP: we don't know how many """ to adjust for triple-quote interpolated string endings
         match stringKind with
         | SynStringKind.Regular ->
           // 'regular' means trailing identifier "
           range.WithStart(range.End.WithColumn(range.EndColumn - 1))
         | SynStringKind.TripleQuote ->
           // 'triple quote' means trailing identifier """
           range.WithStart(range.End.WithColumn(range.EndColumn - 3))
         // no such thing as verbatim interpolated strings
         | SynStringKind.Verbatim -> () |]
    // for all other parts we have a single-character interpolation bracket on the left and a trailing interpolation piece (which can include a format specifier) on the right
    | SynInterpolatedStringPart.String(range = range) ->
      [|
         // leading token adjustment - only a closing bracket }
         range.WithEnd(range.Start.WithColumn(range.StartColumn + 1))
         // trailing token adjustment- only an opening bracket {
         // GAP: this is the feature gap - we don't know about format specifiers here
         range.WithStart(range.End.WithColumn(range.EndColumn - 1)) |])

let private (|Ident|_|) (e: SynExpr) =
  match e with
  | SynExpr.Ident(ident) -> Some([ ident ])
  | SynExpr.LongIdent(longDotId = SynLongIdent(id = ident)) -> Some ident
  | _ -> None

/// in order for nested documents to be recognized as their document types, the string quotes (and other tokens) need to be removed
/// from the actual string content.
let private removeStringTokensFromStringRange (kind: SynStringKind) (range: Range) : Range array =
  match kind with
  | SynStringKind.Regular ->
    // we need to trim the double-quote off of the start and end
    [| Range.mkRange range.FileName range.Start (range.Start.WithColumn(range.StartColumn + 1))
       Range.mkRange range.FileName (range.End.WithColumn(range.EndColumn - 1)) range.End |]
  | SynStringKind.Verbatim ->
    // we need to trim the @+double-quote off of the start and double-quote off the end
    [| Range.mkRange range.FileName range.Start (range.Start.WithColumn(range.StartColumn + 2))
       Range.mkRange range.FileName (range.End.WithColumn(range.EndColumn - 1)) range.End |]
  | SynStringKind.TripleQuote ->
    // we need to trim the @+double-quote off of the start and double-quote off the end
    [| Range.mkRange range.FileName range.Start (range.Start.WithColumn(range.StartColumn + 2))
       Range.mkRange range.FileName (range.End.WithColumn(range.EndColumn - 1)) range.End |]

let rec private (|IsApplicationWithStringParameters|_|) (e: SynExpr) : StringParameter array option =
  match e with
  // lines inside a binding
  // let doThing () =
  //    c.M("<div>")
  //    c.M($"<div>{1 + 1}")
  //    "<div>" |> c.M
  //    $"<div>{1 + 1}" |> c.M
  | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
    let e1Parameters =
      match e1 with
      | IsApplicationWithStringParameters(stringParameter) when not (Array.isEmpty stringParameter) ->
        ValueSome stringParameter
      | _ -> ValueNone

    let e2Parameters =
      match e2 with
      | IsApplicationWithStringParameters(stringParameter) when not (Array.isEmpty stringParameter) ->
        ValueSome stringParameter
      | _ -> ValueNone

    match e1Parameters, e2Parameters with
    | ValueNone, ValueNone -> None
    | ValueSome e1Parameters, ValueNone -> Some e1Parameters
    | ValueNone, ValueSome e2Parameters -> Some e2Parameters
    | ValueSome e1Parameters, ValueSome e2Parameters -> Some(Array.append e1Parameters e2Parameters)

  // method call with string parameter - c.M("<div>")
  | SynExpr.App(
      funcExpr = Ident(ident); argExpr = SynExpr.Paren(expr = SynExpr.Const(SynConst.String(_text, kind, range), _)))
  // method call with string parameter - c.M "<div>"
  | SynExpr.App(funcExpr = Ident(ident); argExpr = SynExpr.Const(SynConst.String(_text, kind, range), _)) ->
    Some(
      [| { methodIdent = ident
           parameterRange = range
           rangesToRemove = removeStringTokensFromStringRange kind range
           parameterPosition = 0 } |]
    )
  // method call with interpolated string parameter - c.M $"<div>{1 + 1}"
  | SynExpr.App(
      funcExpr = Ident(ident)
      argExpr = SynExpr.Paren(
        expr = SynExpr.InterpolatedString(contents = parts; synStringKind = stringKind; range = range)))
  // method call with interpolated string parameter - c.M($"<div>{1 + 1}")
  | SynExpr.App(
    funcExpr = Ident(ident)
    argExpr = SynExpr.InterpolatedString(contents = parts; synStringKind = stringKind; range = range)) ->
    let rangesToRemove =
      discoverRangesToRemoveForInterpolatedString stringKind (Array.ofList parts)

    Some(
      [| { methodIdent = ident
           parameterRange = range
           rangesToRemove = rangesToRemove
           parameterPosition = 0 } |]
    )
  // piped method call with string parameter - "<div>" |> c.M
  // piped method call with interpolated parameter - $"<div>{1 + 1}" |> c.M
  // method call with multiple string or interpolated string parameters (this also covers the case when not all parameters of the member are strings)
  //   c.M("<div>", true) and/or c.M(true, "<div>")
  // piped method call with multiple string or interpolated string parameters (this also covers the case when not all parameters of the member are strings)
  // let binding that is a string value that has the StringSyntax attribute on it - [<StringSyntax("html")>] let html = "<div />"
  // all of the above but with literals
  | _ -> None

/// <summary></summary>
type private StringParameterFinder() =
  inherit SyntaxCollectorBase()

  let languages = ResizeArray<StringParameter>()

  override _.WalkBinding(binding) =
    match binding with
    | SynBinding(expr = IsApplicationWithStringParameters(stringParameters)) -> languages.AddRange stringParameters
    | _ -> ()


  override _.WalkSynModuleDecl(decl) =
    match decl with
    | SynModuleDecl.Expr(expr = IsApplicationWithStringParameters(stringParameters)) ->
      languages.AddRange stringParameters
    | _ -> ()

  member _.NestedLanguages = languages.ToArray()


let private findParametersForParseTree (p: ParsedInput) =
  let walker = StringParameterFinder()
  walkAst walker p
  walker.NestedLanguages

let private (|IsStringSyntax|_|) (a: FSharpAttribute) =
  match a.AttributeType.FullName with
  | "System.Diagnostics.CodeAnalysis.StringSyntaxAttribute" ->
    match a.ConstructorArguments |> Seq.tryHead with
    | Some(_ty, languageValue) -> Some(languageValue :?> string)
    | _ -> None
  | _ -> None

type NestedLanguageDocument =
  { Language: string
    Ranges: Range array }

let rangeMinusRanges (totalRange: Range) (rangesToRemove: Range array) : Range array =
  match rangesToRemove with
  | [||] -> [| totalRange |]
  | _ ->
    let mutable returnVal = ResizeArray()
    let mutable currentStart = totalRange.Start

    for r in rangesToRemove do
      if currentStart = r.Start then
        // no gaps, so just advance the current pointer
        currentStart <- r.End
      else
        returnVal.Add(Range.mkRange totalRange.FileName currentStart r.Start)
        currentStart <- r.End

    // only need to add the final range if there is a gap between where we are and the end of the string
    if currentStart <> totalRange.End then
      returnVal.Add(Range.mkRange totalRange.FileName currentStart totalRange.End)

    returnVal.ToArray()

let private parametersThatAreStringSyntax
  (parameters: StringParameter array, checkResults: FSharpCheckFileResults, text: VolatileFile)
  : NestedLanguageDocument array Async =
  async {
    let returnVal = ResizeArray()

    for p in parameters do
      logger.info (
        Log.setMessageI
          $"Checking parameter: {p.parameterRange.ToString():range} in member {p.methodIdent.ToString():methodName} of {text.FileName:filename}@{text.Version:version} -> {text.Source[p.parameterRange]:sourceText}"
      )

      let lastPart = p.methodIdent[^0]
      let endOfFinalTextToken = lastPart.idRange.End

      match text.Source.GetLine(endOfFinalTextToken) with
      | None -> ()
      | Some lineText ->

        match
          checkResults.GetSymbolUseAtLocation(
            endOfFinalTextToken.Line,
            endOfFinalTextToken.Column,
            lineText,
            p.methodIdent |> List.map (fun x -> x.idText)
          )
        with
        | None -> ()
        | Some usage ->
          logger.info (
            Log.setMessageI
              $"Found symbol use: {usage.Symbol.ToString():symbol} in member {p.methodIdent.ToString():methodName} of {text.FileName:filename}@{text.Version:version} -> {text.Source[p.parameterRange]:sourceText}"
          )

          let sym = usage.Symbol
          // todo: keep MRU map of symbols to parameters and MRU of parameters to StringSyntax status

          match sym with
          | :? FSharpMemberOrFunctionOrValue as mfv ->
            let allParameters = mfv.CurriedParameterGroups |> Seq.collect id |> Seq.toArray
            let fsharpP = allParameters[p.parameterPosition]

            logger.info (
              Log.setMessageI
                $"Found parameter: {fsharpP.ToString():symbol} with {fsharpP.Attributes.Count:attributeCount} in member {p.methodIdent.ToString():methodName} of {text.FileName:filename}@{text.Version:version} -> {text.Source[p.parameterRange]:sourceText}"
            )

            match fsharpP.Attributes |> Seq.tryPick (|IsStringSyntax|_|) with
            | Some language ->
              returnVal.Add
                { Language = language
                  Ranges = rangeMinusRanges p.parameterRange p.rangesToRemove }
            | None -> ()
          | _ -> ()

    return returnVal.ToArray()
  }

/// to find all of the nested language highlights, we're going to do the following:
/// * find all of the interpolated strings or string literals in the file that are in parameter-application positions
/// * get the method calls happening at those positions to check if that method has the StringSyntaxAttribute
/// * if so, return a) the language in the StringSyntaxAttribute, and b) the range of the interpolated string
let findNestedLanguages (tyRes: ParseAndCheckResults, text: VolatileFile) : NestedLanguageDocument array Async =
  async {
    let potentialParameters = findParametersForParseTree tyRes.GetAST

    logger.info (
      Log.setMessageI
        $"Found {potentialParameters.Length:stringParams} potential parameters in {text.FileName:filename}@{text.Version:version}"
    )

    for p in potentialParameters do
      logger.info (
        Log.setMessageI
          $"Potential parameter: {p.parameterRange.ToString():range} in member {p.methodIdent.ToString():methodName} of {text.FileName:filename}@{text.Version:version} -> {text.Source[p.parameterRange]:sourceText}"
      )

    let! actualStringSyntaxParameters = parametersThatAreStringSyntax (potentialParameters, tyRes.GetCheckResults, text)

    logger.info (
      Log.setMessageI
        $"Found {actualStringSyntaxParameters.Length:stringParams} actual parameters in {text.FileName:filename}@{text.Version:version}"
    )

    return actualStringSyntaxParameters
  }
