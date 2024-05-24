module FsAutoComplete.CodeFix.ResolveNamespace

open System
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete.LspHelpers
open FsAutoComplete
open FSharp.Compiler.Text
open FSharp.Compiler.EditorServices
open System.Text.RegularExpressions

type LineText = string

let undefinedName =
  [ "not define"
    "nedefinuje|Není definovaný|Není definované|Není definovaná|Nemáte definovaný"
    "definiert nicht|nicht.*? definiert"
    "no define|no está definido|no está definida"
    "ne définit|n'est pas défini"
    "non definisce|non è definito|non è definita"
    "定義(され|し)ていません"
    "정의(하지 않|되지 않았|되어 있지 않)습니다"
    "nie definiuje|Nie zdefiniowano|nie jest zdefiniowany"
    "não define|não está definido"
    "не определяет|не определено|не определены|не определен"
    "tanımlamıyor|tanımlı değil"
    "未.*?定义"
    "未定義" ]
  |> List.map (fun i ->
    let regex = Regex(i, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    fun (j: string) -> regex.IsMatch(j))

/// a codefix the provides suggestions for opening modules or using qualified names when an identifier is found that needs qualification
let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (getNamespaceSuggestions:
    ParseAndCheckResults
      -> FcsPos
      -> LineText
      -> Async<CoreResponse<string * list<string * string * InsertionContext * bool> * list<string * string>>>)
  =

  /// insert a line of text at a given line
  let insertLine line lineStr =
    { Range =
        { Start = { Line = line; Character = 0u }
          End = { Line = line; Character = 0u } }
      NewText = lineStr }

  let adjustInsertionPoint (lines: ISourceText) (ctx: InsertionContext) : uint32 =
    let l = uint32 ctx.Pos.Line

    let retVal =
      match ctx.ScopeKind with
      | ScopeKind.TopModule when l > 1u ->
        let line = lines.GetLineString(int (l - 2u))

        let isImplicitTopLevelModule =
          not (
            line.StartsWith("module", StringComparison.Ordinal)
            && not (line.EndsWith("=", StringComparison.Ordinal))
          )

        if isImplicitTopLevelModule then 1u else l
      | ScopeKind.TopModule -> 1u
      | ScopeKind.Namespace ->
        let mostRecentNamespaceInScope =
          let lineNos = if l = 0u then [] else [ 0u .. l - 1u ]

          lineNos
          |> List.mapi (fun i line -> uint32 i, lines.GetLineString(int line))
          |> List.choose (fun (i, lineStr) ->
            if lineStr.StartsWith("namespace", StringComparison.Ordinal) then
              Some i
            else
              None)
          |> List.tryLast

        match mostRecentNamespaceInScope with
        // move to the next line below "namespace" and convert it to F# 1-based line number
        | Some line -> line + 2u
        | None -> l
      | _ -> l

    let containsAttribute (x: string) = x.Contains "[<"
    let currentLine = max (retVal - 2u) 0u |> int |> lines.GetLineString

    if currentLine |> containsAttribute then
      retVal + 1u
    else
      retVal

  let qualifierFix file diagnostic qual =
    { SourceDiagnostic = Some diagnostic
      Edits =
        [| { Range = diagnostic.Range
             NewText = qual } |]
      File = file
      Title = $"Use %s{qual}"
      Kind = FixKind.Fix }

  let openFix (text: ISourceText) file diagnostic (word: string) (ns, name: string, ctx, _multiple) : Fix =
    let insertPoint = adjustInsertionPoint text ctx
    let docLine = insertPoint - 1u

    let actualOpen =
      if name.EndsWith(word, StringComparison.Ordinal) && name <> word then
        let prefix = name.Substring(0, name.Length - word.Length).TrimEnd('.')

        $"%s{ns}.%s{prefix}"
      else
        ns

    let lineStr =
      let whitespace =
        let column =
          // HACK: This is a work around for inheriting the correct column of the current module
          // It seems the column we get from FCS is incorrect
          let previousLine = docLine - 1u
          let insertionPointIsNotOutOfBoundsOfTheFile = docLine > 0u

          let theThereAreOtherOpensInThisModule () = text.GetLineString(int previousLine).Contains "open "

          if insertionPointIsNotOutOfBoundsOfTheFile && theThereAreOtherOpensInThisModule () then
            text.GetLineString(int previousLine).Split("open") |> Seq.head |> Seq.length // inherit the previous opens whitespace
          else
            ctx.Pos.Column

        String.replicate column " "

      $"%s{whitespace}open %s{actualOpen}\n"

    let edits = [| yield insertLine docLine lineStr |]

    { Edits = edits
      File = file
      SourceDiagnostic = Some diagnostic
      Title = $"open %s{actualOpen}"
      Kind = FixKind.Fix }

  Run.ifDiagnosticByCheckMessage undefinedName (fun diagnostic codeActionParameter ->
    asyncResult {
      let pos = protocolPosToPos diagnostic.Range.Start

      let filePath = codeActionParameter.TextDocument.GetFilePath() |> Utils.normalizePath

      let! tyRes, line, lines = getParseResultsForFile filePath pos

      match! getNamespaceSuggestions tyRes pos line with
      | CoreResponse.InfoRes _msg
      | CoreResponse.ErrorRes _msg -> return []
      | CoreResponse.Res(word, opens, qualifiers) ->
        let quals =
          qualifiers
          |> List.map (fun (_, qual) -> qualifierFix codeActionParameter.TextDocument diagnostic qual)

        let ops =
          opens
          |> List.map (openFix lines codeActionParameter.TextDocument diagnostic word)

        return [ yield! ops; yield! quals ]
    })
