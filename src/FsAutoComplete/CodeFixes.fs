/// This module contains the logic for codefixes that FSAC surfaces, as well as conversion logic between
/// compiler diagnostics and LSP diagnostics/code actions
module FsAutoComplete.CodeFix

open FsAutoComplete.LspHelpers
open LanguageServerProtocol.Types
open FsAutoComplete.Utils
open System.IO

type FcsRange = FSharp.Compiler.SourceCodeServices.Range

module LspTypes = LanguageServerProtocol.Types

module Types =
  type IsEnabled = unit -> bool

  type Fix =
    { Edits: TextEdit []
      File: TextDocumentIdentifier
      Title: string
      SourceDiagnostic: Diagnostic option }

  type CodeFix = CodeActionParams -> Async<Fix list>

  type CodeAction with
    static member OfFix getFileVersion clientCapabilities (fix: Fix) =
      let filePath = fix.File.GetFilePath()
      let fileVersion = getFileVersion filePath

      CodeAction.OfDiagnostic fix.File fileVersion fix.Title fix.SourceDiagnostic fix.Edits clientCapabilities

    static member OfDiagnostic (fileUri) (fileVersion) title (diagnostic) (edits) clientCapabilities: CodeAction =

      let edit =
        { TextDocument =
            { Uri = fileUri.Uri
              Version = fileVersion }
          Edits = edits }

      let workspaceEdit =
        WorkspaceEdit.Create([| edit |], clientCapabilities)

      { CodeAction.Title = title
        Kind = Some "quickfix"
        Diagnostics = diagnostic |> Option.map Array.singleton
        Edit = workspaceEdit
        Command = None }

module Util =

  let findPosForCharacter (lines: string []) (pos: int) =
    let mutable lineNumber = 0
    let mutable runningLength = 0
    let mutable found = false

    let mutable fcsPos =
      Unchecked.defaultof<FSharp.Compiler.Range.pos>

    while not found do
      let line = lines.[lineNumber]
      let lineLength = line.Length

      if pos <= runningLength + lineLength then
        let column = pos - runningLength
        found <- true
        fcsPos <- FSharp.Compiler.Range.mkPos lineNumber column
      else
        lineNumber <- lineNumber + 1
        runningLength <- runningLength + lineLength

    fcsPos

  /// advance along positions from a starting location, incrementing in a known way until a condition is met.
  /// when the condition is met, return that position.
  /// if the condition is never met, return None
  let walkPos (lines: string []) (pos: LspTypes.Position) posChange condition: LspTypes.Position option =
    let charAt (pos: LspTypes.Position) = lines.[pos.Line].[pos.Character]

    let firstPos = { Line = 0; Character = 0 }

    let finalPos =
      { Line = lines.Length - 1
        Character = lines.[lines.Length - 1].Length - 1 }

    let rec loop pos =
      if firstPos = pos || finalPos = pos then None
      else if not (condition (charAt pos)) then loop (posChange pos)
      else Some pos

    loop pos

  let inc (lines: string []) (pos: LspTypes.Position): LspTypes.Position =
    let lineLength = lines.[pos.Line].Length

    if pos.Character = lineLength - 1 then
      { Line = pos.Line + 1; Character = 0 }
    else
      { pos with
          Character = pos.Character + 1 }

  let dec (lines: string []) (pos: LspTypes.Position): LspTypes.Position =
    if pos.Character = 0 then
      let newLine = pos.Line - 1
      // decrement to end of previous line
      { pos with
          Line = newLine
          Character = lines.[newLine].Length - 1 }
    else
      { pos with
          Character = pos.Character - 1 }

  let walkBackUntilCondition (lines: string []) (pos: LspTypes.Position) condition =
    walkPos lines pos (dec lines) condition

  let walkForwardUntilCondition (lines: string []) (pos: LspTypes.Position) condition =
    walkPos lines pos (inc lines) condition

open Types

let ifEnabled enabled codeFix: CodeFix =
  fun codeActionParams -> if enabled () then codeFix codeActionParams else async.Return []

let ifDiagnosticByMessage handler (checkMessage: string) =
  (fun codeActionParams ->
    match codeActionParams.Context.Diagnostics
          |> Array.tryFind (fun n -> n.Message.Contains checkMessage) with
    | None -> async.Return []
    | Some d -> handler d codeActionParams)

let ifDiagnosticByType handler (diagnosticType: string) =
  (fun codeActionParams ->
    match codeActionParams.Context.Diagnostics
          |> Seq.tryFind (fun n -> n.Source.Contains diagnosticType) with
    | None -> async.Return []
    | Some d -> handler d codeActionParams)

let ifDiagnosticByCode handler codes: CodeFix =
  fun codeActionParams ->
    match codeActionParams.Context.Diagnostics
          |> Seq.tryFind
               (fun d ->
                 match d.Code with
                 | None -> false
                 | Some code -> Set.contains code codes) with
    | None -> async.Return []
    | Some d -> handler d codeActionParams

module Fixes =
  open FSharp.Compiler.SourceCodeServices
  open FsToolkit.ErrorHandling
  open Util
  open FsAutoComplete.FCSPatches

  /// insert a line of text at a given line
  let private insertLine line lineStr =
    { Range =
        { Start = { Line = line; Character = 0 }
          End = { Line = line; Character = 0 } }
      NewText = lineStr }

  let private adjustInsertionPoint (lines: string []) (ctx: InsertContext) =
    let l = ctx.Pos.Line

    match ctx.ScopeKind with
    | TopModule when l > 1 ->
        let line = lines.[l - 2]

        let isImplicitTopLevelModule =
          not
            (line.StartsWith "module"
             && not (line.EndsWith "="))

        if isImplicitTopLevelModule then 1 else l
    | TopModule -> 1
    | ScopeKind.Namespace when l > 1 ->
        [ 0 .. l - 1 ]
        |> List.mapi (fun i line -> i, lines.[line])
        |> List.tryPick (fun (i, lineStr) -> if lineStr.StartsWith "namespace" then Some i else None)
        |> function
        // move to the next line below "namespace" and convert it to F# 1-based line number
        | Some line -> line + 2
        | None -> l
    | ScopeKind.Namespace -> 1
    | _ -> l

  /// a codefix that removes unused open statements from the source
  let unusedOpens =
    ifDiagnosticByMessage
      (fun d codeActionParams ->
        let range =
          { Start =
              { Line = d.Range.Start.Line - 1
                Character = 1000 }
            End =
              { Line = d.Range.End.Line
                Character = d.Range.End.Character } }

        let fix =
          { Edits = [| { Range = range; NewText = "" } |]
            File = codeActionParams.TextDocument
            Title = "Remove unused open"
            SourceDiagnostic = Some d }

        async.Return [ fix ])
      "Unused open statement"


  /// a codefix the provides suggestions for opening modules or using qualified names when an identifier is found that needs qualification
  let resolveNamespace getParseResultsForFile getNamespaceSuggestions =
    let qualifierFix file diagnostic qual =
      { SourceDiagnostic = Some diagnostic
        Edits =
          [| { Range = diagnostic.Range
               NewText = qual } |]
        File = file
        Title = $"Use %s{qual}" }

    let openFix fileLines file diagnostic (word: string) (ns, name: string, ctx, multiple): Fix =
      let insertPoint = adjustInsertionPoint fileLines ctx
      let docLine = insertPoint - 1

      let actualOpen =
        if name.EndsWith word && name <> word then
          let prefix =
            name
              .Substring(0, name.Length - word.Length)
              .TrimEnd('.')

          $"%s{ns}.%s{prefix}"
        else
          ns



      let lineStr =
        let whitespace = String.replicate ctx.Pos.Column " "
        $"%s{whitespace}open %s{actualOpen}\n"

      let edits =
        [| yield insertLine docLine lineStr
           if fileLines.[docLine + 1].Trim() <> "" then yield insertLine (docLine + 1) ""
           if (ctx.Pos.Column = 0 || ctx.ScopeKind = Namespace)
              && docLine > 0
              && not ((fileLines.[docLine - 1]).StartsWith "open") then
             yield insertLine (docLine - 1) "" |]

      { Edits = edits
        File = file
        SourceDiagnostic = Some diagnostic
        Title = $"open %s{actualOpen}" }

    ifDiagnosticByMessage
      (fun diagnostic codeActionParameter ->
        async {
          let pos = protocolPosToPos diagnostic.Range.Start

          let filePath =
            codeActionParameter.TextDocument.GetFilePath()
          match! getParseResultsForFile filePath pos with
          | Ok (tyRes, line, lines) ->
              match! getNamespaceSuggestions tyRes pos line with
              | CoreResponse.InfoRes msg
              | CoreResponse.ErrorRes msg -> return []
              | CoreResponse.Res (word, opens, qualifiers) ->
                  let quals =
                    qualifiers
                    |> List.map (fun (_, qual) -> qualifierFix codeActionParameter.TextDocument diagnostic qual)

                  let ops =
                    opens
                    |> List.map (openFix lines codeActionParameter.TextDocument diagnostic word)

                  return [ yield! ops; yield! quals ]

          | Error _ -> return []
        })
      "is not defined"

  /// a codefix that replaces the use of an unknown identifier with a suggested identitfier
  let errorSuggestion =
    ifDiagnosticByMessage
      (fun diagnostic codeActionParams ->
        diagnostic.Message.Split('\n').[1..]
        |> Array.map
             (fun suggestion ->
               let suggestion = suggestion.Trim()

               let suggestion =
                 if System.Text.RegularExpressions.Regex.IsMatch(suggestion, """^[a-zA-Z][a-zA-Z0-9']+$""")
                 then suggestion
                 else $"``%s{suggestion}``"

               { Edits =
                   [| { Range = diagnostic.Range
                        NewText = suggestion } |]
                 Title = $"Replace with %s{suggestion}"
                 File = codeActionParams.TextDocument
                 SourceDiagnostic = Some diagnostic })
        |> Array.toList
        |> async.Return)
      "Maybe you want one of the following:"

  /// a codefix that removes unnecessary qualifiers from an identifier
  let redundantQualifier =
    ifDiagnosticByMessage
      (fun diagnostic codeActionParams ->
        async.Return [ { Edits =
                           [| { Range = diagnostic.Range
                                NewText = "" } |]
                         File = codeActionParams.TextDocument
                         Title = "Remove redundant qualifier"
                         SourceDiagnostic = Some diagnostic } ])
      "This qualifier is redundant"

  /// a codefix that suggests prepending a _ to unused values
  let unusedValue getFileLines =
    ifDiagnosticByMessage
      (fun diagnostic codeActionParams ->
        async {
          match getFileLines (codeActionParams.TextDocument.GetFilePath()) with
          | Ok lines ->
              match diagnostic.Code with
              | Some _ ->
                  return
                    [ { SourceDiagnostic = Some diagnostic
                        File = codeActionParams.TextDocument
                        Title = "Replace with __"
                        Edits =
                          [| { Range = diagnostic.Range
                               NewText = "__" } |] } ]
              | None ->
                  let replaceSuggestion = "_"
                  let prefixSuggestion = $"_{getText lines diagnostic.Range}"

                  return
                    [ { SourceDiagnostic = Some diagnostic
                        File = codeActionParams.TextDocument
                        Title = "Replace with _"
                        Edits =
                          [| { Range = diagnostic.Range
                               NewText = replaceSuggestion } |] }
                      { SourceDiagnostic = Some diagnostic
                        File = codeActionParams.TextDocument
                        Title = "Prefix with _"
                        Edits =
                          [| { Range = diagnostic.Range
                               NewText = prefixSuggestion } |] } ]
          | Error _ -> return []
        })
      "is unused"

  /// a codefix that suggestes using the 'new' keyword on IDisposables
  let newWithDisposables getFileLines =
    ifDiagnosticByMessage
      (fun diagnostic codeActionParams ->
        match getFileLines (codeActionParams.TextDocument.GetFilePath()) with
        | Ok lines ->
            async.Return [ { SourceDiagnostic = Some diagnostic
                             File = codeActionParams.TextDocument
                             Title = "Add new"
                             Edits =
                               [| { Range = diagnostic.Range
                                    NewText = $"new {getText lines diagnostic.Range}" } |] } ]
        | Error _ -> async.Return [])
      "It is recommended that objects supporting the IDisposable interface are created using the syntax"

  /// a codefix that generates union cases for an incomplete match expression
  let generateUnionCases (getFileLines: string -> Result<string [], _>)
                         (getParseResultsForFile: string -> FSharp.Compiler.Range.pos -> Async<Result<ParseAndCheckResults * string * string array, 'e>>)
                         (generateCases: _ -> _ -> _ -> _ -> Async<CoreResponse<_>>)
                         (getTextReplacements: unit -> Map<string, string>)
                         =
    ifDiagnosticByMessage
      (fun diagnostic codeActionParams ->
        asyncResult {
          let fileName =
            codeActionParams.TextDocument.GetFilePath()

          let! (lines: string []) = getFileLines fileName
          let caseLine = diagnostic.Range.Start.Line + 1
          let col = lines.[caseLine].IndexOf('|') + 3 // Find column of first case in patern matching

          let pos =
            FSharp.Compiler.Range.mkPos (caseLine + 1) (col + 1) //Must points on first case in 1-based system

          let! (tyRes, line, lines) = getParseResultsForFile fileName pos

          match! generateCases tyRes pos lines line |> Async.map Ok with
          | CoreResponse.Res (insertString: string, insertPosition) ->
              let range =
                { Start = fcsPosToLsp insertPosition
                  End = fcsPosToLsp insertPosition }

              let replacements = getTextReplacements ()

              let replaced =
                (insertString, replacements)
                ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))
              // x.CreateFix p.TextDocument.Uri fn "Generate union pattern match case" (Some d) range text
              return
                [ { SourceDiagnostic = Some diagnostic
                    File = codeActionParams.TextDocument
                    Title = "Generate union pattern match cases"
                    Edits = [| { Range = range; NewText = replaced } |] } ]

          | _ -> return []
        }
        |> AsyncResult.foldResult id (fun _ -> []))
      "Incomplete pattern matches on this expression. For example"

  let private mapExternalDiagnostic diagnosticType getFixesForFile =
    ifDiagnosticByType
      (fun diagnostic codeActionParams ->
        let fileName =
          codeActionParams.TextDocument.GetFilePath()

        let normalizedUri = Path.FilePathToUri fileName

        match getFixesForFile normalizedUri
              |> Option.bind
                   (fun fixes ->
                     fixes
                     |> List.tryFind (fun (range, textEdit) -> range = diagnostic.Range)) with
        | Some (range, textEdit) ->
            async.Return [ { SourceDiagnostic = Some diagnostic
                             File = codeActionParams.TextDocument
                             Title = $"Replace with %s{textEdit.NewText}"
                             Edits = [| textEdit |] } ]
        | None -> async.Return [])
      diagnosticType

  /// a codefix that generates fixes reported by FSharpLint
  let mapLinterDiagnostics = mapExternalDiagnostic "F# Linter"

  /// a codefix that generates fixes reported by F# Analyzers
  let mapAnalyzerDiagnostics = mapExternalDiagnostic "F# Analyzers"

  /// a codefix that generates member stubs for an interface declaration
  let generateInterfaceStub (getFileLines: string -> Result<string [], _>)
                            (getParseResultsForFile: string -> FSharp.Compiler.Range.pos -> Async<Result<ParseAndCheckResults * string * string array, 'e>>)
                            (genInterfaceStub: _ -> _ -> _ -> _ -> Async<CoreResponse<string * FSharp.Compiler.Range.pos>>)
                            (getTextReplacements: unit -> Map<string, string>)
                            : CodeFix =
    fun codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath()

        let! (lines: string []) = getFileLines fileName

        let pos =
          protocolPosToPos codeActionParams.Range.Start

        let! (tyRes, line, lines) = getParseResultsForFile fileName pos

        match! genInterfaceStub tyRes pos lines line with
        | CoreResponse.Res (text, position) ->
            let replacements = getTextReplacements ()

            let replaced =
              (text, replacements)
              ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))

            return
              [ { SourceDiagnostic = None
                  Title = "Generate interface stub"
                  File = codeActionParams.TextDocument
                  Edits =
                    [| { Range = fcsPosToProtocolRange pos
                         NewText = replaced } |] } ]
        | _ -> return []

      }
      |> AsyncResult.foldResult id (fun _ -> [])

  /// a codefix that generates member stubs for a record declaration
  let generateRecordStub (getFileLines: string -> Result<string [], _>)
                         (getParseResultsForFile: string -> FSharp.Compiler.Range.pos -> Async<Result<ParseAndCheckResults * string * string array, 'e>>)
                         (genRecordStub: _ -> _ -> _ -> _ -> Async<CoreResponse<string * FSharp.Compiler.Range.pos>>)
                         (getTextReplacements: unit -> Map<string, string>)
                         : CodeFix =
    fun codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath()

        let! (lines: string []) = getFileLines fileName

        let pos =
          protocolPosToPos codeActionParams.Range.Start

        let! (tyRes, line, lines) = getParseResultsForFile fileName pos

        match! genRecordStub tyRes pos lines line with
        | CoreResponse.Res (text, position) ->
            let replacements = getTextReplacements ()

            let replaced =
              (text, replacements)
              ||> Seq.fold (fun text (KeyValue (key, replacement)) -> text.Replace(key, replacement))

            return
              [ { SourceDiagnostic = None
                  Title = "Generate record stub"
                  File = codeActionParams.TextDocument
                  Edits =
                    [| { Range = fcsPosToProtocolRange pos
                         NewText = replaced } |] } ]
        | _ -> return []
      }
      |> AsyncResult.foldResult id (fun _ -> [])

  /// a codefix that adds in missing '=' characters in type declarations
  let addMissingEqualsToTypeDefinition (getFileLines: string -> Result<string [], _>) =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        asyncResult {
          if diagnostic.Message.Contains "Unexpected symbol '{' in type definition"
             || diagnostic.Message.Contains "Unexpected keyword 'member' in type definition" then
            let fileName =
              codeActionParams.TextDocument.GetFilePath()

            let! lines = getFileLines fileName

            match walkBackUntilCondition lines (dec lines diagnostic.Range.Start) (System.Char.IsWhiteSpace >> not) with
            | Some firstNonWhitespaceChar ->
                let insertPos = inc lines firstNonWhitespaceChar

                return
                  [ { SourceDiagnostic = Some diagnostic
                      Title = "Add missing '=' to type definition"
                      File = codeActionParams.TextDocument
                      Edits =
                        [| { Range = { Start = insertPos; End = insertPos }
                             NewText = " =" } |] } ]
            | None -> return []
          else
            return []
        }
        |> AsyncResult.foldResult id (fun _ -> []))
      (Set.ofList [ "10"; "3360" ])

  /// a codefix that corrects -<something> to - <something> when negation is not intended
  let changeNegationToSubtraction (getFileLines: string -> Result<string [], _>): CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        asyncResult {
          let fileName =
            codeActionParams.TextDocument.GetFilePath()

          let! lines = getFileLines fileName

          match walkForwardUntilCondition lines (inc lines diagnostic.Range.End) (fun ch -> ch = '-') with
          | Some dash ->
              return
                [ { SourceDiagnostic = Some diagnostic
                    Title = "Use subtraction instead of negation"
                    File = codeActionParams.TextDocument
                    Edits =
                      [| { Range = { Start = dash; End = inc lines dash }
                           NewText = "- " } |] } ]
          | None -> return []
        }
        |> AsyncResult.foldResult id (fun _ -> []))
      (Set.ofList [ "3" ])

  /// a codefix that corrects == equality to = equality
  let doubleEqualsToSingleEquality: CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        async.Return [ { Title = "Use '=' for equality check"
                         File = codeActionParams.TextDocument
                         SourceDiagnostic = Some diagnostic
                         Edits =
                           [| { Range = diagnostic.Range
                                NewText = "=" } |] } ])
      (Set.ofList [ "43" ])

  /// a codefix that fixes a malformed record type annotation to use colon instead of equals
  let addMissingColonToFieldDefinition: CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        if diagnostic.Message = "Unexpected symbol '=' in field declaration. Expected ':' or other token." then
          async.Return [ { File = codeActionParams.TextDocument
                           Title = "Use ':' for type in field declaration"
                           SourceDiagnostic = Some diagnostic
                           Edits =
                             [| { Range = diagnostic.Range
                                  NewText = ":" } |] } ]
        else
          async.Return [])
      (Set.ofList [ "10" ])

  /// a codefix that parenthesizes a membe rexpression that needs it
  let parenthesizeExpression (getFileLines: string -> Result<string [], _>): CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        match getFileLines (codeActionParams.TextDocument.GetFilePath()) with
        | Ok lines ->
            let erroringExpression = getText lines diagnostic.Range

            async.Return [ { Title = "Wrap expression in parentheses"
                             File = codeActionParams.TextDocument
                             SourceDiagnostic = Some diagnostic
                             Edits =
                               [| { Range = diagnostic.Range
                                    NewText = $"(%s{erroringExpression})" } |] } ]
        | Error _ -> async.Return [])
      (Set.ofList [ "597" ])

  /// a codefix that changes a ref cell deref (!) to a call to 'not'
  let refCellDerefToNot (getFileLines: string -> Result<string [], _>): CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        match getFileLines (codeActionParams.TextDocument.GetFilePath()) with
        | Ok lines ->
            match walkBackUntilCondition lines diagnostic.Range.Start (fun c -> c = '!') with
            | Some bangChar ->
                async.Return [ { SourceDiagnostic = Some diagnostic
                                 Title = "Use 'not' to negate expression"
                                 File = codeActionParams.TextDocument
                                 Edits =
                                   [| { Range =
                                          { Start = bangChar
                                            End = inc lines bangChar }
                                        NewText = "not " } |] } ]
            | None -> async.Return []
        | Error _ -> async.Return [])
      (Set.ofList [ "1" ])

  /// a codefix that replaces unsafe casts with safe casts
  let upcastUsage (getFileLines: string -> Result<string [], _>): CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        match getFileLines (codeActionParams.TextDocument.GetFilePath()) with
        | Ok lines ->
            let expressionText = getText lines diagnostic.Range
            let isDowncastOperator = expressionText.Contains(":?>")
            let isDowncastKeyword = expressionText.Contains("downcast")

            match isDowncastOperator, isDowncastKeyword with
            // must be either/or here, cannot be both
            | true, true -> async.Return []
            | false, false -> async.Return []
            | true, false ->
                async.Return [ { File = codeActionParams.TextDocument
                                 SourceDiagnostic = Some diagnostic
                                 Title = "Use ':>' operator"
                                 Edits =
                                   [| { Range = diagnostic.Range
                                        NewText = expressionText.Replace(":?>", ":>") } |] } ]
            | false, true ->
                async.Return [ { File = codeActionParams.TextDocument
                                 SourceDiagnostic = Some diagnostic
                                 Title = "Use 'upcast' function"
                                 Edits =
                                   [| { Range = diagnostic.Range
                                        NewText = expressionText.Replace("downcast", "upcast") } |] } ]
        | Error _ -> async.Return [])
      (Set.ofList [ "3198" ])

  /// a codefix that makes a binding mutable when a user attempts to mutably set it
  let makeDeclarationMutable (getParseResultsForFile: string -> FSharp.Compiler.Range.pos -> Async<Result<ParseAndCheckResults * string * string array, string>>)
                             (getProjectOptionsForFile: string -> Result<FSharpProjectOptions, string>)
                             : CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        asyncResult {
          let fileName =
            codeActionParams.TextDocument.GetFilePath()

          let fcsPos = protocolPosToPos diagnostic.Range.Start
          let! (tyRes, line, lines) = getParseResultsForFile fileName fcsPos
          let! opts = getProjectOptionsForFile fileName

          match Lexer.getSymbol fcsPos.Line fcsPos.Column line SymbolLookupKind.Fuzzy opts.OtherOptions with
          | Some symbol ->
              match! tyRes.TryFindDeclaration fcsPos line with
              | FindDeclarationResult.Range declRange when declRange.FileName = fileName ->
                  let lspRange = fcsRangeToLsp declRange

                  if tyRes.GetParseResults.IsPositionContainedInACurriedParameter declRange.Start then
                    return []
                  else
                    return
                      [ { File = codeActionParams.TextDocument
                          SourceDiagnostic = Some diagnostic
                          Title = "Make declaration 'mutable'"
                          Edits =
                            [| { Range =
                                   { Start = lspRange.Start
                                     End = lspRange.Start }
                                 NewText = "mutable " } |] } ]
              | _ -> return []
          | None -> return []
        }
        |> AsyncResult.foldResult id (fun _ -> []))
      (Set.ofList [ "27" ])

  /// a codefix that changes equality checking to mutable assignment when the compiler thinks it's relevant
  let comparisonToMutableAssignment
    (getParseResultsForFile: string -> FSharp.Compiler.Range.pos -> Async<Result<ParseAndCheckResults * string * string array, string>>)
    : CodeFix =
    ifDiagnosticByCode
      (fun diagnostic codeActionParams ->
        asyncResult {
          let fileName =
            codeActionParams.TextDocument.GetFilePath()

          let fcsPos = protocolPosToPos diagnostic.Range.Start
          let! (tyRes, line, lines) = getParseResultsForFile fileName fcsPos
          let! (symbol, _) = tyRes.TryGetSymbolUse fcsPos line
          match symbol.Symbol with
          // only do anything if the value is mutable
          | :? FSharpMemberOrFunctionOrValue as mfv when mfv.IsValue && mfv.IsMutable ->
            // try to find the '=' at from the start of the range
            let endOfMutableValue = fcsPosToLsp symbol.RangeAlternate.End
            match walkForwardUntilCondition lines endOfMutableValue (fun c -> c = '=') with
            | Some equalsPos ->
                return [ { File = codeActionParams.TextDocument
                           Title = "Use '<-' to mutate value"
                           SourceDiagnostic = Some diagnostic
                           Edits =
                             [| { Range =
                                    { Start = equalsPos
                                      End = (inc lines equalsPos) }
                                  NewText = "<-" } |] } ]
            | None ->
              return []
          | _ -> return []
        } |> AsyncResult.foldResult id (fun _ -> [])
        )
      (Set.ofList [ "20" ])
