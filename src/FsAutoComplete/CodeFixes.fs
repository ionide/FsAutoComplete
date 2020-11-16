/// This module contains the logic for codefixes that FSAC surfaces, as well as conversion logic between
/// compiler diagnostics and LSP diagnostics/code actions
module FsAutoComplete.CodeFix

open FsAutoComplete.LspHelpers
open LanguageServerProtocol.Types

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

open Types

let ifEnabled enabled codeFix: CodeFix =
  fun codeActionParams -> if enabled () then codeFix codeActionParams else async.Return []

let ifDiagnosticByMessage enabled handler (checkMessage: string) =
  ifEnabled
    enabled
    (fun codeActionParams ->
      match codeActionParams.Context.Diagnostics
            |> Array.tryFind (fun n -> n.Message.Contains checkMessage) with
      | None -> async.Return []
      | Some d -> handler d codeActionParams)

let ifDiagnosticByType enabled handler (diagnosticType: string) =
  ifEnabled
    enabled
    (fun codeActionParams ->
      match codeActionParams.Context.Diagnostics
            |> Seq.tryFind (fun n -> n.Source.Contains diagnosticType) with
      | None -> async.Return []
      | Some d -> handler d)

module Fixes =
  open FSharp.Compiler.SourceCodeServices

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
  let unusedOpens enabled =
    ifDiagnosticByMessage
      enabled
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
  let resolveNamespace enabled getParseResultsForFile getNamespaceSuggestions =
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
      enabled
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
