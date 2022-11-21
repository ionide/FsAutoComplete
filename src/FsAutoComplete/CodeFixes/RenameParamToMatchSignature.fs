module FsAutoComplete.CodeFix.RenameParamToMatchSignature

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Syntax

let title name = $"Replace with '%s{name}'"

/// codefix that renames a parameter to match its signature (specified in fsi file)
let fix (getParseResultsForFile: GetParseResultsForFile) : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "3218" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let tryGetSigName (msg: string) =
        // diag message:
        // > The argument names in the signature 'xxx' and implementation 'yyy' do not match. The argument name from the signature file will be used. This may cause problems when debugging or profiling.

        /// Match exact message
        ///
        /// Issues:
        /// * fails when sig contains `' and implementation '`
        /// * only works with english language (-> exact match in english)
        /// * exact match of message -> fails when diag message should change
        let tryGetByExactMatch (msg: string) =
          let head = "The argument names in the signature '"
          let mid = "' and implementation '"

          if msg.StartsWith head then
            match msg.IndexOf mid with
            | -1 -> None
            | i -> msg.Substring(head.Length, i - head.Length) |> Some
          else
            None

        /// Fallback to match between `'`s
        ///
        /// Fails when either sig or impl contains `'` (except at end) (`my'value`)
        let tryGetBySimpleRegex (msg: string) =
          let parts = System.Text.RegularExpressions.Regex.Match(msg, ".+'(.+)'.+'(.+)'.+")

          if parts.Success then
            parts.Groups[1].Value |> Some
          else
            None

        match tryGetByExactMatch msg with
        | Some name -> Some name
        | None -> tryGetBySimpleRegex msg

      match tryGetSigName diagnostic.Message with
      | None -> return []
      | Some sigName ->
        let sigName = sigName |> PrettyNaming.NormalizeIdentifierBackticks

        // replace usages of parameter with new name
        let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

        let fscPos = protocolPosToPos diagnostic.Range.Start
        let! (tyRes, line, _) = getParseResultsForFile fileName fscPos

        let! (_, usages) = tyRes.TryGetSymbolUseAndUsages fscPos line

        let edits =
          // usages are inside local function -> all usages in same doc
          // `usages` includes parameter position
          usages
          |> Array.map (fun usage ->
            { Range = fcsRangeToLsp usage.Range
              NewText = sigName })

        return
          [ { File = codeActionParams.TextDocument
              Title = title sigName
              Edits = edits
              Kind = FixKind.Fix
              SourceDiagnostic = Some diagnostic } ]
    })
