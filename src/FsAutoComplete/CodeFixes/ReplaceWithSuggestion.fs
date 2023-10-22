module FsAutoComplete.CodeFix.ReplaceWithSuggestion

open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FsAutoComplete
open FSharp.Compiler.Syntax

let title suggestion = $"Replace with '%s{suggestion}'"

let undefinedName =
  [
    "Maybe you want one of the following:"
    "Možná budete potřebovat něco z tohoto:"
    "Vielleicht möchten Sie eine der folgenden Bezeichnungen verwenden:"
    "Puede elegir una de las opciones siguientes:"
    "Peut-être souhaitez-vous l'une des options suivantes :"
    "Si può specificare uno dei nomi seguenti:"
    "次のいずれかの可能性はありませんか:"
    "다음 중 하나가 필요할 수 있습니다:"
    "Możliwe, że chcesz wykonać jedną z następujących czynności:"
    "Talvez você queira um dos seguintes:"
    "Возможно, требуется одно из следующих:"
    "Aşağıdakilerden birini arıyor olabilirsiniz:"
    "你可能需要以下之一:"
    "您可能需要下列其中一項:"
  ] |> List.map (fun i -> fun (j: string) -> j.Contains(i, System.StringComparison.Ordinal))

/// a codefix that replaces the use of an unknown identifier with a suggested identifier
let fix =
  Run.ifDiagnosticByCheckMessage undefinedName (fun diagnostic codeActionParams ->
    diagnostic.Message.Split('\n', '\u001b').[1..]
    |> Array.map (fun suggestion ->
      let suggestion = suggestion.Trim() |> PrettyNaming.NormalizeIdentifierBackticks

      { Edits =
          [| { Range = diagnostic.Range
               NewText = suggestion } |]
        Title = title suggestion
        File = codeActionParams.TextDocument
        SourceDiagnostic = Some diagnostic
        Kind = FixKind.Fix })
    |> Array.toList
    |> AsyncResult.retn)
