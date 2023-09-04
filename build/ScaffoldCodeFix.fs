module ScaffoldCodeFix

open System
open System.IO
open Fake.Core
open Fake.IO.FileSystemOperators
open Fantomas.Core.SyntaxOak

let repositoryRoot = __SOURCE_DIRECTORY__ </> ".."

let mkCodeFixImplementation codeFixName =
  let path =
    repositoryRoot
    </> "src"
    </> "FsAutoComplete"
    </> "CodeFixes"
    </> $"{codeFixName}.fs"

  let content =
    $"""module FsAutoComplete.CodeFix.%s{codeFixName}

open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FsToolkit.ErrorHandling
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete.CodeFix.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

// The syntax tree can be an intimidating set of types to work with.
// It is a tree structure but it consists out of many different types.
// See https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax.html
// It can be useful to inspect a syntax tree via a code sample using https://fsprojects.github.io/fantomas-tools/#/ast
// For example `let a b c = ()` in
// https://fsprojects.github.io/fantomas-tools/#/ast?data=N4KABGBEAmCmBmBLAdrAzpAXFSAacUiaAYmolmPAIYA2as%%2BEkAxgPZwWQ2wAuYVYAEZhmYALxgAFAEo8BSLAAeAByrJoFHgCcArrBABfIA
// Let's say we want to find the (FCS) range for identifier `a`.
let visitSyntaxTree
  (cursor: FSharp.Compiler.Text.pos)
  (tree: ParsedInput)
  =
  // We will use a syntax visitor to traverse the tree from the top to the node of interest.
  // See https://github.com/dotnet/fsharp/blob/main/src/Compiler/Service/ServiceParseTreeWalk.fsi
  // We implement the different members of interest and allow the default traversal to move to the lower levels we care about.
  let visitor =
    // A visitor will report the first item it finds.
    // Think of it as `List.tryPick`
    // It is not an ideal solution to find all nodes inside a tree, be aware of that.
    // For example finding all function names.
    {{ new SyntaxVisitorBase<FSharp.Compiler.Text.range>() with
        // We know that `a` will be part of a `SynPat.LongIdent`
        // This was visible in the online tool.
        member _.VisitPat(path, defaultTraverse, synPat) =
          match synPat with
          | SynPat.LongIdent(longDotId = SynLongIdent(id = [ functionNameIdent ])) ->
            // When our code fix operates on the user's code there is no way of knowing what will be inside the syntax tree.
            // So we need to be careful and verify that the pattern is indeed matching the position of the cursor.
            if FSharp.Compiler.Text.Range.rangeContainsPos functionNameIdent.idRange cursor then
              Some functionNameIdent.idRange
            else
              None
          | _ -> None }}

  // Invoke the visitor and kick off the traversal.
  SyntaxTraversal.Traverse(cursor, tree, visitor)

// TODO: add proper title for code fix
let title = "%s{codeFixName} Codefix"

let fix
  (getParseResultsForFile: GetParseResultsForFile)
  : CodeFix =
  fun (codeActionParams: CodeActionParams) ->
    asyncResult {{
      // Most code fixes have some general setup.
      // We initially want to detect the state of the current code and whether we can propose any text edits to the user.

      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      // The converted LSP start position to an FCS start position.
      let fcsPos = protocolPosToPos codeActionParams.Range.Start
      // The syntax tree and typed tree, current line and sourceText of the current file.
      let! (parseAndCheckResults:ParseAndCheckResults, line:string, sourceText:IFSACSourceText) =
        getParseResultsForFile fileName fcsPos

      // As an example, we want to check whether the users cursor is inside a function definition name.
      // We will traverse the syntax tree to verify this is the case.
      match visitSyntaxTree fcsPos parseAndCheckResults.GetParseResults.ParseTree with
      | None ->
        // The cursor is not in a position we are interested in.
        // This code fix should not trigger any suggestions so we return an empty list.
        return []
      | Some mBindingName ->
        // It turns out we are inside a let binding and we have the range of the function name.
        // Just for fun, we want to detect if there is a matching typed tree symbol present for the current name.
        // We could have passed the function name from the syntax visitor, instead will we grab it from the source text.
        let! functionName = sourceText.GetText mBindingName
        // FSharpSymbolUse is reflecting the typed tree.
        // See https://fsharp.github.io/fsharp-compiler-docs/fcs/symbols.html
        let symbolUse: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse option =
          parseAndCheckResults.GetCheckResults.GetSymbolUseAtLocation(mBindingName.EndLine, mBindingName.EndColumn, line, [ functionName ])

        let hasFunctionDefinitionSymbol =
          match symbolUse with
          | None -> false
          | Some symbolUse ->
            // We want to verify the found symbol is indeed a definition of a function
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue -> true
            | _ -> false

        if not hasFunctionDefinitionSymbol then
          return []
        else
          // Return a list of Fix records for when the code fix is applicable.
          return [
            {{
                  SourceDiagnostic = None
                  Title = title
                  File = codeActionParams.TextDocument
                  // Based on conditional logic, you typically want to suggest a text edit to the user.
                  Edits = [|
                    {{
                      // When dealing with FCS, we typically want to use the FCS flavour of range.
                      // However, to interact correctly with the LSP protocol, we need to return an LSP range.
                      Range = fcsRangeToLsp mBindingName
                      NewText = "Text replaced by %s{codeFixName}"
                     }}
                  |]
                  Kind = FixKind.Fix
            }}
          ]
    }}
"""

  File.WriteAllText(path, content)
  Trace.tracefn $"Generated %s{Path.GetRelativePath(repositoryRoot, path)}"

let mkCodeFixSignature codeFixName =
  let path =
    repositoryRoot
    </> "src"
    </> "FsAutoComplete"
    </> "CodeFixes"
    </> $"{codeFixName}.fsi"

  let content =
    $"""module FsAutoComplete.CodeFix.%s{codeFixName}

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
"""

  File.WriteAllText(path, content)
  Trace.tracefn $"Generated %s{Path.GetRelativePath(repositoryRoot, path)}"

let updateProjectFiles () =
  let fsAutoCompleteProject =
    repositoryRoot </> "src" </> "FsAutoComplete" </> "FsAutoComplete.fsproj"

  File.SetLastWriteTime(fsAutoCompleteProject, DateTime.Now)

  let fsAutoCompleteTestsLsp =
    repositoryRoot
    </> "test"
    </> "FsAutoComplete.Tests.Lsp"
    </> "FsAutoComplete.Tests.Lsp.fsproj"

  File.SetLastWriteTime(fsAutoCompleteTestsLsp, DateTime.Now)

let (|IdentName|_|) (name: string) (identListNode: IdentListNode) =
  match identListNode.Content with
  | [ IdentifierOrDot.Ident stn ] when stn.Text = name -> Some()
  | _ -> None

let getOakFor path =
  let content = File.ReadAllText path

  Fantomas.Core.CodeFormatter.ParseOakAsync(false, content)
  |> Async.RunSynchronously
  |> Array.head
  |> fst

let appendItemToArray codeFixName path (array: ExprArrayOrListNode) =
  let lastElement = array.Elements |> List.last |> Expr.Node
  let startIndent = lastElement.Range.StartColumn
  let lineIdx = lastElement.Range.EndLine - 1
  let arrayEndsOnLastElement = array.Range.EndLine = lastElement.Range.EndLine

  let updatedLines =
    let lines = File.ReadAllLines path
    let currentLastLine = lines.[lineIdx]
    let spaces = String.replicate startIndent " "

    if arrayEndsOnLastElement then
      let endOfLastElement = currentLastLine.Substring(0, lastElement.Range.EndColumn)
      let endOfArray = currentLastLine.Substring(lastElement.Range.EndColumn)

      lines
      |> Array.updateAt
        lineIdx
        $"{endOfLastElement}\n%s{spaces}%s{codeFixName}.fix tryGetParseResultsForFile%s{endOfArray}"
    else
      lines
      |> Array.insertAt (lineIdx + 1) $"%s{spaces}%s{codeFixName}.fix tryGetParseResultsForFile"

  File.WriteAllLines(path, updatedLines)

let wireCodeFixInAdaptiveFSharpLspServer codeFixName =
  let path =
    repositoryRoot
    </> "src"
    </> "FsAutoComplete"
    </> "LspServers"
    </> "AdaptiveFSharpLspServer.fs"

  try
    let oak = getOakFor path

    // namespace FsAutoComplete.Lsp
    let ns = oak.ModulesOrNamespaces |> List.exactlyOne

    // type AdaptiveFSharpLspServer
    let t =
      ns.Declarations
      |> List.pick (function
        | ModuleDecl.TypeDefn t ->
          let tdn = TypeDefn.TypeDefnNode t

          match tdn.TypeName.Identifier with
          | IdentName "AdaptiveFSharpLspServer" -> Some tdn
          | _ -> None
        | _ -> None)

    // let codefixes =
    let codefixesValue =
      t.Members
      |> List.pick (function
        | MemberDefn.LetBinding bindingList ->
          match bindingList.Bindings with
          | bindings ->
            bindings
            |> List.tryPick (fun binding ->
              match binding.FunctionName with
              | Choice1Of2(IdentName "codefixes") -> Some binding
              | _ -> None)
        | _ -> None)

    let infixApp =
      match codefixesValue.Expr with
      | Expr.CompExprBody body ->
        match List.last body.Statements with
        | ComputationExpressionStatement.OtherStatement other ->
          match other with
          | Expr.InfixApp infixApp -> infixApp
          | _ -> raise (exn "Expected |> infix operator")
        | _ -> raise (exn "Expected |> infix operator")
      | _ -> raise (exn "Expected |> infix operator")

    let appWithLambda =
      match infixApp.RightHandSide with
      | Expr.AppWithLambda appWithLambda -> appWithLambda
      | _ -> raise (exn "Expected function with lambda")

    let lambda =
      match appWithLambda.Lambda with
      | Choice1Of2 lambda -> lambda
      | Choice2Of2 _ -> raise (exn "Expected lambda")

    let array =
      match lambda.Expr with
      | Expr.ArrayOrList array -> array
      | _ -> raise (exn "Expected array")

    appendItemToArray codeFixName path array
  with ex ->
    Trace.traceException ex
    Trace.traceError $"Unable to find array of codefixes in %s{path}.\nDid the code structure change?"

let wireCodeFixInFsAutoCompleteLsp codeFixName =
  let path =
    repositoryRoot
    </> "src"
    </> "FsAutoComplete"
    </> "LspServers"
    </> "FsAutoComplete.Lsp.fs"

  try
    let oak = getOakFor path
    // namespace FsAutoComplete.Lsp
    let ns = oak.ModulesOrNamespaces |> List.exactlyOne

    // type AdaptiveFSharpLspServer
    let t =
      ns.Declarations
      |> List.pick (function
        | ModuleDecl.TypeDefn t ->
          let tdn = TypeDefn.TypeDefnNode t

          match tdn.TypeName.Identifier with
          | IdentName "FSharpLspServer" -> Some tdn
          | _ -> None
        | _ -> None)

    // interface IFSharpLspServer with
    let iFSharpLspServer =
      t.Members
      |> List.pick (function
        | MemberDefn.Interface i -> Some i
        | _ -> None)

    // override _.Initialize(p: InitializeParams) =
    let overrideMember =
      iFSharpLspServer.Members
      |> List.pick (function
        | MemberDefn.Member mb ->
          match mb.FunctionName with
          | Choice1Of2 iln ->
            match iln.Content with
            | [ _; _; IdentifierOrDot.Ident ident ] when ident.Text = "Initialize" -> Some mb
            | _ -> None
          | Choice2Of2 _ -> None
        | _ -> None)

    let asyncComp =
      match overrideMember.Expr with
      | Expr.NamedComputation namedComputation -> namedComputation
      | e -> failwithf "Expected Expr.NamedComputation, got %A" e

    let compBody =
      match asyncComp.Body with
      | Expr.CompExprBody body -> body
      | e -> failwithf "Expected Expr.CompExprBody, got %A" e

    let array =
      compBody.Statements
      |> List.pick (function
        | ComputationExpressionStatement.OtherStatement(Expr.LongIdentSet longIdentSet) ->
          match longIdentSet.Identifier with
          | IdentName "codefixes" ->
            match longIdentSet.Expr with
            | Expr.ArrayOrList array -> Some array
            | _ -> None
          | _ -> None
        | _ -> None)

    appendItemToArray codeFixName path array
  with ex ->
    Trace.traceException ex
    Trace.traceError $"Unable to find array of codefixes in %s{path}.\nDid the code structure change?"

let scaffold (codeFixName: string) : unit =
  // generate files in src/CodeFixes/
  mkCodeFixImplementation codeFixName
  mkCodeFixSignature codeFixName

  // Wire up codefix to LSP servers
  wireCodeFixInAdaptiveFSharpLspServer codeFixName
  wireCodeFixInFsAutoCompleteLsp codeFixName

  // Add test file in test/FsAutoComplete.Tests.Lsp/CodeFixTests

  // Wire up tests in test/FsAutoComplete.Tests.Lsp/CodeFixTests/Tests.fs

  updateProjectFiles ()
  Trace.tracefn $"Scaffolding %s{codeFixName} complete!"

// TODO: introduce a target that verifies the codefix can still be added.
// By checking the AST can still reach the entry points of the lists.
