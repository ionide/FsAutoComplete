module FsAutoComplete.CodeFix.ImplementInterface

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// `pos` is expected to be on the leading `{` (main interface) or `interface` (additional interfaces)
/// -> `diagnostic.Range.Start`
let private tryFindInterfaceDeclarationInObjectExpression (pos: Position) (ast: ParsedInput) =
  SyntaxTraversal.Traverse(
    pos,
    ast,
    { new SyntaxVisitorBase<_>() with
        member _.VisitExpr(_, _, defaultTraverse, expr) =
          match expr with
          | SynExpr.ObjExpr (objType = ty; bindings = binds; extraImpls = ifaces) ->
            ifaces
            |> List.tryPick (fun (SynInterfaceImpl (interfaceTy = ty; bindings = binds; range = range)) ->
              if Range.rangeContainsPos range pos then
                Some(InterfaceData.ObjExpr(ty, binds))
              else
                None)
            |> Option.orElseWith (fun _ -> Some(InterfaceData.ObjExpr(ty, binds)))

          | _ -> defaultTraverse expr }
  )

/// `pos`: on corresponding interface identifier
///
/// Returns: `(start, with)`
/// `start`:
/// * start of `interface` keyword (type, secondary interface in obj expr)
/// * start of `new` keyword (primary interface in obj expr)
/// -> alignment for members
/// `with`:
/// * range of `with` keyword if exists
/// -> pos for append
let private tryFindInterfaceStartAndWith (pos: Position) (ast: ParsedInput) =
  SyntaxTraversal.Traverse(
    pos,
    ast,
    { new SyntaxVisitorBase<_>() with
        member _.VisitExpr(_, _, defaultTraverse, expr) =
          match expr with
          // main interface
          | SynExpr.ObjExpr (objType = ty; withKeyword = withRange; newExprRange = startingAtNewRange) when
            Range.rangeContainsPos ty.Range pos
            ->
            // { new IDisposable with }
            //   ^
            let start = startingAtNewRange.Start
            Some(start, withRange)
          // secondary interface
          | SynExpr.ObjExpr (extraImpls = ifaces) ->
            ifaces
            |> List.tryPick
              (fun (SynInterfaceImpl (interfaceTy = ty; withKeyword = withRange; range = startingAtInterfaceRange)) ->
                if Range.rangeContainsPos ty.Range pos then
                  //   { new IDisposable with
                  //       member this.Dispose() = ()
                  //     interface ICloneable with
                  //     ^
                  //   }
                  let start = startingAtInterfaceRange.Start
                  Some(start, withRange)
                else
                  None)
            |> Option.orElseWith (fun _ -> defaultTraverse expr)
          | _ -> defaultTraverse expr

        member _.VisitModuleDecl(_, defaultTraverse, synModuleDecl) =
          match synModuleDecl with
          | SynModuleDecl.Types (typeDefns, _) ->
            let typeDefn =
              typeDefns
              |> List.tryFind (fun typeDef -> Range.rangeContainsPos typeDef.Range pos)

            match typeDefn with
            | Some (SynTypeDefn (typeRepr = typeRepr; members = members)) ->
              let tryFindInMemberDefns (members: SynMemberDefns) =
                members
                |> List.tryPick (function
                  | SynMemberDefn.Interface (interfaceType = ty; withKeyword = withRange; range = range) when
                    Range.rangeContainsPos ty.Range pos
                    ->
                    // interface IDisposable with
                    // ^
                    let start = range.Start
                    Some(start, withRange)
                  | _ -> None)

              match typeRepr with
              | SynTypeDefnRepr.ObjectModel (members = members) ->
                // in class (-> in typeRepr)
                tryFindInMemberDefns members
              | _ -> None
              |> Option.orElseWith (fun _ ->
                // in union, records (-> in members)
                tryFindInMemberDefns members)
              |> Option.orElseWith (fun _ -> defaultTraverse synModuleDecl)
            | _ -> defaultTraverse synModuleDecl
          | _ -> defaultTraverse synModuleDecl }
  )

type private InsertionData =
  {
    /// Indentation of new members
    StartColumn: int
    /// Insert position of new members
    InsertAt: Position
    /// `true`: no existing `with`
    /// -> Insert before members at `InsertAt`
    InsertWith: bool
  // Handled elsewhere:
  //  Detected via diagnostics.Range.End & lookup in source
  // InsertClosingBracket: bool
  }

let private tryFindInsertionData (interfaceData: InterfaceData) (ast: ParsedInput) (indentationSize: int) =

  let lastExistingMember =
    match interfaceData with
    | InterfaceData.Interface (_, None) -> None
    | InterfaceData.Interface (_, Some memberDefns) ->
      memberDefns
      |> List.choose (function
        | SynMemberDefn.Member (memberDefn = binding) -> Some binding
        | _ -> None)
      |> List.tryLast
    | InterfaceData.ObjExpr (_, bindings) -> bindings |> List.tryLast

  match lastExistingMember with
  | Some (SynBinding (attributes = attributes
                      valData = SynValData (memberFlags = memberFlags)
                      headPat = headPat
                      expr = expr)) ->
    // align with existing member
    // insert after last member

    // alignment:
    // ```fsharp
    // type A () =
    //   interface IDisposable with
    //     /// hello world
    //     [<Obsolete>]
    //         member
    //           _.Dispose () = ()
    // ```
    // -> use first non-comment (-> most left)
    //
    // Note: illegal:
    // ```fsharp
    // interface IDisposable with
    //   (*foo bar*)[<Obsolete>]
    //     member
    //       _.Dispose () = ()
    //
    // interface IDisposable with
    //   (*foo bar*)member
    //     _.Dispose () = ()
    // ```
    // -> first non-comment is indentation required by F#
    // But valid:
    // ```fsharp
    // interface IDisposable with
    //     (*foo bar*)member
    //         (*baaaaz*)_.Dispose () = ()
    //
    // interface IDisposable with
    //     (*foo bar*)member
    //   (*baaaaaaaaaz*)_.Dispose () = ()
    // ```
    // (`_` (this) behind `member`)

    let startCol =
      // attribute must be first
      attributes
      |> List.tryHead
      |> Option.map (fun attr -> attr.Range.StartColumn)
      |> Option.orElseWith (fun _ ->
        // leftmost `member` or `override` (and just to be sure: `default`, `abstract` or `static`)
        match memberFlags with
        | Some memberFlags ->
          let trivia = memberFlags.Trivia

          [ trivia.StaticRange
            trivia.MemberRange
            trivia.OverrideRange
            trivia.AbstractRange
            trivia.DefaultRange ]
          |> List.choose id
          |> List.map (fun r -> r.StartColumn)
          // List.tryMin
          |> List.fold
               (fun m c ->
                 match m with
                 | None -> Some c
                 | Some m -> min c m |> Some)
               None
        | None -> None)
      |> Option.defaultValue
           // fallback: start of head pat (should not happen -> always `member`)
           headPat.Range.StartColumn

    let insertPos = expr.Range.End

    { StartColumn = startCol
      InsertAt = insertPos
      InsertWith = false }
    |> Some
  | None ->
    // align with `interface` or `new`
    //    no attributes on interface allowed
    // insert after `with` or identifier
    match tryFindInterfaceStartAndWith interfaceData.Range.End ast with
    | None -> None
    | Some (startPos, withRange) ->
      let startCol = startPos.Column + indentationSize

      let insertPos =
        withRange
        |> Option.map (fun r -> r.End)
        |> Option.defaultValue interfaceData.Range.End

      { StartColumn = startCol
        InsertAt = insertPos
        InsertWith = withRange |> Option.isNone }
      |> Some

type Config =
  { ObjectIdentifier: string
    MethodBody: string
    IndentationSize: int }

let titleWithTypeAnnotation = "Implement interface"
let titleWithoutTypeAnnotation = "Implement interface without type annotation"

/// codefix that generates members for an interface implementation
let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (getProjectOptionsForFile: GetProjectOptionsForFile)
  (config: unit -> Config)
  : CodeFix =
  Run.ifDiagnosticByCode (Set.ofList [ "366" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      // diagnostic range:
      // * object expression:
      //    * main interface: full expression from starting `{` to ending `}`
      //    * sub interface: `interface` to ending `}`
      // * implement interface in type: only interface name

      let fileName = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath

      let startPos = protocolPosToPos codeActionParams.Range.Start
      let! (tyRes, line, lines) = getParseResultsForFile fileName startPos

      let! interfaceData =
        InterfaceStubGenerator.TryFindInterfaceDeclaration startPos tyRes.GetAST
        |> Option.orElseWith (fun _ ->
          // happens when in object expression (`startPos` is on `{` or `interface`, NOT interface name)
          tryFindInterfaceDeclarationInObjectExpression startPos tyRes.GetAST)
        |> Result.ofOption (fun _ -> "No interface at position")

      /// End of Interface identifier
      let ifacePos =
        match interfaceData with
        | InterfaceData.ObjExpr (ty, _) -> ty.Range.End
        | InterfaceData.Interface (ty, _) -> ty.Range.End
      // line might be different -> update
      // (for example when `{` not on same line as main interface name)
      let! line =
        (if ifacePos.Line <> startPos.Line then
           lines.GetLine(ifacePos)
         else
           Some line)
        |> Result.ofOption (fun _ -> "Invalid position")

      let! symbolUse =
        tyRes.TryGetSymbolUse ifacePos line
        |> Result.ofOption (fun _ -> "No symbol use at position")

      match symbolUse.Symbol with
      | :? FSharpEntity as entity when
        InterfaceStubGenerator.IsInterface entity
        && not (InterfaceStubGenerator.HasNoInterfaceMember entity)
        ->

        let existingMembers = InterfaceStubGenerator.GetMemberNameAndRanges interfaceData
        let interfaceMembers = InterfaceStubGenerator.GetInterfaceMembers entity

        if List.length existingMembers <> Seq.length interfaceMembers then
          let getMemberByLocation (name, range: FcsRange) =
            match lines.GetLine range.End with
            | None -> None
            | Some lineStr ->
              tyRes.GetCheckResults.GetSymbolUseAtLocation(range.EndLine, range.EndColumn, lineStr, [ name ])

          let! implementedMemberSignatures =
            InterfaceStubGenerator.GetImplementedMemberSignatures
              getMemberByLocation
              symbolUse.DisplayContext
              interfaceData

          let config = config ()

          let! insertionData =
            tryFindInsertionData interfaceData tyRes.GetAST config.IndentationSize
            |> Result.ofOption (fun _ -> "No insert location found")

          let appendWithEdit =
            if insertionData.InsertWith then
              { Range = fcsPosToProtocolRange insertionData.InsertAt
                NewText = " with" }
              |> Some
            else
              None

          let appendClosingBracketEdit =
            match interfaceData with
            | InterfaceData.ObjExpr _ ->
              // `diagnostic.Range`:
              // * main interface: over full range of object expression (opening `{` to closing `}`)
              // * sub interface: `interface XXX with` to closing `}`
              match lines.TryGetChar(protocolPosToPos diagnostic.Range.End) with
              | Some '}' -> None
              | _ ->
                let pos = diagnostic.Range.End

                { Range = { Start = pos; End = pos }
                  NewText = " }" }
                |> Some
            | _ -> None

          let getMainEdit withTypeAnnotation =
            let stub =
              let stub =
                InterfaceStubGenerator.FormatInterface
                  insertionData.StartColumn
                  config.IndentationSize
                  interfaceData.TypeParameters
                  config.ObjectIdentifier
                  config.MethodBody
                  symbolUse.DisplayContext
                  implementedMemberSignatures
                  entity
                  withTypeAnnotation

              stub.TrimEnd(System.Environment.NewLine.ToCharArray())

            { Range = fcsPosToProtocolRange insertionData.InsertAt
              NewText = stub }

          let getFix title (mainEdit: TextEdit) =
            { Title = title
              File = codeActionParams.TextDocument
              SourceDiagnostic = Some diagnostic
              Kind = FixKind.Fix
              Edits =
                [| match appendWithEdit with
                   | Some edit -> edit
                   | None -> ()

                   mainEdit

                   match appendClosingBracketEdit with
                   | Some edit -> edit
                   | None -> () |] }

          return
            [ getFix titleWithTypeAnnotation (getMainEdit true)
              getFix titleWithoutTypeAnnotation (getMainEdit false) ]
        else
          return []
      | _ -> return []
    })
