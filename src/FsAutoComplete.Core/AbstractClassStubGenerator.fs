module FsAutoComplete.AbstractClassStubGenerator

open FsAutoComplete.CodeGenerationUtils
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FsToolkit.ErrorHandling


type AbstractClassData =
  | ObjExpr of baseTy: SynType * bindings: SynBinding list * newExpression: Range * withKeyword: Range option

  | ExplicitImpl of
    baseTy: SynType *
    members: SynMemberDefn list *
    /// the place where the inherit expression is declared - the code-fix should insert
    /// at an indent of .Start.Column, but insert a newline after .End
    inheritExpressionRange: Range

  member x.AbstractTypeIdentRange =
    match x with
    | ObjExpr(baseTy = baseTy)
    | ExplicitImpl(baseTy = baseTy) -> baseTy.Range

  member x.TypeParameters =
    match x with
    | ObjExpr(baseTy = t)
    | ExplicitImpl(baseTy = t) -> expandTypeParameters t

/// Walk the parse tree for the given document and look for the definition of any abstract classes in use at the given pos.
/// This looks for implementations of abstract types in object expressions, as well as inheriting of abstract types inside class type declarations.
let tryFindAbstractClassExprInBufferAtPos
  (codeGenService: ICodeGenerationService)
  (pos: Position)
  (document: IFSACSourceText)
  =
  asyncOption {
    let! parseResults = codeGenService.ParseFileInProject document.FileName

    return!
      (pos, parseResults.ParseTree)
      ||> ParsedInput.tryPick (fun _path node ->
        match node with
        | SyntaxNode.SynExpr(SynExpr.ObjExpr(
            objType = baseTy; withKeyword = withKeyword; bindings = bindings; newExprRange = newExprRange)) ->
          Some(ObjExpr(baseTy, bindings, newExprRange, withKeyword))

        | SyntaxNode.SynTypeDefn(SynTypeDefn(_, repr, members, implicitCtor, _, _)) ->
          option {
            let reprMembers =
              match repr with
              | SynTypeDefnRepr.ObjectModel(_, members, _) -> members // repr members already includes the implicit ctor if present
              | _ -> Option.toList implicitCtor

            let allMembers = reprMembers @ members

            let! inheritType, inheritMemberRange = // this must exist for abstract types
              allMembers
              |> List.tryPick (function
                | SynMemberDefn.ImplicitInherit(inheritType, _, _, range, _) -> Some(inheritType, range)
                | _ -> None)

            let furthestMemberToSkip, otherMembers =
              ((inheritMemberRange, []), allMembers)
              // find the last of the following kinds of members, as object-programming members must come after these
              // * implicit/explicit constructors
              // * `inherit` expressions
              // * class-level `do`/`let` bindings (`do` bindings are actually `LetBindings` in the AST)
              ||> List.fold (fun (m, otherMembers) memb ->
                match memb with
                | SynMemberDefn.ImplicitCtor _
                | SynMemberDefn.Member(
                  memberDefn = SynBinding(
                    valData = SynValData(memberFlags = Some { MemberKind = SynMemberKind.Constructor })))
                | SynMemberDefn.ImplicitInherit _
                | SynMemberDefn.LetBindings _ as possible ->
                  let c = Range.rangeOrder.Compare(m, possible.Range)

                  let m' = if c < 0 then possible.Range else m

                  m', otherMembers
                | otherMember -> m, otherMember :: otherMembers)

            let otherMembersInDeclarationOrder = otherMembers |> List.rev
            return ExplicitImpl(inheritType, otherMembersInDeclarationOrder, furthestMemberToSkip)
          }

        | _ -> None)
  }

let getMemberNameAndRanges abstractClassData =
  match abstractClassData with
  | AbstractClassData.ExplicitImpl(members = members) ->
    members
    |> Seq.choose (function
      | SynMemberDefn.Member(binding, _) -> Some binding
      | _ -> None)
    |> Seq.choose (|MemberNamePlusRangeAndKeywordRange|_|)
    |> Seq.toList
  | AbstractClassData.ObjExpr(bindings = bindings) -> List.choose (|MemberNamePlusRangeAndKeywordRange|_|) bindings

/// Try to find the start column, so we know what the base indentation should be
let inferStartColumn
  (abstractClassData: AbstractClassData)
  (memberNamesAndRanges: (_ * _ * Range) list)
  (indentSize: int)
  =
  match memberNamesAndRanges with
  | (_, _, leadingKeywordRange) :: _ ->
    // if we have any members, then we can use the start of the leading keyword to give us the indent correctly
    leadingKeywordRange.StartColumn
  | [] ->
    match abstractClassData with
    | AbstractClassData.ExplicitImpl(inheritExpressionRange = inheritRange) ->
      // 'interface ISomething with' is often in a new line, we use the indentation of that line
      inheritRange.StartColumn
    | AbstractClassData.ObjExpr(newExpression = newExpr; withKeyword = withKeyword; bindings = _) ->
      // two cases here to consider:
      // * has a with keyword on same line as newExpr
      match withKeyword with
      | None ->
        // if no withKeyword, then we add an indent to the start of the new Expression to get our final indent
        newExpr.StartColumn + indentSize
      | Some keyword ->
        // if we have a keyword, if it's on the same line then we can do the same
        if keyword.StartLine = newExpr.StartLine then
          newExpr.StartColumn + indentSize
        else if keyword.StartColumn = newExpr.StartColumn then
          keyword.StartColumn + indentSize
        else
          keyword.StartColumn

/// Try to write any missing members of the given abstract type at the given location.
/// If the destination type isn't an abstract class, or if there are no missing members to implement,
/// nothing is written. Otherwise, a list of missing members is generated and written
let writeAbstractClassStub
  (codeGenServer: ICodeGenerationService)
  (checkResultForFile: ParseAndCheckResults)
  (doc: IFSACSourceText)
  (_: string)
  (abstractClassData: AbstractClassData)
  =
  asyncOption {
    let pos =
      Position.mkPos
        abstractClassData.AbstractTypeIdentRange.Start.Line
        abstractClassData.AbstractTypeIdentRange.End.Column

    let! _lexerSym, usages = codeGenServer.GetSymbolAndUseAtPositionOfKind(doc.FileName, pos, SymbolKind.Ident)
    let! usage = usages

    let! displayContext, entity =
      asyncOption {
        // need the enclosing entity because we're always looking at a ctor, which isn't an Entity, but a MemberOrFunctionOrValue
        match usage.Symbol with
        | :? FSharpMemberOrFunctionOrValue as v ->
          if isAbstractClass v.ApparentEnclosingEntity then
            let! displayContext = checkResultForFile.GetCheckResults.GetDisplayContextForPos(pos)
            return! Some(displayContext, v.ApparentEnclosingEntity)
          else
            return! None
        | _ -> return! None
      }

    let getMemberByLocation (_: string, range: Range, _: Range) =
      match doc.GetLine range.Start with
      | Some lineText ->
        match
          Lexer.getSymbol
            (uint32 range.Start.Line)
            (uint32 range.Start.Column)
            lineText
            SymbolLookupKind.ByLongIdent
            [||]
        with
        | Some sym ->
          checkResultForFile.GetCheckResults.GetSymbolUseAtLocation(
            range.StartLine,
            range.EndColumn,
            lineText,
            sym.Text.Split('.') |> List.ofArray
          )
        | None -> None
      | None -> None

    let desiredMemberNamesWithRanges = getMemberNameAndRanges abstractClassData

    let implementedSignatures =
      getImplementedMemberSignatures getMemberByLocation displayContext desiredMemberNamesWithRanges

    let start = inferStartColumn abstractClassData desiredMemberNamesWithRanges 4 // 4 here correspond to the indent size

    // this entire file could potentially be replaced by something very much like
    // FSharp.Compiler.EditorServices.InterfaceStubGenerator.FormatInterface, if the
    // function to 'get the members we want to implement' was exposed somehow instead of being hard-coded
    // to just interface lookups
    let formattedString =
      formatMembersAt
        start
        4 // Should we make it a setting from the IDE ?
        abstractClassData.TypeParameters
        "$objectIdent"
        "$methodBody"
        displayContext
        implementedSignatures
        entity
        getAbstractNonVirtualMembers
        true // Always generate the verbose version of the code

    let generatedString = formattedString.TrimEnd('\n')

    // If generatedString is empty it means nothing is missing to the abstract class
    // So we return None, in order to not show a "Falsy Hint"
    if System.String.IsNullOrEmpty generatedString then
      return! None
    else
      match abstractClassData with
      | AbstractClassData.ObjExpr(newExpression = newExpr; withKeyword = keyword) ->
        match keyword with
        | None -> return newExpr.End, " with\n" + generatedString
        | Some k -> return k.End, "\n" + generatedString

      | AbstractClassData.ExplicitImpl(_, _, inheritExpressionRange) ->
        return inheritExpressionRange.End, "\n" + generatedString
  }
