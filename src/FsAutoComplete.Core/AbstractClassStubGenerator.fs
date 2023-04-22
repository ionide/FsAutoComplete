module FsAutoComplete.AbstractClassStubGenerator

open FsAutoComplete.CodeGenerationUtils
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.Tokenization
open FsAutoComplete.Logging
open FsToolkit.ErrorHandling


type AbstractClassData =
  | ObjExpr of baseTy: SynType * bindings: SynBinding list * overallRange: Range
  | ExplicitImpl of baseTy: SynType * members: SynMemberDefn list * safeInsertPosition: Position

  member x.AbstractTypeIdentRange =
    match x with
    | ObjExpr(baseTy, _, _)
    | ExplicitImpl(baseTy, _, _) -> baseTy.Range

  member x.TypeParameters =
    match x with
    | ObjExpr(t, _, _)
    | ExplicitImpl(t, _, _) -> expandTypeParameters t

let private (|ExplicitCtor|_|) =
  function
  | SynMemberDefn.Member(SynBinding(valData = SynValData(Some({ MemberKind = SynMemberKind.Constructor }), _, _)), _) ->
    Some()
  | _ -> None

/// checks to see if a type definition inherits an abstract class, and if so collects the members defined at that
let private walkTypeDefn (SynTypeDefn(info, repr, members, implicitCtor, range, trivia)) =
  option {
    let reprMembers =
      match repr with
      | SynTypeDefnRepr.ObjectModel(_, members, _) -> members
      | _ -> []

    let allMembers = reprMembers @ (Option.toList implicitCtor) @ members

    let! inheritType, inheritMemberRange = // this must exist for abstract types
      allMembers
      |> List.tryPick (function
        | SynMemberDefn.ImplicitInherit(inheritType, inheritArgs, alias, range) -> Some(inheritType, range)
        | _ -> None)

    let furthestMemberToSkip, otherMembers =
      ((inheritMemberRange, []), allMembers)
      // find the last of the following kinds of members, as object-programming members must come after these
      // * implicit/explicit constructors
      // * `inherit` expressions
      // * class-level `do`/`let` bindings (`do` bindings are actually `LetBindings` in the AST)
      ||> List.fold (fun (m, otherMembers) memb ->
        match memb with
        | (SynMemberDefn.ImplicitCtor _ | ExplicitCtor | SynMemberDefn.ImplicitInherit _ | SynMemberDefn.LetBindings _) as possible ->
          let c = Range.rangeOrder.Compare(m, possible.Range)

          let m' =
            if c < 0 then m
            else if c = 0 then m
            else possible.Range

          m', otherMembers
        | otherMember -> m, otherMember :: otherMembers)

    let safeInsertPosition =
      Position.mkPos (furthestMemberToSkip.EndLine + 1) (inheritMemberRange.StartColumn + 2)

    let otherMembersInDeclarationOrder = otherMembers |> List.rev
    return AbstractClassData.ExplicitImpl(inheritType, otherMembersInDeclarationOrder, safeInsertPosition)

  }

/// find the declaration of the abstract class being filled in at the given position
let private tryFindAbstractClassExprInParsedInput
  (pos: Position)
  (parsedInput: ParsedInput)
  : AbstractClassData option =
  SyntaxTraversal.Traverse(
    pos,
    parsedInput,
    { new SyntaxVisitorBase<_>() with
        member _.VisitExpr(path, traverseExpr, defaultTraverse, expr) =
          match expr with
          | SynExpr.ObjExpr(baseTy, constructorArgs, withKeyword, bindings, members, extraImpls, newExprRange, range) ->
            Some(AbstractClassData.ObjExpr(baseTy, bindings, range))
          | _ -> defaultTraverse expr

        override _.VisitModuleDecl(_, defaultTraverse, decl) =
          match decl with
          | SynModuleDecl.Types(types, m) -> List.tryPick walkTypeDefn types
          | _ -> defaultTraverse decl }
  )

/// Walk the parse tree for the given document and look for the definition of any abstract classes in use at the given pos.
/// This looks for implementations of abstract types in object expressions, as well as inheriting of abstract types inside class type declarations.
let tryFindAbstractClassExprInBufferAtPos
  (codeGenService: ICodeGenerationService)
  (pos: Position)
  (document: Document)
  =
  asyncMaybe {
    let! parseResults = codeGenService.ParseFileInProject(document.FullName)
    return! tryFindAbstractClassExprInParsedInput pos parseResults.ParseTree
  }

let getAbstractClassIdentifier (abstractClassData: AbstractClassData) tokens =
  let newKeywordIndex =
    match abstractClassData with
    | AbstractClassData.ObjExpr _ ->
      tokens
      // Find the `new` keyword
      |> List.findIndex (fun token -> token.CharClass = FSharpTokenCharKind.Keyword && token.TokenName = "NEW")
    | _ -> failwith "don't call me with this bro"

  findLastIdentifier tokens.[newKeywordIndex + 2 ..] tokens.[newKeywordIndex + 2]

let getMemberNameAndRanges (abstractClassData) =
  match abstractClassData with
  | AbstractClassData.ExplicitImpl(ty, members, _) ->
    members
    |> Seq.choose (function
      | (SynMemberDefn.Member(binding, _)) -> Some binding
      | _ -> None)
    |> Seq.choose (|MemberNameAndRange|_|)
    |> Seq.toList
  | AbstractClassData.ObjExpr(_, bindings, _) -> List.choose (|MemberNameAndRange|_|) bindings

/// Try to find the start column, so we know what the base indentation should be
let inferStartColumn
  (codeGenServer: ICodeGenerationService)
  (pos: Position)
  (doc: Document)
  (lines: ISourceText)
  (lineStr: string)
  (abstractClassData: AbstractClassData)
  (indentSize: int)
  =
  async {
    match getMemberNameAndRanges abstractClassData with
    | (_, range) :: _ -> return getLineIdent (lines.GetLineString(range.StartLine - 1))
    | [] ->
      match abstractClassData with
      | AbstractClassData.ExplicitImpl _ ->
        // 'interface ISomething with' is often in a new line, we use the indentation of that line
        return getLineIdent lineStr + indentSize
      | AbstractClassData.ObjExpr(_, _, newExprRange) ->
        match! codeGenServer.TokenizeLine(doc.FullName, pos.Line) with
        | Some tokens ->
          return
            tokens
            |> List.tryPick (fun (t: FSharpTokenInfo) ->
              if t.CharClass = FSharpTokenCharKind.Keyword && t.TokenName = "NEW" then
                // We round to nearest so the generated code will align on the indentation guides
                findGreaterMultiple (t.LeftColumn + indentSize) indentSize |> Some
              else
                None)
            // There is no reference point, we indent the content at the start column of the interface
            |> Option.defaultValue newExprRange.StartColumn
        | None -> return newExprRange.StartColumn
  }

/// Try to write any missing members of the given abstract type at the given location.
/// If the destination type isn't an abstract class, or if there are no missing members to implement,
/// nothing is written. Otherwise, a list of missing members is generated and written
let writeAbstractClassStub
  (codeGenServer: ICodeGenerationService)
  (checkResultForFile: ParseAndCheckResults)
  (doc: Document)
  (lines: ISourceText)
  (lineStr: string)
  (abstractClassData: AbstractClassData)
  =
  asyncOption {
    let pos =
      Position.mkPos
        abstractClassData.AbstractTypeIdentRange.Start.Line
        (abstractClassData.AbstractTypeIdentRange.End.Column)

    let! (_lexerSym, usages) = codeGenServer.GetSymbolAndUseAtPositionOfKind(doc.FullName, pos, SymbolKind.Ident)
    let! usage = usages

    let! (displayContext, entity) =
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

    let getMemberByLocation (name, range: Range) =
      asyncOption {
        let pos = Position.fromZ (range.StartLine - 1) (range.StartColumn + 1)
        return! checkResultForFile.GetCheckResults.GetSymbolUseAtLocation(pos.Line, pos.Column, lineStr, [])
      }

    let insertInfo =
      asyncOption {
        let! tokens = codeGenServer.TokenizeLine(doc.FullName, pos.Line)

        return
          match abstractClassData with
          | AbstractClassData.ObjExpr _ ->
            findLastPositionOfWithKeyword tokens entity pos (getAbstractClassIdentifier abstractClassData)
          | AbstractClassData.ExplicitImpl(_, _, safeInsertPosition) -> Some(false, safeInsertPosition)

      }

    let desiredMemberNamesWithRanges = getMemberNameAndRanges abstractClassData

    let! implementedSignatures =
      getImplementedMemberSignatures getMemberByLocation displayContext desiredMemberNamesWithRanges
      |> Async.map Some

    let! generatedString =
      async {
        let! start = (inferStartColumn codeGenServer pos doc lines lineStr abstractClassData 4) // 4 here correspond to the indent size

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

        // If we are in a object expression, we remove the last new line, so the `}` stay on the same line
        match abstractClassData with
        | AbstractClassData.ExplicitImpl _ -> return formattedString
        | AbstractClassData.ObjExpr _ -> return formattedString.TrimEnd('\n')
      }

    // If generatedString is empty it means nothing is missing to the abstract class
    // So we return None, in order to not show a "Falsy Hint"
    if System.String.IsNullOrEmpty generatedString then
      return! None
    else
      match! insertInfo with
      | Some(shouldAppendWith, insertPosition) ->
        if shouldAppendWith then
          return! Some(insertPosition, " with" + generatedString)
        else
          return! Some(insertPosition, generatedString)
      | None ->
        // Unable to find an optimal insert position so return the position under the cursor
        // By doing that we allow the user to copy/paste the code if the insertion break the code
        // If we return None, then user would not benefit from abstract stub generation at all
        return! Some(pos, generatedString)
  }
