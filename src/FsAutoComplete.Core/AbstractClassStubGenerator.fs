module FsAutoComplete.AbstractClassStubGenerator

open FsAutoComplete.CodeGenerationUtils
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.Tokenization

type AbstractClassData =
  | ObjExpr of baseTy: SynType * bindings: SynBinding list * overallRange: Range
  | ExplicitImpl of baseTy: SynType * members: SynMemberDefn list * safeInsertPosition: Position
  member x.AbstractTypeIdentRange =
    match x with
    | ObjExpr (baseTy, _, _)
    | ExplicitImpl (baseTy, _, _) -> baseTy.Range
  member x.TypeParameters =
    match x with
    | ObjExpr(t, _, _)
    | ExplicitImpl(t, _, _) -> expandTypeParameters t

/// checks to see if a type definition inherits an abstract class, and if so collects the members defined at that
let private walkTypeDefn (SynTypeDefn(info, repr, members, implicitCtor, range, trivia)) =
  let reprMembers =
    match repr with
    | SynTypeDefnRepr.ObjectModel (_, members, _) -> members
    | _ -> []
  let allMembers = reprMembers @ (Option.toList implicitCtor) @ members
  let inheritMember =
    allMembers
    |> List.tryPick (function SynMemberDefn.ImplicitInherit(inheritType, inheritArgs, alias, range) -> Some (inheritType) | _ -> None)

  let otherMembers =
    allMembers
    |> List.filter (
      // filter out implicit/explicit constructors and inherit statements, as all members _must_ come after these
      function | SynMemberDefn.ImplicitCtor _
               | SynMemberDefn.ImplicitInherit _ -> false
               | SynMemberDefn.Member (SynBinding(valData = SynValData(Some({ MemberKind = SynMemberKind.Constructor } ), _, _)) ,  _) -> false
               | _ -> true)
  match inheritMember with
  | Some inheritMember ->
    let safeInsertPosition =
      match otherMembers with
      | [] -> Position.mkPos (inheritMember.Range.End.Line + 1) (inheritMember.Range.Start.Column + 2)
      | x :: _ -> Position.mkPos (x.Range.End.Line - 1) (x.Range.Start.Column + 2)

    Some(AbstractClassData.ExplicitImpl(inheritMember, otherMembers, safeInsertPosition))
  | _ -> None

/// find the declaration of the abstract class being filled in at the given position
let private tryFindAbstractClassExprInParsedInput (pos: Position) (parsedInput: ParsedInput) : AbstractClassData option =
  SyntaxTraversal.Traverse(pos, parsedInput,
    { new SyntaxVisitorBase<_>() with
        member _.VisitExpr (path, traverseExpr, defaultTraverse, expr) =
          match expr with
          | SynExpr.ObjExpr (baseTy, constructorArgs, withKeyword, bindings, members, extraImpls, newExprRange, range) ->
            Some (AbstractClassData.ObjExpr(baseTy, bindings, range))
          | _ -> defaultTraverse expr
        override _.VisitModuleDecl (_, defaultTraverse, decl) =
          match decl with
          | SynModuleDecl.Types(types, m) ->
            List.tryPick walkTypeDefn types
          | _ -> defaultTraverse decl
      })

/// Walk the parse tree for the given document and look for the definition of any abstract classes in use at the given pos.
/// This looks for implementations of abstract types in object expressions, as well as inheriting of abstract types inside class type declarations.
let tryFindAbstractClassExprInBufferAtPos (codeGenService: CodeGenerationService) (pos: Position) (document : Document) =
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
        |> List.findIndex (fun token ->
            token.CharClass = FSharpTokenCharKind.Keyword
                && token.TokenName = "NEW"
        )
    | _ -> failwith "don't call me with this bro"

  findLastIdentifier tokens.[newKeywordIndex + 2..] tokens.[newKeywordIndex + 2]

let getMemberNameAndRanges (abstractClassData) =
  match abstractClassData with
  | AbstractClassData.ExplicitImpl(ty, members, _) ->
    members
    |> Seq.choose (function (SynMemberDefn.Member(binding, _)) -> Some binding | _ -> None)
    |> Seq.choose (|MemberNameAndRange|_|)
    |> Seq.toList
  | AbstractClassData.ObjExpr (_, bindings, _) -> List.choose (|MemberNameAndRange|_|) bindings

/// Try to find the start column, so we know what the base indentation should be
let inferStartColumn  (codeGenServer : CodeGenerationService) (pos : Position) (doc : Document) (lines: ISourceText) (lineStr : string) (abstractClassData : AbstractClassData) (indentSize : int) =
    match getMemberNameAndRanges abstractClassData with
    | (_, range) :: _ ->
        getLineIdent (lines.GetLineString(range.StartLine - 1))
    | [] ->
        match abstractClassData with
        | AbstractClassData.ExplicitImpl _ ->
            // 'interface ISomething with' is often in a new line, we use the indentation of that line
            getLineIdent lineStr + indentSize
        | AbstractClassData.ObjExpr (_, _, newExprRange) ->
            match codeGenServer.TokenizeLine(doc.FullName, pos.Line) with
            | Some tokens ->
                tokens
                |> List.tryPick (fun (t: FSharpTokenInfo) ->
                        if t.CharClass = FSharpTokenCharKind.Keyword && t.TokenName = "NEW" then
                            // We round to nearest so the generated code will align on the indentation guides
                            findGreaterMultiple (t.LeftColumn + indentSize) indentSize
                            |> Some
                        else None)
                // There is no reference point, we indent the content at the start column of the interface
                |> Option.defaultValue newExprRange.StartColumn
            | None -> newExprRange.StartColumn

/// Try to write any missing members of the given abstract type at the given location.
/// If the destination type isn't an abstract class, or if there are no missing members to implement,
/// nothing is written. Otherwise, a list of missing members is generated and written
let writeAbstractClassStub (codeGenServer : CodeGenerationService) (checkResultForFile: ParseAndCheckResults) (doc : Document) (lines: ISourceText) (lineStr : string) (abstractClassData : AbstractClassData) =
  asyncMaybe {
    let pos = Position.mkPos abstractClassData.AbstractTypeIdentRange.Start.Line (abstractClassData.AbstractTypeIdentRange.End.Column)
    let! (_lexerSym, usages) = codeGenServer.GetSymbolAndUseAtPositionOfKind(doc.FullName, pos, SymbolKind.Ident)
    let! usage = usages
    let! (displayContext, entity) =
      asyncMaybe {
        // need the enclosing entity because we're always looking at a ctor, which isn't an Entity, but a MemberOrFunctionOrValue
        match usage.Symbol with
        | :? FSharpMemberOrFunctionOrValue as v ->
          if isAbstractClass v.ApparentEnclosingEntity
          then
            let! displayContext  = checkResultForFile.GetCheckResults.GetDisplayContextForPos(pos)
            return! Some (displayContext, v.ApparentEnclosingEntity)
          else
            return! None
        | _ -> return! None
      }

    let getMemberByLocation (name, range: Range) =
        asyncMaybe {
            let pos = Position.fromZ (range.StartLine - 1) (range.StartColumn + 1)
            return! checkResultForFile.GetCheckResults.GetSymbolUseAtLocation (pos.Line, pos.Column, lineStr, [])
        }

    let insertInfo =
        match codeGenServer.TokenizeLine(doc.FullName, pos.Line) with
        | Some tokens ->
          match abstractClassData with
          | AbstractClassData.ObjExpr _ -> findLastPositionOfWithKeyword tokens entity pos (getAbstractClassIdentifier abstractClassData)
          | AbstractClassData.ExplicitImpl (_, _, safeInsertPosition) -> Some (false, safeInsertPosition)
        | None -> None

    let desiredMemberNamesWithRanges = getMemberNameAndRanges abstractClassData

    let! implementedSignatures =
      getImplementedMemberSignatures getMemberByLocation displayContext desiredMemberNamesWithRanges
      |> Async.map Some

    let generatedString =
        let formattedString =
            formatMembersAt
                (inferStartColumn codeGenServer pos doc lines lineStr abstractClassData 4) // 4 here correspond to the indent size
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
        | AbstractClassData.ExplicitImpl _ ->
            formattedString
        | AbstractClassData.ObjExpr _ ->
            formattedString.TrimEnd('\n')

    // If generatedString is empty it means nothing is missing to the abstract class
    // So we return None, in order to not show a "Falsy Hint"
    if System.String.IsNullOrEmpty generatedString then
        return! None
    else
        match insertInfo with
        | Some (shouldAppendWith, insertPosition) ->
            if shouldAppendWith then
                return! Some (insertPosition, " with" + generatedString)
            else
                return! Some (insertPosition, generatedString)
        | None ->
            // Unable to find an optimal insert position so return the position under the cursor
            // By doing that we allow the user to copy/paste the code if the insertion break the code
            // If we return None, then user would not benefit from abstract stub generation at all
            return! Some (pos, generatedString)
  }
