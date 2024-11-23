/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/CodeGeneration/UnionPatternMatchCaseGenerator.fs
module FsAutoComplete.UnionPatternMatchCaseGenerator

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsAutoComplete.CodeGenerationUtils
open FSharp.Compiler.Symbols
open FsToolkit.ErrorHandling

[<NoEquality; NoComparison>]
type PatternMatchExpr =
  {
    /// Range of 'match x with' or 'function'
    MatchWithOrFunctionRange: Range
    /// The whole pattern match expression
    Expr: SynExpr
    Clauses: SynMatchClause list
  }

[<NoComparison>]
type UnionMatchCasesInsertionParams =
  { InsertionPos: Position
    IndentColumn: int }

[<NoComparison>]
type private Context =
  {
    Writer: ColumnIndentedTextWriter
    /// A single-line skeleton for each case
    CaseDefaultValue: string
    UnionTypeName: string
    Qualifier: string option
  }

let private clauseIsCandidateForCodeGen (cursorPos: Position) (SynMatchClause(pat, _, _, _, _, _)) =
  let rec patIsCandidate (pat: SynPat) =
    match pat with
    | SynPat.Paren(innerPat, _)
    | SynPat.Attrib(innerPat, _, _) -> patIsCandidate innerPat
    | SynPat.Const _ -> false
    | SynPat.Wild _ -> false
    // TODO: check if we have to handle these cases
    | SynPat.Typed(innerPat, _, _) -> patIsCandidate innerPat
    | SynPat.OptionalVal _ -> false
    | SynPat.Or(lhsPat = leftPat; rhsPat = rightPat) -> patIsCandidate leftPat || patIsCandidate rightPat
    | SynPat.Ands(innerPatList, _) -> List.exists patIsCandidate innerPatList
    // This is the 'hd :: tail -> ...' pattern
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ ident ])) when ident.idText = "op_ColonColon" -> false
    | SynPat.LongIdent(argPats = ConstructorPats nestedPats; range = r) ->
      // The cursor should not be in the nested patterns
      Range.rangeContainsPos r cursorPos
      && List.forall (not << patIsCandidate) nestedPats
    | SynPat.ListCons(lhs, rhs, _, _) -> patIsCandidate lhs || patIsCandidate rhs
    | SynPat.Tuple _
    | SynPat.ArrayOrList _
    | SynPat.Record _
    | SynPat.Null _
    | SynPat.IsInst _
    | SynPat.QuoteExpr _
    | SynPat.InstanceMember _
    | SynPat.FromParseError _
    | SynPat.As _
    | SynPat.Named _
    | SynPat.LongIdent _ -> false

  patIsCandidate pat

let private posIsInLhsOfClause (pos: Position) (clause: SynMatchClause) =
  match clause with
  | SynMatchClause(whenExpr = None; range = patternRange) -> Range.rangeContainsPos patternRange pos
  | SynMatchClause(whenExpr = Some guardExpr; range = patternRange) ->
    Range.rangeContainsPos (Range.unionRanges guardExpr.Range patternRange) pos

let private tryFindPatternMatchExprInParsedInput (pos: Position) (parsedInput: ParsedInput) =
  let inline getIfPosInRange range f = if Range.rangeContainsPos range pos then f () else None

  let rec walkImplFileInput (ParsedImplFileInput(contents = moduleOrNamespaceList)) =
    List.tryPick walkSynModuleOrNamespace moduleOrNamespaceList

  and walkSynModuleOrNamespace (SynModuleOrNamespace(decls = decls; range = range)) =
    getIfPosInRange range (fun () -> List.tryPick walkSynModuleDecl decls)

  and walkSynModuleDecl (decl: SynModuleDecl) =
    getIfPosInRange decl.Range (fun () ->
      match decl with
      | SynModuleDecl.Exception(SynExceptionDefn(members = synMembers), _) -> List.tryPick walkSynMemberDefn synMembers
      | SynModuleDecl.Let(_isRecursive, bindings, _range) -> List.tryPick walkBinding bindings
      | SynModuleDecl.ModuleAbbrev(_lhs, _rhs, _range) -> None
      | SynModuleDecl.NamespaceFragment(fragment) -> walkSynModuleOrNamespace fragment
      | SynModuleDecl.NestedModule(decls = modules) -> List.tryPick walkSynModuleDecl modules
      | SynModuleDecl.Types(typeDefs, _range) -> List.tryPick walkSynTypeDefn typeDefs
      | SynModuleDecl.Expr(expr = expr) -> walkExpr expr
      | SynModuleDecl.Attributes _
      | SynModuleDecl.HashDirective _
      | SynModuleDecl.Open _ -> None)

  and walkSynTypeDefn
    (SynTypeDefn(typeRepr = representation; members = members; implicitConstructor = implicitCtor; range = range))
    =
    getIfPosInRange range (fun () ->
      walkSynTypeDefnRepr representation
      |> Option.orElseWith (fun _ -> Option.bind walkSynMemberDefn implicitCtor)
      |> Option.orElseWith (fun _ -> List.tryPick walkSynMemberDefn members))

  and walkSynTypeDefnRepr (typeDefnRepr: SynTypeDefnRepr) =
    getIfPosInRange typeDefnRepr.Range (fun () ->
      match typeDefnRepr with
      | SynTypeDefnRepr.ObjectModel(_kind, members, _range) -> List.tryPick walkSynMemberDefn members
      | SynTypeDefnRepr.Simple _
      | SynTypeDefnRepr.Exception _ -> None)

  and walkSynMemberDefn (memberDefn: SynMemberDefn) =
    getIfPosInRange memberDefn.Range (fun () ->
      match memberDefn with
      | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, _range, _) -> None
      | SynMemberDefn.AutoProperty(synExpr = expr) -> walkExpr expr
      | SynMemberDefn.Interface(members = members) -> Option.bind (List.tryPick walkSynMemberDefn) members
      | SynMemberDefn.Member(binding, _range) -> walkBinding binding
      | SynMemberDefn.NestedType(typeDef, _access, _range) -> walkSynTypeDefn typeDef
      | SynMemberDefn.ValField(_field, _range) -> None
      | SynMemberDefn.LetBindings(bindings, _isStatic, _isRec, _range) -> List.tryPick walkBinding bindings
      | SynMemberDefn.GetSetMember(_get, _set, _range, _) -> None
      | SynMemberDefn.Open _
      | SynMemberDefn.ImplicitInherit _
      | SynMemberDefn.Inherit _
      | SynMemberDefn.ImplicitCtor _ -> None)

  and walkBinding (SynBinding(expr = expr) as binding) =
    getIfPosInRange binding.RangeOfBindingWithRhs (fun () -> walkExpr expr)

  and walkExpr expr =
    getIfPosInRange expr.Range (fun () ->
      match expr with
      | SynExpr.Quote(synExpr1, _, synExpr2, _, _range) -> List.tryPick walkExpr [ synExpr1; synExpr2 ]

      | SynExpr.Const(_synConst, _range) -> None

      | SynExpr.Typed(synExpr, _, _)
      | SynExpr.Paren(synExpr, _, _, _)
      | SynExpr.New(_, _, synExpr, _)
      | SynExpr.ArrayOrListComputed(_, synExpr, _)
      | SynExpr.ComputationExpr(_, synExpr, _)
      | SynExpr.Lambda(body = synExpr)
      | SynExpr.Lazy(synExpr, _)
      | SynExpr.Do(synExpr, _)
      | SynExpr.Assert(synExpr, _) -> walkExpr synExpr

      | SynExpr.Tuple(_, synExprList, _, _range)
      | SynExpr.ArrayOrList(_, synExprList, _range) -> List.tryPick walkExpr synExprList

      | SynExpr.Record(_inheritOpt, copyOpt, fields, _range) ->
        let fieldExprList =
          fields |> List.choose (fun (SynExprRecordField(expr = expr)) -> expr)

        match copyOpt with
        | Some(copyExpr, _blockSeparator) -> List.tryPick walkExpr (copyExpr :: fieldExprList)
        | None -> List.tryPick walkExpr fieldExprList

      | SynExpr.ObjExpr(bindings = binds; extraImpls = ifaces) ->
        List.tryPick walkBinding binds
        |> Option.orElseWith (fun _ -> List.tryPick walkSynInterfaceImpl ifaces)

      | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
        List.tryPick walkExpr [ synExpr1; synExpr2 ]
      | SynExpr.ForEach(enumExpr = synExpr1; bodyExpr = synExpr2) -> List.tryPick walkExpr [ synExpr1; synExpr2 ]

      | SynExpr.For(identBody = synExpr1; toBody = synExpr2; doBody = synExpr3) ->
        List.tryPick walkExpr [ synExpr1; synExpr2; synExpr3 ]

      | SynExpr.MatchLambda(isExnMatch, functionKeywordRange, synMatchClauseList, _, _wholeExprRange) as matchLambdaExpr ->
        synMatchClauseList
        |> List.tryPick (fun (SynMatchClause(resultExpr = e)) -> walkExpr e)
        |> Option.orElseWith (fun () ->
          if isExnMatch then
            None
          else
            let currentClause = List.tryFind (posIsInLhsOfClause pos) synMatchClauseList

            if
              currentClause
              |> Option.map (clauseIsCandidateForCodeGen pos)
              |> Option.defaultValue false
            then
              { MatchWithOrFunctionRange = functionKeywordRange
                Expr = matchLambdaExpr
                Clauses = synMatchClauseList }
              |> Some
            else
              None)

      | SynExpr.Match(matchDebugPoint = debugPoint; expr = synExpr; clauses = synMatchClauseList) as matchExpr ->
        getIfPosInRange synExpr.Range (fun () -> walkExpr synExpr)
        |> Option.orElseWith (fun () ->
          synMatchClauseList
          |> List.tryPick (fun (SynMatchClause(resultExpr = e)) -> walkExpr e))
        |> Option.orElseWith (fun () ->
          let currentClause = List.tryFind (posIsInLhsOfClause pos) synMatchClauseList

          if
            currentClause
            |> Option.map (clauseIsCandidateForCodeGen pos)
            |> Option.defaultValue false
          then
            match debugPoint with
            | DebugPointAtBinding.Yes range ->
              { MatchWithOrFunctionRange = range
                Expr = matchExpr
                Clauses = synMatchClauseList }
              |> Some
            | _ -> None
          else
            None)
        |> Option.orElseWith (fun () ->
          if synMatchClauseList.IsEmpty then
            match debugPoint with
            | DebugPointAtBinding.Yes range ->
              { MatchWithOrFunctionRange = range
                Expr = matchExpr
                Clauses = [] }
              |> Some
            | _ -> None
          else
            None

        )

      | SynExpr.App(_exprAtomicFlag, _isInfix, synExpr1, synExpr2, _range) ->
        List.tryPick walkExpr [ synExpr1; synExpr2 ]

      | SynExpr.TypeApp(synExpr, _, _synTypeList, _commas, _, _, _range) -> walkExpr synExpr

      | SynExpr.LetOrUse(body = synExpr; bindings = synBindingList) ->
        walkExpr synExpr
        |> Option.orElseWith (fun _ -> List.tryPick walkBinding synBindingList)

      | SynExpr.TryWith(tryExpr = synExpr) -> walkExpr synExpr

      | SynExpr.TryFinally(tryExpr = synExpr1; finallyExpr = synExpr2) -> List.tryPick walkExpr [ synExpr1; synExpr2 ]

      | Sequentials exprs -> List.tryPick walkExpr exprs

      | SynExpr.IfThenElse(ifExpr = synExpr1; thenExpr = synExpr2; elseExpr = synExprOpt) ->
        match synExprOpt with
        | Some synExpr3 -> List.tryPick walkExpr [ synExpr1; synExpr2; synExpr3 ]
        | None -> List.tryPick walkExpr [ synExpr1; synExpr2 ]

      | SynExpr.Ident(_ident) -> None
      | SynExpr.LongIdent(_, _longIdent, _altNameRefCell, _range) -> None

      | SynExpr.LongIdentSet(_longIdent, synExpr, _range) -> walkExpr synExpr
      | SynExpr.DotGet(synExpr, _dotm, _longIdent, _range) -> walkExpr synExpr

      | SynExpr.DotSet(synExpr1, _longIdent, synExpr2, _range) -> List.tryPick walkExpr [ synExpr1; synExpr2 ]

      | SynExpr.DotIndexedGet(_, argList, _range, _range2) -> walkExpr argList

      | SynExpr.DotIndexedSet(synExpr1, argList, synExpr2, _, _range, _range2) ->
        [ synExpr1; argList; synExpr2 ] |> List.tryPick walkExpr

      | SynExpr.JoinIn(synExpr1, _range, synExpr2, _range2) -> List.tryPick walkExpr [ synExpr1; synExpr2 ]
      | SynExpr.NamedIndexedPropertySet(_longIdent, synExpr1, synExpr2, _range) ->
        List.tryPick walkExpr [ synExpr1; synExpr2 ]

      | SynExpr.DotNamedIndexedPropertySet(synExpr1, _longIdent, synExpr2, synExpr3, _range) ->
        List.tryPick walkExpr [ synExpr1; synExpr2; synExpr3 ]

      | SynExpr.TypeTest(synExpr, _synType, _range)
      | SynExpr.Upcast(synExpr, _synType, _range)
      | SynExpr.Downcast(synExpr, _synType, _range) -> walkExpr synExpr
      | SynExpr.InferredUpcast(synExpr, _range)
      | SynExpr.InferredDowncast(synExpr, _range) -> walkExpr synExpr
      | SynExpr.AddressOf(_, synExpr, _range, _range2) -> walkExpr synExpr
      | SynExpr.TraitCall(_synTyparList, _synMemberSig, synExpr, _range) -> walkExpr synExpr

      | SynExpr.Null(_range)
      | SynExpr.ImplicitZero(_range) -> None

      | SynExpr.YieldOrReturn(_, synExpr, _range, _)
      | SynExpr.YieldOrReturnFrom(_, synExpr, _range, _)
      | SynExpr.DoBang(synExpr, _range, _) -> walkExpr synExpr

      | SynExpr.LetOrUseBang(rhs = synExpr1; andBangs = ands; body = synExpr2) ->
        [ synExpr1
          yield! ands |> List.map (fun (SynExprAndBang(body = body)) -> body)
          synExpr2 ]
        |> List.tryPick walkExpr

      | SynExpr.LibraryOnlyILAssembly _
      | SynExpr.LibraryOnlyStaticOptimization _
      | SynExpr.LibraryOnlyUnionCaseFieldGet _
      | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> None
      | SynExpr.ArbitraryAfterError(_debugStr, _range) -> None

      | SynExpr.FromParseError(synExpr, _range)
      | SynExpr.DiscardAfterMissingQualificationAfterDot(synExpr, _, _range) -> walkExpr synExpr

      | _ -> None)

  and walkSynInterfaceImpl (SynInterfaceImpl(bindings = synBindings)) = List.tryPick walkBinding synBindings

  match parsedInput with
  | ParsedInput.SigFile _input -> None
  | ParsedInput.ImplFile input -> walkImplFileInput input

let getWrittenCases (patMatchExpr: PatternMatchExpr) =
  let rec checkPattern pat =
    match pat with
    | SynPat.Const(_const, _) -> false
    // TODO: figure out if these cases are supposed to happen or not
    | SynPat.Or _
    | SynPat.Ands _
    | SynPat.LongIdent _
    | SynPat.ArrayOrList _
    | SynPat.Null _
    | SynPat.InstanceMember _
    | SynPat.IsInst _
    | SynPat.QuoteExpr _
    | SynPat.ListCons _
    | SynPat.FromParseError _ -> false

    | SynPat.Tuple(elementPats = innerPatList) -> List.forall checkPattern innerPatList

    | SynPat.Record(recordInnerPatList, _) ->
      recordInnerPatList
      |> List.map (fun (_, _, innerPat) -> innerPat)
      |> List.forall checkPattern

    | SynPat.OptionalVal _ -> true
    | SynPat.Named _
    | SynPat.Wild _ -> true
    | SynPat.Typed(innerPat, _, _)
    | SynPat.Attrib(innerPat, _, _)
    | SynPat.Paren(innerPat, _) -> checkPattern innerPat
    | SynPat.As(lhsPat, rhsPat, _range) -> checkPattern lhsPat && checkPattern rhsPat

  let getIfArgsAreFree constructorArgs func =
    match constructorArgs with
    | SynArgPats.Pats patList ->
      if List.forall checkPattern patList then
        Some(func ())
      else
        None
    | SynArgPats.NamePatPairs(namedPatList, _, _) ->
      let patList = namedPatList |> List.unzip3 |> (fun (_, _, pat) -> pat)

      if List.forall checkPattern patList then
        Some(func ())
      else
        None

  let rec getCasesInPattern (pat: SynPat) =
    match pat with
    | SynPat.LongIdent(longDotId = SynLongIdent(id = unionCaseLongIdent); argPats = constructorArgs) ->
      // Get list of qualifiers, this can be checked for length later.
      let reversedIdents =
        unionCaseLongIdent |> List.map (fun id -> id.idText) |> List.rev

      match reversedIdents with
      | [] -> []
      | name :: quals ->
        getIfArgsAreFree constructorArgs (fun () -> [ (name, quals |> List.rev) ])
        |> Option.defaultValue []

    | SynPat.Or(lhsPat = left; rhsPat = right) -> (getCasesInPattern left) @ (getCasesInPattern right)
    | SynPat.Ands(patList, _) ->
      patList
      |> List.map (getCasesInPattern >> Set.ofList)
      |> Set.intersectMany
      |> Set.toList
    | SynPat.Paren(innerPat, _) -> getCasesInPattern innerPat
    | _ -> []

  let rec getCasesInClause (x: SynMatchClause) =
    match x with
    | SynMatchClause(pat, None, _, _, _, _) -> getCasesInPattern pat
    | _ -> []

  patMatchExpr.Clauses |> List.collect getCasesInClause |> Set.ofList

let shouldGenerateUnionPatternMatchCases (patMatchExpr: PatternMatchExpr) (entity: FSharpEntity) =
  let caseCount = entity.UnionCases.Count
  let writtenCaseCount = getWrittenCases patMatchExpr |> Set.count
  caseCount > 0 && writtenCaseCount < caseCount

let tryFindPatternMatchExprInBufferAtPos (codeGenService: ICodeGenerationService) (pos: Position) (document: Document) =
  asyncOption {
    let! parseResults = codeGenService.ParseFileInProject(document.FullName)
    let input = parseResults.ParseTree
    return! tryFindPatternMatchExprInParsedInput pos input
  }

let tryFindBarTokenLPosInRange (codeGenService: ICodeGenerationService) (range: Range) (document: Document) =
  tryFindTokenLPosInRange codeGenService range document (fun tokenInfo -> tokenInfo.TokenName = "BAR")

let tryFindInsertionParams (codeGenService: ICodeGenerationService) document (patMatchExpr: PatternMatchExpr) =
  async {
    match List.rev patMatchExpr.Clauses with
    | [] ->
      // Not possible normally
      return None

    | last :: _ ->
      // Interesting cases:
      //
      // (1)
      // match x with
      // | Case1 -> () | Case2 -> ()
      // <indent-here>
      //
      //
      // (2)
      // match x with
      // Case1 -> () | Case2 -> ()
      // <indent-here>
      //
      // (3)
      // match x with
      // | Case1 -> ()
      //      | Case2 -> ()
      //      <indent-here>
      //
      // (4)
      // match x with
      // | Case1 -> ()
      //      |
      //    Case2 -> ()
      //    <indent-here>
      //
      // (5)
      // match x with | Case1 -> () | Case2 -> ()
      //              <indent-here>

      // To know the indentation column,
      // We want to find the first clause of the clauses that are on the same last line
      // All clause f(i) start at line l(i)
      // We want to 'f(k)' such that k = min { i >= k such that l(i) = l(k) }
      // And l(k) = max { l(i) }

      // TODO: report this bug:
      // when renaming it like this: ``list of (clause, line index)``
      // FSI interactive bugs:
      // error FS0192: internal error: binding null type in envBindTypeRef: list of (clause, line index)
      let clauseAndLineIdxList =
        [ for clause in patMatchExpr.Clauses do
            yield clause, clause.Range.StartLine ]

      // Get first of the clauses that are on the same last line
      let lastLineIdx =
        clauseAndLineIdxList |> List.map (fun (_, lineIdx) -> lineIdx) |> Seq.last

      let firstClauseOnLastLine =
        clauseAndLineIdxList
        |> List.find (fun (_, lineIdx) -> lineIdx = lastLineIdx)
        |> fst

      // Find if this clause has a pipe before it on the same line as itself
      let possibleBarLocationRange =
        // Special case (5):
        // 'match-with'/'function' is on the same line as the first clause
        // on the last line
        if patMatchExpr.MatchWithOrFunctionRange.EndLine = firstClauseOnLastLine.Range.StartLine then
          Range.unionRanges patMatchExpr.MatchWithOrFunctionRange.EndRange firstClauseOnLastLine.Range.StartRange
        else
          let clause = firstClauseOnLastLine
          let start = Position.mkPos clause.Range.StartLine 0
          Range.mkRange clause.Range.FileName start clause.Range.Start

      let! barTokenOpt = tryFindBarTokenLPosInRange codeGenService possibleBarLocationRange document

      match barTokenOpt with
      | Some(_, barTokenLPos) ->
        return
          { IndentColumn = barTokenLPos.Column
            InsertionPos = last.Range.End }
          |> Some

      | None ->
        return
          { IndentColumn = firstClauseOnLastLine.Range.StartColumn
            InsertionPos = last.Range.End }
          |> Some
  }


let checkThatPatternMatchExprEndsWithCompleteClause (expr: PatternMatchExpr) =
  match List.rev expr.Clauses with
  | [] -> false
  | lastClause :: _ ->
    match lastClause with
    // In the case when there's nothing in the RHS of the arrow
    // FCS compiler apparently uses this particular AST representation
    // but with unitRange = empty
    | SynMatchClause(resultExpr = SynExpr.Const(SynConst.Unit, unitRange)) ->
      let rhsExprExists =
        unitRange.StartLine <> unitRange.EndLine
        || unitRange.StartColumn <> unitRange.EndColumn

      rhsExprExists && not (FCSPatches.SyntaxTreeOps.synExprContainsError expr.Expr)

    | _ -> not (FCSPatches.SyntaxTreeOps.synExprContainsError expr.Expr)


let tryFindCaseInsertionParamsAtPos (codeGenService: ICodeGenerationService) pos document =
  asyncOption {
    let! patMatchExpr = tryFindPatternMatchExprInBufferAtPos codeGenService pos document

    if checkThatPatternMatchExprEndsWithCompleteClause patMatchExpr then
      let! insertionParams = tryFindInsertionParams codeGenService document patMatchExpr
      return patMatchExpr, insertionParams
    else
      return
        patMatchExpr,
        { InsertionPos = patMatchExpr.Expr.Range.End
          IndentColumn = patMatchExpr.Expr.Range.Start.Column }
  }

let tryFindUnionDefinitionFromPos (codeGenService: ICodeGenerationService) pos document =
  asyncOption {
    let! _, symbolUse = codeGenService.GetSymbolAndUseAtPositionOfKind(document.FullName, pos, SymbolKind.Ident)
    let! patMatchExpr, insertionParams = tryFindCaseInsertionParamsAtPos codeGenService pos document


    let! superficialTypeDefinition =
      asyncOption {
        let! symbolUse = symbolUse

        match symbolUse.Symbol with
        | SymbolPatterns.MemberFunctionOrValue(mfv) -> return Some mfv.FullType.TypeDefinition
        | SymbolPatterns.UnionCase(case) when case.ReturnType.HasTypeDefinition ->
          return Some case.ReturnType.TypeDefinition
        | SymbolPatterns.FSharpEntity(entity, _, _) -> return Some entity
        | _ -> return None
      }

    let! _ = superficialTypeDefinition

    let! realTypeDefinition =
      match superficialTypeDefinition with
      | Some(AbbreviatedType(TypeWithDefinition typeDef)) when typeDef.IsFSharpUnion -> Some typeDef
      | Some(UnionType _) -> superficialTypeDefinition
      | _ -> None

    return patMatchExpr, realTypeDefinition, insertionParams
  }

let private formatCase (ctxt: Context) (case: FSharpUnionCase) =
  let writer = ctxt.Writer

  let caseName =
    match ctxt.Qualifier with
    | None -> case.Name
    | Some qual -> sprintf "%s.%s" qual case.Name

  let paramsPattern =
    let unionCaseFieldsCount = case.Fields.Count

    if unionCaseFieldsCount <= 0 then
      ""
    else
      let fieldNames =
        [| for field in case.Fields ->
             if String.IsNullOrEmpty(field.Name) || isUnnamedUnionCaseField field then
               "_"
             else
               // Lowercase the first character of the field name
               sprintf "%c%s" (Char.ToLower(field.Name.[0])) (field.Name.Substring(1)) |]

      // De-duplicate field names if there are conflicts
      let newFieldNames =
        Seq.unfold
          (fun (i, currentNamesWithIndices as _state) ->
            if i < fieldNames.Length then
              let name = fieldNames.[i]

              let newName, newNamesWithIndices =
                if name = "_" then
                  name, currentNamesWithIndices
                else
                  normalizeArgName currentNamesWithIndices name

              Some(newName, (i + 1, newNamesWithIndices))
            else
              None)
          (0, Map.empty)

      newFieldNames |> String.concat ", " |> sprintf "(%s)"

  writer.WriteLine("")
  writer.Write("| {0}{1} -> {2}", caseName, paramsPattern, ctxt.CaseDefaultValue)

let formatMatchExpr insertionParams (caseDefaultValue: string) (patMatchExpr: PatternMatchExpr) (entity: FSharpEntity) =
  use writer = new ColumnIndentedTextWriter()

  let casesWritten = getWrittenCases patMatchExpr

  let casesToWrite =
    entity.UnionCases
    |> Seq.filter (fun case -> casesWritten |> Set.forall (fun (name, _) -> name <> case.Name))

  // Use the shortest qualified style for further cases
  let shortestQualifier =
    if casesWritten.IsEmpty then
      None
    else
      casesWritten
      |> Seq.minBy (fun (_, lst) -> lst.Length)
      |> snd
      |> function
        | [] -> None
        | lst -> Some(String.concat "." lst)

  let ctxt =
    { UnionTypeName = entity.DisplayName
      Writer = writer
      CaseDefaultValue = caseDefaultValue
      Qualifier = shortestQualifier }

  writer.Indent insertionParams.IndentColumn

  casesToWrite |> Seq.iter (formatCase ctxt)

  writer.Dump()
