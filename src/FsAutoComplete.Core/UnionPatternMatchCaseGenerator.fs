/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/CodeGeneration/UnionPatternMatchCaseGenerator.fs
module FsAutoComplete.UnionPatternMatchCaseGenerator

open System
open System.Diagnostics
open FsAutoComplete.UntypedAstUtils
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<NoEquality; NoComparison>]
type PatternMatchExpr = {
    /// Range of 'match x with' or 'function'
    MatchWithOrFunctionRange: range
    /// The whole pattern match expression
    Expr: SynExpr
    Clauses: SynMatchClause list
}

[<NoComparison>]
type UnionMatchCasesInsertionParams = {
    InsertionPos: pos
    IndentColumn: int
}

[<NoComparison>]
type private Context = {
    Writer: ColumnIndentedTextWriter
    /// A single-line skeleton for each case
    CaseDefaultValue: string
    UnionTypeName: string
    Qualifier: string option
}

let private clauseIsCandidateForCodeGen (cursorPos: pos) (clause: SynMatchClause) =
    //printfn "Checking pos (%+A) inside clause (%+A)..." cursorPos clause
    let rec patIsCandidate (pat: SynPat) =
        match pat with
        | SynPat.Paren(innerPat, _)
        | SynPat.Attrib(innerPat, _, _) -> patIsCandidate innerPat
        | SynPat.Const(_, _) -> false
        | SynPat.Wild(_) -> false
        // TODO: check if we have to handle these cases
        | SynPat.Typed(innerPat, _, _)
        | SynPat.Named(innerPat, _, _, _, _) ->
            patIsCandidate innerPat
        | SynPat.OptionalVal(_, _) ->
            false
        | SynPat.Or(leftPat, rightPat, _) -> patIsCandidate leftPat || patIsCandidate rightPat
        | SynPat.Ands(innerPatList, _) -> List.exists patIsCandidate innerPatList
        // This is the 'hd :: tail -> ...' pattern
        | SynPat.LongIdent(LongIdentWithDots([ident], []), _, _, _, _, _)
            when ident.idText = "op_ColonColon" -> false
        | SynPat.LongIdent(_, _, _, ConstructorPats nestedPats, _, r) ->
            // The cursor should not be in the nested patterns
            rangeContainsPos r cursorPos && List.forall (not << patIsCandidate) nestedPats
        | SynPat.Tuple _
        | SynPat.StructTuple _ -> false
        | SynPat.ArrayOrList _
        | SynPat.Record _
        | SynPat.Null _
        | SynPat.IsInst _
        | SynPat.QuoteExpr _
        | SynPat.DeprecatedCharRange _
        | SynPat.InstanceMember _
        | SynPat.FromParseError _ -> false

    match clause with
    | Clause(pat, _, _, _, _) -> patIsCandidate pat

let private posIsInLhsOfClause (pos: pos) (clause: SynMatchClause) =
    match clause with
    | Clause(_, None, _, patternRange, _) -> rangeContainsPos patternRange pos
    | Clause(_, Some guardExpr, _, patternRange, _) ->
        rangeContainsPos (unionRanges guardExpr.Range patternRange) pos

let private tryFindPatternMatchExprInParsedInput (pos: pos) (parsedInput: ParsedInput) =
    let inline getIfPosInRange range f =
        if rangeContainsPos range pos then f()
        else None

    let rec walkImplFileInput (ParsedImplFileInput(_name, _isScript, _fileName, _scopedPragmas, _hashDirectives, moduleOrNamespaceList, _)) =
        List.tryPick walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace(SynModuleOrNamespace(_, _, _, decls, _, _, _, range)) =
        getIfPosInRange range (fun () ->
            List.tryPick walkSynModuleDecl decls
        )

    and walkSynModuleDecl(decl: SynModuleDecl) =
        getIfPosInRange decl.Range (fun () ->
            match decl with
            | SynModuleDecl.Exception(SynExceptionDefn(_, synMembers, _), _) ->
                List.tryPick walkSynMemberDefn synMembers
            | SynModuleDecl.Let(_isRecursive, bindings, _range) ->
                List.tryPick walkBinding bindings
            | SynModuleDecl.ModuleAbbrev(_lhs, _rhs, _range) ->
                None
            | SynModuleDecl.NamespaceFragment(fragment) ->
                walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(_, _, modules, _, _) ->
                List.tryPick walkSynModuleDecl modules
            | SynModuleDecl.Types(typeDefs, _range) ->
                List.tryPick walkSynTypeDefn typeDefs
            | SynModuleDecl.DoExpr (_, expr, _) ->
                walkExpr expr
            | SynModuleDecl.Attributes _
            | SynModuleDecl.HashDirective _
            | SynModuleDecl.Open _ ->
                None
        )

    and walkSynTypeDefn(TypeDefn(_componentInfo, representation, members, range)) =
        getIfPosInRange range (fun () ->
            walkSynTypeDefnRepr representation
            |> Option.orElse (List.tryPick walkSynMemberDefn members)
        )

    and walkSynTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) =
        getIfPosInRange typeDefnRepr.Range (fun () ->
            match typeDefnRepr with
            | SynTypeDefnRepr.ObjectModel(_kind, members, _range) ->
                List.tryPick walkSynMemberDefn members
            | SynTypeDefnRepr.Simple _
            | SynTypeDefnRepr.Exception _ -> None
        )

    and walkSynMemberDefn (memberDefn: SynMemberDefn) =
        getIfPosInRange memberDefn.Range (fun () ->
            match memberDefn with
            | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, _range) ->
                None
            | SynMemberDefn.AutoProperty(_attributes, _isStatic, _id, _type, _memberKind, _memberFlags, _xmlDoc, _access, expr, _r1, _r2) ->
                walkExpr expr
            | SynMemberDefn.Interface(_, members, _range) ->
                Option.bind (List.tryPick walkSynMemberDefn) members
            | SynMemberDefn.Member(binding, _range) ->
                walkBinding binding
            | SynMemberDefn.NestedType(typeDef, _access, _range) ->
                walkSynTypeDefn typeDef
            | SynMemberDefn.ValField(_field, _range) ->
                None
            | SynMemberDefn.LetBindings(bindings, _isStatic, _isRec, _range) ->
                List.tryPick walkBinding bindings
            | SynMemberDefn.Open _
            | SynMemberDefn.ImplicitInherit _
            | SynMemberDefn.Inherit _
            | SynMemberDefn.ImplicitCtor _ ->
                None
        )

    and walkBinding (Binding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, _valData, _headPat, _retTy, expr, _bindingRange, _seqPoint) as binding) =
        getIfPosInRange binding.RangeOfBindingAndRhs (fun () -> walkExpr expr)

    and walkExpr expr =
        getIfPosInRange expr.Range (fun () ->
            match expr with
            | SynExpr.Quote(synExpr1, _, synExpr2, _, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Const(_synConst, _range) ->
                None

            | SynExpr.Typed(synExpr, _, _)
            | SynExpr.Paren(synExpr, _, _, _)
            | SynExpr.New(_, _, synExpr, _)
            | SynExpr.ArrayOrListOfSeqExpr(_, synExpr, _)
            | SynExpr.CompExpr(_, _, synExpr, _)
            | SynExpr.Lambda(_, _, _, synExpr, _)
            | SynExpr.Lazy(synExpr, _)
            | SynExpr.Do(synExpr, _)
            | SynExpr.Assert(synExpr, _) ->
                walkExpr synExpr

            | SynExpr.Tuple(synExprList, _, _range)
            | SynExpr.ArrayOrList(_, synExprList, _range) ->
                List.tryPick walkExpr synExprList

            | SynExpr.Record(_inheritOpt, copyOpt, fields, _range) ->
                let fieldExprList =
                    fields
                    |> List.choose (fun (_, fieldExprOpt, _) -> fieldExprOpt)

                match copyOpt with
                | Some(copyExpr, _blockSeparator) ->
                    List.tryPick walkExpr (copyExpr :: fieldExprList)
                | None ->
                    List.tryPick walkExpr fieldExprList

            | SynExpr.ObjExpr(_ty, _baseCallOpt, binds, ifaces, _range1, _range2) ->
                List.tryPick walkBinding binds
                |> Option.orElse (List.tryPick walkSynInterfaceImpl ifaces)

            | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]
            | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _isFromSource, _synPat, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.For(_sequencePointInfoForForLoop, _ident, synExpr1, _, synExpr2, synExpr3, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]

            | SynExpr.MatchLambda(isExnMatch,
                                  functionKeywordRange,
                                  synMatchClauseList,
                                  _, _wholeExprRange) as matchLambdaExpr ->
                synMatchClauseList
                |> List.tryPick (fun (Clause(_, _, e, _, _)) -> walkExpr e)
                |> Option.orTry (fun () ->
                    if isExnMatch then
                        None
                    else
                       let currentClause = List.tryFind (posIsInLhsOfClause pos) synMatchClauseList
                       if currentClause |> Option.map (clauseIsCandidateForCodeGen pos) |> Option.getOrElse false then
                            { MatchWithOrFunctionRange = functionKeywordRange
                              Expr = matchLambdaExpr
                              Clauses = synMatchClauseList }
                            |> Some
                       else None
                )

            | SynExpr.Match(sequencePointInfoForBinding, synExpr, synMatchClauseList, isExnMatch, _range) as matchExpr ->
                getIfPosInRange synExpr.Range (fun () -> walkExpr synExpr)
                |> Option.orTry (fun () ->
                    synMatchClauseList
                    |> List.tryPick (fun (Clause(_, _, e, _, _)) -> walkExpr e)
                )
                |> Option.orTry (fun () ->
                    if isExnMatch then
                        None
                    else
                       let currentClause = List.tryFind (posIsInLhsOfClause pos) synMatchClauseList
                       if currentClause |> Option.map (clauseIsCandidateForCodeGen pos) |> Option.getOrElse false then
                            match sequencePointInfoForBinding with
                            | SequencePointAtBinding range ->
                                { MatchWithOrFunctionRange = range
                                  Expr = matchExpr
                                  Clauses = synMatchClauseList }
                                |> Some
                            | _ -> None
                        else
                            None
                )

            | SynExpr.App(_exprAtomicFlag, _isInfix, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.TypeApp(synExpr, _, _synTypeList, _commas, _, _, _range) ->
                walkExpr synExpr

            | SynExpr.LetOrUse(_, _, synBindingList, synExpr, _range) ->
                Option.orElse (List.tryPick walkBinding synBindingList) (walkExpr synExpr)

            | SynExpr.TryWith(synExpr, _range, _synMatchClauseList, _range2, _range3, _sequencePointInfoForTry, _sequencePointInfoForWith) ->
                walkExpr synExpr

            | SynExpr.TryFinally(synExpr1, synExpr2, _range, _sequencePointInfoForTry, _sequencePointInfoForFinally) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | Sequentials exprs ->
                List.tryPick walkExpr exprs

            | SynExpr.IfThenElse(synExpr1, synExpr2, synExprOpt, _sequencePointInfoForBinding, _isRecovery, _range, _range2) ->
                match synExprOpt with
                | Some synExpr3 ->
                    List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]
                | None ->
                    List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Ident(_ident) ->
                None
            | SynExpr.LongIdent(_, _longIdent, _altNameRefCell, _range) ->
                None

            | SynExpr.LongIdentSet(_longIdent, synExpr, _range) ->
                walkExpr synExpr
            | SynExpr.DotGet(synExpr, _dotm, _longIdent, _range) ->
                walkExpr synExpr

            | SynExpr.DotSet(synExpr1, _longIdent, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.DotIndexedGet(synExpr, IndexerArgList synExprList, _range, _range2) ->
                Option.orElse (walkExpr synExpr) (List.tryPick walkExpr synExprList)

            | SynExpr.DotIndexedSet(synExpr1, IndexerArgList synExprList, synExpr2, _, _range, _range2) ->
                [ yield synExpr1
                  yield! synExprList
                  yield synExpr2 ]
                |> List.tryPick walkExpr

            | SynExpr.JoinIn(synExpr1, _range, synExpr2, _range2) ->
                List.tryPick walkExpr [synExpr1; synExpr2]
            | SynExpr.NamedIndexedPropertySet(_longIdent, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.DotNamedIndexedPropertySet(synExpr1, _longIdent, synExpr2, synExpr3, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]

            | SynExpr.TypeTest(synExpr, _synType, _range)
            | SynExpr.Upcast(synExpr, _synType, _range)
            | SynExpr.Downcast(synExpr, _synType, _range) ->
                walkExpr synExpr
            | SynExpr.InferredUpcast(synExpr, _range)
            | SynExpr.InferredDowncast(synExpr, _range) ->
                walkExpr synExpr
            | SynExpr.AddressOf(_, synExpr, _range, _range2) ->
                walkExpr synExpr
            | SynExpr.TraitCall(_synTyparList, _synMemberSig, synExpr, _range) ->
                walkExpr synExpr

            | SynExpr.Null(_range)
            | SynExpr.ImplicitZero(_range) ->
                None

            | SynExpr.YieldOrReturn(_, synExpr, _range)
            | SynExpr.YieldOrReturnFrom(_, synExpr, _range)
            | SynExpr.DoBang(synExpr, _range) ->
                walkExpr synExpr

            | SynExpr.LetOrUseBang(_sequencePointInfoForBinding, _, _, _synPat, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.LibraryOnlyILAssembly _
            | SynExpr.LibraryOnlyStaticOptimization _
            | SynExpr.LibraryOnlyUnionCaseFieldGet _
            | SynExpr.LibraryOnlyUnionCaseFieldSet _ ->
                None
            | SynExpr.ArbitraryAfterError(_debugStr, _range) ->
                None

            | SynExpr.FromParseError(synExpr, _range)
            | SynExpr.DiscardAfterMissingQualificationAfterDot(synExpr, _range) ->
                walkExpr synExpr

            | _ -> None
        )

    and walkSynInterfaceImpl (InterfaceImpl(_synType, synBindings, _range)) =
        List.tryPick walkBinding synBindings

    match parsedInput with
    | ParsedInput.SigFile _input -> None
    | ParsedInput.ImplFile input -> walkImplFileInput input

let getWrittenCases (patMatchExpr: PatternMatchExpr) =
    let rec checkPattern pat =
        match pat with
        | SynPat.Const(_const, _) -> false
        // TODO: figure out if these cases are supposed to happen or not
        | SynPat.Or(_, _, _)
        | SynPat.Ands(_, _)
        | SynPat.LongIdent(_, _, _, _, _, _)
        | SynPat.ArrayOrList(_, _, _)
        | SynPat.Null(_)
        | SynPat.InstanceMember(_, _, _, _, _)
        | SynPat.IsInst(_, _)
        | SynPat.QuoteExpr(_, _)
        | SynPat.DeprecatedCharRange(_, _, _)
        | SynPat.FromParseError(_, _) -> false

        | SynPat.Tuple(innerPatList, _)
        | SynPat.StructTuple(innerPatList, _) -> List.forall checkPattern innerPatList

        | SynPat.Record(recordInnerPatList, _) ->
            recordInnerPatList
            |> List.map (fun (_, innerPat) -> innerPat)
            |> List.forall checkPattern

        | SynPat.OptionalVal(_, _) -> true
        | SynPat.Wild(_) -> true
        | SynPat.Named(innerPat, _, _, _, _)
        | SynPat.Typed(innerPat, _, _)
        | SynPat.Attrib(innerPat, _, _)
        | SynPat.Paren(innerPat, _) -> checkPattern innerPat

    let getIfArgsAreFree constructorArgs func =
        match constructorArgs with
        | SynConstructorArgs.Pats patList ->
            if List.forall checkPattern patList then
                Some (func())
            else
                None
        | SynConstructorArgs.NamePatPairs(namedPatList, _) ->
            let patList =
                namedPatList
                |> List.unzip
                |> (fun (_, pat) -> pat)

            if List.forall checkPattern patList then
                Some (func())
            else
                None

    let rec getCasesInPattern (pat: SynPat) =
        match pat with
        | SynPat.LongIdent(LongIdentWithDots(unionCaseLongIdent, _), _, _,
                           constructorArgs, _, _) ->
            // Get list of qualifiers, this can be checked for length later.
            let reversedIdents =
                unionCaseLongIdent
                |> List.map (fun id -> id.idText)
                |> List.rev
            match reversedIdents with
            | [] -> []
            | name::quals ->
                getIfArgsAreFree constructorArgs (fun () ->
                    [ (name, quals |> List.rev) ]
                )
                |> Option.getOrElse []

        | SynPat.Or(left, right, _) ->
            (getCasesInPattern left) @ (getCasesInPattern right)
        | SynPat.Ands(patList, _) ->
            patList
            |> List.map (getCasesInPattern >> Set.ofList)
            |> Set.intersectMany
            |> Set.toList
        | SynPat.Paren(innerPat, _) -> getCasesInPattern innerPat
        | _ -> []

    let rec getCasesInClause (x: SynMatchClause) =
        match x with
        | Clause(pat, None, _, _, _) -> getCasesInPattern pat
        | _ -> []

    patMatchExpr.Clauses
    |> List.collect (getCasesInClause)
    |> Set.ofList

let shouldGenerateUnionPatternMatchCases (patMatchExpr: PatternMatchExpr) (entity: FSharpEntity) =
    let caseCount = entity.UnionCases.Count
    let writtenCaseCount =
        getWrittenCases patMatchExpr
        |> Set.count
    caseCount > 0 && writtenCaseCount < caseCount

let tryFindPatternMatchExprInBufferAtPos (codeGenService: CodeGenerationService) (pos: pos) (document : Document) =
    asyncMaybe {
        let! parseResults = codeGenService.ParseFileInProject(document.FullName)
        let! input = parseResults.ParseTree
        return! tryFindPatternMatchExprInParsedInput pos input
    }

let tryFindBarTokenLPosInRange (codeGenService: CodeGenerationService) (range: range) (document: Document) =
    tryFindTokenLPosInRange codeGenService range document (fun tokenInfo -> tokenInfo.TokenName = "BAR")

let tryFindInsertionParams (codeGenService: CodeGenerationService) document (patMatchExpr: PatternMatchExpr) =
    match List.rev patMatchExpr.Clauses with
    | [] ->
        // Not possible normally
        None

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
            [
                for clause in patMatchExpr.Clauses do
                    yield clause, clause.Range.StartLine
            ]

        // Get first of the clauses that are on the same last line
        let lastLineIdx =
            clauseAndLineIdxList
            |> List.map (fun (_, lineIdx) -> lineIdx)
            |> Seq.last

        let firstClauseOnLastLine =
            clauseAndLineIdxList
            |> List.find (fun (_, lineIdx) -> lineIdx = lastLineIdx)
            |> fst

        // Find if this clause has a pipe before it on the same line as itself
        let possibleBarLocationRange =
            // Special case (5):
            // 'match-with'/'function' is on the same line as the first clause
            // on the last line
            if patMatchExpr.MatchWithOrFunctionRange.EndLine
               = firstClauseOnLastLine.Range.StartLine
            then
                unionRanges
                    patMatchExpr.MatchWithOrFunctionRange.EndRange
                    firstClauseOnLastLine.Range.StartRange
            else
                let clause = firstClauseOnLastLine
                let start = mkPos clause.Range.StartLine 0
                mkFileIndexRange (clause.Range.FileIndex) start clause.Range.Start

        let barTokenOpt =
            tryFindBarTokenLPosInRange codeGenService possibleBarLocationRange document

        match barTokenOpt with
        | Some(_, barTokenLPos) ->
            { IndentColumn = barTokenLPos.Column
              InsertionPos = last.Range.End }
            |> Some

        | None ->
            { IndentColumn = firstClauseOnLastLine.Range.StartColumn
              InsertionPos = last.Range.End }
            |> Some


let checkThatPatternMatchExprEndsWithCompleteClause (expr: PatternMatchExpr) =
    match List.rev expr.Clauses with
    | [] -> false
    | lastClause :: _ ->
        match lastClause with
        // In the case when there's nothing in the RHS of the arrow
        // FCS compiler apparently uses this particular AST representation
        // but with unitRange = empty
        | Clause(_, _, SynExpr.Const(SynConst.Unit, unitRange), _, _) ->
            let rhsExprExists =
                unitRange.StartLine <> unitRange.EndLine ||
                unitRange.StartColumn <> unitRange.EndColumn

            rhsExprExists && not (synExprContainsError expr.Expr)

        | _ -> not (synExprContainsError expr.Expr)


let tryFindCaseInsertionParamsAtPos (codeGenService: CodeGenerationService) pos document =
    asyncMaybe {
        let! patMatchExpr = tryFindPatternMatchExprInBufferAtPos codeGenService pos document

        if checkThatPatternMatchExprEndsWithCompleteClause patMatchExpr then
            let! insertionParams = tryFindInsertionParams codeGenService document patMatchExpr
            return patMatchExpr, insertionParams
        else
            return! None
    }

let tryFindUnionDefinitionFromPos (codeGenService: CodeGenerationService) pos document =
    asyncMaybe {
        let! patMatchExpr, insertionParams = tryFindCaseInsertionParamsAtPos codeGenService pos document
        let! symbol, symbolUse = codeGenService.GetSymbolAndUseAtPositionOfKind(document.FullName, pos, SymbolKind.Ident)

        let! superficialTypeDefinition =
            match symbolUse.Symbol with
            | TypedAstPatterns.UnionCase(case) when case.ReturnType.HasTypeDefinition ->
                Some case.ReturnType.TypeDefinition
            | TypedAstPatterns.FSharpEntity(entity, _, _) -> Some entity
            | _ -> None

        let! realTypeDefinition =
            match superficialTypeDefinition with
            | AbbreviatedType(TypeWithDefinition typeDef) when typeDef.IsFSharpUnion ->
                Some typeDef
            | UnionType(_) -> Some superficialTypeDefinition
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
        let unionCaseFieldsCount = case.UnionCaseFields.Count
        if unionCaseFieldsCount <= 0 then
            ""
        else
            let fieldNames =
                [|
                    for field in case.UnionCaseFields ->
                        if String.IsNullOrEmpty(field.Name) || isUnnamedUnionCaseField field then
                            "_"
                        else
                            // Lowercase the first character of the field name
                            sprintf "%c%s" (Char.ToLower(field.Name.[0])) (field.Name.Substring(1))
                |]

            // De-duplicate field names if there are conflicts
            let newFieldNames =
                Seq.unfold (fun ((i, currentNamesWithIndices) as _state) ->
                    if i < fieldNames.Length then
                        let name = fieldNames.[i]
                        let newName, newNamesWithIndices =
                            if name = "_" then
                                name, currentNamesWithIndices
                            else
                                normalizeArgName currentNamesWithIndices name

                        Some(newName, (i+1, newNamesWithIndices))
                    else
                        None
                ) (0, Map.empty)

            newFieldNames
            |> String.concat ", "
            |> sprintf "(%s)"

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
        if casesWritten.IsEmpty
        then None
        else
            casesWritten
            |> Seq.minBy (fun (_, lst) -> lst.Length)
            |> snd
            |> function
               | [] -> None
               | lst -> Some (String.concat "." lst)

    let ctxt =
        { UnionTypeName = entity.DisplayName
          Writer = writer
          CaseDefaultValue = caseDefaultValue
          Qualifier = shortestQualifier }

    writer.Indent insertionParams.IndentColumn

    casesToWrite
    |> Seq.iter (formatCase ctxt)

    writer.Dump()