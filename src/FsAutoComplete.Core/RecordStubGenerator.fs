/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/CodeGeneration/RecordStubGenerator.fs
module FsAutoComplete.RecordStubGenerator

open FsAutoComplete.UntypedAstUtils
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Diagnostics

// Algorithm
// [x] Make sure '}' is the last token of the expression
// [x] Make sure that the last field, if it exists, is assigned an expression
// [x] Careful when manipulating ranges returned by FCS which are sometimes incorrect
// [x] If fields is empty, then insert after '{' or 'with'
// [x] If fields are not empty, insert after the last field's expression

[<NoEquality; NoComparison>]
type RecordExpr = {
    Expr: SynExpr
    CopyExprOption: option<SynExpr * BlockSeparator>
    FieldExprList: (RecordFieldName * SynExpr option * BlockSeparator option) list
}

[<RequireQualifiedAccess>]
type PositionKind =
    /// let record = {<insert-here>}
    | AfterLeftBrace
    /// let y = { x with<insert-here> }
    | AfterCopyExpression
    /// let x = { Field1 = expr<insert-here> }
    | AfterLastField

[<NoComparison>]
type RecordStubsInsertionParams =
    {
        Kind: PositionKind
        InsertionPos: pos
        IndentColumn: int
    }
    static member TryCreateFromRecordExpression (expr: RecordExpr) =
        match expr.FieldExprList with
        | [] ->
            match expr.CopyExprOption with
            | None ->
                let exprRange = expr.Expr.Range
                let pos = Pos.fromZ (exprRange.StartLine - 1) (exprRange.StartColumn + 1)
                { Kind = PositionKind.AfterLeftBrace
                  IndentColumn = pos.Column + 1
                  InsertionPos = pos }
                |> Some
            | Some(_toCopy, (withSeparator, _)) ->
                { Kind = PositionKind.AfterCopyExpression
                  IndentColumn = withSeparator.End.Column + 1
                  InsertionPos = withSeparator.End }
                |> Some

        | _ ->
            // To know the indentation column,
            // We want to find the first field to be on the last field line
            // All fields f(i) start at line l(i)
            // We want to 'f(k)' such that k = min { i >= k such that l(i) = l(k) }
            // And l(k) = max { l(i) }
            let fieldAndStartColumnAndLineIdxList =
                expr.FieldExprList
                |> List.choose (fun fieldInfo ->
                    match fieldInfo with
                    | (LongIdentWithDots(identHead :: _, _), true as _isSyntacticallyCorrect),
                       _exprOpt, _semiColonOpt ->
                        let fieldLine = identHead.idRange.StartLine
                        let indentColumn = identHead.idRange.StartColumn
                        Some (fieldInfo, indentColumn, fieldLine)
                    | _ -> None
                )

            maybe {
                let! maxLineIdx =
                    fieldAndStartColumnAndLineIdxList
                    |> List.unzip3
                    |> (fun (_, _, lineIdx) -> lineIdx)
                    |> (function
                        | [] -> None
                        | nonEmptyList -> Some (List.max nonEmptyList)
                    )

                let indentColumn =
                    fieldAndStartColumnAndLineIdxList
                    |> List.pick (fun (_, indentColumn, lineIdx) ->
                        if lineIdx = maxLineIdx
                        then Some indentColumn
                        else None
                    )

                let lastFieldInfo = Seq.last expr.FieldExprList

                return!
                    match lastFieldInfo with
                    | _recordFieldName, None, _ -> None
                    | (LongIdentWithDots(_ :: _, _), true as _isSyntacticallyCorrect),
                      Some expr, semiColonOpt ->
                        match semiColonOpt with
                        | None ->
                            { Kind = PositionKind.AfterLastField
                              IndentColumn = indentColumn
                              InsertionPos = expr.Range.End }
                            |> Some
                        | Some (_range, Some semiColonEndPos) ->
                            { Kind = PositionKind.AfterLastField
                              IndentColumn = indentColumn
                              InsertionPos = semiColonEndPos }
                            |> Some
                        | _ -> None
                    | _ -> None
            }


[<NoComparison>]
type private Context = {
    Writer: ColumnIndentedTextWriter
    /// A single-line skeleton for each field
    FieldDefaultValue: string
    RecordTypeName: string
    RequireQualifiedAccess: bool
}

let private formatField (ctxt: Context) prependNewLine
    prependExtraSpace (field: FSharpField) =
    let writer = ctxt.Writer

    if prependNewLine then
        writer.WriteLine("")

    let name =
        if ctxt.RequireQualifiedAccess
        then sprintf "%s.%s" ctxt.RecordTypeName field.Name
        else field.Name

    let prependedSpace = if prependExtraSpace then " " else ""

    writer.Write("{0}{1} = {2}", prependedSpace, name, ctxt.FieldDefaultValue)

let formatRecord (insertionPos: RecordStubsInsertionParams) (fieldDefaultValue: string)
                 (entity: FSharpEntity)
                 (fieldsWritten: (RecordFieldName * _ * Option<_>) list) =
    Debug.Assert(entity.IsFSharpRecord, "Entity has to be an F# record.")
    use writer = new ColumnIndentedTextWriter()
    let ctxt =
        { RecordTypeName = entity.DisplayName
          RequireQualifiedAccess = hasAttribute<RequireQualifiedAccessAttribute> entity.Attributes
          Writer = writer
          FieldDefaultValue = fieldDefaultValue }

    let fieldsWritten =
        fieldsWritten
        |> List.collect (function
            ((fieldName, _), _, _) ->
                // Extract <Field> in qualified identifiers: A.B.<Field> = ...
                if fieldName.Lid.Length > 0 then
                    [(fieldName.Lid.Item (fieldName.Lid.Length - 1)).idText]
                else [])
        |> Set.ofList

    let fieldsToWrite =
        entity.FSharpFields
        |> Seq.filter (fun field -> not <| fieldsWritten.Contains field.Name)

    writer.Indent insertionPos.IndentColumn

    match List.ofSeq fieldsToWrite with
    | [] -> ()
    | firstField :: otherFields ->
        let prependNewLineToFstField, prependNewLineToOtherFields =
            match insertionPos.Kind with
            | PositionKind.AfterLastField -> true, true
            | PositionKind.AfterLeftBrace
            | PositionKind.AfterCopyExpression -> false, true

        let prependExtraSpaceToFstField =
            match insertionPos.Kind with
            | PositionKind.AfterCopyExpression
            | PositionKind.AfterLeftBrace -> true
            | PositionKind.AfterLastField -> false

        formatField ctxt prependNewLineToFstField prependExtraSpaceToFstField firstField
        otherFields
        |> List.iter (formatField ctxt prependNewLineToOtherFields false)

    writer.Dump()

let private tryFindRecordBindingInParsedInput (pos: pos) (parsedInput: ParsedInput) =
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
            | SynModuleDecl.Let(_isRecursive, bindings, _) ->
                List.tryPick walkBinding bindings
            | SynModuleDecl.ModuleAbbrev _ ->
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

    and walkBinding (Binding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, _valData, _headPat, retTy, expr, _bindingRange, _seqPoint) as binding) =
        getIfPosInRange binding.RangeOfBindingAndRhs (fun () ->
            match retTy with
            | Some(SynBindingReturnInfo(_ty, _range, _attributes)) ->
                match expr with
                // Situation 1:
                // NOTE: 'buggy' parse tree when a type annotation is given before the '=' (but workable corner case)
                // Ex: let x: MyRecord = { f1 = e1; f2 = e2; ... }
                | SynExpr.Typed(SynExpr.Record(_inheritOpt, copyOpt, fields, _range0), synType, _range1) ->
                    [
                        // Get type annotation range
                        yield synType.Range

                        // Get all field identifiers ranges
                        for (recordFieldIdent, _), _, _ in fields do
                            yield recordFieldIdent.Range
                    ]
                    |> List.tryPick (fun range ->
                        getIfPosInRange range (fun () ->
                            Some { Expr = expr
                                   CopyExprOption = copyOpt
                                   FieldExprList = fields }
                        )
                    )
                    |> Option.orTry (fun () ->
                        fields
                        |> List.tryPick walkRecordField
                    )
                | _ -> walkExpr expr
            | None ->
                walkExpr expr
        )

    and walkExpr expr =
        getIfPosInRange expr.Range (fun () ->
            match expr with
            | SynExpr.Quote(synExpr1, _, synExpr2, _, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.Const(_synConst, _range) ->
                None

            | SynExpr.Typed(synExpr, synType, _) ->
                match synExpr with
                // Situation 2: record is typed on the right
                // { f1 = e1; f2 = e2; ... } : MyRecord
                | SynExpr.Record(_inheritOpt, copyOpt, fields, _range) ->
                    [
                        // Get type annotation range
                        yield synType.Range

                        // Get all field identifiers ranges
                        for (recordFieldIdent, _), _, _ in fields do
                            yield recordFieldIdent.Range
                    ]
                    |> List.tryPick (fun range ->
                        getIfPosInRange range (fun () ->
                            Some { Expr = expr
                                   CopyExprOption = copyOpt
                                   FieldExprList = fields }
                        )
                    )
                    |> Option.orTry (fun () ->
                        fields
                        |> List.tryPick walkRecordField
                    )
                | _ -> walkExpr synExpr

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
                fields
                |> List.tryPick (function
                    | (recordFieldIdent, _), _, _
                        when rangeContainsPos (recordFieldIdent.Range) pos ->
                        Some { Expr = expr
                               CopyExprOption = copyOpt
                               FieldExprList = fields }
                    | field -> walkRecordField field
                )

            | SynExpr.ObjExpr(_ty, _baseCallOpt, binds, ifaces, _range1, _range2) ->
                List.tryPick walkBinding binds
                |> Option.orElse (List.tryPick walkSynInterfaceImpl ifaces)

            | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]
            | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _isFromSource, _synPat, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.For(_sequencePointInfoForForLoop, _ident, synExpr1, _, synExpr2, synExpr3, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]

            | SynExpr.MatchLambda(_isExnMatch, _argm, synMatchClauseList, _spBind, _wholem) ->
                synMatchClauseList |> List.tryPick (fun (Clause(_, _, e, _, _)) -> walkExpr e)
            | SynExpr.Match(_sequencePointInfoForBinding, synExpr, synMatchClauseList, _, _range) ->
                walkExpr synExpr
                |> Option.orElse (synMatchClauseList |> List.tryPick (fun (Clause(_, _, e, _, _)) -> walkExpr e))

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

    and walkRecordField (_recordFieldName, synExprOpt, _) =
        match synExprOpt with
        | Some synExpr when rangeContainsPos synExpr.Range pos -> walkExpr synExpr
        | _ -> None

    and walkSynInterfaceImpl (InterfaceImpl(_synType, synBindings, _range)) =
        List.tryPick walkBinding synBindings

    match parsedInput with
    | ParsedInput.SigFile _input -> None
    | ParsedInput.ImplFile input -> walkImplFileInput input

let tryFindRecordExprInBufferAtPos (codeGenService: CodeGenerationService) (pos: pos) (document : Document) =
    asyncMaybe {
        let! parseResults = codeGenService.ParseFileInProject(document.FullName)

        return!
            parseResults.ParseTree
            |> Option.bind (tryFindRecordBindingInParsedInput pos)
    }

let checkThatRecordExprEndsWithRBrace (codeGenService: CodeGenerationService) (document : Document) (expr: RecordExpr) =

    maybe {
        let! rangeWhereToLookForEnclosingRBrace =
            match expr.FieldExprList with
            | [] -> Some expr.Expr.Range
            | _ ->
                let lastField = Seq.last expr.FieldExprList
                match lastField with
                | _fieldName, Some _fieldExpr, Some (semiColonRange, Some _semiColonEndPos) ->
                    // The last field ends with a ';'
                    // Look here: { field = expr;<start> ... }<end>
                    Some (unionRanges semiColonRange.EndRange expr.Expr.Range.EndRange)


                | _fieldName, Some fieldExpr, _ ->
                    // The last field doesn't end with a ';'
                    // Look here: { field = expr<start> ... }<end>
                    Some (unionRanges fieldExpr.Range.EndRange expr.Expr.Range.EndRange)

                | _fieldName, None, _ ->
                    // We don't allow generation when the last field isn't assigned an expression
                    None

        return! tryFindTokenLPosInRange codeGenService rangeWhereToLookForEnclosingRBrace document
                (fun tokenInfo -> tokenInfo.TokenName = "RBRACE")
    }
    |> Option.isSome

let tryFindStubInsertionParamsAtPos (codeGenService: CodeGenerationService) (pos: pos) (document : Document) =
    asyncMaybe {
        let! recordExpression = tryFindRecordExprInBufferAtPos codeGenService pos document
        if checkThatRecordExprEndsWithRBrace codeGenService document recordExpression then
            let! insertionPos = RecordStubsInsertionParams.TryCreateFromRecordExpression recordExpression
            return recordExpression, insertionPos
        else
            return! None
    }

// Check whether the record has been fully defined
let shouldGenerateRecordStub (recordExpr: RecordExpr) (entity: FSharpEntity) =
    let fieldCount = entity.FSharpFields.Count
    let writtenFieldCount = recordExpr.FieldExprList.Length
    fieldCount > 0 && writtenFieldCount < fieldCount

let tryFindRecordDefinitionFromPos (codeGenService: CodeGenerationService) (pos: pos) (document : Document) =
    asyncMaybe {
        let! recordExpression, insertionPos =
            tryFindStubInsertionParamsAtPos codeGenService pos document

        let! symbol, symbolUse =
            codeGenService.GetSymbolAndUseAtPositionOfKind(document.FullName, pos, SymbolKind.Ident)

        match symbolUse.Symbol with
        | :? FSharpEntity as entity when entity.IsFSharpRecord && entity.DisplayName = symbol.Text ->
            return! Some (recordExpression, entity, insertionPos)

        | :? FSharpField as field when
            field.DeclaringEntity.IsFSharpRecord &&
            field.DisplayName = symbol.Text ->
                return! Some (recordExpression, field.DeclaringEntity, insertionPos)
        | _ ->
            return! None
    }
