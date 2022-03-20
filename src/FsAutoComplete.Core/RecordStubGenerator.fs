/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/CodeGeneration/RecordStubGenerator.fs
module FsAutoComplete.RecordStubGenerator

open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open System.Diagnostics
open FsAutoComplete.CodeGenerationUtils
open FSharp.Compiler.Symbols

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
    FieldExprList: SynExprRecordField list
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
        InsertionPos: Position
        IndentColumn: int
    }
    static member TryCreateFromRecordExpression (expr: RecordExpr) =
        match expr.FieldExprList with
        | [] ->
            match expr.CopyExprOption with
            | None ->
                let exprRange = expr.Expr.Range
                let pos = Position.fromZ (exprRange.StartLine - 1) (exprRange.StartColumn + 1)
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
                    | SynExprRecordField(fieldName = (LongIdentWithDots(identHead :: _, _), true)) ->
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
                    | SynExprRecordField(expr = None) -> None
                    | SynExprRecordField(fieldName = (LongIdentWithDots(_ :: _, _), true); expr = Some expr; blockSeparator = semiColonOpt) ->
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
                 (fieldsWritten: SynExprRecordField list) =
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
            SynExprRecordField(fieldName = (fieldName, _)) ->
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

let private tryFindRecordBindingInParsedInput (pos: Position) (parsedInput: ParsedInput) =
    let inline getIfPosInRange range f =
        if Range.rangeContainsPos range pos then f()
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
            | SynModuleDecl.Exception(exnDefn = SynExceptionDefn(members = synMembers)) ->
                List.tryPick walkSynMemberDefn synMembers
            | SynModuleDecl.Let(_isRecursive, bindings, _) ->
                List.tryPick walkBinding bindings
            | SynModuleDecl.ModuleAbbrev _ ->
                None
            | SynModuleDecl.NamespaceFragment(fragment) ->
                walkSynModuleOrNamespace fragment
            | SynModuleDecl.NestedModule(decls = modules) ->
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

    and walkSynTypeDefn(SynTypeDefn(typeRepr = representation; members = members; implicitConstructor = implicitCtor; range = range)) =
        getIfPosInRange range (fun () ->
            walkSynTypeDefnRepr representation
            |> Option.orElseWith (fun _ -> Option.bind walkSynMemberDefn implicitCtor)
            |> Option.orElseWith (fun _ -> List.tryPick walkSynMemberDefn members)
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
            | SynMemberDefn.AutoProperty(synExpr = expr) ->
                walkExpr expr
            | SynMemberDefn.Interface(members = members) ->
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

    and walkBinding (SynBinding(returnInfo = retTy; expr = expr) as binding) =
        getIfPosInRange binding.RangeOfBindingWithRhs (fun () ->
            match retTy with
            | Some(SynBindingReturnInfo(_)) ->
                match expr with
                // Situation 1:
                // NOTE: 'buggy' parse tree when a type annotation is given before the '=' (but workable corner case)
                // Ex: let x: MyRecord = { f1 = e1; f2 = e2; ... }
                | SynExpr.Typed(expr = SynExpr.Record(recordFields = fields; copyInfo = copyOpt;); targetType = synType) ->
                    [
                        // Get type annotation range
                        yield synType.Range

                        // Get all field identifiers ranges
                        for (SynExprRecordField(fieldName = (recordFieldIdent, _))) in fields do
                            yield recordFieldIdent.Range
                    ]
                    |> List.tryPick (fun range ->
                        getIfPosInRange range (fun () ->
                            Some { Expr = expr
                                   CopyExprOption = copyOpt
                                   FieldExprList = fields }
                        )
                    )
                    |> Option.orElseWith (fun () ->
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
                        for (SynExprRecordField(fieldName = (recordFieldIdent, _))) in fields do
                            yield recordFieldIdent.Range
                    ]
                    |> List.tryPick (fun range ->
                        getIfPosInRange range (fun () ->
                            Some { Expr = expr
                                   CopyExprOption = copyOpt
                                   FieldExprList = fields }
                        )
                    )
                    |> Option.orElseWith (fun () ->
                        fields
                        |> List.tryPick walkRecordField
                    )
                | _ -> walkExpr synExpr

            | SynExpr.Paren(synExpr, _, _, _)
            | SynExpr.New(_, _, synExpr, _)
            | SynExpr.ArrayOrListComputed(_, synExpr, _)
            | SynExpr.ComputationExpr(_, synExpr, _)
            | SynExpr.Lambda(body = synExpr)
            | SynExpr.Lazy(synExpr, _)
            | SynExpr.Do(synExpr, _)
            | SynExpr.Assert(synExpr, _) ->
                walkExpr synExpr

            | SynExpr.Tuple(_, synExprList, _, _range)
            | SynExpr.ArrayOrList(_, synExprList, _range) ->
                List.tryPick walkExpr synExprList

            | SynExpr.Record(_inheritOpt, copyOpt, fields, _range) ->
                fields
                |> List.tryPick (function
                    | SynExprRecordField(fieldName = (recordFieldIdent, _))
                        when Range.rangeContainsPos (recordFieldIdent.Range) pos ->
                        Some { Expr = expr
                               CopyExprOption = copyOpt
                               FieldExprList = fields }
                    | field -> walkRecordField field
                )

            | SynExpr.ObjExpr(bindings = binds; extraImpls = ifaces) ->
                List.tryPick walkBinding binds
                |> Option.orElseWith (fun _ -> List.tryPick walkSynInterfaceImpl ifaces)

            | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]
            | SynExpr.ForEach(enumExpr = synExpr1; bodyExpr = synExpr2) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.For(identBody = synExpr1; toBody = synExpr2; doBody = synExpr3) ->
                List.tryPick walkExpr [synExpr1; synExpr2; synExpr3]

            | SynExpr.MatchLambda(_isExnMatch, _argm, synMatchClauseList, _spBind, _wholem) ->
                synMatchClauseList
                |> List.tryPick (fun (SynMatchClause(resultExpr = e)) -> walkExpr e)
            | SynExpr.Match(expr = synExpr; clauses = synMatchClauseList) ->
                walkExpr synExpr
                |> Option.orElseWith (fun _ -> synMatchClauseList |> List.tryPick (fun (SynMatchClause(resultExpr = e)) -> walkExpr e))

            | SynExpr.App(_exprAtomicFlag, _isInfix, synExpr1, synExpr2, _range) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | SynExpr.TypeApp(synExpr, _, _synTypeList, _commas, _, _, _range) ->
                walkExpr synExpr

            | SynExpr.LetOrUse(bindings =  synBindingList; body = synExpr) ->
                walkExpr synExpr
                |> Option.orElseWith (fun _ -> List.tryPick walkBinding synBindingList)

            | SynExpr.TryWith(tryExpr = synExpr) ->
                walkExpr synExpr

            | SynExpr.TryFinally(tryExpr = synExpr1; finallyExpr = synExpr2) ->
                List.tryPick walkExpr [synExpr1; synExpr2]

            | Sequentials exprs ->
                List.tryPick walkExpr exprs

            | SynExpr.IfThenElse(ifExpr = synExpr1; thenExpr = synExpr2; elseExpr = synExprOpt) ->
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

            | SynExpr.DotIndexedGet(synExpr, argList, _range, _range2) ->
                walkExpr argList
                |> Option.orElseWith (fun _ -> walkExpr synExpr)

            | SynExpr.DotIndexedSet(synExpr1, argList, synExpr2, _, _range, _range2) ->
                [ synExpr1
                  argList
                  synExpr2 ]
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

            | SynExpr.LetOrUseBang(rhs = synExpr1; andBangs = ands; body = synExpr2) ->
                [ synExpr1
                  yield! ands |> List.map (fun (SynExprAndBang(body = body)) -> body)
                  synExpr2 ] |> List.tryPick walkExpr

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

    and walkRecordField (SynExprRecordField(expr = synExprOpt)) =
        match synExprOpt with
        | Some synExpr when Range.rangeContainsPos synExpr.Range pos -> walkExpr synExpr
        | _ -> None

    and walkSynInterfaceImpl (SynInterfaceImpl(bindings = synBindings): SynInterfaceImpl) =
        List.tryPick walkBinding synBindings

    match parsedInput with
    | ParsedInput.SigFile _input -> None
    | ParsedInput.ImplFile input -> walkImplFileInput input

let tryFindRecordExprInBufferAtPos (codeGenService: CodeGenerationService) (pos: Position) (document : Document) =
    asyncMaybe {
        let! parseResults = codeGenService.ParseFileInProject(document.FullName)

        return!
            tryFindRecordBindingInParsedInput pos parseResults.ParseTree
    }

let checkThatRecordExprEndsWithRBrace (codeGenService: CodeGenerationService) (document : Document) (expr: RecordExpr) =

    maybe {
        let! rangeWhereToLookForEnclosingRBrace =
            match expr.FieldExprList with
            | [] -> Some expr.Expr.Range
            | _ ->
                let lastField = Seq.last expr.FieldExprList
                match lastField with
                | SynExprRecordField(blockSeparator =  Some (semiColonRange, Some _semiColonEndPos)) ->
                    // The last field ends with a ';'
                    // Look here: { field = expr;<start> ... }<end>
                    Some (Range.unionRanges semiColonRange.EndRange expr.Expr.Range.EndRange)


                | SynExprRecordField(expr = Some fieldExpr) ->
                    // The last field doesn't end with a ';'
                    // Look here: { field = expr<start> ... }<end>
                    Some (Range.unionRanges fieldExpr.Range.EndRange expr.Expr.Range.EndRange)

                | SynExprRecordField(expr = None) ->
                    // We don't allow generation when the last field isn't assigned an expression
                    None

        return! tryFindTokenLPosInRange codeGenService rangeWhereToLookForEnclosingRBrace document
                (fun tokenInfo -> tokenInfo.TokenName = "RBRACE")
    }
    |> Option.isSome

let tryFindStubInsertionParamsAtPos (codeGenService: CodeGenerationService) (pos: Position) (document : Document) =
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

let tryFindRecordDefinitionFromPos (codeGenService: CodeGenerationService) (pos: Position) (document : Document) =
    asyncMaybe {
        let! recordExpression, insertionPos =
            tryFindStubInsertionParamsAtPos codeGenService pos document

        let! symbol, symbolUse =
            codeGenService.GetSymbolAndUseAtPositionOfKind(document.FullName, pos, SymbolKind.Ident)
        let! symbolUse = symbolUse
        match symbolUse.Symbol with
        | :? FSharpEntity as entity when entity.IsFSharpRecord && entity.DisplayName = symbol.Text ->
            return! Some (recordExpression, Some entity, insertionPos)

        | :? FSharpField as field ->
            match field.DeclaringEntity with
            | Some decl when decl.IsFSharpRecord && field.DisplayName = symbol.Text ->
                return! Some (recordExpression, field.DeclaringEntity, insertionPos)
            | _ -> return! None
        | _ ->
            return! None
    }
