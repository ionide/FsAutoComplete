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
type RecordExpr =
  { Expr: SynExpr
    CopyExprOption: option<SynExpr * BlockSeparator>
    FieldExprList: SynExprRecordField list
    LastKnownGoodPosForSymbolLookup: Position }

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
  { Kind: PositionKind
    InsertionPos: Position
    IndentColumn: int }

  static member TryCreateFromRecordExpression(expr: RecordExpr) =
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
            Some(fieldInfo, indentColumn, fieldLine)
          | _ -> None)

      maybe {
        let! maxLineIdx =
          fieldAndStartColumnAndLineIdxList
          |> List.unzip3
          |> (fun (_, _, lineIdx) -> lineIdx)
          |> (function
          | [] -> None
          | nonEmptyList -> Some(List.max nonEmptyList))

        let indentColumn =
          fieldAndStartColumnAndLineIdxList
          |> List.pick (fun (_, indentColumn, lineIdx) -> if lineIdx = maxLineIdx then Some indentColumn else None)

        let lastFieldInfo = Seq.last expr.FieldExprList

        return!
          match lastFieldInfo with
          | SynExprRecordField(expr = None) -> None
          | SynExprRecordField(
              fieldName = (LongIdentWithDots(_ :: _, _), true); expr = Some expr; blockSeparator = semiColonOpt) ->
            match semiColonOpt with
            | None ->
              { Kind = PositionKind.AfterLastField
                IndentColumn = indentColumn
                InsertionPos = expr.Range.End }
              |> Some
            | Some(_range, Some semiColonEndPos) ->
              { Kind = PositionKind.AfterLastField
                IndentColumn = indentColumn
                InsertionPos = semiColonEndPos }
              |> Some
            | _ -> None
          | _ -> None
      }


[<NoComparison>]
type private Context =
  {
    Writer: ColumnIndentedTextWriter
    /// A single-line skeleton for each field
    FieldDefaultValue: string
    RecordTypeName: string
    RequireQualifiedAccess: bool
  }

let private formatField (ctxt: Context) prependNewLine prependExtraSpace (field: FSharpField) =
  let writer = ctxt.Writer

  if prependNewLine then
    writer.WriteLine("")

  let name =
    if ctxt.RequireQualifiedAccess then
      sprintf "%s.%s" ctxt.RecordTypeName field.Name
    else
      field.Name

  let prependedSpace = if prependExtraSpace then " " else ""

  writer.Write("{0}{1} = {2}", prependedSpace, name, ctxt.FieldDefaultValue)

let formatRecord
  (insertionPos: RecordStubsInsertionParams)
  (fieldDefaultValue: string)
  (entity: FSharpEntity)
  (fieldsWritten: SynExprRecordField list)
  =
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
      | SynExprRecordField(fieldName = (fieldName, _)) ->
        // Extract <Field> in qualified identifiers: A.B.<Field> = ...
        if fieldName.Lid.Length > 0 then
          [ (fieldName.Lid.Item(fieldName.Lid.Length - 1)).idText ]
        else
          [])
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

    otherFields |> List.iter (formatField ctxt prependNewLineToOtherFields false)

  writer.Dump()


let walkAndFindRecordBinding (pos, input) =
  let walker =
    { new SyntaxVisitorBase<RecordExpr>() with
        member x.VisitExpr
          (
            path: SyntaxVisitorPath,
            traverseSynExpr: (SynExpr -> _ option),
            defaultTraverse: (SynExpr -> _ option),
            synExpr: SynExpr
          ) =
          match synExpr with
          | SynExpr.Record(recordFields = recordFields; copyInfo = copyInfo) ->
            Some
              { Expr = synExpr
                CopyExprOption = copyInfo
                FieldExprList = recordFields
                LastKnownGoodPosForSymbolLookup =
                  recordFields
                  |> List.tryLast
                  |> Option.map (function
                    | SynExprRecordField(fieldName = (id, _)) -> id.Range.Start)
                  |> Option.defaultValue synExpr.Range.Start }
          | _ -> defaultTraverse synExpr }

  SyntaxTraversal.Traverse(pos, input, walker)

let tryFindRecordExprInBufferAtPos (codeGenService: ICodeGenerationService) (pos: Position) (document: Document) =
  asyncMaybe {
    let! parseResults = codeGenService.ParseFileInProject(document.FullName)

    let! found = walkAndFindRecordBinding (pos, parseResults.ParseTree)
    return found
  }

let checkThatRecordExprEndsWithRBrace (codeGenService: ICodeGenerationService) (document: Document) (expr: RecordExpr) =

  maybe {
    let! rangeWhereToLookForEnclosingRBrace =
      match expr.FieldExprList with
      | [] -> Some expr.Expr.Range
      | _ ->
        let lastField = Seq.last expr.FieldExprList

        match lastField with
        | SynExprRecordField(blockSeparator = Some(semiColonRange, Some _semiColonEndPos)) ->
          // The last field ends with a ';'
          // Look here: { field = expr;<start> ... }<end>
          Some(Range.unionRanges semiColonRange.EndRange expr.Expr.Range.EndRange)


        | SynExprRecordField(expr = Some fieldExpr) ->
          // The last field doesn't end with a ';'
          // Look here: { field = expr<start> ... }<end>
          Some(Range.unionRanges fieldExpr.Range.EndRange expr.Expr.Range.EndRange)

        | SynExprRecordField(expr = None) ->
          // We don't allow generation when the last field isn't assigned an expression
          None

    return!
      tryFindTokenLPosInRange codeGenService rangeWhereToLookForEnclosingRBrace document (fun tokenInfo ->
        tokenInfo.TokenName = "RBRACE")
  }
  |> Option.isSome

let tryFindStubInsertionParamsAtPos (codeGenService: ICodeGenerationService) (pos: Position) (document: Document) =
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

let tryFindRecordDefinitionFromPos (codeGenService: ICodeGenerationService) (pos: Position) (document: Document) =
  asyncMaybe {
    let! recordExpression, insertionPos = tryFindStubInsertionParamsAtPos codeGenService pos document

    let! symbol, symbolUse =
      codeGenService.GetSymbolAndUseAtPositionOfKind(
        document.FullName,
        recordExpression.LastKnownGoodPosForSymbolLookup,
        SymbolKind.Ident
      )

    let! symbolUse = symbolUse

    match symbolUse.Symbol with
    | :? FSharpEntity as entity when entity.IsFSharpRecord && entity.DisplayName = symbol.Text ->
      return! Some(recordExpression, Some entity, insertionPos)

    | :? FSharpField as field ->
      match field.DeclaringEntity with
      | Some decl when decl.IsFSharpRecord && field.DisplayName = symbol.Text ->
        return! Some(recordExpression, field.DeclaringEntity, insertionPos)
      | _ -> return! None
    | _ -> return! None
  }
