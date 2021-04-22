module OptionAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FsAutoComplete.Logging

let rec visitExpr memberCallHandler (e:FSharpExpr) =
    match e with
    | AddressOf(lvalueExpr) ->
        visitExpr memberCallHandler lvalueExpr
    | AddressSet(lvalueExpr, rvalueExpr) ->
        visitExpr memberCallHandler lvalueExpr; visitExpr memberCallHandler rvalueExpr
    | Application(funcExpr, typeArgs, argExprs) ->
        visitExpr memberCallHandler funcExpr; visitExprs memberCallHandler argExprs
    | Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
        memberCallHandler e.Range memberOrFunc
        visitObjArg memberCallHandler objExprOpt; visitExprs memberCallHandler argExprs
    | Coerce(targetType, inpExpr) ->
        visitExpr memberCallHandler inpExpr
    | FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) ->
        visitExpr memberCallHandler startExpr; visitExpr memberCallHandler limitExpr; visitExpr memberCallHandler consumeExpr
    | ILAsm(asmCode, typeArgs, argExprs) ->
        visitExprs memberCallHandler argExprs
    | ILFieldGet (objExprOpt, fieldType, fieldName) ->
        visitObjArg memberCallHandler objExprOpt
    | ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) ->
        visitObjArg memberCallHandler objExprOpt
    | IfThenElse (guardExpr, thenExpr, elseExpr) ->
        visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler thenExpr; visitExpr memberCallHandler elseExpr
    | Lambda(lambdaVar, bodyExpr) ->
        visitExpr memberCallHandler bodyExpr
    | Let((bindingVar, bindingExpr), bodyExpr) ->
        visitExpr memberCallHandler bindingExpr; visitExpr memberCallHandler bodyExpr
    | LetRec(recursiveBindings, bodyExpr) ->
        List.iter (snd >> visitExpr memberCallHandler) recursiveBindings; visitExpr memberCallHandler bodyExpr
    | NewArray(arrayType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | NewDelegate(delegateType, delegateBodyExpr) ->
        visitExpr memberCallHandler delegateBodyExpr
    | NewObject(objType, typeArgs, argExprs) ->
        visitExprs memberCallHandler argExprs
    | NewRecord(recordType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | NewTuple(tupleType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | NewUnionCase(unionType, unionCase, argExprs) ->
        visitExprs memberCallHandler argExprs
    | Quote(quotedExpr) ->
        visitExpr memberCallHandler quotedExpr
    | FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) ->
        visitObjArg memberCallHandler objExprOpt
    | FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) ->
        visitObjArg memberCallHandler objExprOpt; visitExpr memberCallHandler argExpr
    | Sequential(firstExpr, secondExpr) ->
        visitExpr memberCallHandler firstExpr; visitExpr memberCallHandler secondExpr
    | TryFinally(bodyExpr, finalizeExpr) ->
        visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler finalizeExpr
    | TryWith(bodyExpr, _, _, catchVar, catchExpr) ->
        visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler catchExpr
    | TupleGet(tupleType, tupleElemIndex, tupleExpr) ->
        visitExpr memberCallHandler tupleExpr
    | DecisionTree(decisionExpr, decisionTargets) ->
        visitExpr memberCallHandler decisionExpr; List.iter (snd >> visitExpr memberCallHandler) decisionTargets
    | DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
        visitExprs memberCallHandler decisionTargetExprs
    | TypeLambda(genericParam, bodyExpr) ->
        visitExpr memberCallHandler bodyExpr
    | TypeTest(ty, inpExpr) ->
        visitExpr memberCallHandler inpExpr
    | UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) ->
        visitExpr memberCallHandler unionExpr; visitExpr memberCallHandler valueExpr
    | UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) ->
        visitExpr memberCallHandler unionExpr
    | UnionCaseTest(unionExpr, unionType, unionCase) ->
        visitExpr memberCallHandler unionExpr
    | UnionCaseTag(unionExpr, unionType) ->
        visitExpr memberCallHandler unionExpr
    | ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) ->
        visitExpr memberCallHandler baseCallExpr
        List.iter (visitObjMember memberCallHandler) overrides
        List.iter (snd >> List.iter (visitObjMember memberCallHandler)) interfaceImplementations
    | TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) ->
        visitExprs memberCallHandler argExprs
    | ValueSet(valToSet, valueExpr) ->
        visitExpr memberCallHandler valueExpr
    | WhileLoop(guardExpr, bodyExpr) ->
        visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler bodyExpr
    | BaseValue baseType -> ()
    | DefaultValue defaultType -> ()
    | ThisValue thisType -> ()
    | Const(constValueObj, constType) -> ()
    | Value(valueToGet) -> ()
    | _ -> ()

and visitExprs f exprs =
    List.iter (visitExpr f) exprs

and visitObjArg f objOpt =
    Option.iter (visitExpr f) objOpt

and visitObjMember f memb =
    visitExpr f memb.Body

let rec visitDeclaration f d =
    match d with
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
        for subDecl in subDecls do
            visitDeclaration f subDecl
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) ->
        visitExpr f e
    | FSharpImplementationFileDeclaration.InitAction(e) ->
        visitExpr f e

let notUsed() =
    let option : Option<int> = None
    option.Value

let logger = LogProvider.getLoggerByName "OptionAnalyzer"

let info message items =
  let mutable log = Log.setMessage message
  for (name, value) in items do
    log <- log >> Log.addContextDestructured name value
  logger.info log

let inline (==>) x y = x, box y

[<Analyzer "OptionAnalyzer">]
let optionValueAnalyzer : Analyzer =
  fun ctx ->
    info "analyzing {file} for uses of Option.Value" [ "file" ==> ctx.FileName ]
    let state = ResizeArray<Range>()
    let handler (range: Range) (m: FSharpMemberOrFunctionOrValue) =
      let rangeString = sprintf "(%d,%d)-(%d,%d)" range.Start.Line range.Start.Column range.End.Line range.End.Column
      let name = String.Join(".", m.DeclaringEntity.Value.FullName, m.DisplayName)
      info "checking value at {range} with name {name}" [ "range" ==> rangeString
                                                          "name" ==> name ]
      if name = "Microsoft.FSharp.Core.FSharpOption`1.Value" then
        info "matched at range {range}" [ "range" ==> rangeString ]
        state.Add range
    ctx.TypedTree.Declarations |> List.iter (visitDeclaration handler)
    state
    |> Seq.map (fun r -> { Type = "Option.Value analyzer"
                           Message = "Option.Value shouldn't be used"
                           Code = "OV001"
                           Severity = Warning
                           Range = r
                           Fixes = [] })
    |> Seq.toList
