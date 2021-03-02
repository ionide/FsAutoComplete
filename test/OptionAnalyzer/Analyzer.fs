module OptionAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FsAutoComplete.Logging

let rec visitExpr memberCallHandler (e:FSharpExpr) =
    match e with
    | BasicPatterns.AddressOf(lvalueExpr) ->
        visitExpr memberCallHandler lvalueExpr
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) ->
        visitExpr memberCallHandler lvalueExpr; visitExpr memberCallHandler rvalueExpr
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) ->
        visitExpr memberCallHandler funcExpr; visitExprs memberCallHandler argExprs
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
        memberCallHandler e.Range memberOrFunc
        visitObjArg memberCallHandler objExprOpt; visitExprs memberCallHandler argExprs
    | BasicPatterns.Coerce(targetType, inpExpr) ->
        visitExpr memberCallHandler inpExpr
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) ->
        visitExpr memberCallHandler startExpr; visitExpr memberCallHandler limitExpr; visitExpr memberCallHandler consumeExpr
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) ->
        visitObjArg memberCallHandler objExprOpt
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) ->
        visitObjArg memberCallHandler objExprOpt
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler thenExpr; visitExpr memberCallHandler elseExpr
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
        visitExpr memberCallHandler bodyExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->
        visitExpr memberCallHandler bindingExpr; visitExpr memberCallHandler bodyExpr
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) ->
        List.iter (snd >> visitExpr memberCallHandler) recursiveBindings; visitExpr memberCallHandler bodyExpr
    | BasicPatterns.NewArray(arrayType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) ->
        visitExpr memberCallHandler delegateBodyExpr
    | BasicPatterns.NewObject(objType, typeArgs, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewRecord(recordType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewTuple(tupleType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.Quote(quotedExpr) ->
        visitExpr memberCallHandler quotedExpr
    | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) ->
        visitObjArg memberCallHandler objExprOpt
    | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) ->
        visitObjArg memberCallHandler objExprOpt; visitExpr memberCallHandler argExpr
    | BasicPatterns.Sequential(firstExpr, secondExpr) ->
        visitExpr memberCallHandler firstExpr; visitExpr memberCallHandler secondExpr
    | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) ->
        visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler finalizeExpr
    | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) ->
        visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler catchExpr
    | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) ->
        visitExpr memberCallHandler tupleExpr
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        visitExpr memberCallHandler decisionExpr; List.iter (snd >> visitExpr memberCallHandler) decisionTargets
    | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
        visitExprs memberCallHandler decisionTargetExprs
    | BasicPatterns.TypeLambda(genericParam, bodyExpr) ->
        visitExpr memberCallHandler bodyExpr
    | BasicPatterns.TypeTest(ty, inpExpr) ->
        visitExpr memberCallHandler inpExpr
    | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) ->
        visitExpr memberCallHandler unionExpr; visitExpr memberCallHandler valueExpr
    | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) ->
        visitExpr memberCallHandler unionExpr
    | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) ->
        visitExpr memberCallHandler unionExpr
    | BasicPatterns.UnionCaseTag(unionExpr, unionType) ->
        visitExpr memberCallHandler unionExpr
    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) ->
        visitExpr memberCallHandler baseCallExpr
        List.iter (visitObjMember memberCallHandler) overrides
        List.iter (snd >> List.iter (visitObjMember memberCallHandler)) interfaceImplementations
    | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.ValueSet(valToSet, valueExpr) ->
        visitExpr memberCallHandler valueExpr
    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
        visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler bodyExpr
    | BasicPatterns.BaseValue baseType -> ()
    | BasicPatterns.DefaultValue defaultType -> ()
    | BasicPatterns.ThisValue thisType -> ()
    | BasicPatterns.Const(constValueObj, constType) -> ()
    | BasicPatterns.Value(valueToGet) -> ()
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
