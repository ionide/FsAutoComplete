module FsAutoComplete.TestAdapter


open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

type TestAdapterEntry<'range> =
  { Name: string
    Range: 'range
    Childs: ResizeArray<TestAdapterEntry<'range>>
    Id: int
    List: bool
    ModuleType: string
    Type: string }

[<Literal>]
let private ExpectoType = "Expecto"

[<Literal>]
let private NUnitType = "NUnit"

[<Literal>]
let private XUnitType = "XUnit"

[<Literal>]
let private NoneModuleType = "NoneModule"

[<Literal>]
let private ModuleType = "Module"

[<Literal>]
let private TypeInModule = "TypeInModule"

[<Literal>]
let private ModuleWithSuffixType = "ModuleWithSuffix"

let rec private (|Sequentials|_|) =
  function
  | SynExpr.Sequential(_, _, e, Sequentials es, _) -> Some(e :: es)
  | SynExpr.Sequential(_, _, e1, e2, _) -> Some [ e1; e2 ]
  | _ -> None

let getExpectoTests (ast: ParsedInput) : TestAdapterEntry<range> list =
  let mutable ident = 0

  let isExpectoName (str: string) =
    str.EndsWith "testCase"
    || str.EndsWith "ftestCase"
    || str.EndsWith "ptestCase"
    || str.EndsWith "testCaseAsync"
    || str.EndsWith "ftestCaseAsync"
    || str.EndsWith "ptestCaseAsync"
    || (str.EndsWith "test"
        && not (str.EndsWith "failtest")
        && not (str.EndsWith "skiptest"))
    || str.EndsWith "ftest"
    || (str.EndsWith "ptest" && not (str.EndsWith "skiptest"))
    || str.EndsWith "testAsync"
    || str.EndsWith "ftestAsync"
    || str.EndsWith "ptestAsync"
    || str.EndsWith "testProperty"
    || str.EndsWith "ptestProperty"
    || str.EndsWith "ftestProperty"
    || str.EndsWith "testPropertyWithConfig"
    || str.EndsWith "ptestPropertyWithConfig"
    || str.EndsWith "ftestPropertyWithConfig"
    || str.EndsWith "testPropertyWithConfigs"
    || str.EndsWith "ptestPropertyWithConfigs"
    || str.EndsWith "ftestPropertyWithConfigs"

  let isExpectoListName (str: string) =
    str.EndsWith "testList" || str.EndsWith "ftestList" || str.EndsWith "ptestList"

  let (|Case|List|NotExpecto|) =
    function
    | SynExpr.Ident i ->
      if isExpectoName i.idText then Case
      elif isExpectoListName i.idText then List
      else NotExpecto
    | SynExpr.LongIdent(_, SynLongIdent(id = lst), _, _) ->
      let i = lst |> List.last

      if isExpectoName i.idText then Case
      elif isExpectoListName i.idText then List
      else NotExpecto
    | _ -> NotExpecto

  let rec visitExpr (parent: TestAdapterEntry<range>) =
    function
    | SynExpr.App(_, _, SynExpr.App(_, _, expr1, SynExpr.Const(SynConst.String(text = s), _), range), expr2, _) ->
      match expr1, expr2 with
      | List, SynExpr.ArrayOrList _
      | List, SynExpr.ArrayOrListComputed _ ->
        ident <- ident + 1

        let entry =
          { Name = s
            Range = range
            Childs = ResizeArray()
            Id = ident
            List = true
            ModuleType = NoneModuleType
            Type = ExpectoType }

        parent.Childs.Add entry

        visitExpr entry expr1
        visitExpr entry expr2
      | Case, SynExpr.ComputationExpr _
      | Case, SynExpr.Lambda _
      | Case, SynExpr.Paren(SynExpr.Lambda _, _, _, _) ->
        ident <- ident + 1

        let entry =
          { Name = s
            Range = expr1.Range
            Childs = ResizeArray()
            Id = ident
            List = false
            ModuleType = NoneModuleType
            Type = ExpectoType }

        parent.Childs.Add entry
      | _ ->
        visitExpr parent expr1
        visitExpr parent expr2
    | SynExpr.App(_, _, SynExpr.App(_, _, expr1, _, _range), SynExpr.Const(SynConst.String(text = s), _), _)
    | SynExpr.App(_,
                  _,
                  SynExpr.App(_, _, SynExpr.App(_, _, expr1, _, _range), _, _),
                  SynExpr.Const(SynConst.String(text = s), _),
                  _)
    | SynExpr.App(_,
                  _,
                  SynExpr.App(_, _, SynExpr.App(_, _, SynExpr.App(_, _, expr1, _, _range), _, _), _, _),
                  SynExpr.Const(SynConst.String(text = s), _),
                  _)
    | SynExpr.App(_, _, expr1, SynExpr.Const(SynConst.String(text = s), _), _range) -> //Take those applications that are using string constant as an argument
      match expr1 with
      | Case ->
        ident <- ident + 1

        let entry =
          { Name = s
            Range = expr1.Range
            Childs = ResizeArray()
            Id = ident
            List = false
            ModuleType = NoneModuleType
            Type = ExpectoType }

        parent.Childs.Add entry
      | List -> ()
      | NotExpecto -> ()
    | SynExpr.ArrayOrListComputed(_, expr, _)
    | SynExpr.ComputationExpr(expr = expr)
    | SynExpr.Lambda(body = expr)
    | SynExpr.YieldOrReturn(_, expr, _)
    | SynExpr.YieldOrReturnFrom(_, expr, _)
    | SynExpr.New(_, _, expr, _)
    | SynExpr.Assert(expr, _)
    | SynExpr.Do(expr, _)
    | SynExpr.Typed(expr, _, _)
    | SynExpr.Paren(expr, _, _, _)
    | SynExpr.DoBang(expr, _)
    | SynExpr.Downcast(expr, _, _)
    | SynExpr.For(doBody = expr)
    | SynExpr.Lazy(expr, _)
    | SynExpr.TypeTest(expr, _, _)
    | SynExpr.Upcast(expr, _, _)
    | SynExpr.InferredUpcast(expr, _)
    | SynExpr.InferredDowncast(expr, _)
    | SynExpr.LongIdentSet(_, expr, _)
    | SynExpr.DotGet(expr, _, _, _)
    | SynExpr.ForEach(bodyExpr = expr) -> visitExpr parent expr
    | SynExpr.App(_, _, expr1, expr2, _)
    | SynExpr.TryFinally(tryExpr = expr1; finallyExpr = expr2)
    | SynExpr.NamedIndexedPropertySet(_, expr1, expr2, _)
    | SynExpr.DotNamedIndexedPropertySet(_, _, expr1, expr2, _)
    | SynExpr.LetOrUseBang(rhs = expr1; body = expr2)
    | SynExpr.While(_, expr1, expr2, _) ->
      visitExpr parent expr1
      visitExpr parent expr2
    | Sequentials exprs
    | SynExpr.Tuple(_, exprs, _, _)
    | SynExpr.ArrayOrList(_, exprs, _) -> List.iter (visitExpr parent) exprs
    | SynExpr.Match(expr = expr; clauses = clauses)
    | SynExpr.TryWith(tryExpr = expr; withCases = clauses) ->
      visitExpr parent expr
      visitMatches parent clauses
    | SynExpr.IfThenElse(ifExpr = cond; thenExpr = trueBranch; elseExpr = falseBranchOpt) ->
      visitExpr parent cond
      visitExpr parent trueBranch
      falseBranchOpt |> Option.iter (visitExpr parent)
    | SynExpr.LetOrUse(bindings = bindings; body = body) ->
      visitBindindgs parent bindings
      visitExpr parent body
    | SynExpr.Record(_, _, fields, _) ->
      fields
      |> List.choose (fun (SynExprRecordField(expr = expr)) -> expr)
      |> List.iter (visitExpr parent)
    | SynExpr.MatchLambda(_, _, clauses, _, _) -> visitMatches parent clauses
    | SynExpr.ObjExpr(bindings = bindings) -> visitBindindgs parent bindings
    | _ -> ()

  and visitBinding prefix (SynBinding(expr = body)) = visitExpr prefix body
  and visitBindindgs prefix s = s |> List.iter (visitBinding prefix)
  and visitMatch prefix (SynMatchClause(resultExpr = expr)) = visitExpr prefix expr
  and visitMatches prefix s = s |> List.iter (visitMatch prefix)

  let rec visitDeclarations prefix decls =
    for declaration in decls do
      match declaration with
      | SynModuleDecl.Let(_, bindings, _) -> visitBindindgs prefix bindings
      | SynModuleDecl.NestedModule(decls = decls) -> visitDeclarations prefix decls
      | _ -> ()

  let visitModulesAndNamespaces prefix modulesOrNss =
    Seq.iter (fun (SynModuleOrNamespace(decls = decls)) -> visitDeclarations prefix decls) modulesOrNss

  let allTests =
    { Name = ""
      Range = Range.range0
      Childs = ResizeArray()
      Id = -1
      List = false
      ModuleType = NoneModuleType
      Type = "" }

  match ast with
  | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) -> visitModulesAndNamespaces allTests modules
  | _ -> ()

  List.ofSeq allTests.Childs


let getNUnitTest (ast: ParsedInput) : TestAdapterEntry<range> list =
  let mutable ident = 0

  let isNUnitTest (attrs: SynAttributes) =
    attrs
    |> List.collect (fun (attr: SynAttributeList) -> attr.Attributes)
    |> List.exists (fun a ->
      let str = a.TypeName.LongIdent |> List.last

      str.idText.EndsWith "Test"
      || str.idText.EndsWith "TestAttribute"
      || str.idText.EndsWith "TestCase"
      || str.idText.EndsWith "TestCaseAttribute"
      || str.idText.EndsWith "TestCaseSource"
      || str.idText.EndsWith "TestCaseSourceAttribute"
      || str.idText.EndsWith "Theory"
      || str.idText.EndsWith "TheoryAttribute"
      || str.idText.EndsWith "Property"
      || str.idText.EndsWith "PropertyAttribute")

  let getName =
    function
    | SynPat.Named(ident = SynIdent(ident = name)) -> name.idText
    | SynPat.LongIdent(longDotId = SynLongIdent(id = ident)) -> ident |> List.last |> (fun n -> n.idText)
    | _ -> ""

  let rec visitMember (parent: TestAdapterEntry<range>) =
    function
    | SynMemberDefn.Member(b, _) -> visitBinding parent b
    | SynMemberDefn.LetBindings(bindings, _, _, _) ->
      for b in bindings do
        visitBinding parent b
    | SynMemberDefn.NestedType(typeDef, _, _) -> visitTypeDef parent typeDef
    | _ -> ()

  and visitTypeDef parent t =
    let (SynTypeDefn(typeInfo = ci; typeRepr = om; members = members)) = t
    let (SynComponentInfo(longId = ids; range = r)) = ci
    let name = String.concat "." [ for i in ids -> i.idText ]
    let moduleType = if parent.ModuleType = ModuleType || parent.ModuleType = ModuleWithSuffixType then TypeInModule else NoneModuleType
    ident <- ident + 1

    let entry =
      { Name = name
        Range = r
        Childs = ResizeArray()
        Id = ident
        List = true
        ModuleType = moduleType
        Type = NUnitType }

    parent.Childs.Add entry

    match om with
    | SynTypeDefnRepr.ObjectModel(_, ms, _) ->
      for m in ms do
        visitMember entry m
    | _ -> ()

    for m in members do
      visitMember entry m

    if entry.Childs.Count = 0 then
      parent.Childs.Remove entry |> ignore

  and visitBinding parent b =
    let (SynBinding(attributes = attrs; headPat = pat; range = r)) = b

    if isNUnitTest attrs then
      ident <- ident + 1

      let entry =
        { Name = getName pat
          Range = r
          Childs = ResizeArray()
          Id = ident
          List = false
          ModuleType = NoneModuleType
          Type = NUnitType }

      parent.Childs.Add entry

  let rec visitDeclarations (parent: TestAdapterEntry<range>) decls =
    let typeNames =
      decls
      |> List.fold
        (fun types declaration ->
          match declaration with
          | SynModuleDecl.Types(typeDefns, _) ->
            typeDefns
            |> List.map (fun (SynTypeDefn(typeInfo = ci)) ->
              let (SynComponentInfo(longId = ids)) = ci
              String.concat "." [ for i in ids -> i.idText ])
            |> List.append types
          | _ -> types)
        ([])
      |> Set.ofList

    for declaration in decls do
      match declaration with
      | SynModuleDecl.Let(_, bindings, _) ->
        for b in bindings do
          visitBinding parent b
      | SynModuleDecl.NestedModule(moduleInfo = ci; decls = decls) ->
        let (SynComponentInfo(longId = ids; range = r)) = ci
        let name = String.concat "." [ for i in ids -> i.idText ]

        let moduleType =
          if Set.contains name typeNames then
            ModuleWithSuffixType
          else
            ModuleType

        ident <- ident + 1

        let entry =
          { Name = name
            Range = r
            Childs = ResizeArray()
            Id = ident
            List = true
            ModuleType = moduleType
            Type = NUnitType }

        parent.Childs.Add entry
        visitDeclarations entry decls

        if entry.Childs.Count = 0 then
          parent.Childs.Remove entry |> ignore
      | SynModuleDecl.Types(types, _) ->
        for t in types do
          visitTypeDef parent t
      | _ -> ()

  let visitModulesAndNamespaces parent modulesOrNss =
    Seq.iter
      (fun (SynModuleOrNamespace(longId = ids; decls = decls; range = r; kind = kind)) ->
        let name = String.concat "." [ for i in ids -> i.idText ]
        let moduleType = if kind.IsModule then ModuleType else NoneModuleType
        ident <- ident + 1

        let entry =
          { Name = name
            Range = r
            Childs = ResizeArray()
            Id = ident
            List = true
            ModuleType = moduleType
            Type = NUnitType }

        parent.Childs.Add entry
        visitDeclarations entry decls

        if entry.Childs.Count = 0 then
          parent.Childs.Remove entry |> ignore)
      modulesOrNss

  let allTests =
    { Name = ""
      Range = Range.range0
      Childs = ResizeArray()
      Id = -1
      List = false
      ModuleType = NoneModuleType
      Type = "" }

  match ast with
  | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) -> visitModulesAndNamespaces allTests modules
  | _ -> ()

  List.ofSeq allTests.Childs

let getXUnitTest ast : TestAdapterEntry<range> list =
  let mutable ident = 0

  let isXUnitTest (attrs: SynAttributes) =
    attrs
    |> List.collect (fun (attr: SynAttributeList) -> attr.Attributes)
    |> List.exists (fun a ->
      let str = a.TypeName.LongIdent |> List.last

      str.idText.EndsWith "Fact"
      || str.idText.EndsWith "FactAttribute"
      || str.idText.EndsWith "Theory"
      || str.idText.EndsWith "TheoryAttribute"
      || str.idText.EndsWith "Property"
      || str.idText.EndsWith "PropertyAttribute")

  let getName =
    function
    | SynPat.Named(ident = SynIdent(ident = name)) -> name.idText
    | SynPat.LongIdent(longDotId = SynLongIdent(id = ident)) -> ident |> List.last |> (fun n -> n.idText)
    | _ -> ""

  let rec visitMember (parent: TestAdapterEntry<range>) =
    function
    | SynMemberDefn.Member(b, _) -> visitBinding parent b
    | SynMemberDefn.LetBindings(bindings, _, _, _) ->
      for b in bindings do
        visitBinding parent b
    | SynMemberDefn.NestedType(typeDef, _, _) -> visitTypeDef parent typeDef
    | _ -> ()

  and visitTypeDef parent t =
    let (SynTypeDefn(typeInfo = ci; typeRepr = om; members = members)) = t
    let (SynComponentInfo(longId = ids; range = r)) = ci
    let name = String.concat "." [ for i in ids -> i.idText ]
    let moduleType = if parent.ModuleType = ModuleType || parent.ModuleType = ModuleWithSuffixType then TypeInModule else NoneModuleType
    ident <- ident + 1

    let entry =
      { Name = name
        Range = r
        Childs = ResizeArray()
        Id = ident
        List = true
        ModuleType = moduleType
        Type = XUnitType }

    parent.Childs.Add entry

    match om with
    | SynTypeDefnRepr.ObjectModel(_, ms, _) ->
      for m in ms do
        visitMember entry m
    | _ -> ()

    for m in members do
      visitMember entry m

    if entry.Childs.Count = 0 then
      parent.Childs.Remove entry |> ignore

  and visitBinding parent b =
    let (SynBinding(attributes = attrs; headPat = pat; range = r)) = b

    if isXUnitTest attrs then
      ident <- ident + 1

      let entry =
        { Name = getName pat
          Range = r
          Childs = ResizeArray()
          Id = ident
          List = false
          ModuleType = NoneModuleType
          Type = XUnitType }

      parent.Childs.Add entry

  let rec visitDeclarations (parent: TestAdapterEntry<range>) decls =
    let typeNames =
      decls
      |> List.fold
        (fun types declaration ->
          match declaration with
          | SynModuleDecl.Types(typeDefns, _) ->
            typeDefns
            |> List.map (fun (SynTypeDefn(typeInfo = ci)) ->
              let (SynComponentInfo(longId = ids)) = ci
              String.concat "." [ for i in ids -> i.idText ])
            |> List.append types
          | _ -> types)
        ([])
      |> Set.ofList

    for declaration in decls do
      match declaration with
      | SynModuleDecl.Let(_, bindings, _) ->
        for b in bindings do
          visitBinding parent b
      | SynModuleDecl.NestedModule(moduleInfo = ci; decls = decls) ->
        let (SynComponentInfo(longId = ids; range = r)) = ci
        let name = String.concat "." [ for i in ids -> i.idText ]

        let moduleType =
          if Set.contains name typeNames then
            ModuleWithSuffixType
          else
            ModuleType

        ident <- ident + 1

        let entry =
          { Name = name
            Range = r
            Childs = ResizeArray()
            Id = ident
            List = true
            ModuleType = moduleType
            Type = XUnitType }

        parent.Childs.Add entry
        visitDeclarations entry decls

        if entry.Childs.Count = 0 then
          parent.Childs.Remove entry |> ignore
      | SynModuleDecl.Types(types, _) ->
        for t in types do
          visitTypeDef parent t
      | _ -> ()

  let visitModulesAndNamespaces parent modulesOrNss =
    Seq.iter
      (fun (SynModuleOrNamespace(longId = ids; decls = decls; range = r; kind = kind)) ->
        let name = String.concat "." [ for i in ids -> i.idText ]
        let moduleType = if kind.IsModule then ModuleType else NoneModuleType
        ident <- ident + 1

        let entry =
          { Name = name
            Range = r
            Childs = ResizeArray()
            Id = ident
            List = true
            ModuleType = moduleType
            Type = XUnitType }

        parent.Childs.Add entry
        visitDeclarations entry decls

        if entry.Childs.Count = 0 then
          parent.Childs.Remove entry |> ignore)
      modulesOrNss

  let allTests =
    { Name = ""
      Range = Range.range0
      Childs = ResizeArray()
      Id = -1
      List = false
      ModuleType = NoneModuleType
      Type = "" }

  match ast with
  | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) -> visitModulesAndNamespaces allTests modules
  | _ -> ()

  List.ofSeq allTests.Childs
