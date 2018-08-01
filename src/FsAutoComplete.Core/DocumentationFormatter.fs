namespace FsAutoComplete


open System.Text.RegularExpressions
module A =
    type T =
        | Z of string * string
        | X of int

    type B<'a> = {o: string; p: string * int; i :'a option}

    let x = "", ""

    let z = Some(1)

    let y = Z ("", "")

    let xz = Seq.empty<int>

    let yx : int seq = Seq.empty




module DocumentationFormatter =

    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open System
    open System.Text

    let maxPadding = 200


    /// Concat two strings with a space between if both a and b are not IsNullOrWhiteSpace
    let internal (++) (a:string) (b:string) =
        match String.IsNullOrEmpty a, String.IsNullOrEmpty b with
        | true, true -> ""
        | false, true -> a
        | true, false -> b
        | false, false -> a + " " + b



    let rec format dispalyContext (typ : FSharpType) : string =
        let typ2 = if typ.IsAbbreviation then typ.AbbreviatedType else typ
        let t =
            try
                if typ.IsTupleType || typ.IsStructTupleType then
                    typ.GenericArguments
                    |> Seq.map (format dispalyContext)
                    |> String.concat " * "
                elif typ.GenericArguments.Count > 0 then
                    let r =
                        typ.GenericArguments
                        |> Seq.map (format dispalyContext)
                        |> String.concat ","
                    let org = typ.Format dispalyContext
                    let t = Regex.Replace(org, """<.*>""", sprintf "<%s>" r)
                    let t = Regex.Replace(t, """(.*?) list""", sprintf "List<%s>" r)
                    let t = Regex.Replace(t, """(.*?) option""", sprintf "Option<%s>" r)
                    let t = Regex.Replace(t, """(.*?) list""", sprintf "List<%s>" r)
                    let t = Regex.Replace(t, """(.*?) lazy""", sprintf "Lazy<%s>" r)
                    let t = Regex.Replace(t, """(.*?) seq""", sprintf "Seq<%s>" r)
                    let t = Regex.Replace(t, """(.*?) \[\]""", sprintf "Array<%s>" r)
                    t
                elif typ.IsGenericParameter then
                    (if typ.GenericParameter.IsSolveAtCompileTime then "^" else "'") + typ.GenericParameter.Name
                else
                    typ.Format dispalyContext
            with
            | _ -> typ.Format dispalyContext
        let ft =
            try
                if typ2.HasTypeDefinition then
                    typ2.TypeDefinition.XmlDocSig
                else
                    "-"
            with
            | _ -> "-"
        let a =
            try
                if typ2.IsGenericParameter then
                    "-"
                else
                    typ2.TypeDefinition.Assembly.SimpleName
            with
            | _ -> "-"
        sprintf "[@%s, %s, %s@]" t ft a

    and formatGenericParameter includeMemberConstraintTypes displayContext (param:FSharpGenericParameter) =

        let asGenericParamName (param: FSharpGenericParameter) =
            (if param.IsSolveAtCompileTime then "^" else "'") + param.Name

        let sb = StringBuilder()

        let getConstraint (constrainedBy: FSharpGenericParameterConstraint) =
            let memberConstraint (c: FSharpGenericParameterMemberConstraint) =
                let formattedMemberName, isProperty =
                    match c.IsProperty, PrettyNaming.TryChopPropertyName c.MemberName with
                    | true, Some(chopped) when chopped <> c.MemberName ->
                        chopped, true
                    | _, _ ->
                        if PrettyNaming.IsMangledOpName c.MemberName then
                            PrettyNaming.DemangleOperatorName c.MemberName, false
                        else
                            c.MemberName, false

                seq {
                    if c.MemberIsStatic then yield "static "
                    yield "member "
                    yield formattedMemberName
                    if includeMemberConstraintTypes then
                        yield " : "
                        if isProperty then
                            yield (c.MemberReturnType |> format displayContext)
                        else
                            if c.MemberArgumentTypes.Count <= 1 then
                                yield "unit"
                            else
                                yield asGenericParamName param
                            yield " -> "
                            yield ((c.MemberReturnType |> format displayContext).TrimStart())
                } |> String.concat ""

            let typeConstraint (tc: FSharpType) =
                sprintf ":> %s" (tc |> format displayContext)

            let enumConstraint (ec: FSharpType) =
                sprintf "enum<%s>" (ec |> format displayContext)

            let delegateConstraint (tc: FSharpGenericParameterDelegateConstraint) =
                sprintf "delegate<%s, %s>" (tc.DelegateTupledArgumentType |> format displayContext) (tc.DelegateReturnType |> format displayContext)

            let symbols =
                match constrainedBy with
                | _ when constrainedBy.IsCoercesToConstraint -> Some(typeConstraint constrainedBy.CoercesToTarget)
                | _ when constrainedBy.IsMemberConstraint -> Some(memberConstraint constrainedBy.MemberConstraintData)
                | _ when constrainedBy.IsSupportsNullConstraint -> Some("null")
                | _ when constrainedBy.IsRequiresDefaultConstructorConstraint -> Some("default constructor")
                | _ when constrainedBy.IsReferenceTypeConstraint -> Some("reference")
                | _ when constrainedBy.IsEnumConstraint -> Some(enumConstraint constrainedBy.EnumConstraintTarget)
                | _ when constrainedBy.IsComparisonConstraint -> Some("comparison")
                | _ when constrainedBy.IsEqualityConstraint -> Some("equality")
                | _ when constrainedBy.IsDelegateConstraint -> Some(delegateConstraint constrainedBy.DelegateConstraintData)
                | _ when constrainedBy.IsUnmanagedConstraint -> Some("unmanaged")
                | _ when constrainedBy.IsNonNullableValueTypeConstraint -> Some("struct")
                | _ -> None

            symbols

        if param.Constraints.Count > 0 then
            param.Constraints
            |> Seq.choose getConstraint
            |> Seq.distinct
            |> Seq.iteri(fun i symbol -> if i > 0 then print sb " and "
                                         print sb symbol)

        sb.ToString()

    and getUnioncaseSignature displayContext (unionCase:FSharpUnionCase) =
        if unionCase.UnionCaseFields.Count > 0 then
            let typeList =
                unionCase.UnionCaseFields
                |> Seq.map (fun unionField ->
                    if unionField.Name.StartsWith "Item" then //TODO: Some better way of dettecting default names for the union cases' fields
                        unionField.FieldType |> format displayContext

                    else
                        unionField.Name ++ ":" ++ ((unionField.FieldType |> format displayContext)))
                |> String.concat " * "
            unionCase.DisplayName + " of " + typeList
         else unionCase.DisplayName

    and getFuncSignatureWithIdent displayContext (func: FSharpMemberOrFunctionOrValue) (ident:int) =
        let maybeGetter = func.LogicalName.StartsWith "get_"
        let indent = String.replicate ident " "
        let functionName =
            let name =
                if func.IsConstructor then
                    match func.EnclosingEntitySafe with
                    | Some ent -> ent.DisplayName
                    | _ -> func.DisplayName
                elif func.IsOperatorOrActivePattern then func.DisplayName
                elif func.DisplayName.StartsWith "( " then PrettyNaming.QuoteIdentifierIfNeeded func.LogicalName
                else func.DisplayName
            name

        let modifiers =
            let accessibility =
                match func.Accessibility with
                | a when a.IsInternal -> "internal"
                | a when a.IsPrivate -> "private"
                | _ -> ""

            let modifier =
                //F# types are prefixed with new, should non F# types be too for consistancy?
                if func.IsConstructor then
                    match func.EnclosingEntitySafe with
                    | Some ent -> if ent.IsFSharp then "new" ++ accessibility
                                  else accessibility
                    | _ -> accessibility
                elif func.IsProperty then
                    if func.IsInstanceMember then
                        if func.IsDispatchSlot then "abstract property" ++ accessibility
                        else "property" ++ accessibility
                    else "static property" ++ accessibility
                elif func.IsMember then
                    if func.IsInstanceMember then
                        if func.IsDispatchSlot then "abstract member" ++ accessibility
                        else "member" ++ accessibility
                    else "static member" ++ accessibility
                else
                    if func.InlineAnnotation = FSharpInlineAnnotation.AlwaysInline then "val" ++ accessibility ++ "inline"
                    elif func.IsInstanceMember then "val" ++ accessibility
                    else "val" ++ accessibility //does this need to be static prefixed?
            modifier

        let argInfos =
            func.CurriedParameterGroups
            |> Seq.map Seq.toList
            |> Seq.toList

        let retType =
            //This try block will be removed when FCS updates
            try func.ReturnParameter.Type |> format displayContext
            with _ex -> "Unknown"

        let retTypeConstraint =
            if func.ReturnParameter.Type.IsGenericParameter then
                let formattedParam = formatGenericParameter false displayContext func.ReturnParameter.Type.GenericParameter
                if String.IsNullOrWhiteSpace formattedParam then formattedParam
                else "(requires " + formattedParam + " )"
            else ""

        let padLength =
            let allLengths =
                argInfos
                |> List.concat
                |> List.map (fun p -> match p.Name with Some name -> name.Length | None -> p.DisplayName.Length)
            match allLengths with
            | [] -> 0
            | l -> l |> List.maxUnderThreshold maxPadding

        let formatName indent padding (parameter:FSharpParameter) =
            let name = match parameter.Name with Some name -> name | None -> parameter.DisplayName
            indent + name.PadRight padding + ":"

        let isDelegate =
            match func.EnclosingEntitySafe with
            | Some ent -> ent.IsDelegate
            | _ -> false

        let formatParameter (p:FSharpParameter) =
            try p.Type |> format displayContext
            with :? InvalidOperationException -> p.DisplayName

        match argInfos with
        | [] ->
            //When does this occur, val type within  module?
            if isDelegate then retType
            else modifiers ++ functionName + ":" ++ retType

        | [[]] ->
            if isDelegate then retType
            //A ctor with () parameters seems to be a list with an empty list.
            // Also abstract members and abstract member overrides with one () parameter seem to be a list with an empty list.
            elif func.IsConstructor || (func.IsMember && (not func.IsPropertyGetterMethod)) then modifiers + ": unit -> " ++ retType
            else modifiers ++ functionName + ":" ++ retType //Value members seems to be a list with an empty list
        | [[p]] when  maybeGetter && formatParameter p = "unit" -> //Member or property with only getter
            modifiers ++ functionName + ":" ++ retType
        | many ->

            let allParamsLengths =
                many |> List.map (List.map (fun p -> (formatParameter p).Length) >> List.sum)
            let maxLength = (allParamsLengths |> List.maxUnderThreshold maxPadding)+1

            let parameterTypeWithPadding (p: FSharpParameter) length =
                (formatParameter p) + (String.replicate (if length >= maxLength then 1 else maxLength - length) " ")

            let formatParameterPadded length p =
                let paddedParam = formatName indent padLength p ++ (parameterTypeWithPadding p length)
                if p.Type.IsGenericParameter then
                    let paramConstraint =
                        let formattedParam = formatGenericParameter false displayContext p.Type.GenericParameter
                        if String.IsNullOrWhiteSpace formattedParam then formattedParam
                        else "(requires " + formattedParam + " )"
                    if paramConstraint = retTypeConstraint then paddedParam
                    else paddedParam + paramConstraint
                else paddedParam

            let allParams =
                List.zip many allParamsLengths
                |> List.map(fun (paramTypes, length) ->
                                paramTypes
                                |> List.map (formatParameterPadded length)
                                |> String.concat (" *\n"))
                |> String.concat ("->\n")

            let typeArguments =
                allParams +  "\n" + indent + (String.replicate (max (padLength-1) 0) " ") + "->" ++ retType ++ retTypeConstraint

            if isDelegate then typeArguments
            else modifiers ++ functionName + ": \n" + typeArguments

    and getFuncSignatureForTypeSignature displayContext (func: FSharpMemberOrFunctionOrValue) (getter: bool) (setter : bool) =
        let functionName =
            let name =
                if func.IsConstructor then "new"
                elif func.IsOperatorOrActivePattern then func.DisplayName
                elif func.DisplayName.StartsWith "( " then PrettyNaming.QuoteIdentifierIfNeeded func.LogicalName
                elif func.LogicalName.StartsWith "get_" || func.LogicalName.StartsWith "set_" then PrettyNaming.TryChopPropertyName func.DisplayName |> Option.fill func.DisplayName
                else func.DisplayName
            name

        let modifiers =
            let accessibility =
                match func.Accessibility with
                | a when a.IsInternal -> "internal"
                | a when a.IsPrivate -> "private"
                | _ -> ""

            let modifier =
                //F# types are prefixed with new, should non F# types be too for consistancy?
                if func.IsConstructor then
                      accessibility
                elif func.IsProperty then
                    if func.IsInstanceMember then
                        if func.IsDispatchSlot then "abstract property" ++ accessibility
                        else "property" ++ accessibility
                    else "static property" ++ accessibility
                elif func.IsMember then
                    if func.IsInstanceMember then
                        if func.IsDispatchSlot then "abstract member" ++ accessibility
                        else "member" ++ accessibility
                    else "static member" ++ accessibility
                else
                    if func.InlineAnnotation = FSharpInlineAnnotation.AlwaysInline then "val" ++ accessibility ++ "inline"
                    elif func.IsInstanceMember then "val" ++ accessibility
                    else "val" ++ accessibility //does this need to be static prefixed?
            modifier

        let argInfos =
            func.CurriedParameterGroups
            |> Seq.map Seq.toList
            |> Seq.toList

        let retType =
            //This try block will be removed when FCS updates
            try
                if func.IsConstructor then
                    match func.EnclosingEntitySafe with
                    | Some ent -> ent.DisplayName
                    | _ -> func.DisplayName
                else
                    func.ReturnParameter.Type |> format displayContext
            with _ex ->
                try
                    if func.FullType.GenericArguments.Count > 0 then
                        let lastArg = func.FullType.GenericArguments |> Seq.last
                        lastArg |> format displayContext
                    else "Unknown"
                with _ -> "Unknown"

        let formatName (parameter:FSharpParameter) =
            parameter.Name |> Option.getOrElse parameter.DisplayName

        let isDelegate =
            match func.EnclosingEntitySafe with
            | Some ent -> ent.IsDelegate
            | _ -> false

        let res =
            match argInfos with
            | [] ->
                //When does this occur, val type within  module?
                if isDelegate then retType
                else modifiers ++ functionName + ": " ++ retType

            | [[]] ->
                if isDelegate then retType
                elif func.IsConstructor then
                    modifiers + ": unit ->" ++ retType //A ctor with () parameters seems to be a list with an empty list
                else modifiers ++ functionName + ": " ++ retType //Value members seems to be a list with an empty list
            | many ->
                let formatParameter (p:FSharpParameter) =
                    try
                        p.Type |> format displayContext
                    with
                    | :? InvalidOperationException -> p.DisplayName

                let allParams =
                    many
                    |> List.map(fun (paramTypes) ->
                        paramTypes
                        |> List.map(fun p -> formatName p + ":" ++ (formatParameter p))
                        |> String.concat (" * "))
                    |> String.concat ("-> ")

                let typeArguments =
                    allParams ++ "->" ++ retType

                if isDelegate then typeArguments
                else modifiers ++ functionName + ": " +  typeArguments

        match getter, setter with
        | true, true -> res ++ "with get,set"
        | true, false -> res ++ "with get"
        | false, true -> res ++ "with set"
        | false, false -> res

    and getFuncSignature f c = getFuncSignatureWithIdent f c 3

    and getValSignature displayContext (v:FSharpMemberOrFunctionOrValue) =
        let retType = v.FullType  |> format displayContext
        let prefix =
            if v.IsMutable then "val mutable"
            else "val"
        let name =
            if v.DisplayName.StartsWith "( "
            then PrettyNaming.QuoteIdentifierIfNeeded v.LogicalName
            else v.DisplayName
        let constraints =
            match v.FullTypeSafe with
            | Some fulltype when fulltype.IsGenericParameter ->
                let formattedParam = formatGenericParameter false displayContext fulltype.GenericParameter
                if String.IsNullOrWhiteSpace formattedParam then None
                else Some formattedParam
            | _ -> None
        match constraints with
        | Some constraints -> prefix ++ name ++ ":" ++ constraints
        | None -> prefix ++ name ++ ":" ++ retType

    and getFieldSignature displayContext (field: FSharpField) =
        let retType = field.FieldType |> format displayContext
        match field.LiteralValue with
        | Some lv -> field.DisplayName + ":" ++ retType ++ "=" ++ (string lv)
        | None ->
            let prefix =
                if field.IsMutable then "val mutable"
                else "val"
            prefix ++ field.DisplayName + ":" ++ retType

    and getAPCaseSignature displayContext (apc:FSharpActivePatternCase) =
        let findVal =
            apc.Group.DeclaringEntity
            |> Option.bind (fun ent -> ent.MembersFunctionsAndValues
                                    |> Seq.tryFind (fun func -> func.DisplayName.Contains apc.DisplayName)
                                    |> Option.map (getFuncSignature displayContext))
            |> Option.bind (fun n ->
                try
                    Some (n.Split([|':' |], 2).[1])
                with _ -> None )
            |> Option.getOrElse ""
        sprintf "active pattern %s: %s" apc.Name findVal


    and getEntitySignature displayContext (fse: FSharpEntity) =
        let modifier =
            match fse.Accessibility with
            | a when a.IsInternal -> "internal "
            | a when a.IsPrivate -> "private "
            | _ -> ""

        let typeName =
            match fse with
            | _ when fse.IsFSharpModule -> "module"
            | _ when fse.IsEnum         -> "enum"
            | _ when fse.IsValueType    -> "struct"
            | _ when fse.IsNamespace    -> "namespace"
            | _ when fse.IsFSharpRecord -> "record"
            | _ when fse.IsFSharpUnion  -> "union"
            | _ when fse.IsInterface    -> "interface"
            | _                         -> "type"

        let enumtip () =
            " =\n  |" ++
            (fse.FSharpFields
            |> Seq.filter (fun f -> not f.IsCompilerGenerated)
            |> Seq.map (fun field -> match field.LiteralValue with
                                     | Some lv -> field.Name + " = " + (string lv)
                                     | None -> field.Name )
            |> String.concat ("\n  | " ) )

        let uniontip () =
            " =\n  |" ++ (fse.UnionCases
                          |> Seq.map (getUnioncaseSignature displayContext)
                          |> String.concat ("\n  | " ) )

        let delegateTip () =
            let invoker =
                fse.MembersFunctionsAndValues |> Seq.find (fun f -> f.DisplayName = "Invoke")
            let invokerSig = getFuncSignatureWithIdent displayContext invoker 6
            " =\n   delegate of\n" + invokerSig

        let typeTip () =
            let constrc =
                fse.MembersFunctionsAndValues
                |> Seq.filter (fun n -> n.IsConstructor && n.Accessibility.IsPublic)
                |> Seq.collect (fun f ->
                    seq {
                        yield getFuncSignatureForTypeSignature displayContext f false false
                        yield! f.Overloads false |> Option.map (Seq.map (fun f -> getFuncSignatureForTypeSignature displayContext f false false)) |> Option.defaultValue Seq.empty
                    })

                |> Seq.toArray

            let fields =
                fse.FSharpFields
                |> Seq.filter (fun n -> n.Accessibility.IsPublic ) //TODO: If defined in same project as current scope then show also internals
                |> Seq.sortBy (fun n -> n.DisplayName )
                |> Seq.map (getFieldSignature displayContext)
                |> Seq.toArray

            let funcs =
                fse.MembersFunctionsAndValues
                |> Seq.filter (fun n -> n.Accessibility.IsPublic && (not n.IsConstructor) ) //TODO: If defined in same project as current scope then show also internals
                |> Seq.groupBy (fun n -> n.FullName)
                |> Seq.collect (fun (_,v) ->
                    match v |> Seq.tryFind (fun f -> f.IsProperty) with
                    | Some prop ->
                        let getter = v |> Seq.exists (fun f -> f.IsPropertyGetterMethod)
                        let setter = v |> Seq.exists (fun f -> f.IsPropertySetterMethod)
                        [getFuncSignatureForTypeSignature displayContext prop getter setter] //Ensure properties are displayed only once, properly report
                    | None ->
                        let f = Seq.head v
                        [
                            yield getFuncSignatureForTypeSignature displayContext f false false
                            yield! f.Overloads false |> Option.map (Seq.map (fun f -> getFuncSignatureForTypeSignature displayContext f false false)) |> Option.defaultValue Seq.empty
                        ])
                |> Seq.toArray

            constrc, fields, funcs

        let typeDisplay =
            let name =
                if fse.GenericParameters.Count > 0 then
                    let paramsAndConstraints =
                        fse.GenericParameters
                        |> Seq.groupBy (fun p -> p.Name)
                        |> Seq.map (fun (name, constraints) ->
                                             let renderedConstraints =
                                                 constraints
                                                 |> Seq.map (formatGenericParameter false displayContext)
                                                 |>  String.concat " and"
                                             if String.IsNullOrWhiteSpace renderedConstraints
                                             then "'" + name
                                             else sprintf "'%s (requires %s)" name renderedConstraints )

                    fse.DisplayName + "<" + (paramsAndConstraints |> String.concat ",") + ">"
                else fse.DisplayName

            let basicName = modifier + typeName ++ name

            if fse.IsFSharpAbbreviation then
                let unannotatedType = fse.UnAnnotate()
                basicName ++ "=" ++ (unannotatedType.DisplayName)
            else
                basicName

        if fse.IsFSharpUnion then (typeDisplay + uniontip ()), ([||], [||], [||])
        elif fse.IsEnum then (typeDisplay + enumtip ()), ([||], [||], [||])
        elif fse.IsDelegate then (typeDisplay + delegateTip ()), ([||], [||], [||])
        else typeDisplay, typeTip ()



    let footerForType (entity:FSharpSymbolUse) =
        try
            match entity with
            | SymbolUse.MemberFunctionOrValue m ->
                sprintf "Full name: %s\nAssembly: %s" m.FullName m.Assembly.SimpleName

            | SymbolUse.Entity (c, _) ->
                sprintf "Full name: %s\nAssembly: %s" c.FullName c.Assembly.SimpleName

            | SymbolUse.Field f ->
                sprintf "Full name: %s\nAssembly: %s" f.FullName f.Assembly.SimpleName

            | SymbolUse.ActivePatternCase ap ->
                sprintf "Full name: %s\nAssembly: %s" ap.FullName ap.Assembly.SimpleName

            | SymbolUse.UnionCase uc ->
                sprintf "Full name: %s\nAssembly: %s" uc.FullName uc.Assembly.SimpleName
            | _ -> ""
        with
        | _ -> ""

    let compiledNameType (entity:FSharpSymbolUse) =
        try
            entity.Symbol.XmlDocSig
        with
        | _ -> ""

    ///Returns formated symbol signature and footer that can be used to enhance standard FCS' text tooltips
    let getTooltipDetailsFromSymbolUse (symbol:FSharpSymbolUse) =
        let cn = compiledNameType symbol
        match symbol with
        | SymbolUse.Entity (fse,_) ->
            try
                let signature = getEntitySignature symbol.DisplayContext fse
                Some(signature, footerForType symbol, cn)
            with _ ->
                None

        | SymbolUse.Constructor func ->
            match func.EnclosingEntitySafe with
            | Some ent when ent.IsValueType || ent.IsEnum ->
                  //ValueTypes
                  let signature = getFuncSignature symbol.DisplayContext func
                  Some((signature, ([||], [||], [||])), footerForType symbol, cn)
            | _ ->
                  //ReferenceType constructor
                  let signature = getFuncSignature symbol.DisplayContext func
                  Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.Operator func ->
            let signature = getFuncSignature symbol.DisplayContext func
            Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.Pattern func ->
            //Active pattern or operator
            let signature = getFuncSignature symbol.DisplayContext func
            Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.Property prop ->
            let signature = getFuncSignature symbol.DisplayContext prop
            Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.ClosureOrNestedFunction func ->
            //represents a closure or nested function
            let signature = getFuncSignature symbol.DisplayContext func
            Some((signature, ([||], [||], [||])), footerForType symbol, cn)

        | SymbolUse.Function func ->
            let signature = getFuncSignature symbol.DisplayContext func
            Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.Val func ->
            //val name : Type
            let signature = getValSignature symbol.DisplayContext func
            Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.Field fsf ->
            let signature = getFieldSignature symbol.DisplayContext fsf
            Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.UnionCase uc ->
            let signature = getUnioncaseSignature symbol.DisplayContext uc
            Some((signature, ([||], [||], [||])), footerForType symbol, cn)

        | SymbolUse.ActivePatternCase apc ->
            let signature = getAPCaseSignature symbol.DisplayContext apc
            Some((signature, ([||], [||], [||])),  footerForType symbol, cn)

        | SymbolUse.ActivePattern ap ->
            let signature = getFuncSignature symbol.DisplayContext ap
            Some((signature, ([||], [||], [||])), footerForType symbol, cn)

        | SymbolUse.GenericParameter gp ->
            let signature =
                sprintf "%s (requires %s)"
                    (if gp.IsSolveAtCompileTime then "^" + gp.Name else "'" + gp.Name)
                    (formatGenericParameter false symbol.DisplayContext gp)
            Some((signature, ([||], [||], [||])), footerForType symbol, cn)

        | _ ->
            None