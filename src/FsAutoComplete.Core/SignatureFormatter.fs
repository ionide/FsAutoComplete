namespace FsAutoComplete

[<AutoOpen>]
module PrintParameter =
    let print sb = Printf.bprintf sb "%s"


module SignatureFormatter =
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open System
    open System.Text

    let maxPadding = 20

    /// Concat two strings with a space between if both a and b are not IsNullOrWhiteSpace
    let internal (++) (a:string) (b:string) =
        match String.IsNullOrEmpty a, String.IsNullOrEmpty b with
        | true, true -> ""
        | false, true -> a
        | true, false -> b
        | false, false -> a + " " + b

    let getUnioncaseSignature displayContext (unionCase:FSharpUnionCase) =
        if unionCase.UnionCaseFields.Count > 0 then
            let typeList =
                unionCase.UnionCaseFields
                |> Seq.map (fun unionField ->
                    if unionField.Name.StartsWith "Item" then //TODO: Some better way of dettecting default names for the union cases' fields
                        unionField.FieldType.Format displayContext
                    else
                        unionField.Name ++ ":" ++ ((unionField.FieldType.Format displayContext)))
                |> String.concat " * "
            unionCase.DisplayName ++ "of" ++ typeList
         else unionCase.DisplayName

    let chopStringTo (s:string) (c:char) =
        // chopStringTo "abcdef" 'c' --> "def"
        if s.IndexOf c <> -1 then
            let i =  s.IndexOf c + 1
            s.Substring(i, s.Length - i)
        else
            s

    let tryChopPropertyName (s: string) =
        // member names start with get_ or set_ when the member is a property
        let s =
            if s.StartsWith("get_", StringComparison.Ordinal) ||
                s.StartsWith("set_", StringComparison.Ordinal)
            then s
            else chopStringTo s '.'

        if s.Length <= 4 || (let s = s.Substring(0,4) in s <> "get_" && s <> "set_") then
            None
        else
            Some(s.Substring(4,s.Length - 4))

    let formatGenericParameter displayContext (param:FSharpGenericParameter) =




        let asGenericParamName (param: FSharpGenericParameter) =
            (if param.IsSolveAtCompileTime then "^" else "'") + param.Name

        let sb = StringBuilder()

        print sb (asGenericParamName param)

        let getConstraintSymbols (constrainedBy: FSharpGenericParameterConstraint) =
            let memberConstraint (c: FSharpGenericParameterMemberConstraint) =

                let formattedMemberName, isProperty =
                    match c.IsProperty, tryChopPropertyName c.MemberName with
                    | true, Some(chopped) when chopped <> c.MemberName ->
                        chopped, true
                    | _, _ -> c.MemberName, false

                seq {
                    yield " : ("
                    if c.MemberIsStatic then yield "static "

                    yield "member "
                    yield formattedMemberName
                    yield " : "

                    if isProperty then
                        yield (c.MemberReturnType.Format displayContext)
                    else
                        if c.MemberArgumentTypes.Count <= 1 then
                            yield "unit"
                        else
                            yield asGenericParamName param
                        yield " -> "
                        yield ((c.MemberReturnType.Format displayContext).TrimStart())

                    yield ")"
                }

            let typeConstraint (tc: FSharpType) =
                seq {
                    yield " :> "
                    yield (tc.Format displayContext)
                }

            let constructorConstraint () =
                seq {
                    yield " : "
                    yield "("
                    yield "new"
                    yield " : "
                    yield "unit"
                    yield " -> '"
                    yield param.DisplayName
                    yield ")"
                }
            let enumConstraint (ec: FSharpType) =
                seq {
                    yield " : "
                    yield "enum"
                    yield "<"
                    yield ec.Format displayContext
                    yield ">"
                }

            let delegateConstraint (tc: FSharpGenericParameterDelegateConstraint) =
                seq {
                    yield " : "
                    yield "delegate"
                    yield "<"
                    yield tc.DelegateTupledArgumentType.Format displayContext
                    yield ", "
                    yield tc.DelegateReturnType.Format displayContext
                    yield ">"
                }

            let symbols =
                match constrainedBy with
                | _ when constrainedBy.IsCoercesToConstraint -> typeConstraint constrainedBy.CoercesToTarget
                | _ when constrainedBy.IsMemberConstraint -> memberConstraint constrainedBy.MemberConstraintData
                | _ when constrainedBy.IsSupportsNullConstraint -> seq { yield " : "; yield "null" }
                | _ when constrainedBy.IsRequiresDefaultConstructorConstraint -> constructorConstraint()
                | _ when constrainedBy.IsReferenceTypeConstraint -> seq { yield " : "; yield "not struct" }
                | _ when constrainedBy.IsEnumConstraint -> enumConstraint constrainedBy.EnumConstraintTarget
                | _ when constrainedBy.IsComparisonConstraint -> seq { yield " : "; yield "comparison" }
                | _ when constrainedBy.IsEqualityConstraint -> seq { yield " : "; yield "equality" }
                | _ when constrainedBy.IsDelegateConstraint -> delegateConstraint constrainedBy.DelegateConstraintData
                | _ when constrainedBy.IsUnmanagedConstraint -> seq { yield " : "; yield "unmanaged"}
                | _ when constrainedBy.IsNonNullableValueTypeConstraint -> seq { yield " : "; yield "struct" }
                | _ -> Seq.empty

            seq {
                yield " when "
                yield asGenericParamName param
                yield! symbols
            }

        if param.Constraints.Count > 0 then
            param.Constraints
            |> Seq.collect getConstraintSymbols
            |> Seq.iter(fun symbol -> print sb symbol)

        sb.ToString()

    let getFuncSignatureWithIdent displayContext (func: FSharpMemberOrFunctionOrValue) (ident:int) =
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
                    | _ ->
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
                func.ReturnParameter.Type.Format displayContext
            with _ex ->
                try
                    if func.FullType.GenericArguments.Count > 0 then
                        let lastArg = func.FullType.GenericArguments |> Seq.last
                        lastArg.Format displayContext
                    else "Unknown"
                with _ -> "Unknown"

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
            | _ ->
                false

        let formatParameter (p:FSharpParameter) =
            try
                p.Type.Format displayContext
            with
            | :? InvalidOperationException -> p.DisplayName

        match argInfos with
        | [] ->
            //When does this occur, val type within  module?
            if isDelegate then retType
            else modifiers ++ functionName + ":" ++ retType

        | [[]] ->
            if isDelegate then retType
            elif func.IsConstructor then modifiers + ": unit -> " ++ retType //A ctor with () parameters seems to be a list with an empty list
            else modifiers ++ functionName + ":" ++ retType //Value members seems to be a list with an empty list
        | [[p]] when  maybeGetter && formatParameter p = "unit" -> //Member or property with only getter
            modifiers ++ functionName + ":" ++ retType
        | many ->

            let allParamsLengths =
                many |> List.map (List.map (fun p -> (formatParameter p).Length) >> List.sum)
            let maxLength = (allParamsLengths |> List.maxUnderThreshold maxPadding)+1

            let parameterTypeWithPadding (p: FSharpParameter) length =
                (formatParameter p) + (String.replicate (if length >= maxLength then 1 else maxLength - length) " ")

            let allParams =
                List.zip many allParamsLengths
                |> List.map(fun (paramTypes, length) ->
                                paramTypes
                                |> List.map(fun p -> formatName indent padLength p ++ (parameterTypeWithPadding p length))
                                |> String.concat ("*" + "\n"))
                |> String.concat ("->\n")

            let typeArguments =
                allParams +  "\n" + indent + (String.replicate (max (padLength-1) 0) " ") + "->" ++ retType

            if isDelegate then typeArguments
            else modifiers ++ functionName + ": " + "\n" + typeArguments

    let getFuncSignatureForTypeSignature displayContext (func: FSharpMemberOrFunctionOrValue) (overloads : int) (getter: bool) (setter : bool) =
        let functionName =
            let name =
                if func.IsConstructor then "new"
                elif func.IsOperatorOrActivePattern then func.DisplayName
                elif func.DisplayName.StartsWith "( " then PrettyNaming.QuoteIdentifierIfNeeded func.LogicalName
                elif func.LogicalName.StartsWith "get_" || func.LogicalName.StartsWith "set_" then tryChopPropertyName func.DisplayName |> Option.fill func.DisplayName
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
                    | _ ->
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
                    func.ReturnParameter.Type.Format displayContext
            with _ex ->
                try
                    if func.FullType.GenericArguments.Count > 0 then
                        let lastArg = func.FullType.GenericArguments |> Seq.last
                        lastArg.Format displayContext
                    else "Unknown"
                with _ -> "Unknown"

        let formatName (parameter:FSharpParameter) =
            match parameter.Name with Some name -> name | None -> parameter.DisplayName

        let isDelegate =
            match func.EnclosingEntitySafe with
            | Some ent -> ent.IsDelegate
            | _ ->
                false

        let res =
            match argInfos with
            | [] ->
                //When does this occur, val type within  module?
                if isDelegate then retType
                else modifiers ++ functionName + ": " ++ retType

            | [[]] ->
                if isDelegate then retType
                elif func.IsConstructor then modifiers + ": unit ->" ++ retType //A ctor with () parameters seems to be a list with an empty list
                else modifiers ++ functionName + ": " ++ retType //Value members seems to be a list with an empty list
            | many ->
                let formatParameter (p:FSharpParameter) =
                    try
                        p.Type.Format displayContext
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

        let res =
            if overloads = 1 then
                res
            else
                sprintf "%s (+ %d overloads)" res (overloads - 1)

        match getter, setter with
        | true, true -> res ++ "with get,set"
        | true, false -> res ++ "with get"
        | false, true -> res ++ "with set"
        | false, false -> res

    let getFuncSignature f c = getFuncSignatureWithIdent f c 3

    let getValSignature displayContext (v:FSharpMemberOrFunctionOrValue) =
        let retType = v.FullType.Format displayContext
        let prefix =
            if v.IsMutable then "val" ++ "mutable"
            else "val"
        let name =
            if v.DisplayName.StartsWith "( "
            then PrettyNaming.QuoteIdentifierIfNeeded v.LogicalName
            else v.DisplayName
        prefix ++ name ++ ":" ++ retType

    let getFieldSignature displayContext (field: FSharpField) =
        let retType = field.FieldType.Format displayContext
        match field.LiteralValue with
        | Some lv -> field.DisplayName + ":" ++ retType ++ "=" ++ (string lv)
        | None ->
            let prefix =
                if field.IsMutable then "val" ++ "mutable"
                else "val"
            prefix ++ field.DisplayName + ":" ++ retType

    let getAPCaseSignature displayContext (apc:FSharpActivePatternCase) =
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
        sprintf "active recognizer %s: %s" apc.Name findVal

    let getEntitySignature displayContext (fse: FSharpEntity) =
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
            " =\n" +
            "  |" ++
            (fse.FSharpFields
            |> Seq.filter (fun f -> not f.IsCompilerGenerated)
            |> Seq.map (fun field -> match field.LiteralValue with
                                     | Some lv -> field.Name + " = " + (string lv)
                                     | None -> field.Name )
            |> String.concat ("\n" + "  | " ) )

        let uniontip () =
            " =" + "\n" +
            "  |" ++ (fse.UnionCases
                                  |> Seq.map (getUnioncaseSignature displayContext)
                                  |> String.concat ("\n" + "  | " ) )

        let delegateTip () =
            let invoker =
                fse.MembersFunctionsAndValues |> Seq.find (fun f -> f.DisplayName = "Invoke")
            let invokerSig = getFuncSignatureWithIdent displayContext invoker 6
            " =" + "\n" +
            "   " + "delegate" + " of\n" + invokerSig

        let typeTip () =
            let constrc =
                fse.MembersFunctionsAndValues
                |> Seq.filter (fun n -> n.IsConstructor)
                |> fun v ->
                    match Seq.tryHead v with
                    | None -> ""
                    | Some f ->
                        let l = Seq.length v
                        getFuncSignatureForTypeSignature displayContext f l false false

            let fields =
                fse.FSharpFields
                |> Seq.filter (fun n -> n.Accessibility.IsPublic ) //TODO: If defined in same project as current scope then show also internals
                |> Seq.sortBy (fun n -> n.DisplayName )
                |> Seq.map (getFieldSignature displayContext)

            let fields = if Seq.length fields > 11 then seq {yield! Seq.take 11 fields; yield "..." } else fields

            let funcs =
                fse.MembersFunctionsAndValues
                |> Seq.filter (fun n -> n.Accessibility.IsPublic ) //TODO: If defined in same project as current scope then show also internals
                |> Seq.filter (fun n -> not n.IsConstructor)
                |> Seq.groupBy (fun n -> n.FullName)
                |> Seq.map (fun (_,v) ->
                    match v |> Seq.tryFind (fun f -> f.IsProperty) with
                    | Some prop ->
                        let getter = v |> Seq.exists (fun f -> f.IsPropertyGetterMethod)
                        let setter = v |> Seq.exists (fun f -> f.IsPropertySetterMethod)
                        getFuncSignatureForTypeSignature displayContext prop 1 getter setter //Ensure properties are displayed only once, properly report
                    | None ->
                        let f = Seq.head v
                        let l = Seq.length v
                        getFuncSignatureForTypeSignature displayContext f l false false )

            let funcs = if Seq.length funcs > 11 then seq {yield! Seq.take 11 funcs; yield "..." } else funcs


            let res =
                [ yield constrc
                  yield! fields
                  if Seq.length fields > 0 then yield "\n"
                  yield! funcs]
                |> Seq.distinct
                |> String.concat "\n  "

            if String.IsNullOrWhiteSpace res then "" else  "\n  " + res

        let typeDisplay =
            let name =
                if fse.GenericParameters.Count > 0 then
                    let p = fse.GenericParameters |> Seq.map (formatGenericParameter displayContext) |> String.concat ","
                    fse.DisplayName + ("<") + p + (">")
                else fse.DisplayName

            let basicName = modifier + typeName ++ name

            if fse.IsFSharpAbbreviation then
                let unannotatedType = fse.UnAnnotate()
                basicName ++ "=" ++ (unannotatedType.DisplayName)
            else
                basicName

        if fse.IsFSharpUnion then typeDisplay + uniontip ()
        elif fse.IsEnum then typeDisplay + enumtip ()
        elif fse.IsDelegate then typeDisplay + delegateTip ()
        else typeDisplay + typeTip ()



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

    ///Returns formated symbol signature and footer that can be used to enhance standard FCS' text tooltips
    let getTooltipDetailsFromSymbolUse (symbol:FSharpSymbolUse) =
        match symbol with
        | SymbolUse.Entity (fse,_) ->
            try
                let signature = getEntitySignature symbol.DisplayContext fse
                Some(signature, footerForType symbol)
            with _ ->
                None

        | SymbolUse.Constructor func ->
            match func.EnclosingEntitySafe with
            | Some ent when ent.IsValueType || ent.IsEnum ->
                  //ValueTypes
                  let signature = getFuncSignature symbol.DisplayContext func
                  Some(signature, footerForType symbol)
            | _ ->
                  //ReferenceType constructor
                  let signature = getFuncSignature symbol.DisplayContext func
                  Some(signature,  footerForType symbol)

        | SymbolUse.Operator func ->
            let signature = getFuncSignature symbol.DisplayContext func
            Some(signature,  footerForType symbol)

        | SymbolUse.Pattern func ->
            //Active pattern or operator
            let signature = getFuncSignature symbol.DisplayContext func
            Some(signature,  footerForType symbol)

        | SymbolUse.Property prop ->
            let signature = getFuncSignature symbol.DisplayContext prop
            Some(signature,  footerForType symbol)

        | SymbolUse.ClosureOrNestedFunction func ->
            //represents a closure or nested function
            let signature = getFuncSignature symbol.DisplayContext func
            Some(signature, footerForType symbol)

        | SymbolUse.Function func ->
            let signature = getFuncSignature symbol.DisplayContext func
            Some(signature,  footerForType symbol)

        | SymbolUse.Val func ->
            //val name : Type
            let signature = getValSignature symbol.DisplayContext func
            Some(signature,  footerForType symbol)

        | SymbolUse.Field fsf ->
            let signature = getFieldSignature symbol.DisplayContext fsf
            Some(signature,  footerForType symbol)

        | SymbolUse.UnionCase uc ->
            let signature = getUnioncaseSignature symbol.DisplayContext uc
            Some(signature, footerForType symbol)

        | SymbolUse.ActivePatternCase apc ->
            let signature = getAPCaseSignature symbol.DisplayContext apc
            Some(signature,  footerForType symbol)

        | SymbolUse.ActivePattern ap ->
            let signature = getFuncSignature symbol.DisplayContext ap
            Some(signature, footerForType symbol)

        | SymbolUse.GenericParameter gp ->
            let signature = formatGenericParameter symbol.DisplayContext gp
            Some(signature, footerForType symbol)

        | _ ->
            None

