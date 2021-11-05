/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/CodeGeneration/CodeGeneration.fs
namespace FsAutoComplete

open System
open System.IO
open System.CodeDom.Compiler
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FSharp.Compiler.Tokenization
open FSharp.Compiler.CodeAnalysis

[<Measure>] type Line0
[<Measure>] type Line1

type CodeGenerationService(checker : FSharpCompilerServiceChecker, state : State) =
    member x.TokenizeLine(fileName, i) =
        match state.TryGetFileCheckerOptionsWithLines fileName with
        | ResultOrString.Error _ -> None
        | ResultOrString.Ok (opts, text) ->
            try
                let line = text.GetLineString (i - 1)
                Lexer.tokenizeLine [||] line |> Some
            with
            | _ -> None

    member x.GetSymbolAtPosition(fileName, pos: Position) =
        match state.TryGetFileCheckerOptionsWithLinesAndLineStr(fileName, pos) with
        | ResultOrString.Error _ -> None
        | ResultOrString.Ok (opts, lines, line) ->
            try
                Lexer.getSymbol pos.Line pos.Column line SymbolLookupKind.Fuzzy [||]
            with
            | _ -> None

    member x.GetSymbolAndUseAtPositionOfKind(fileName, pos: Position, kind) =
        asyncMaybe {
            let! symbol = x.GetSymbolAtPosition(fileName,pos)
            if symbol.Kind = kind then
                match state.TryGetFileCheckerOptionsWithLinesAndLineStr(fileName, pos) with
                | ResultOrString.Error _ -> return! None
                | ResultOrString.Ok (opts, _, line) ->
                    let! result = checker.TryGetRecentCheckResultsForFile(fileName, opts)
                    let symbolUse = result.TryGetSymbolUse pos line
                    return! Some (symbol, symbolUse)
            else
                return! None
        }

    member x.ParseFileInProject(fileName) =
        match state.TryGetFileCheckerOptionsWithLines fileName with
        | ResultOrString.Error _ -> None
        | ResultOrString.Ok (opts, lines) ->
            try
                checker.TryGetRecentCheckResultsForFile(fileName, opts) |> Option.map (fun n -> n.GetParseResults)
            with
            | _ -> None

module CodeGenerationUtils =
    open FSharp.Compiler.Syntax.PrettyNaming

    type ColumnIndentedTextWriter() =
        let stringWriter = new StringWriter()
        let indentWriter = new IndentedTextWriter(stringWriter, " ")

        member __.Write(s: string) =
            indentWriter.Write("{0}", s)

        member __.Write(s: string, [<ParamArray>] objs: obj []) =
            indentWriter.Write(s, objs)

        member __.WriteLine(s: string) =
            indentWriter.WriteLine("{0}", s)

        member __.WriteLine(s: string, [<ParamArray>] objs: obj []) =
            indentWriter.WriteLine(s, objs)

        member x.WriteBlankLines count =
            for _ in 0 .. count - 1 do
                x.WriteLine ""

        member __.Indent i =
            indentWriter.Indent <- indentWriter.Indent + i

        member __.Unindent i =
            indentWriter.Indent <- max 0 (indentWriter.Indent - i)

        member __.Dump() =
            indentWriter.InnerWriter.ToString()

        interface IDisposable with
            member __.Dispose() =
                stringWriter.Dispose()
                indentWriter.Dispose()

    let hasAttribute<'T> (attrs: seq<FSharpAttribute>) =
        attrs |> Seq.exists (fun a -> a.AttributeType.CompiledName = typeof<'T>.Name)

    let rec internal getNonAbbreviatedType (typ: FSharpType) =
      if typ.HasTypeDefinition && typ.TypeDefinition.IsFSharpAbbreviation then
          getNonAbbreviatedType typ.AbbreviatedType
      else typ

    let tryFindTokenLPosInRange (codeGenService: CodeGenerationService) (range: Range) (document: Document) (predicate: FSharpTokenInfo -> bool) =
        // Normalize range
        // NOTE: FCS compiler sometimes returns an invalid range. In particular, the
        // range end limit can exceed the end limit of the document
        let range =
            if range.EndLine > document.LineCount then
                let newEndLine = document.LineCount
                let newEndColumn = document.GetLineText1(document.LineCount).Length
                let newEndPos = Position.mkPos newEndLine newEndColumn

                Range.mkRange range.FileName range.Start newEndPos
            else
                range

        let lineIdxAndTokenSeq = seq {
            for lineIdx = range.StartLine to range.EndLine do
              match codeGenService.TokenizeLine(document.FullName, lineIdx)
                    |> Option.map (List.map (fun tokenInfo -> lineIdx * 1<Line1>, tokenInfo)) with
              | Some xs -> yield! xs
              | None -> ()
        }

        lineIdxAndTokenSeq
        |> Seq.tryFind (fun (line1, tokenInfo) ->
            if range.StartLine = range.EndLine then
                tokenInfo.LeftColumn >= range.StartColumn &&
                tokenInfo.RightColumn < range.EndColumn &&
                predicate tokenInfo
            elif range.StartLine = int line1 then
                tokenInfo.LeftColumn >= range.StartColumn &&
                predicate tokenInfo
            elif int line1 = range.EndLine then
                tokenInfo.RightColumn < range.EndColumn &&
                predicate tokenInfo
            else
                predicate tokenInfo
        )
        |> Option.map (fun (line1, tokenInfo) ->
            tokenInfo, (Position.fromZ (int line1 - 1) tokenInfo.LeftColumn)
        )

    /// Represent environment where a captured identifier should be renamed
    type NamesWithIndices = Map<string, Set<int>>

    let keywordSet = set FSharpKeywords.KeywordNames

    let getTypeParameterName (typar: FSharpGenericParameter) =
      (if typar.IsSolveAtCompileTime then "^" else "'") + typar.Name

    [<NoComparison>]
    type Context =
        {
            Writer: ColumnIndentedTextWriter
            /// Map generic types to specific instances for specialized interface implementation
            TypeInstantations: Map<string, string>
            /// Data for interface instantiation
            ArgInstantiations: (FSharpGenericParameter * FSharpType) seq
            /// Indentation inside method bodies
            Indentation: int
            /// Object identifier of the interface e.g. 'x', 'this', '__', etc.
            ObjectIdent: string
            /// A list of lines represents skeleton of each member
            MethodBody: string []
            /// Context in order to display types in the short form
            DisplayContext: FSharpDisplayContext
        }

    // Adapt from MetadataFormat module in FSharp.Formatting

    let (|AllAndLast|_|) (xs: 'T list) =
        match xs with
        | [] ->
            None
        | _ ->
            let revd = List.rev xs
            Some (List.rev revd.Tail, revd.Head)

    let bracket (str: string) =
        if str.Contains(" ") then "(" + str + ")" else str

    let formatType ctx (typ: FSharpType) =
        let genericDefinition = typ.Instantiate(Seq.toList ctx.ArgInstantiations).Format(ctx.DisplayContext)
        (genericDefinition, ctx.TypeInstantations)
        ||> Map.fold (fun s k v -> s.Replace(k, v))

    let normalizeArgName (namesWithIndices: NamesWithIndices) nm =
        match nm with
        | "()" -> nm, namesWithIndices
        | _ ->
            let nm = String.lowerCaseFirstChar nm
            let nm, index = String.extractTrailingIndex nm

            let index, namesWithIndices =
                match namesWithIndices |> Map.tryFind nm, index with
                | Some indexes, index ->
                    let rec getAvailableIndex idx =
                        if indexes |> Set.contains idx then
                            getAvailableIndex (idx + 1)
                        else idx
                    let index = index |> Option.defaultValue 1 |> getAvailableIndex
                    Some index, namesWithIndices |> Map.add nm (indexes |> Set.add index)
                | None, Some index -> Some index, namesWithIndices |> Map.add nm (Set.ofList [index])
                | None, None -> None, namesWithIndices |> Map.add nm Set.empty

            let nm =
                match index with
                | Some index -> sprintf "%s%d" nm index
                | None -> nm

            let nm = if Set.contains nm keywordSet then sprintf "``%s``" nm else nm
            nm, namesWithIndices

    // Format each argument, including its name and type
    let formatArgUsage ctx hasTypeAnnotation (namesWithIndices: Map<string, Set<int>>) (arg: FSharpParameter) =
        let nm =
            match arg.Name with
            | None ->
                if arg.Type.HasTypeDefinition && arg.Type.TypeDefinition.XmlDocSig = "T:Microsoft.FSharp.Core.unit" then "()"
                else sprintf "arg%d" (namesWithIndices |> Map.toSeq |> Seq.map snd |> Seq.sumBy Set.count |> max 1)
            | Some x -> x

        let nm, namesWithIndices = normalizeArgName namesWithIndices nm

        // Detect an optional argument
        let isOptionalArg = hasAttribute<OptionalArgumentAttribute> arg.Attributes
        let argName = if isOptionalArg then "?" + nm else nm
        (if hasTypeAnnotation && argName <> "()" then
            argName + ": " + formatType ctx arg.Type
        else argName),
        namesWithIndices

    let formatArgsUsage ctx hasTypeAnnotation (v: FSharpMemberOrFunctionOrValue) args =
        let isItemIndexer = (v.IsInstanceMember && v.DisplayName = "Item")
        let unit, argSep, tupSep = "()", " ", ", "
        let args, namesWithIndices =
            args
            |> List.fold (fun (argsSoFar: string list list, namesWithIndices) args ->
                let argsSoFar', namesWithIndices =
                    args
                    |> List.fold (fun (acc: string list, allNames) arg ->
                        let name, allNames = formatArgUsage ctx hasTypeAnnotation allNames arg
                        name :: acc, allNames) ([], namesWithIndices)
                List.rev argsSoFar' :: argsSoFar, namesWithIndices)
                ([], Map.ofList [ ctx.ObjectIdent, Set.empty ])
        args
        |> List.rev
        |> List.map (function
            | [] -> unit
            | [arg] when arg = unit -> unit
            | [arg] when not v.IsMember || isItemIndexer -> arg
            | args when isItemIndexer -> String.concat tupSep args
            | args -> bracket (String.concat tupSep args))
        |> String.concat argSep
        , namesWithIndices

    [<RequireQualifiedAccess; NoComparison>]
    type MemberInfo =
        | PropertyGetSet of FSharpMemberOrFunctionOrValue * FSharpMemberOrFunctionOrValue
        | Member of FSharpMemberOrFunctionOrValue

    let getArgTypes (ctx: Context) (v: FSharpMemberOrFunctionOrValue) =
        let argInfos = v.CurriedParameterGroups |> Seq.map Seq.toList |> Seq.toList

        let retType = v.ReturnParameter.Type

        let argInfos, retType =
            match argInfos, v.IsPropertyGetterMethod, v.IsPropertySetterMethod with
            | [ AllAndLast(args, last) ], _, true -> [ args ], Some last.Type
            | [[]], true, _ -> [], Some retType
            | _, _, _ -> argInfos, Some retType

        let retType =
            match retType with
            | Some typ ->
                let coreType = formatType ctx typ
                if v.IsEvent then
                    let isEventHandler =
                        typ.BaseType
                        |> Option.bind (fun t ->
                            if t.HasTypeDefinition then
                                t.TypeDefinition.TryGetFullName()
                             else None)
                        |> Option.exists ((=) "System.MulticastDelegate")
                    if isEventHandler then sprintf "IEvent<%s, _>" coreType else coreType
                else coreType
            | None ->
                "unit"

        argInfos, retType

    /// Convert a getter/setter to its canonical form
    let normalizePropertyName (v: FSharpMemberOrFunctionOrValue) =
        let displayName = v.DisplayName
        if (v.IsPropertyGetterMethod && displayName.StartsWith("get_")) ||
            (v.IsPropertySetterMethod && displayName.StartsWith("set_")) then
            displayName.[4..]
        else displayName

    let isEventMember (m: FSharpMemberOrFunctionOrValue) =
        m.IsEvent || hasAttribute<CLIEventAttribute> m.Attributes

        /// Rename a given argument if the identifier has been used


    let formatMember (ctx: Context) m verboseMode =
        let getParamArgs (argInfos: FSharpParameter list list) (ctx: Context) (v: FSharpMemberOrFunctionOrValue) =
            let args, namesWithIndices =
                match argInfos with
                | [[x]] when v.IsPropertyGetterMethod && x.Name.IsNone
                                && x.Type.TypeDefinition.XmlDocSig = "T:Microsoft.FSharp.Core.unit" ->
                    "", Map.ofList [ctx.ObjectIdent, Set.empty]
                | _  -> formatArgsUsage ctx verboseMode v argInfos

            if String.IsNullOrWhiteSpace(args) then ""
            elif args.StartsWith("(") then args
            elif v.CurriedParameterGroups.Count > 1 && (not verboseMode) then " " + args
            else sprintf "(%s)" args
            , namesWithIndices

        let preprocess (ctx: Context) (v: FSharpMemberOrFunctionOrValue) =
            let buildUsage argInfos =
                let parArgs, _ = getParamArgs argInfos ctx v
                match v.IsMember, v.IsInstanceMember, v.LogicalName, v.DisplayName with
                // Constructors
                | _, _, ".ctor", _ -> "new" + parArgs
                // Properties (skipping arguments)
                | _, true, _, name when v.IsPropertyGetterMethod || v.IsPropertySetterMethod ->
                    if name.StartsWith("get_") || name.StartsWith("set_") then name.[4..] else name
                // Ordinary instance members
                | _, true, _, name -> name + parArgs
                // Ordinary functions or values
                | false, _, _, name when
                    not (hasAttribute<RequireQualifiedAccessAttribute> v.ApparentEnclosingEntity.Attributes) ->
                    name + " " + parArgs
                // Ordinary static members or things (?) that require fully qualified access
                | _, _, _, name -> name + parArgs

            let modifiers =
                [ if v.InlineAnnotation = FSharpInlineAnnotation.AlwaysInline then yield "inline"
                  if v.Accessibility.IsInternal then yield "internal" ]

            let argInfos, retType = getArgTypes ctx v
            let usage = buildUsage argInfos
            usage, modifiers, argInfos, retType

        // A couple of helper methods for emitting close declarations of members and stub method bodies.
        let closeDeclaration (returnType:string) (writer:ColumnIndentedTextWriter) =
            if verboseMode then writer.Write(": {0}", returnType)
            writer.Write(" = ", returnType)
            if verboseMode then writer.WriteLine("")
        let writeImplementation (ctx:Context) (writer:ColumnIndentedTextWriter) =
            match verboseMode, ctx.MethodBody with
            | false, [| singleLine |] -> writer.WriteLine(singleLine)
            | _, lines ->
                writer.Indent ctx.Indentation
                for line in lines do
                    writer.WriteLine(line)
                writer.Unindent ctx.Indentation

        let memberPrefix (m: FSharpMemberOrFunctionOrValue) =
          if m.IsDispatchSlot
          then "override "
          else "member "

        match m with
        | MemberInfo.PropertyGetSet(getter, setter) ->
            let (usage, modifiers, getterArgInfos, retType) = preprocess ctx getter
            let closeDeclaration = closeDeclaration retType
            let writeImplementation = writeImplementation ctx
            let (_, _, setterArgInfos, _) = preprocess ctx setter
            let writer = ctx.Writer

            writer.Write(memberPrefix getter)
            for modifier in modifiers do
                writer.Write("{0} ", modifier)
            writer.Write("{0}.", ctx.ObjectIdent)

            // Try to print getters and setters on the same identifier
            writer.WriteLine(usage)
            writer.Indent ctx.Indentation
            match getParamArgs getterArgInfos ctx getter with
            | "", _ | "()", _ -> writer.Write("with get ()")
            | args, _ -> writer.Write("with get {0}", args)
            writer |> closeDeclaration
            writer |> writeImplementation
            match getParamArgs setterArgInfos ctx setter with
            | "", _ | "()", _ ->
                if verboseMode then writer.WriteLine("and set (v: {0}): unit = ", retType)
                else writer.Write("and set v = ")
            | args, namesWithIndices ->
                let valueArgName, _ = normalizeArgName namesWithIndices "v"
                if verboseMode then writer.WriteLine("and set {0} ({1}: {2}): unit = ", args, valueArgName, retType)
                else writer.Write("and set {0} {1} = ", args, valueArgName)
            writer |> writeImplementation
            writer.Unindent ctx.Indentation

        | MemberInfo.Member v ->
            let (usage, modifiers, argInfos, retType) = preprocess ctx v
            let closeDeclaration = closeDeclaration retType
            let writeImplementation = writeImplementation ctx
            let writer = ctx.Writer
            if isEventMember v then
                writer.WriteLine("[<CLIEvent>]")
            writer.Write(memberPrefix v)
            for modifier in modifiers do
                writer.Write("{0} ", modifier)
            writer.Write("{0}.", ctx.ObjectIdent)

            if v.IsEvent then
                writer.Write(usage)
                writer |> closeDeclaration
                writer |> writeImplementation
            elif v.IsPropertySetterMethod then
                writer.WriteLine(usage)
                writer.Indent ctx.Indentation
                match getParamArgs argInfos ctx v with
                | "", _ | "()", _ ->
                    writer.WriteLine("with set (v: {0}): unit = ", retType)
                | args, namesWithIndices ->
                    let valueArgName, _ = normalizeArgName namesWithIndices "v"
                    writer.Write("with set {0} ({1}", args, valueArgName)
                    if verboseMode then writer.Write(": {0}): unit", retType)
                    else writer.Write(")")
                    writer.Write(" = ")
                    if verboseMode then writer.WriteLine("")

                writer |> writeImplementation
                writer.Unindent ctx.Indentation
            elif v.IsPropertyGetterMethod then
                writer.Write(usage)
                match getParamArgs argInfos ctx v with
                | "", _ | "()", _ ->
                    // Use the short-hand notation for getters without arguments
                    writer |> closeDeclaration
                    writer |> writeImplementation
                | args, _ ->
                    writer.WriteLine("")
                    writer.Indent ctx.Indentation
                    writer.Write("with get {0}", args)
                    writer |> closeDeclaration
                    writer |> writeImplementation
                    writer.Unindent ctx.Indentation
            else
                writer.Write(usage)
                writer |> closeDeclaration
                writer |> writeImplementation

    // Sometimes interface members are stored in the form of `IInterface<'T> -> ...`,
    // so we need to get the 2nd generic argument
    let (|MemberFunctionType|_|) (typ: FSharpType) =
        if typ.IsFunctionType && typ.GenericArguments.Count = 2 then
            Some typ.GenericArguments.[1]
        else None

    let (|TypeOfMember|_|) (m: FSharpMemberOrFunctionOrValue) =
        match m.FullTypeSafe with
        | Some (MemberFunctionType typ) when m.IsProperty
                                            && m.DeclaringEntity.IsSome
                                            && m.DeclaringEntity.Value.IsFSharp ->
            Some typ
        | Some typ -> Some typ
        | None -> None

    let (|EventFunctionType|_|) (typ: FSharpType) =
        match typ with
        | MemberFunctionType typ ->
            if typ.IsFunctionType && typ.GenericArguments.Count = 2 then
                let retType = typ.GenericArguments.[0]
                let argType = typ.GenericArguments.[1]
                if argType.GenericArguments.Count = 2 then
                    Some (argType.GenericArguments.[0], retType)
                else None
            else None
        | _ ->
            None

    let removeWhitespace (str: string) =
        str.Replace(" ", "")

    /// Filter out duplicated interfaces in inheritance chain
    let rec internal getInterfaces (e: FSharpEntity) =
        seq { for iface in e.AllInterfaces ->
                let typ = getNonAbbreviatedType iface
                // Argument should be kept lazy so that it is only evaluated when instantiating a new type
                typ.TypeDefinition, Seq.zip typ.TypeDefinition.GenericParameters typ.GenericArguments
        }
        |> Seq.distinct

    /// Use this hack when FCS doesn't return enough information on .NET properties and events.
    /// we use this to filter out the 'meta' members in favor of providing the underlying members for template generation
    /// eg: a property _also_ has the relevant get/set members, so we don't need them.
    let isSyntheticMember (m: FSharpMemberOrFunctionOrValue) =
      m.IsProperty || m.IsEventAddMethod || m.IsEventRemoveMethod

    /// Get members in the decreasing order of inheritance chain
    let getInterfaceMembers (e: FSharpEntity) =
        seq {
            for (iface, instantiations) in getInterfaces e do
                yield! iface.TryGetMembersFunctionsAndValues()
                       |> Seq.choose (fun m ->
                           if isSyntheticMember m then
                               None
                           else Some (m, instantiations))
         }

    let isAbstractNonVirtualMember (m: FSharpMemberOrFunctionOrValue) =
      // is an abstract member
      m.IsDispatchSlot
      // this member doesn't implement anything
      && (try m.ImplementedAbstractSignatures <> null &&  m.ImplementedAbstractSignatures.Count = 0 with _ -> true) // exceptions here trying to acces the member means we're safe
      // this member is not an override
      && not m.IsOverrideOrExplicitInterfaceImplementation

    let isAbstractClass (e: FSharpEntity) = e.IsAbstractClass

    let getAbstractNonVirtualMembers (e: FSharpEntity) =
      seq {
         let genericParams = e.GenericParameters :> seq<_>
         // todo: generic param instantiations?
         yield!
          e.MembersFunctionsAndValues
          |> Seq.choose (fun m ->
              if isSyntheticMember m then None
              else if isAbstractNonVirtualMember m then Some(m, Seq.empty)
              else None
          )
      }

    /// Check whether an interface is empty
    let hasNoInterfaceMember e =
        getInterfaceMembers e |> Seq.isEmpty

    let (|LongIdentPattern|_|) = function
        | SynPat.LongIdent(LongIdentWithDots(xs, _), _, _, _, _, _) ->
    //            let (name, range) = xs |> List.map (fun x -> x.idText, x.idRange) |> List.last
            let last = List.last xs
            Some(last.idText, last.idRange)
        | _ ->
            None

    // Get name and associated range of a member
    // On merged properties (consisting both getters and setters), they have the same range values,
    // so we use 'get_' and 'set_' prefix to ensure corresponding symbols are retrieved correctly.
    let (|MemberNameAndRange|_|) = function
        | SynBinding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, SynValData(Some mf, _, _), LongIdentPattern(name, range),
                     _retTy, _expr, _bindingRange, _seqPoint) when mf.MemberKind = SynMemberKind.PropertyGet ->
            if name.StartsWith("get_") then Some(name, range) else Some("get_" + name, range)
        | SynBinding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, SynValData(Some mf, _, _), LongIdentPattern(name, range),
                     _retTy, _expr, _bindingRange, _seqPoint) when mf.MemberKind = SynMemberKind.PropertySet ->
            if name.StartsWith("set_") then Some(name, range) else Some("set_" + name, range)
        | SynBinding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, _valData, LongIdentPattern(name, range),
                     _retTy, _expr, _bindingRange, _seqPoint) ->
            Some(name, range)
        | _ ->
            None

    let normalizeEventName (m: FSharpMemberOrFunctionOrValue) =
        let name = m.DisplayName
        if name.StartsWith("add_") then name.[4..]
        elif name.StartsWith("remove_")  then name.[7..]
        else name

    /// Ideally this info should be returned in error symbols from FCS.
    /// Because it isn't, we implement a crude way of getting member signatures:
    ///  (1) Crack ASTs to get member names and their associated ranges
    ///  (2) Check symbols of those members based on ranges
    ///  (3) If any symbol found, capture its member signature
    let getImplementedMemberSignatures (getMemberByLocation: string * range -> Async<FSharpSymbolUse option>) displayContext memberNamesAndRanges =
        let formatMemberSignature (symbolUse: FSharpSymbolUse) =
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as m ->
                match m.FullTypeSafe with
                | Some _ when isEventMember m ->
                    // Events don't have overloads so we use only display names for comparison
                    let signature = normalizeEventName m
                    Some [signature]
                | Some typ ->
                    let signature = removeWhitespace (sprintf "%s:%s" m.DisplayName (typ.Format(displayContext)))
                    Some [signature]
                | None ->
                    None
            | _ ->
                fail "Should only accept symbol uses of members."
                None
        async {
            let! symbolUses =
                memberNamesAndRanges
                |> List.toArray
                |> Async.Array.map getMemberByLocation
            return symbolUses |> Array.choose (Option.bind formatMemberSignature >> Option.map String.Concat)
                              |> Set.ofArray
        }

    /// Check whether an entity is an interface or type abbreviation of an interface
    let rec isInterface (e: FSharpEntity) =
        e.IsInterface || (e.IsFSharpAbbreviation && isInterface e.AbbreviatedType.TypeDefinition)

    let findLastGreaterOperator (tokens : FSharpTokenInfo list) =
      tokens
      |> List.findBack(fun token ->
          token.CharClass = FSharpTokenCharKind.Operator
              && token.TokenName = "GREATER"
      )

    /// Return the greater multiple of `powerNumber` which is smaller than `value`
    /// Ex: roundToNearest 14 4 -> 12
    let findGreaterMultiple (value : int) (powerNumber : int) =
        let mutable res = powerNumber
        while res + powerNumber < value do
            res <- res + powerNumber
        res

    let findLastPositionOfWithKeyword (tokens: FSharpTokenInfo list) (entity: FSharpEntity) (pos: Position) (entityAdvancer) =
      let endPosOfWidth =
          tokens
          |> List.tryPick (fun (t: FSharpTokenInfo) ->
                  if t.CharClass = FSharpTokenCharKind.Keyword && t.LeftColumn >= pos.Column && t.TokenName = "WITH" then
                      Some (Position.fromZ (pos.Line - 1) (t.RightColumn + 1))
                  else None)

      match endPosOfWidth with
      // If we found the position of `with` keyword, return it as it will serve as a reference position for insertion
      | Some pos -> Some (false, pos)
      // `with` not found, so we need to find the end position of the interface identifer
      | None ->
          let position =
              if entity.GenericParameters.Count = 0 then
                  let token = entityAdvancer tokens
                  Position.fromZ (pos.Line - 1) (token.RightColumn + 1)
              // Otherwise, returns the position after the last greater angle (>)
              else
                  let token = findLastGreaterOperator tokens
                  Position.fromZ (pos.Line - 1) (token.RightColumn + 1)

          Some (true, position)

    let rec findLastIdentifier (tokens : FSharpTokenInfo list) (lastValidToken : FSharpTokenInfo) =
        match tokens with
        // This rule try to move on step in the namespace declaration
        // System.Collections.ICollection
        //          ^
        // { new System.Collections.ICollection }
        //                 ^
        | _::potentialDot::validIdentifier::tail
            when potentialDot.CharClass = FSharpTokenCharKind.Delimiter
                    && potentialDot.TokenName = "DOT"
                    && validIdentifier.CharClass = FSharpTokenCharKind.Identifier
                    && validIdentifier.TokenName = "IDENT" ->
            findLastIdentifier tail validIdentifier
        // This rule match when we are at the end of the namespace and there is some tokens left in the pile
        // { new System.Collections.ICollection }
        //                              ^
        | potentialDot::validIdentifier::_
            when potentialDot.CharClass = FSharpTokenCharKind.Delimiter
                    && potentialDot.TokenName = "DOT"
                    && validIdentifier.CharClass = FSharpTokenCharKind.Identifier
                    && validIdentifier.TokenName = "IDENT" ->
            validIdentifier
        // This rule match when we are at the end of the namespace and there is no more tokens
        // interface System.Collections.ICollection
        //                              ^
        | potentialDot::validIdentifier::[]
            when potentialDot.CharClass = FSharpTokenCharKind.Delimiter
                    && potentialDot.TokenName = "DOT"
                    && validIdentifier.CharClass = FSharpTokenCharKind.Identifier
                    && validIdentifier.TokenName = "IDENT" ->
            validIdentifier
        // If no special rule found, return the last valid token found
        | _ -> lastValidToken

    /// The code below is responsible for handling the code generation and determining the insert position
    let getLineIdent (lineStr: string) =
      lineStr.Length - lineStr.TrimStart(' ').Length

    let formatMembersAt startColumn indentation (typeInstances: string []) objectIdent
        (methodBody: string) (displayContext: FSharpDisplayContext) excludedMemberSignatures
        (e: FSharpEntity)
        (getMembersToImplement: FSharpEntity -> seq<FSharpMemberOrFunctionOrValue * seq<FSharpGenericParameter * FSharpType>>) verboseMode =
      let lines = String.getLines methodBody
      use writer = new ColumnIndentedTextWriter()
      let typeParams = Seq.map getTypeParameterName e.GenericParameters
      let instantiations =
          let insts =
              Seq.zip typeParams typeInstances
              // Filter out useless instances (when it is replaced by the same name or by wildcard)
              |> Seq.filter(fun (t1, t2) -> t1 <> t2 && t2 <> "_")
              |> Map.ofSeq
          // A simple hack to handle instantiation of type alias
          if e.IsFSharpAbbreviation then
              let typ = getNonAbbreviatedType e.AbbreviatedType
              (typ.TypeDefinition.GenericParameters |> Seq.map getTypeParameterName,
                  typ.GenericArguments |> Seq.map (fun typ -> typ.Format(displayContext)))
              ||> Seq.zip
              |> Seq.fold (fun acc (x, y) -> Map.add x y acc) insts
          else insts
      let ctx = { Writer = writer; TypeInstantations = instantiations; ArgInstantiations = Seq.empty;
                  Indentation = indentation; ObjectIdent = objectIdent; MethodBody = lines; DisplayContext = displayContext }
      let missingMembers =
          getMembersToImplement e
          |> Seq.groupBy (fun (m, insts) ->
              match m with
              | _ when isEventMember m  ->
                  Some (normalizeEventName m)
              | TypeOfMember typ ->
                  let signature = removeWhitespace (sprintf "%s:%s" m.DisplayName (formatType { ctx with ArgInstantiations = insts } typ))
                  Some signature
              | _ ->
                  debug "FullType throws exceptions due to bugs in FCS."
                  None)
          |> Seq.collect (fun (signature, members) ->
              match signature with
              | None ->
                  members
              | Some signature when not (Set.contains signature excludedMemberSignatures) ->
                  // Return the first member from a group of members for a particular signature
                  Seq.truncate 1 members
              | _ -> Seq.empty)

      // All members have already been implemented
      if Seq.isEmpty missingMembers then
          String.Empty
      else
          writer.Indent startColumn
          writer.WriteLine("")
          let duplicatedMembers =
              missingMembers
              |> Seq.countBy(fun (m, insts) -> m.DisplayName, insts |> Seq.length)
              |> Seq.filter (snd >> (<) 1)
              |> Seq.map (fst >> fst)
              |> Set.ofSeq

          let getReturnType v = snd (getArgTypes ctx v)
          let rec formatMembers (members : (FSharpMemberOrFunctionOrValue * _) list) =
              match members with
              // Since there is no unified source of information for properties,
              // we try to merge getters and setters when they seem to match.
              // Assume that getter and setter come right after each other.
              // They belong to the same property if names and return types are the same
              | (getter as first, insts) :: (setter, _) :: otherMembers
              | (setter as first, _) :: (getter, insts) :: otherMembers when
                  getter.IsPropertyGetterMethod && setter.IsPropertySetterMethod &&
                  normalizePropertyName getter = normalizePropertyName setter &&
                  getReturnType getter = getReturnType setter ->
                  let useVerboseMode = verboseMode || duplicatedMembers.Contains first.DisplayName
                  formatMember { ctx with ArgInstantiations = insts } (MemberInfo.PropertyGetSet(getter, setter)) useVerboseMode
                  formatMembers otherMembers
              | (m, insts) :: otherMembers ->
                  let useVerboseMode = verboseMode || duplicatedMembers.Contains m.DisplayName
                  formatMember { ctx with ArgInstantiations = insts } (MemberInfo.Member m) useVerboseMode
                  formatMembers otherMembers
              | [] -> ()

          missingMembers
          |> Seq.sortBy (fun (m, _) ->
              // Sort by normalized name and return type so that getters and setters of the same properties
              // are guaranteed to be neighboring.
              normalizePropertyName m, getReturnType m)
          |> Seq.toList
          |> formatMembers
          writer.Dump()

    let rec (|RationalConst|) = function
        | SynRationalConst.Integer i ->
            string i
        | SynRationalConst.Rational(numerator, denominator, _) ->
            sprintf "(%i/%i)" numerator denominator
        | SynRationalConst.Negate (RationalConst s) ->
            sprintf "- %s" s

    let rec (|TypeIdent|_|) = function
        | SynType.Var(SynTypar(s, req , _), _) ->
            match req with
            | TyparStaticReq.None ->
                Some ("'" + s.idText)
            | TyparStaticReq.HeadType ->
                Some ("^" + s.idText)
        | SynType.LongIdent(LongIdentWithDots(xs, _)) ->
            xs |> Seq.map (fun x -> x.idText) |> String.concat "." |> Some
        | SynType.App(t, _, ts, _, _, isPostfix, _) ->
            match t, ts with
            | TypeIdent typeName, [] -> Some typeName
            | TypeIdent typeName, [TypeIdent typeArg] ->
                if isPostfix then
                    Some (sprintf "%s %s" typeArg typeName)
                else
                    Some (sprintf "%s<%s>" typeName typeArg)
            | TypeIdent typeName, _ ->
                let typeArgs = ts |> Seq.choose (|TypeIdent|_|) |> String.concat ", "
                if isPostfix then
                    Some (sprintf "(%s) %s" typeArgs typeName)
                else
                    Some(sprintf "%s<%s>" typeName typeArgs)
            | _ ->
                debug "Unsupported case with %A and %A" t ts
                None
        | SynType.Anon _ ->
            Some "_"
        | SynType.Tuple(_, ts, _) ->
            Some (ts |> Seq.choose (snd >> (|TypeIdent|_|)) |> String.concat " * ")
        | SynType.Array(dimension, TypeIdent typeName, _) ->
            Some (sprintf "%s [%s]" typeName (String(',', dimension-1)))
        | SynType.MeasurePower(TypeIdent typeName, RationalConst power, _) ->
            Some (sprintf "%s^%s" typeName power)
        | SynType.MeasureDivide(TypeIdent numerator, TypeIdent denominator, _) ->
            Some (sprintf "%s/%s" numerator denominator)
        | _ ->
            None

    let expandTypeParameters (typ: SynType) =
      match typ with
      | SynType.App(_, _, ts, _, _, _, _)
      | SynType.LongIdentApp(_, _, _, ts, _, _, _) ->
          ts |> Seq.choose (|TypeIdent|_|) |> Seq.toArray
      | _ ->
          [||]
