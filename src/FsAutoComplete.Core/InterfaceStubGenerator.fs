/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/CodeGeneration/InterfaceStubGenerator.fs
module FsAutoComplete.InterfaceStubGenerator

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open FsAutoComplete.CodeGenerationUtils
open FSharp.Compiler.Tokenization

/// Capture information about an interface in ASTs
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type InterfaceData =
    | Interface of SynType * SynMemberDefns option
    | ObjExpr of SynType * SynBinding list
    member x.Range =
        match x with
        | InterfaceData.Interface(typ, _) ->
            typ.Range
        | InterfaceData.ObjExpr(typ, _) ->
            typ.Range
    member x.TypeParameters =
        match x with
        | InterfaceData.Interface(typ, _)
        | InterfaceData.ObjExpr(typ, _) -> expandTypeParameters typ

/// Get associated member names and ranges
/// In case of properties, intrinsic ranges might not be correct for the purpose of getting
/// positions of 'member', which indicate the indentation for generating new members
let getMemberNameAndRanges = function
    | InterfaceData.Interface(_, None) ->
        []
    | InterfaceData.Interface(_, Some memberDefns) ->
        memberDefns
        |> Seq.choose (function (SynMemberDefn.Member(binding, _)) -> Some binding | _ -> None)
        |> Seq.choose (|MemberNameAndRange|_|)
        |> Seq.toList
    | InterfaceData.ObjExpr(_, bindings) ->
        List.choose (|MemberNameAndRange|_|) bindings

let private walkTypeDefn pos (SynTypeDefn(members = members; implicitConstructor = implicitCtor)) =
  Option.toList implicitCtor @ members
  |> List.tryPick (fun m ->
    if Range.rangeContainsPos m.Range pos
    then
      match m with
      | SynMemberDefn.Interface(interfaceType = iface; members = members) ->
        Some (InterfaceData.Interface(iface, members))
      | _ -> None
    else
      None
  )

let tryFindInterfaceDeclAt (pos: Position) (tree: ParsedInput) =
  SyntaxTraversal.Traverse(pos, tree, {
      new SyntaxVisitorBase<_>() with
        member _.VisitExpr (_, _, defaultTraverse, expr) =
          match expr with
            SynExpr.ObjExpr(objType = ty; argOptions = baseCallOpt; bindings = binds; extraImpls = ifaces) ->
                match baseCallOpt with
                | None ->
                    if Range.rangeContainsPos ty.Range pos then
                        Some (InterfaceData.ObjExpr(ty, binds))
                    else
                        ifaces
                        |> List.tryPick (fun (SynInterfaceImpl(interfaceTy = ty; bindings = binds; range = range)) ->
                            if Range.rangeContainsPos range pos then
                                Some (InterfaceData.ObjExpr(ty, binds))
                            else None
                          )
                | Some _ -> None
          | _ -> defaultTraverse expr
        override _.VisitModuleDecl (_, defaultTraverse, decl) =
          match decl with
          | SynModuleDecl.Types(types, _) ->
            List.tryPick (walkTypeDefn pos) types
          | _ -> defaultTraverse decl
    })

let tryFindInterfaceExprInBufferAtPos (codeGenService: CodeGenerationService) (pos: Position) (document : Document) =
    asyncMaybe {
        let! parseResults = codeGenService.ParseFileInProject(document.FullName)
        return! tryFindInterfaceDeclAt pos parseResults.ParseTree
    }

/// Return the interface identifier
/// Useful, to determine the insert position when no `with` keyword has been found.
let getInterfaceIdentifier (interfaceData : InterfaceData) (tokens : FSharpTokenInfo list) =
    let newKeywordIndex =
        match interfaceData with
        | InterfaceData.ObjExpr _ ->
            tokens
            // Find the `new` keyword
            |> List.findIndex(fun token ->
                token.CharClass = FSharpTokenCharKind.Keyword
                    && token.TokenName = "NEW"
            )
        | InterfaceData.Interface _ ->
            tokens
            // Find the `new` keyword
            |> List.findIndex(fun token ->
                token.CharClass = FSharpTokenCharKind.Keyword
                    && token.TokenName = "INTERFACE"
            )

    CodeGenerationUtils.findLastIdentifier tokens.[newKeywordIndex + 2..] tokens.[newKeywordIndex + 2]

/// Try to find the start column, so we know what the base indentation should be
let inferStartColumn  (codeGenServer : CodeGenerationService) (pos : Position) (doc : Document) (lines: ISourceText) (lineStr : string) (interfaceData : InterfaceData) (indentSize : int) =
    match getMemberNameAndRanges interfaceData with
    | (_, range) :: _ ->
        getLineIdent (lines.GetLineString(range.StartLine - 1))
    | [] ->
        match interfaceData with
        | InterfaceData.Interface _ as iface ->
            // 'interface ISomething with' is often in a new line, we use the indentation of that line
            getLineIdent lineStr + indentSize
        | InterfaceData.ObjExpr _ as iface ->
            match codeGenServer.TokenizeLine(doc.FullName, pos.Line) with
            | Some tokens ->
                tokens
                |> List.tryPick (fun (t: FSharpTokenInfo) ->
                        if t.CharClass = FSharpTokenCharKind.Keyword && t.TokenName = "NEW" then
                            // We round to nearest so the generated code will align on the indentation guides
                            findGreaterMultiple (t.LeftColumn + indentSize) indentSize
                            |> Some
                        else None)
                // There is no reference point, we indent the content at the start column of the interface
                |> Option.defaultValue iface.Range.StartColumn
            | None -> iface.Range.StartColumn

/// Return None, if we failed to handle the interface implementation
/// Return Some (insertPosition, generatedString):
/// `insertPosition`: representation the position where the editor should insert the `generatedString`
let handleImplementInterface (codeGenServer : CodeGenerationService) (checkResultForFile: ParseAndCheckResults) (pos : Position) (doc : Document) (lines: ISourceText) (lineStr : string) (interfaceData : InterfaceData) =
    async {
        let! result = asyncMaybe {
            let! _symbol, symbolUse = codeGenServer.GetSymbolAndUseAtPositionOfKind(doc.FullName, pos, SymbolKind.Ident)
            let thing =
                match symbolUse with
                | None -> None
                | Some symbolUse ->
                  match symbolUse.Symbol with
                  | :? FSharpEntity as entity ->
                      if isInterface entity then
                          match checkResultForFile.GetCheckResults.GetDisplayContextForPos(pos) with
                          | Some displayContext ->
                            Some (interfaceData, displayContext, entity)
                          | None -> None
                      else
                          None
                  | _ -> None
            return! thing
        }

        match result with
        | Some (interfaceData, displayContext, entity) ->
            let getMemberByLocation (name, range: Range) =
                asyncMaybe {
                    let pos = Position.fromZ (range.StartLine - 1) (range.StartColumn + 1)
                    return! checkResultForFile.GetCheckResults.GetSymbolUseAtLocation (pos.Line, pos.Column, lineStr, [])
                }

            let insertInfo =
                match codeGenServer.TokenizeLine(doc.FullName, pos.Line) with
                | Some tokens -> findLastPositionOfWithKeyword tokens entity pos (getInterfaceIdentifier interfaceData)
                | None -> None

            let desiredMemberNamesAndRanges = getMemberNameAndRanges interfaceData
            let! implementedSignatures =
                getImplementedMemberSignatures getMemberByLocation displayContext desiredMemberNamesAndRanges

            let generatedString =
                let formattedString =
                    formatMembersAt
                        (inferStartColumn codeGenServer pos doc lines lineStr interfaceData 4) // 4 here correspond to the indent size
                        4 // Should we make it a setting from the IDE ?
                        interfaceData.TypeParameters
                        "$objectIdent"
                        "$methodBody"
                        displayContext
                        implementedSignatures
                        entity
                        getInterfaceMembers
                        true // Always generate the verbose version of the code

                // If we are in a object expression, we remove the last new line, so the `}` stay on the same line
                match interfaceData with
                | InterfaceData.Interface _ ->
                    formattedString
                | InterfaceData.ObjExpr _ ->
                    formattedString.TrimEnd('\n')

            // If generatedString is empty it means nothing is missing to the interface
            // So we return None, in order to not show a "Falsy Hint"
            if String.IsNullOrEmpty generatedString then
                return None
            else
                match insertInfo with
                | Some (shouldAppendWith, insertPosition) ->
                    if shouldAppendWith then
                        return Some (insertPosition, " with" + generatedString)
                    else
                        return Some (insertPosition, generatedString)
                | None ->
                    // Unable to find an optimal insert position so return the position under the cursor
                    // By doing that we allow the user to copy/paste the code if the insertion break the code
                    // If we return None, then user would not benefit from interface stub generation at all
                    return Some (pos, generatedString)
        | None ->
            return None
    }
