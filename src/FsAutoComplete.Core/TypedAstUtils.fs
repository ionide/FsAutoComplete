///Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/Common/TypedAstUtils.fs
namespace FsAutoComplete

open System
open System.Text.RegularExpressions
open FSharp.Compiler.SourceCodeServices
open UntypedAstUtils


[<AutoOpen>]
module TypedAstUtils =
  let isSymbolLocalForProject (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpParameter -> true
    | :? FSharpMemberOrFunctionOrValue as m ->
        not m.IsModuleValueOrMember
        || not m.Accessibility.IsPublic
    | :? FSharpEntity as m -> not m.Accessibility.IsPublic
    | :? FSharpGenericParameter -> true
    | :? FSharpUnionCase as m -> not m.Accessibility.IsPublic
    | :? FSharpField as m -> not m.Accessibility.IsPublic
    | _ -> false

  let getTypeIfConstructor (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpMemberOrFunctionOrValue as m ->
        match m.CompiledName with
        | ".ctor"
        | ".cctor" -> m.DeclaringEntity
        | _ -> None
    | _ -> None

  let isAttribute<'T> (attribute: FSharpAttribute) =
    // CompiledName throws exception on DataContractAttribute generated by SQLProvider
    match Option.attempt (fun _ -> attribute.AttributeType.CompiledName) with
    | Some name when name = typeof<'T>.Name -> true
    | _ -> false

  let hasAttribute<'T> (attributes: seq<FSharpAttribute>) =
    attributes |> Seq.exists isAttribute<'T>

  let tryGetAttribute<'T> (attributes: seq<FSharpAttribute>) =
    attributes |> Seq.tryFind isAttribute<'T>

  let hasModuleSuffixAttribute (entity: FSharpEntity) =
    entity.Attributes
    |> tryGetAttribute<CompilationRepresentationAttribute>
    |> Option.bind
         (fun a ->
           Option.attempt (fun _ -> a.ConstructorArguments)
           |> Option.bind
                (fun args ->
                  args
                  |> Seq.tryPick
                       (fun (_, arg) ->
                         let res =
                           match arg with
                           | :? int32 as arg when arg = int CompilationRepresentationFlags.ModuleSuffix -> Some()
                           | :? CompilationRepresentationFlags as arg when
                             arg = CompilationRepresentationFlags.ModuleSuffix -> Some()
                           | _ -> None

                         res)))
    |> Option.isSome

  let isOperator (name: string) =
    name.StartsWith "( "
    && name.EndsWith " )"
    && name.Length > 4
    && name.Substring(2, name.Length - 4)
       |> String.forall (fun c -> c <> ' ' && not (Char.IsLetter c))

  let private UnnamedUnionFieldRegex =
    Regex("^Item(\d+)?$", RegexOptions.Compiled)

  let isUnnamedUnionCaseField (field: FSharpField) =
    UnnamedUnionFieldRegex.IsMatch(field.Name)

[<AutoOpen>]
module TypedAstExtensionHelpers =
  type FSharpEntity with
    member x.TryGetFullName() =
      x.TryFullName
      |> Option.orElseWith (fun _ -> Option.attempt (fun _ -> String.Join(".", x.AccessPath, x.DisplayName)))

    member x.TryGetFullDisplayName() =
      let fullName =
        x.TryGetFullName()
        |> Option.map (fun fullName -> fullName.Split '.')

      let res =
        match fullName with
        | Some fullName ->
            match Option.attempt (fun _ -> x.DisplayName) with
            | Some shortDisplayName when not (shortDisplayName.Contains ".") ->
                Some(
                  fullName
                  |> Array.replace (fullName.Length - 1) shortDisplayName
                )
            | _ -> Some fullName
        | None -> None
        |> Option.map (fun fullDisplayName -> String.Join(".", fullDisplayName))
      //debug "GetFullDisplayName: FullName = %A, Result = %A" fullName res
      res

    member x.TryGetFullCompiledName() =
      let fullName =
        x.TryGetFullName()
        |> Option.map (fun fullName -> fullName.Split '.')

      let res =
        match fullName with
        | Some fullName ->
            match Option.attempt (fun _ -> x.CompiledName) with
            | Some shortCompiledName when not (shortCompiledName.Contains ".") ->
                Some(
                  fullName
                  |> Array.replace (fullName.Length - 1) shortCompiledName
                )
            | _ -> Some fullName
        | None -> None
        |> Option.map (fun fullDisplayName -> String.Join(".", fullDisplayName))
      //debug "GetFullCompiledName: FullName = %A, Result = %A" fullName res
      res

    member x.PublicNestedEntities =
      x.NestedEntities
      |> Seq.filter (fun entity -> entity.Accessibility.IsPublic)

    member x.TryGetMembersFunctionsAndValues =
      Option.attempt (fun _ -> x.MembersFunctionsAndValues)
      |> Option.defaultValue ([||] :> _)

    member x.TryGetFullNameWithUnderScoreTypes() =
      try
        let name =
          String.Join(".", x.AccessPath, x.DisplayName)

        if x.GenericParameters.Count > 0 then
          Some(
            name
            + "<"
            + String.concat
                ","
                (x.GenericParameters
                 |> Seq.map (fun gp -> gp.DisplayName))
            + ">"
          )
        else
          Some name
      with _ -> None

    member x.UnAnnotate() =
      let rec realEntity (s: FSharpEntity) =
        if s.IsFSharpAbbreviation then
          realEntity s.AbbreviatedType.TypeDefinition
        else
          s

      realEntity x

    member x.InheritanceDepth() =
      let rec loop (ent: FSharpEntity) l =
        match ent.BaseType with
        | Some bt -> loop (bt.TypeDefinition.UnAnnotate()) l + 1
        | None -> l

      loop x 0

    //TODO: Do we need to unannotate like above?
    member x.AllBaseTypes =
      let rec allBaseTypes (entity: FSharpEntity) =
        [ match entity.TryFullName with
          | Some _ ->
              match entity.BaseType with
              | Some bt ->
                  yield bt

                  if bt.HasTypeDefinition then
                    yield! allBaseTypes bt.TypeDefinition
              | _ -> ()
          | _ -> () ]

      allBaseTypes x

  type FSharpMemberOrFunctionOrValue with
    // FullType may raise exceptions (see https://github.com/fsharp/fsharp/issues/307).
    member x.FullTypeSafe = Option.attempt (fun _ -> x.FullType)

    member x.TryGetFullDisplayName() =
      let fullName =
        Option.attempt (fun _ -> x.FullName.Split '.')

      match fullName with
      | Some fullName ->
          match Option.attempt (fun _ -> x.DisplayName) with
          | Some shortDisplayName when not (shortDisplayName.Contains ".") ->
              Some(
                fullName
                |> Array.replace (fullName.Length - 1) shortDisplayName
              )
          | _ -> Some fullName
      | None -> None
      |> Option.map (fun fullDisplayName -> String.Join(".", fullDisplayName))

    member x.TryGetFullCompiledOperatorNameIdents(): Idents option =
      // For operator ++ displayName is ( ++ ) compiledName is op_PlusPlus
      if isOperator x.DisplayName
         && x.DisplayName <> x.CompiledName then
        x.DeclaringEntity
        |> Option.bind (fun e -> e.TryGetFullName())
        |> Option.map
             (fun enclosingEntityFullName -> Array.append (enclosingEntityFullName.Split '.') [| x.CompiledName |])
      else
        None

    member x.IsConstructor = x.CompiledName = ".ctor"

    member x.IsOperatorOrActivePattern =
      let name = x.DisplayName

      if name.StartsWith "( "
         && name.EndsWith " )"
         && name.Length > 4 then
        name.Substring(2, name.Length - 4)
        |> String.forall (fun c -> c <> ' ')
      else
        false

    member x.EnclosingEntitySafe =
      try
        x.DeclaringEntity
      with :? InvalidOperationException -> None

  type FSharpAssemblySignature with
    member x.TryGetEntities() =
      try
        x.Entities :> _ seq
      with _ -> Seq.empty

  type FSharpSymbol with

    member this.GetAbbriviatedParent() =
      match this with
      | :? FSharpEntity as m ->
          if m.IsFSharpAbbreviation then
            m.AbbreviatedType.TypeDefinition.GetAbbriviatedParent()
          else
            this
      | _ -> this

    member this.IsPrivateToFile =
      match this with
      | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember
      | :? FSharpEntity as m -> m.Accessibility.IsPrivate
      | :? FSharpGenericParameter -> true
      | :? FSharpUnionCase as m -> m.Accessibility.IsPrivate
      | :? FSharpField as m -> m.Accessibility.IsPrivate
      | _ -> false

    member this.IsInternalToProject =
      match this with
      | :? FSharpParameter -> true
      | :? FSharpMemberOrFunctionOrValue as m ->
          not m.IsModuleValueOrMember
          || not m.Accessibility.IsPublic
      | :? FSharpEntity as m -> not m.Accessibility.IsPublic
      | :? FSharpGenericParameter -> true
      | :? FSharpUnionCase as m -> not m.Accessibility.IsPublic
      | :? FSharpField as m -> not m.Accessibility.IsPublic
      | _ -> false

    member x.XmlDocSig =
      match x with
      | :? FSharpMemberOrFunctionOrValue as func -> func.XmlDocSig
      | :? FSharpEntity as fse -> fse.XmlDocSig
      | :? FSharpField as fsf -> fsf.XmlDocSig
      | :? FSharpUnionCase as fsu -> fsu.XmlDocSig
      | :? FSharpActivePatternCase as apc -> apc.XmlDocSig
      | :? FSharpGenericParameter -> ""
      | _ -> ""

    member x.XmlDoc =
      match x with
      | :? FSharpMemberOrFunctionOrValue as func -> func.XmlDoc
      | :? FSharpEntity as fse -> fse.XmlDoc
      | :? FSharpField as fsf -> fsf.XmlDoc
      | :? FSharpUnionCase as fsu -> fsu.XmlDoc
      | :? FSharpActivePatternCase as apc -> apc.XmlDoc
      | :? FSharpGenericParameter as gp -> gp.XmlDoc
      | _ -> ResizeArray() :> Collections.Generic.IList<_>

  type FSharpGenericParameterMemberConstraint with
    member x.IsProperty =
      (x.MemberIsStatic
       && x.MemberArgumentTypes.Count = 0)
      || (not x.MemberIsStatic
          && x.MemberArgumentTypes.Count = 1)
