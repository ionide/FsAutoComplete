module FsAutoComplete.Decompiler

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Utils
open dnlib
open dnlib.DotNet
open dnSpy.Contracts.Decompiler
open dnSpy.Decompiler.ILSpy.Core
open System.Collections.Generic
open System.Text.RegularExpressions

type Position = Position of line:int * column:int

let private tryGetValue key (dict:IReadOnlyDictionary<_,_>) =
    match dict.TryGetValue(key) with
    | true, x -> Some x
    | false, _ -> None

type OutputPositions =
    OutputPositions of
        methodsPosition:IReadOnlyDictionary<MethodDef, Position> *
        fieldsPosition:IReadOnlyDictionary<FieldDef, Position> *
        propertiesPosition:IReadOnlyDictionary<PropertyDef, Position> *
        eventsPosition:IReadOnlyDictionary<EventDef, Position> *
        typesPosition:IReadOnlyDictionary<TypeDef, Position>

type OutputPositions with
    member this.TryFindMethod def =
        let (OutputPositions (methodsPosition=methodsPosition)) = this
        methodsPosition |> tryGetValue def

    member this.TryFindField def =
        let (OutputPositions (fieldsPosition=fieldsPosition)) = this
        fieldsPosition |> tryGetValue def

    member this.TryFindProperty def =
        let (OutputPositions (propertiesPosition=propertiesPosition)) = this
        propertiesPosition |> tryGetValue def

    member this.TryFindEvent def =
        let (OutputPositions (eventsPosition=eventsPosition)) = this
        eventsPosition |> tryGetValue def

    member this.TryFindType def =
        let (OutputPositions (typesPosition=typesPosition)) = this
        typesPosition |> tryGetValue def


type TextWriterWithPositions(writer: TextWriter) =
    inherit TextWriterDecompilerOutput(writer)

    let mutable line = 1
    let mutable lineStartAtPosition = 0
    let methodsPosition = System.Collections.Generic.Dictionary<MethodDef, Position>()
    let fieldsPosition = System.Collections.Generic.Dictionary<FieldDef, Position>()
    let propertiesPosition = System.Collections.Generic.Dictionary<PropertyDef, Position>()
    let eventsPosition = System.Collections.Generic.Dictionary<EventDef, Position>()
    let typesPosition = System.Collections.Generic.Dictionary<TypeDef, Position>()

    let storeDefinitionIfNeeded nextPosition (reference:obj) =
        let position = Position(line, nextPosition - lineStartAtPosition)
        match reference with
        | :? MethodDef as def -> methodsPosition.[def] <- position
        | :? FieldDef as def -> fieldsPosition.[def] <- position
        | :? PropertyDef as def -> propertiesPosition.[def] <- position
        | :? EventDef as def -> eventsPosition.[def] <- position
        | :? TypeDef as def -> typesPosition.[def] <- position
        | _ -> ()

    override this.WriteLine() =
        base.WriteLine()
        line <- line + 1
        lineStartAtPosition <- this.Length

    override this.Write(text:string, reference:obj, flags:DecompilerReferenceFlags, color:obj ) =
        storeDefinitionIfNeeded this.NextPosition reference

        base.Write(text, reference, flags, color)

    override this.Write(text:string, index, length, reference:obj, flags:DecompilerReferenceFlags, color:obj ) =
        storeDefinitionIfNeeded this.NextPosition reference

        base.Write(text, index, length, reference, flags, color)

    member __.Positions
        with get () = OutputPositions (  methodsPosition :> IReadOnlyDictionary<MethodDef, Position>,
                                         fieldsPosition :> IReadOnlyDictionary<FieldDef, Position>,
                                         propertiesPosition :> IReadOnlyDictionary<PropertyDef, Position>,
                                         eventsPosition :> IReadOnlyDictionary<EventDef, Position>,
                                         typesPosition :> IReadOnlyDictionary<TypeDef, Position> )




let decompileType (typeDef:TypeDef) =
    let sw = new StringWriter()
    let output = new TextWriterWithPositions(sw :> TextWriter)

    let csDecompiler =
        DecompilerFactory.CreateCSharpVBDecompilerSettings()
        |> DecompilerFactory.CreateCSharpDecompiler

    let decompilationContext = new DecompilationContext();
    csDecompiler.Decompile(typeDef, output, decompilationContext)
    sw.GetStringBuilder() |> string, output.Positions

let tryResolveType (assemblyPath:string) typeName =
    let moduleContext = ModuleDef.CreateModuleContext(false)
    let moduleDef = ModuleDefMD.Load(assemblyPath, moduleContext)
    moduleDef.GetTypes()
    |> Seq.tryFind(fun x ->
        let fullName = FullNameCreator.FullName(x, false)
        fullName = typeName )

let tryDecompileType (assemblyPath:string) typeName =
    tryResolveType assemblyPath typeName
    |> Option.map decompileType

let rec formatExtTypeFullName externalType =
    match externalType with
    | ExternalType.Type (name, genericArgs) ->
        match genericArgs with
        | [] -> ""
        | args ->
            args
            |> List.map formatExtTypeFullName
            |> String.concat ","
            |> sprintf "<%s>"
        |> sprintf "%s%s" name
    | ExternalType.Array inner -> sprintf "%s[]" (formatExtTypeFullName inner)
    | ExternalType.Pointer inner -> sprintf "&%s" (formatExtTypeFullName inner)
    | ExternalType.TypeVar name -> sprintf "%s" name

let areSameTypes ((mParam,paramSym):Parameter*ParamTypeSymbol) =
    let compareToExternalType (extType:ExternalType) =
        let parameterTypeFullName = mParam.Type.FullName.Trim([| '&'; ' '|])
        let extTypeFullName = formatExtTypeFullName extType
        parameterTypeFullName = extTypeFullName

    match paramSym with
    | ParamTypeSymbol.Param extType -> compareToExternalType extType
    | ParamTypeSymbol.Byref extType ->
        mParam.Type.IsByRef && compareToExternalType extType


let getDeclaringTypeName = function
    | ExternalSymbol.Type (fullName) -> fullName
    | ExternalSymbol.Constructor (typeName, _args) -> typeName
    | ExternalSymbol.Method (typeName, _name, _paramSyms, _genericArity) -> typeName
    | ExternalSymbol.Field (typeName, _name) -> typeName
    | ExternalSymbol.Event (typeName, _name) -> typeName
    | ExternalSymbol.Property (typeName, _name) -> typeName

let findMethodFromArgs (args:ParamTypeSymbol list) (methods:MethodDef seq) =
    methods
    |> Seq.tryFind (fun m ->
        let mParams = m.Parameters|> Seq.where (fun mp -> mp.IsNormalMethodParameter) |> Seq.toArray

        mParams.Length = args.Length
        && (Seq.zip mParams args) |> Seq.forall areSameTypes )

type ExternalContentPosition =
  { File: string
    Column: int
    Line: int }

let toSafeFileNameRegex = 
    System.Text.RegularExpressions.Regex("[^\w\.`\s]+", RegexOptions.Compiled)
let toSafeFileName str =
    toSafeFileNameRegex.Replace(str, "_")

let decompile (externalSym:ExternalSymbol) assemblyPath =
    getDeclaringTypeName externalSym
    |> tryResolveType assemblyPath
    |> Option.map (fun typeDef ->

        let (contents, positions) = decompileType typeDef
        let fileName = sprintf "%s.cs" (toSafeFileName typeDef.AssemblyQualifiedName)
        let tempFile =
            System.IO.Path.GetTempPath() </> fileName

        System.IO.File.WriteAllText(tempFile, contents)


        let defPosition =
            match externalSym with
            | ExternalSymbol.Type _ -> positions.TryFindType typeDef
            | ExternalSymbol.Constructor (_typeName, args) ->
                typeDef.FindConstructors()
                |> findMethodFromArgs args
                |> Option.bind (fun def -> positions.TryFindMethod def)

            | ExternalSymbol.Method (_typeName, name, args, genericArity) ->
                typeDef.FindMethods( UTF8String name)
                |> Seq.where (fun (m:MethodDef) -> m.GenericParameters.Count = genericArity)
                |> findMethodFromArgs args
                |> Option.bind (fun def -> positions.TryFindMethod def)

            | ExternalSymbol.Field (_typeName, name) ->
                typeDef.FindField(UTF8String name)
                |> Option.ofObj
                |> Option.bind (fun def -> positions.TryFindField def)

            | ExternalSymbol.Event (_typeName, name) ->
                typeDef.FindEvent(UTF8String name)
                |> Option.ofObj
                |> Option.bind (fun def -> positions.TryFindEvent def)

            | ExternalSymbol.Property (_typeName, name) ->
                typeDef.FindProperty(UTF8String name)
                |> Option.ofObj
                |> Option.bind (fun def -> positions.TryFindProperty def)

        match defPosition with
        | Some (Position (line, column)) ->
            { File = tempFile
              Column = column
              Line = line }
        | None ->
            { File = tempFile
              Column = 0
              Line = 0 })



let tryFindExternalDeclaration (checkResults:FSharpCheckFileResults) (assembly, externalSym) =
    checkResults.ProjectContext.GetReferencedAssemblies()
    |> List.tryFind(fun x -> x.SimpleName = assembly)
    |> Option.bind(fun x -> x.FileName)
    |> Option.bind(decompile externalSym)
