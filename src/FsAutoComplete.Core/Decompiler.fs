module FsAutoComplete.Decompiler

open System
open System.IO
open System.Xml
open FSharp.Compiler.Text
open Utils
open System.Text.RegularExpressions
open ICSharpCode.Decompiler.CSharp
open ICSharpCode.Decompiler.CSharp.OutputVisitor
open ICSharpCode.Decompiler
open ICSharpCode.Decompiler.TypeSystem
open ICSharpCode.Decompiler.CSharp.Syntax
open ICSharpCode.Decompiler.CSharp.Transforms
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.EditorServices

type TextWriterWithLocationFinder(tokenWriter: TextWriterTokenWriter, formattingPolicy, symbol: ISymbol option) =
  inherit CSharpOutputVisitor(tokenWriter, formattingPolicy)

  let tokenWriter = tokenWriter

  let mutable matchingLocation = None

  let saveMatchingSymbol (node: #AstNode) =
    let result = node.GetResolveResult()

    match symbol with
    | Some s when result.GetSymbol() = s -> matchingLocation <- Some tokenWriter.Location
    | _ -> ()

  override this.VisitConstructorDeclaration(declaration: ConstructorDeclaration) =
    saveMatchingSymbol declaration
    ``base``.VisitConstructorDeclaration(declaration)

  override this.VisitMethodDeclaration(declaration: MethodDeclaration) =
    saveMatchingSymbol declaration
    ``base``.VisitMethodDeclaration(declaration)

  override this.VisitPropertyDeclaration(declaration: PropertyDeclaration) =
    saveMatchingSymbol declaration
    ``base``.VisitPropertyDeclaration(declaration)

  override this.VisitFieldDeclaration(declaration: FieldDeclaration) =
    saveMatchingSymbol declaration
    ``base``.VisitFieldDeclaration(declaration)

  override this.VisitEventDeclaration(declaration: EventDeclaration) =
    saveMatchingSymbol declaration
    ``base``.VisitEventDeclaration(declaration)

  override this.VisitTypeDeclaration(declaration: TypeDeclaration) =
    saveMatchingSymbol declaration
    ``base``.VisitTypeDeclaration(declaration)

  member __.MatchingLocation = matchingLocation

/// For NuGet packages: if the assembly is in a "ref" folder, try the corresponding "lib" folder.
/// E.g. .../some.package/1.0.0/ref/net8.0/Foo.dll → .../some.package/1.0.0/lib/net8.0/Foo.dll
let private tryNugetLibFromRef (assemblyPath: string) =
  let sep = [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]
  let parts = assemblyPath.Split(sep, StringSplitOptions.None)
  // Find last "ref" segment (to avoid matching "ref" in a package name)
  match parts |> Array.tryFindIndexBack (fun p -> p = "ref") with
  | None -> None
  | Some refIdx ->
    let candidate =
      [| yield! parts[.. refIdx - 1]; yield "lib"; yield! parts[refIdx + 1 ..] |]
      |> String.concat (string Path.DirectorySeparatorChar)

    if File.Exists(candidate) then Some candidate else None

/// For .NET targeting packs: maps packs/[packName]/[version]/ref/[tfm]/[dll]
/// to shared/[sdkName]/[version]/[dll] by reading FrameworkList.xml.
/// E.g. .../packs/Microsoft.NETCore.App.Ref/8.0.6/ref/net8.0/System.Runtime.dll
///   → .../shared/Microsoft.NETCore.App/8.0.6/System.Runtime.dll
let private trySharedSdkFromTargetingPack (assemblyPath: string) =
  let dllFileName = Path.GetFileName(assemblyPath)
  let tfmDir = Path.GetDirectoryName(assemblyPath) // e.g. .../ref/net8.0
  let refDir = Path.GetDirectoryName(tfmDir) // e.g. .../ref
  let versionDir = Path.GetDirectoryName(refDir) // e.g. .../8.0.6
  let packDir = Path.GetDirectoryName(versionDir) // e.g. .../Microsoft.NETCore.App.Ref
  let packsDir = Path.GetDirectoryName(packDir) // e.g. .../packs

  if Path.GetFileName(refDir) <> "ref" || Path.GetFileName(packsDir) <> "packs" then
    None
  else
    let packVersion = Path.GetFileName(versionDir)
    let dotnetRoot = Path.GetDirectoryName(packsDir)
    let frameworkXml = Path.Combine(versionDir, "data", "FrameworkList.xml")

    if not (File.Exists(frameworkXml)) then
      None
    else
      try
        use reader = XmlReader.Create(frameworkXml)
        reader.MoveToContent() |> ignore
        let sdkName = reader.GetAttribute("FrameworkName")

        if String.IsNullOrEmpty(sdkName) then
          None
        else
          let candidate =
            Path.Combine(dotnetRoot, "shared", sdkName, packVersion, dllFileName)

          if File.Exists(candidate) then Some candidate else None
      with _ ->
        None

/// Tries to find the actual implementation assembly for a given assembly path.
/// Reference assemblies (e.g. from targeting packs or NuGet ref folders) contain
/// only signatures with stub bodies (`throw null`). When an implementation assembly
/// is available at a predictable location, returning it allows the decompiler to
/// show real method bodies.
let tryFindImplementationAssembly (assemblyPath: string) =
  match tryNugetLibFromRef assemblyPath with
  | Some p -> Some p
  | None -> trySharedSdkFromTargetingPack assemblyPath

let decompilerForFile (file: string) =
  let settings =
    DecompilerSettings(CSharp.LanguageVersion.Latest, ThrowOnAssemblyResolveErrors = true)

  let decompiler = CSharpDecompiler(file, settings)
  decompiler.AstTransforms.Add(EscapeInvalidIdentifiers())
  decompiler

let decompileTypeAndFindLocation
  (decompiler: CSharpDecompiler)
  (typeDefinition: ITypeDefinition)
  (symbol: ISymbol option)
  =
  let sw = new StringWriter()

  let writerVisitor =
    TextWriterWithLocationFinder(TextWriterTokenWriter(sw), FormattingOptionsFactory.CreateSharpDevelop(), symbol)

  let syntaxTree = typeDefinition.FullTypeName |> decompiler.DecompileType

  syntaxTree.AcceptVisitor(writerVisitor)
  sw.GetStringBuilder() |> string, writerVisitor.MatchingLocation

let resolveType (typeSystem: IDecompilerTypeSystem) (typeName: string) =
  typeName |> FullTypeName |> typeSystem.MainModule.GetTypeDefinition

let rec formatExtTypeFullName externalType =
  match externalType with
  | FindDeclExternalType.Type(name, genericArgs) ->
    (match genericArgs with
     | [] -> ""
     | args ->
       args
       |> List.map (formatExtTypeFullName >> sprintf "[%s]")
       |> String.concat ","
       |> sprintf "[%s]")
    |> sprintf "%s%s" name
  | FindDeclExternalType.Array inner -> sprintf "%s[]" (formatExtTypeFullName inner)
  | FindDeclExternalType.Pointer inner -> sprintf "&%s" (formatExtTypeFullName inner)
  | FindDeclExternalType.TypeVar name -> sprintf "%s" name

let areSameTypes (typeArguments: IReadOnlyList<IType>) ((mParam, paramSym): IParameter * FindDeclExternalParam) =
  let compareToExternalType (extType: FindDeclExternalType) =
    let parameterTypeFullName =
      typeArguments
      |> Seq.fold
        (fun (str: string) t -> str.Replace(t.ReflectionName, t.Name))
        (mParam.Type.ReflectionName.Trim([| '&'; ' ' |]))

    let extTypeFullName = formatExtTypeFullName extType
    parameterTypeFullName = extTypeFullName

  if paramSym.IsByRef then
    mParam.IsRef && compareToExternalType paramSym.ParameterType
  else
    compareToExternalType paramSym.ParameterType


let getDeclaringTypeName =
  function
  | FindDeclExternalSymbol.Type(fullName) -> fullName
  | FindDeclExternalSymbol.Constructor(typeName, _args) -> typeName
  | FindDeclExternalSymbol.Method(typeName, _name, _paramSyms, _genericArity) -> typeName
  | FindDeclExternalSymbol.Field(typeName, _name) -> typeName
  | FindDeclExternalSymbol.Event(typeName, _name) -> typeName
  | FindDeclExternalSymbol.Property(typeName, _name) -> typeName

let findMethodFromArgs (args: FindDeclExternalParam list) (methods: IMethod seq) =
  methods
  |> Seq.tryFind (fun m ->
    let mParams = m.Parameters

    mParams.Count = args.Length
    && (Seq.zip mParams args) |> Seq.forall (areSameTypes m.TypeArguments))

type ExternalContentPosition =
  { File: string
    Position: FSharp.Compiler.Text.Position }

let toSafeFileNameRegex =
  System.Text.RegularExpressions.Regex("[^\w\.`\s]+", RegexOptions.Compiled)

let toSafeFileName (typeDef: ITypeDefinition) =
  let str = sprintf "%s %s.cs" typeDef.FullName typeDef.ParentModule.FullAssemblyName
  toSafeFileNameRegex.Replace(str, "_")

type DecompileError = Exception of symbol: FindDeclExternalSymbol * filePath: string * error: exn

let decompile (externalSym: FindDeclExternalSymbol) assemblyPath : Result<ExternalContentPosition, DecompileError> =
  try
    let decompiler = decompilerForFile assemblyPath

    let typeSystem = decompiler.TypeSystem

    let typeDef = getDeclaringTypeName externalSym |> resolveType typeSystem

    let symbol =
      match externalSym with
      | FindDeclExternalSymbol.Type _ -> Some(typeDef :> ISymbol)
      | FindDeclExternalSymbol.Constructor(_typeName, args) ->
        typeDef.GetConstructors()
        |> findMethodFromArgs args
        |> Option.map (fun x -> x :> ISymbol)

      | FindDeclExternalSymbol.Method(_typeName, name, args, genericArity) ->
        typeDef.GetMethods(filter = Predicate(fun m -> m.Name = name))
        |> Seq.where (fun m -> m.TypeParameters.Count = genericArity)
        |> findMethodFromArgs args
        |> Option.map (fun x -> x :> ISymbol)

      | FindDeclExternalSymbol.Field(_typeName, name) ->
        typeDef.GetFields(filter = Predicate(fun m -> m.Name = name))
        |> Seq.tryHead
        |> Option.map (fun x -> x :> ISymbol)

      | FindDeclExternalSymbol.Event(_typeName, name) ->
        typeDef.GetEvents(filter = Predicate(fun m -> m.Name = name))
        |> Seq.tryHead
        |> Option.map (fun x -> x :> ISymbol)

      | FindDeclExternalSymbol.Property(_typeName, name) ->
        typeDef.GetProperties(filter = Predicate(fun m -> m.Name = name))
        |> Seq.tryHead
        |> Option.map (fun x -> x :> ISymbol)

    let (contents, location) = decompileTypeAndFindLocation decompiler typeDef symbol

    let fileName = typeDef |> toSafeFileName
    let tempFile = System.IO.Path.GetTempPath() </> fileName

    System.IO.File.WriteAllText(tempFile, contents)

    match location with
    | Some l ->
      Ok
        { File = tempFile
          // external library columns are 1-based, not 0-based like FCS Pos columns
          Position = FSharp.Compiler.Text.Position.mkPos l.Line (l.Column - 1) }
    | None ->
      Ok
        { File = tempFile
          Position = FSharp.Compiler.Text.Position.pos0 }
  with e ->
    Result.Error(Exception(externalSym, assemblyPath, e))

type FindExternalDeclarationError =
  | ReferenceNotFound of assembly: string
  | ReferenceHasNoFileName of assembly: FSharpAssembly
  | DecompileError of error: DecompileError

let tryFindExternalDeclaration
  (checkResults: FSharpCheckFileResults)
  (assembly, externalSym: FindDeclExternalSymbol)
  : Result<ExternalContentPosition, FindExternalDeclarationError> =
  match
    checkResults.ProjectContext.GetReferencedAssemblies()
    |> List.tryFind (fun x -> x.SimpleName = assembly)
  with
  | None -> Result.Error(ReferenceNotFound assembly)
  | Some assembly when assembly.FileName = None -> Result.Error(ReferenceHasNoFileName assembly)
  | Some assembly ->
    let assemblyPath =
      assembly.FileName.Value
      |> tryFindImplementationAssembly
      |> Option.defaultValue assembly.FileName.Value

    match decompile externalSym assemblyPath with
    | Error err -> Result.Error(DecompileError err)
    | Ok result -> Ok result
