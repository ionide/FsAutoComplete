module FsAutoComplete.Tests.Lsp.DecompilerTests

open System
open System.IO
open Expecto
open FsAutoComplete.Decompiler

/// Helpers for creating temporary filesystem structures used by the decompiler path-resolution tests.
module private TempFs =
  let private sep = string Path.DirectorySeparatorChar

  /// Creates a temp directory, calls `f` with its path, then deletes it.
  let withTempDir (f: string -> unit) =
    let dir = Path.Combine(Path.GetTempPath(), "FSAC_DecompilerTests", Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(dir) |> ignore

    try
      f dir
    finally
      try
        Directory.Delete(dir, recursive = true)
      with _ ->
        ()

  /// Creates all intermediate directories and then creates an empty file at `path`.
  let touchFile (path: string) =
    Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
    File.WriteAllText(path, "")

  /// Writes a minimal FrameworkList.xml with the given `frameworkName` attribute.
  let writeFrameworkList (path: string) (frameworkName: string) =
    Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
    let xml =
      sprintf
        """<?xml version="1.0" encoding="utf-8"?>
<FileList FrameworkName="%s">
</FileList>"""
        frameworkName

    File.WriteAllText(path, xml)

/// Unit tests for `FsAutoComplete.Decompiler.tryFindImplementationAssembly`.
///
/// The function has two path-based heuristics:
///  1. NuGet ref→lib  (`tryNugetLibFromRef`)
///  2. Targeting pack → shared SDK  (`trySharedSdkFromTargetingPack`)
let decompilerTests =
  testList
    "Decompiler.tryFindImplementationAssembly"
    [

      testList
        "NuGet ref→lib heuristic"
        [

          testCase "returns None when path has no 'ref' segment"
          <| fun _ ->
            // A regular assembly outside any "ref" folder should yield None.
            let path = Path.Combine("some", "package", "1.0.0", "lib", "net8.0", "Foo.dll")
            Expect.isNone (tryFindImplementationAssembly path) "Expected None for path with no 'ref' segment"

          testCase "returns None when 'ref' segment present but 'lib' counterpart does not exist"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              // Create ref assembly but NOT the lib counterpart.
              let refDll = Path.Combine(root, "some.package", "1.0.0", "ref", "net8.0", "Foo.dll")
              TempFs.touchFile refDll
              Expect.isNone (tryFindImplementationAssembly refDll) "Expected None when lib counterpart is missing")

          testCase "returns lib path when 'lib' counterpart exists"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              let refDll = Path.Combine(root, "some.package", "1.0.0", "ref", "net8.0", "Foo.dll")
              let libDll = Path.Combine(root, "some.package", "1.0.0", "lib", "net8.0", "Foo.dll")
              TempFs.touchFile refDll
              TempFs.touchFile libDll

              let result = tryFindImplementationAssembly refDll

              Expect.equal result (Some libDll) "Expected the lib counterpart to be returned")

          testCase "matches last 'ref' segment, not an earlier one in a package name"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              // Package name itself contains "ref" — the heuristic must match the *last* "ref" segment.
              let refDll =
                Path.Combine(root, "my.ref.package", "1.0.0", "ref", "net8.0", "Bar.dll")

              let libDll =
                Path.Combine(root, "my.ref.package", "1.0.0", "lib", "net8.0", "Bar.dll")

              TempFs.touchFile refDll
              TempFs.touchFile libDll

              let result = tryFindImplementationAssembly refDll

              Expect.equal result (Some libDll) "Expected lib path even when package name contains 'ref'")

        ]

      testList
        "targeting-pack → shared-SDK heuristic"
        [

          testCase "returns None when path is not inside a 'packs' directory"
          <| fun _ ->
            let path =
              Path.Combine(
                "dotnet",
                "notpacks",
                "Microsoft.NETCore.App.Ref",
                "8.0.6",
                "ref",
                "net8.0",
                "System.Runtime.dll"
              )

            Expect.isNone (tryFindImplementationAssembly path) "Expected None when not inside a 'packs' directory"

          testCase "returns None when FrameworkList.xml is absent"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              // Build the packs layout but omit FrameworkList.xml.
              let refDll =
                Path.Combine(root, "packs", "Microsoft.NETCore.App.Ref", "8.0.6", "ref", "net8.0", "System.Runtime.dll")

              TempFs.touchFile refDll
              // Do NOT create the FrameworkList.xml.
              Expect.isNone (tryFindImplementationAssembly refDll) "Expected None when FrameworkList.xml is absent")

          testCase "returns None when FrameworkList.xml has no FrameworkName attribute"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              let packVersionDir = Path.Combine(root, "packs", "Microsoft.NETCore.App.Ref", "8.0.6")

              let refDll =
                Path.Combine(packVersionDir, "ref", "net8.0", "System.Runtime.dll")

              TempFs.touchFile refDll

              let xmlPath = Path.Combine(packVersionDir, "data", "FrameworkList.xml")
              TempFs.writeFrameworkList xmlPath "" // empty FrameworkName

              Expect.isNone (tryFindImplementationAssembly refDll) "Expected None when FrameworkName attribute is empty")

          testCase "returns None when shared implementation DLL does not exist"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              let packVersionDir = Path.Combine(root, "packs", "Microsoft.NETCore.App.Ref", "8.0.6")

              let refDll =
                Path.Combine(packVersionDir, "ref", "net8.0", "System.Runtime.dll")

              TempFs.touchFile refDll

              let xmlPath = Path.Combine(packVersionDir, "data", "FrameworkList.xml")
              TempFs.writeFrameworkList xmlPath "Microsoft.NETCore.App"

              // Do NOT create the shared implementation DLL.
              Expect.isNone (tryFindImplementationAssembly refDll) "Expected None when shared implementation DLL is absent")

          testCase "returns shared implementation path when everything is in place"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              let packVersionDir = Path.Combine(root, "packs", "Microsoft.NETCore.App.Ref", "8.0.6")

              let refDll =
                Path.Combine(packVersionDir, "ref", "net8.0", "System.Runtime.dll")

              TempFs.touchFile refDll

              let xmlPath = Path.Combine(packVersionDir, "data", "FrameworkList.xml")
              TempFs.writeFrameworkList xmlPath "Microsoft.NETCore.App"

              let implDll = Path.Combine(root, "shared", "Microsoft.NETCore.App", "8.0.6", "System.Runtime.dll")
              TempFs.touchFile implDll

              let result = tryFindImplementationAssembly refDll

              Expect.equal result (Some implDll) "Expected the shared implementation DLL path to be returned")

        ]

      testList
        "priority: NuGet ref→lib is checked before targeting-pack heuristic"
        [

          testCase "prefers lib counterpart over shared-SDK path when both exist"
          <| fun _ ->
            TempFs.withTempDir (fun root ->
              // Construct a path that satisfies BOTH heuristics:
              // the "ref" segment is present (NuGet check) AND the directory structure
              // looks like a targeting pack (packs/…/ref/… with FrameworkList.xml).
              let packVersionDir = Path.Combine(root, "packs", "My.Package.Ref", "1.0.0")
              let refDll = Path.Combine(packVersionDir, "ref", "net8.0", "MyLib.dll")
              TempFs.touchFile refDll

              // NuGet lib counterpart
              let libDll = Path.Combine(packVersionDir, "lib", "net8.0", "MyLib.dll")
              TempFs.touchFile libDll

              // Targeting-pack shared DLL
              let xmlPath = Path.Combine(packVersionDir, "data", "FrameworkList.xml")
              TempFs.writeFrameworkList xmlPath "My.Runtime"

              let sharedDll = Path.Combine(root, "shared", "My.Runtime", "1.0.0", "MyLib.dll")
              TempFs.touchFile sharedDll

              let result = tryFindImplementationAssembly refDll

              Expect.equal result (Some libDll) "Expected NuGet lib path to be preferred over shared-SDK path")

        ]

    ]
