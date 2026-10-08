module TestPlatformTests

open Expecto
open FsAutoComplete.TestServer
open Ionide.ProjInfo

let private emptyProject: Types.ProjectOptions =
  { ProjectId = None
    ProjectFileName = "/repo/Tests.fsproj"
    TargetFramework = "net8.0"
    SourceFiles = []
    OtherOptions = []
    ReferencedProjects = []
    PackageReferences = []
    LoadTime = System.DateTime.MinValue
    TargetPath = "/repo/bin/Tests.dll"
    TargetRefPath = None
    ProjectOutputType = Types.ProjectOutputType.Library
    ProjectSdkInfo =
      { IsTestProject = false
        Configuration = ""
        IsPackable = false
        TargetFramework = "net8.0"
        TargetFrameworkIdentifier = ""
        TargetFrameworkVersion = ""
        MSBuildAllProjects = []
        MSBuildToolsVersion = ""
        ProjectAssetsFile = ""
        RestoreSuccess = true
        Configurations = []
        TargetFrameworks = []
        RunArguments = None
        RunCommand = None
        IsPublishable = None }
    Items = []
    Properties = []
    CustomProperties = []
    AllProperties = Map.empty
    AllItems = Map.empty
    Analyzers = [] }

let private withPackages names project =
  { project with
      Types.ProjectOptions.PackageReferences =
        names
        |> List.map (fun name ->
          { Types.PackageReference.Name = name
            Version = "1.0.0"
            FullPath = $"/packages/{name}" }) }

let private withProperty name value project =
  { project with
      Types.ProjectOptions.CustomProperties =
        [ { Types.Property.Name = name
            Value = value } ] }

let private vsTestPackages =
  withPackages [ "Microsoft.NET.Test.Sdk"; "Microsoft.TestPlatform.TestHost" ]

[<Tests>]
let tests =
  testList
    "TestProject.classify"
    [ testCase "a project without tests has no platform"
      <| fun _ ->
        let actual = TestProject.classify emptyProject

        Expect.isNone actual "a library is not a test project"

      testCase "the VSTest packages alone select VSTest"
      <| fun _ ->
        let actual = emptyProject |> vsTestPackages |> TestProject.classify

        Expect.equal actual (Some TestPlatformKind.VSTest) "the test SDK runs under VSTest"

      testCase "IsTestingPlatformApplication selects MTP"
      <| fun _ ->
        let actual =
          emptyProject
          |> vsTestPackages
          |> withProperty "IsTestingPlatformApplication" "true"
          |> TestProject.classify

        Expect.equal actual (Some TestPlatformKind.Mtp) "the property overrides the packages"

      testCase "a testing platform application needs no VSTest packages"
      <| fun _ ->
        let actual =
          emptyProject
          |> withProperty "IsTestingPlatformApplication" "true"
          |> TestProject.classify

        Expect.equal actual (Some TestPlatformKind.Mtp) "a pure MTP application carries the property alone"

      testCase "IsTestingPlatformApplication of 'false' leaves VSTest"
      <| fun _ ->
        let actual =
          emptyProject
          |> vsTestPackages
          |> withProperty "IsTestingPlatformApplication" "false"
          |> TestProject.classify

        Expect.equal actual (Some TestPlatformKind.VSTest) "an opted-out project runs under VSTest"

      testCase "an unparseable property value leaves VSTest"
      <| fun _ ->
        let actual =
          emptyProject
          |> vsTestPackages
          |> withProperty "IsTestingPlatformApplication" ""
          |> TestProject.classify

        Expect.equal actual (Some TestPlatformKind.VSTest) "an ambiguous project runs under VSTest"

      testCase "the loader is asked for the classifying properties"
      <| fun _ ->
        Expect.contains
          TestProject.requiredCustomProperties
          "IsTestingPlatformApplication"
          "classification reads a property the loader must be told to retain" ]

[<Tests>]
let platformSelectionTests =
  let mtpDisabled = TestProject.platformFor false
  let mtpEnabled = TestProject.platformFor true

  testList
    "TestProject.platformFor"
    [ testCase "a plain VSTest project is unaffected by the flag"
      <| fun _ ->
        let project = emptyProject |> vsTestPackages

        Expect.equal (mtpDisabled project) (Some TestPlatformKind.VSTest) "the flag is off"
        Expect.equal (mtpEnabled project) (Some TestPlatformKind.VSTest) "the flag is on"

      testCase "a disabled flag keeps a bridged project on VSTest"
      <| fun _ ->
        let actual =
          emptyProject
          |> vsTestPackages
          |> withProperty "IsTestingPlatformApplication" "true"
          |> mtpDisabled

        Expect.equal actual (Some TestPlatformKind.VSTest) "the VSTest packages still run it"

      testCase "a disabled flag hides a project only MTP can run"
      <| fun _ ->
        let actual =
          emptyProject
          |> withProperty "IsTestingPlatformApplication" "true"
          |> mtpDisabled

        Expect.isNone actual "no platform available can run it"

      testCase "an enabled flag selects MTP for a bridged project"
      <| fun _ ->
        let actual =
          emptyProject
          |> vsTestPackages
          |> withProperty "IsTestingPlatformApplication" "true"
          |> mtpEnabled

        Expect.equal actual (Some TestPlatformKind.Mtp) "the property wins once MTP is available"

      testCase "an enabled flag selects MTP for a project only MTP can run"
      <| fun _ ->
        let actual =
          emptyProject |> withProperty "IsTestingPlatformApplication" "true" |> mtpEnabled

        Expect.equal actual (Some TestPlatformKind.Mtp) "a pure MTP application becomes reachable"

      testCase "a project without tests stays hidden under either flag"
      <| fun _ ->
        Expect.isNone (mtpDisabled emptyProject) "the flag is off"
        Expect.isNone (mtpEnabled emptyProject) "the flag is on" ]
