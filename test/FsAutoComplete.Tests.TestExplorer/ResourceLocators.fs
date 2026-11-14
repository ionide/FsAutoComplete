module internal ResourceLocators

open FsAutoComplete.TestServer
open System.IO

let tryFindVsTest () : string =
  let dotnetBinary =
    Ionide.ProjInfo.Paths.dotnetRoot.Value
    |> Option.defaultWith (fun () ->
      failwith "Couldn't find dotnet root. The dotnet sdk must be installed to run these tests")

  let cwd = System.Environment.CurrentDirectory |> Some

  VSTestWrapper.tryFindVsTestFromDotnetRoot dotnetBinary.FullName cwd
  |> Result.defaultWith failwith
  |> _.FullName

let sampleProjectsRootDir = Path.Combine(__SOURCE_DIRECTORY__, "SampleTestProjects")

/// A path where no test application has been built.
let missingApp =
  Path.Combine(sampleProjectsRootDir, "Mtp.Unbuilt/bin/Debug/net8.0/Mtp.Unbuilt.dll")

/// Runs <c>body</c> with a file that exists where a test application should be but is not one,
/// so launching it fails as a crashing application would.
let withBrokenApp (body: string -> Async<unit>) =
  async {
    let path =
      Path.Combine(Path.GetTempPath(), $"fsac-mtp-not-an-application-{System.Guid.NewGuid():N}.dll")

    File.WriteAllText(path, "not an assembly")

    try
      do! body path
    finally
      File.Delete path
  }
