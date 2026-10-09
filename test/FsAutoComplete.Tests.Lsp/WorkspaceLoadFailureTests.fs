module FsAutoComplete.Tests.Lsp.WorkspaceLoadFailureTests

open System
open System.IO
open System.Threading
open Expecto
open Helpers
open Ionide.ProjInfo
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

/// A workspace loader that fails the way MSBuild does when it cannot be loaded into the process.
type private ThrowingLoader() =
  let notifications = Event<Types.WorkspaceProjectState>()
  let loaded = new SemaphoreSlim(0)

  member _.WaitForLoad() = loaded.WaitAsync(TimeSpan.FromSeconds 30.) |> Async.AwaitTask

  member private _.Fail() : seq<Types.ProjectOptions> =
    loaded.Release() |> ignore
    raise (FileNotFoundException "Could not load file or assembly 'Microsoft.Build'")

  interface IWorkspaceLoader with
    member x.LoadProjects(_, _, _) = x.Fail()
    member x.LoadProjects(_) = x.Fail()
    member x.LoadSln(_) = x.Fail()
    member x.LoadSln(_, _, _) = x.Fail()

    [<CLIEvent>]
    member _.Notifications = notifications.Publish

let tests sourceTextFactory =
  testCaseAsync "a workspace loader that throws does not end the server"
  <| async {
    let directory = Directory.CreateTempSubdirectory "fsac-load-failure"

    try
      let project = Path.Combine(directory.FullName, "Failing.fsproj")
      let source = Path.Combine(directory.FullName, "Library.fs")
      File.WriteAllText(project, "<Project Sdk=\"Microsoft.NET.Sdk\" />")
      File.WriteAllText(source, "module Library\n\nlet x = 1\n")

      let loader = ThrowingLoader()

      let server, _events =
        createAdaptiveServer (fun () -> loader) sourceTextFactory false

      use _ = server

      let! initialized =
        server.Initialize
          { ProcessId = Some 1
            RootPath = Some directory.FullName
            RootUri = Some(Path.FilePathToUri directory.FullName)
            InitializationOptions = Some(Server.serialize defaultConfigDto)
            Capabilities = clientCaps
            ClientInfo = None
            WorkspaceFolders =
              Some
                [| { Uri = Path.FilePathToUri directory.FullName
                     Name = "Test Folder" } |]
            Trace = None
            Locale = None
            WorkDoneToken = None }

      Expect.isOk initialized "Initialize succeeds"
      do! server.Initialized()
      let! loaded = loader.WaitForLoad()
      Expect.isTrue loaded "The server loads the workspace"

      // The server retries the load in the background 200 ms after the projects went out of date. Nothing signals
      // that retry, so give it time to fail: before the fix, that failure ended the process.
      do! Async.Sleep(TimeSpan.FromSeconds 1.)

      do!
        server.TextDocumentDidOpen
          { TextDocument =
              { Uri = Path.FilePathToUri source
                LanguageId = "fsharp"
                Version = 1
                Text = File.ReadAllText source } }

      let! hover =
        server.TextDocumentHover
          { TextDocument = { Uri = Path.FilePathToUri source }
            Position = { Line = 2u; Character = 4u }
            WorkDoneToken = None }

      Expect.isError hover "A request that needs the projects reports the failure"
    finally
      directory.Delete(true)
  }
