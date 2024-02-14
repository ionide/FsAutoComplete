open Fake.Core
open Fake.IO
open Fake.Tools

System.Environment.CurrentDirectory <- (Path.combine __SOURCE_DIRECTORY__ "..")

let init args =
  let execContext = Context.FakeExecutionContext.Create false "build.fsx" args
  Context.setExecutionContext (Context.RuntimeContext.Fake execContext)
  Target.initEnvironment ()

  Target.create "EnsureRepoConfig" (fun _ ->
    // Configure custom git hooks
    // * Currently only used to ensure that code is formatted before pushing
    Git.CommandHelper.gitCommand "" "config core.hooksPath .githooks")

  Target.create "ScaffoldCodeFix" (fun ctx ->
    let codeFixName = ctx.Context.Arguments |> List.tryHead

    match codeFixName with
    | None -> failwith "Usage: dotnet run --project ./build/build.fsproj -- -t ScaffoldCodeFix <name>"
    | Some codeFixName -> ScaffoldCodeFix.scaffold codeFixName)

  Target.create "EnsureCanScaffoldCodeFix" (fun _ -> ScaffoldCodeFix.ensureScaffoldStillWorks ())

[<EntryPoint>]
let main args =
  init (args |> List.ofArray)

  try
    Target.runOrDefaultWithArguments "EnsureCanScaffoldCodeFix"
    0
  with e ->
    printfn "%A" e
    1
