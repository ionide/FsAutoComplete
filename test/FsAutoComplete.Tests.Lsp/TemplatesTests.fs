module FsAutoComplete.Tests.Templates

open Expecto

let withEnvironmentVariable (variable: string) (value: string) (f: unit -> unit) =
  let priorValue = System.Environment.GetEnvironmentVariable variable
  System.Environment.SetEnvironmentVariable(variable, value)

  try
    f ()
  finally
    // if the env variable was inexistant before, priorValue is null and the call will delete the variable
    System.Environment.SetEnvironmentVariable(variable, priorValue)

let tests () =
  testList
    "Templates Tests"
    [ testCase "Templates are not empty"
      <| fun () ->
           FsAutoComplete.DotnetNewTemplate.installedTemplates ()
           |> Expect.isNonEmpty
           <| "Templates are empty"
      testCase "Listing doesn't fail when DOTNET_CLI_UI_LANGUAGE is set by the user"
      <| fun () ->
           withEnvironmentVariable "DOTNET_CLI_UI_LANGUAGE" "fr-fr"
           <| fun () ->
                FsAutoComplete.DotnetNewTemplate.installedTemplates ()
                |> Expect.isNonEmpty
                <| "Parsing failed"
      testCase "Detailed templates are not empty"
      <| fun () ->
           FsAutoComplete.DotnetNewTemplate.templateDetails ()
           |> Expect.isNonEmpty
           <| "Detailed templates are empty" ]
