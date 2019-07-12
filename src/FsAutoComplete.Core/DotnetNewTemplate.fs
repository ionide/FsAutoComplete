namespace FsAutoComplete

module DotnetNewTemplate =
  type Template = {
    Name : string;
    ShortName : string;
    Language: TemplateLanguage list;
    Tags: string list
  } 
  and TemplateLanguage = CSharp | FSharp | VB
  and DetailedTemplate = {
    TemplateName : string;
    Author : string;
    TemplateDescription : string;
    Options : TemplateParameter list;
  }
  and TemplateParameter = {
    ParameterName : string;
    ShortName : string;
    ParameterType : TemplateParameterType;
    ParameterDescription : string;
    DefaultValue : string
  }
  and TemplateParameterType =
  | Bool
  | String
  | Choice of string list

  let installedTemplates () : Template list =
    [ { Name = "Console Application";
        ShortName = "console";
        Language = [ TemplateLanguage.CSharp; TemplateLanguage.FSharp; TemplateLanguage.VB ];
        Tags = ["Common"; "Console"] };

      { Name = "Class library";
        ShortName = "classlib";
        Language = [ TemplateLanguage.CSharp; TemplateLanguage.FSharp; TemplateLanguage.VB ];
        Tags = ["Common"; "Library"]  }; 
    ]

  let templateDetails () : DetailedTemplate list =
    [
      { TemplateName = "Console Application";
        Author = "Microsoft";
        TemplateDescription = "A project for creating a command-line application that can run on .NET Core on Windows, Linux and macOS";
        Options = 
        [ { ParameterName = "--no-restore";
            ShortName = "";
            ParameterType = TemplateParameterType.Bool;
            ParameterDescription = "If specified, skips the automatic restore of the project on create.";
            DefaultValue = "false / (*) true" }; 
        ] };

      { TemplateName = "Class library";
        Author = "Microsoft";
        TemplateDescription = "A project for creating a class library that targets .NET Standard or .NET Core";
        Options = 
        [ { ParameterName = "--framework";
            ShortName = "-f";
            ParameterType = TemplateParameterType.Choice ["netcoreapp2.1     - Target netcoreapp2.1";"netstandard2.0    - Target netstandard2.0"];
            ParameterDescription = "The target framework for the project.";
            DefaultValue = "netstandard2.0" };
            
          { ParameterName = "--no-restore";
            ShortName = "";
            ParameterType = TemplateParameterType.Bool;
            ParameterDescription = "If specified, skips the automatic restore of the project on create.";
            DefaultValue = "false / (*) true" }; 

        ] };
    ]
  
  let isMatch (filterstr: string) (x: string) =
    x.ToLower().Contains(filterstr.ToLower())

  let nameMatch (filterstr: string) (x: string) =
    x.ToLower() = filterstr.ToLower()

  let extractString (t : Template) =
    [t.Name; t.ShortName]

  let extractDetailedString (t : DetailedTemplate) =
    [t.TemplateName]

  let dotnetnewlist (userInput : string) =
    installedTemplates ()
    |> List.map (fun t -> t, extractString t) // extract keywords from the template
    |> List.filter (fun (t, strings) ->  strings |> List.exists (isMatch userInput)) // check if a keyword match the user string
    |> List.map (fun (t,strings) -> t)  // return the template

  let dotnetnewgetDetails (userInput : string) =
    let templates =
      templateDetails ()
      |> List.map (fun t -> t, extractDetailedString t)
      |> List.filter (fun (t,strings) -> strings |> List.exists (nameMatch userInput))
      |> List.map (fun (t,strings) -> t)
    
    match templates with 
    | [] -> failwithf "No template exists with name : %s" userInput
    | [x] -> x
    | _ -> failwithf "Multiple templates found : \n%A" templates

  let dotnetnewCreateCli (templateShortName : string) (parameterList : (string * obj) list) : (string * string) =
    let result1 = "dotnet "
    let str = "new " + templateShortName

    // let plist =
    //   parameterList
    //   |> 

    ("a", "b")